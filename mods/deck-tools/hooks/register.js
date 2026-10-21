// /render-deck <deck> [<deck> ...]
//   deck = "lec9" (shorthand for lec9/lec9) or "lab4/lab4_instr" (no .qmd)
// Renders each deck with quarto, then runs scripts/check_slide_overflow.R,
// and prints a short report. Runs without a Claude turn, so it costs no tokens.

const DECK_PATTERN = /^[A-Za-z0-9_\-]+(\/[A-Za-z0-9_\-]+)?$/

// Turn "lec9" into "lec9/lec9"; leave "lab4/lab4_instr" alone
function deckPath(arg) {
  return arg.includes('/') ? arg : arg + '/' + arg
}

// Keep the lines of quarto's output that matter (quarto colors them with ANSI codes)
function problemLines(text) {
  const clean = text.replace(/\u001b\[[0-9;]*m/g, '')
  const hits = clean
    .split('\n')
    .filter((line) => /^\s*(ERROR|WARNING|Error|Warning)\b|Error in |Execution halted/.test(line))
  return hits.slice(0, 8)
}

async function renderOne($, arg) {
  const deck = deckPath(arg)
  const lines = []

  const render = await $.process.run(['quarto', 'render', deck + '.qmd'], { timeoutMs: 300000 })
  const renderText = render.stdout + '\n' + render.stderr
  if (render.exitCode !== 0) {
    lines.push('RENDER FAILED: ' + deck)
    const found = problemLines(renderText)
    for (const l of found.length ? found : [renderText.replace(/\u001b\[[0-9;]*m/g, '').trim().split('\n')[0]]) lines.push('  ' + l)
    return lines.join('\n')
  }
  const warnings = problemLines(renderText)
  lines.push('Rendered: ' + deck + (warnings.length ? ' (with warnings)' : ''))
  for (const l of warnings) lines.push('  ' + l)

  const check = await $.process.run(['Rscript', 'scripts/check_slide_overflow.R', deck], {
    timeoutMs: 400000,
  })
  const slides = check.stdout.match(/== .*slides/)
  const over = check.stdout
    .split('\n')
    .filter((l) => /slide \d+ \(\d+px\)/.test(l) && !/slide 1 \(/.test(l))
  if (check.exitCode !== 0 && !slides) {
    lines.push('  Overflow check could not run: ' + check.stderr.split('\n').slice(-3).join(' '))
  } else if (over.length === 0) {
    lines.push('  No slides over 900px (title slide always reports ~980px, ignored).')
  } else {
    lines.push('  Slides over 900px:')
    for (const l of over) lines.push('  ' + l.trim())
  }
  return lines.join('\n')
}

// ---- /commit-decks ----
// /commit-decks <dir> [<dir> ...] [--go] [--push] [-m <message>]
//   dir = a lecture/lab folder name, e.g. lec9 or lab4.
// Default is a PREVIEW: lists what would be staged and what is left alone.
//   --go    stage the named dirs (plus their _freeze/ and docs/ output) and commit; needs -m
//   --push  with --go, also push
// Each path is staged on its own, so one missing path can never abort the rest.
// Nothing outside the named dirs (and docs/search.json) is ever staged.

const DIR_PATTERN = /^[A-Za-z0-9_\-]+$/

async function git($, args) {
  return $.process.run(['git'].concat(args), { timeoutMs: 120000 })
}

// The paths a deck dir owns: source, frozen output, rendered site output
async function pathsFor($, dir) {
  const candidates = [dir, '_freeze/' + dir, 'docs/' + dir]
  const found = []
  for (const p of candidates) {
    if (await $.fs.exists(p)) found.push(p)
  }
  return found
}

function parse(argText) {
  const tokens = argText.trim().split(/\s+/).filter(Boolean)
  const dirs = []
  let go = false
  let push = false
  let message = ''
  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i]
    if (t === '--go') go = true
    else if (t === '--push') push = true
    else if (t === '-m') {
      message = tokens.slice(i + 1).join(' ')
      break
    } else dirs.push(t)
  }
  return { dirs, go, push, message }
}

async function runCommit($, argText) {
  const { dirs, go, push, message } = parse(argText)
  if (dirs.length === 0) {
    return 'Usage: /commit-decks lec9 lab4 [--go] [--push] [-m message]\nWithout --go this is a preview only.'
  }
  const bad = dirs.filter((d) => !DIR_PATTERN.test(d))
  if (bad.length) return 'Not a folder name: ' + bad.join(', ') + ' (use e.g. lec9 or lab4)'

  const paths = []
  const missing = []
  for (const d of dirs) {
    const found = await pathsFor($, d)
    if (found.length === 0) missing.push(d)
    for (const p of found) paths.push(p)
  }
  if (missing.length) return 'No such folder: ' + missing.join(', ')
  if (await $.fs.exists('docs/search.json')) paths.push('docs/search.json')

  const lines = []

  // What the named paths would add
  const mine = await git($, ['status', '--short', '--'].concat(paths))
  const changed = mine.stdout.split('\n').filter(Boolean)
  lines.push('Changes in ' + dirs.join(', ') + ' (' + changed.length + ' files):')
  lines.push(changed.length ? changed.slice(0, 40).map((l) => '  ' + l).join('\n') : '  (nothing to commit)')
  if (changed.length > 40) lines.push('  ... and ' + (changed.length - 40) + ' more')

  // What stays untouched
  const all = await git($, ['status', '--short'])
  const mineSet = new Set(changed)
  const others = all.stdout.split('\n').filter((l) => l && !mineSet.has(l))
  if (others.length) {
    lines.push('Left alone (' + others.length + '):')
    for (const l of others.slice(0, 12)) lines.push('  ' + l)
    if (others.length > 12) lines.push('  ... and ' + (others.length - 12) + ' more')
  }

  if (!go) {
    lines.push('')
    lines.push('Preview only. To commit: /commit-decks ' + dirs.join(' ') + ' --go -m your message  (add --push to push)')
    return lines.join('\n')
  }

  if (!message) return lines.join('\n') + '\n\nNeed a message: add -m your message at the end.'
  if (changed.length === 0) return lines.join('\n') + '\n\nNothing to commit.'

  // Refuse if something else is already staged, so it can't ride along
  const pre = await git($, ['diff', '--cached', '--name-only'])
  if (pre.stdout.trim()) {
    return lines.join('\n') + '\n\nStopped: other files are already staged:\n' + pre.stdout.trim() + '\nUnstage them (git restore --staged <file>) and run again.'
  }

  // Stage each path on its own (-A so deletions of renumbered figures are included)
  for (const p of paths) {
    const r = await git($, ['add', '-A', '--', p])
    if (r.exitCode !== 0) return lines.join('\n') + '\n\ngit add failed for ' + p + ': ' + r.stderr.trim()
  }

  const commit = await git($, ['commit', '-m', message])
  if (commit.exitCode !== 0) {
    return lines.join('\n') + '\n\nCommit failed: ' + (commit.stderr.trim() || commit.stdout.trim())
  }
  const first = commit.stdout.split('\n')[0]
  lines.push('')
  lines.push('Committed: ' + first)

  if (push) {
    const pushed = await git($, ['push'])
    const out = (pushed.stderr + '\n' + pushed.stdout).trim().split('\n').slice(-1)[0]
    lines.push(pushed.exitCode === 0 ? 'Pushed: ' + out : 'Push failed: ' + out)
  }
  return lines.join('\n')
}

// ---- registration ----
export function register(on) {
  on('session.start', async ($, e, next) => {
    await $.command.register({
      name: 'render-deck',
      description: 'Render a deck and report errors and slides over 900px',
      argumentHint: '<lec9 | lab4/lab4_instr> [more decks]',
    })
    await $.command.register({
      name: 'commit-decks',
      description: 'Preview, commit, and push only the named deck folders',
      argumentHint: '<lec9 lab4 ...> [--go] [--push] [-m message]',
    })
    return next(e)
  })

  on('command.run', { command: 'render-deck' }, async ($, e) => {
    const args = e.args.trim().split(/\s+/).filter(Boolean)
    if (args.length === 0) return { text: 'Usage: /render-deck lec9 [lab4/lab4 ...]' }
    const bad = args.filter((a) => !DECK_PATTERN.test(a))
    if (bad.length) return { text: 'Not a deck name: ' + bad.join(', ') + ' (use e.g. lec9 or lab4/lab4_instr)' }

    const reports = []
    for (const arg of args) {
      try {
        reports.push(await renderOne($, arg))
      } catch (err) {
        reports.push('Could not run for ' + arg + ': ' + String(err && err.message ? err.message : err))
      }
    }
    return { text: reports.join('\n\n') }
  })

  on('command.run', { command: 'commit-decks' }, async ($, e) => {
    try {
      return { text: await runCommit($, e.args) }
    } catch (err) {
      return { text: 'commit-decks failed: ' + String(err && err.message ? err.message : err) }
    }
  })
}
