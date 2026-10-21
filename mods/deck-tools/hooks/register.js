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

export function register(on) {
  on('session.start', async ($, e, next) => {
    await $.command.register({
      name: 'render-deck',
      description: 'Render a deck and report errors and slides over 900px',
      argumentHint: '<lec9 | lab4/lab4_instr> [more decks]',
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
}
