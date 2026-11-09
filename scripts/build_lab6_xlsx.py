"""Rebuild lab6/lab6_instr.xlsx (sheets: Lab 6 Key, Data, Worked Example, Worked Data).

Run from the repo root:   python3 scripts/build_lab6_xlsx.py
Needs: xlsxwriter, and Rscript on the PATH. Statistics come from R (matching lab6.qmd) and are
written as cached values next to live Excel formulas. Paired data: columns B (condition 1,
"x") and C (condition 2, "y"); D = x - y is computed in the sheet.
"""
import subprocess, xlsxwriter

R_CODE = """
stats <- function(x, y) {
  D <- x - y; n <- length(D); db <- mean(D); sdd <- sd(D); se <- sdd / sqrt(n); df <- n - 1
  t2 <- t.test(x = x, y = y, paired = TRUE, alternative = "two.sided")
  tg <- t.test(x = x, y = y, paired = TRUE, alternative = "greater")
  tl <- t.test(x = x, y = y, paired = TRUE, alternative = "less")
  ind <- t.test(x = x, y = y, paired = FALSE, var.equal = TRUE)
  cat(sprintf("%s=%.16g", c("n","db","ss","sd","se","t","df","crit2","crit1","p2","lo","hi","pg","pl","dz","pind","tind","r"),
      c(n, db, sum((D - db)^2), sdd, se, db / se, df, qt(0.975, df), qt(0.95, df), t2$p.value,
        t2$conf.int[1], t2$conf.int[2], tg$p.value, tl$p.value, db / sdd, ind$p.value, ind$statistic, cor(x, y))), sep = "\n")
  cat("X=", paste(x, collapse = ","), "\n", sep = "")
  cat("Y=", paste(y, collapse = ","), "\n", sep = "")
}
set.seed(seed = 133)
near <- round(x = rnorm(n = 10, mean = 14, sd = 3), digits = 0)
away <- round(x = near + 1.5 + rnorm(n = 10, mean = 0, sd = 2.2), digits = 0)
cat("## RECALL\n"); stats(away, near)
set.seed(seed = 6)
before <- round(x = rnorm(n = 12, mean = 20, sd = 4), digits = 0)
after  <- round(x = before - 3 + rnorm(n = 12, mean = 0, sd = 2.5), digits = 0)
cat("## ANXIETY\n"); stats(after, before)
"""

def r_values():
    out = subprocess.run(["Rscript", "-e", R_CODE], capture_output=True, text=True, check=True).stdout
    sets, cur = {}, None
    for line in out.splitlines():
        if line.startswith("## "):
            cur = line[3:].strip(); sets[cur] = {}
        elif "=" in line and cur:
            k, v = line.split("=", 1)
            sets[cur][k.strip()] = [float(z) for z in v.split(",")] if k in ("X", "Y") else float(v)
    return sets

SETS = r_values()
wb = xlsxwriter.Workbook("lab6/lab6_instr.xlsx")
title = wb.add_format({"bold": True, "font_size": 14}); note = wb.add_format({"italic": True, "font_color": "#555555"})
sec = wb.add_format({"bold": True, "bg_color": "#5C715E", "font_color": "white"})
hdr = wb.add_format({"bold": True, "bg_color": "#E4EAE4", "border": 1})
inp = wb.add_format({"bg_color": "#FFF2CC", "border": 1}); wrap = wb.add_format({"text_wrap": True, "valign": "top", "border": 1})
num = wb.add_format({"num_format": "0.0000", "valign": "top", "border": 1})
txt = wb.add_format({"valign": "top", "border": 1, "bold": True, "text_wrap": True})

def build(sheet, dsheet, V, labels, spec):
    """spec: dict with title, inputs text, n, xname, yname, direction ('greater'/'less'), alt text, decisions, apa."""
    ws = wb.add_worksheet(sheet); dw = wb.add_worksheet(dsheet)
    n = int(V["n"]); last = n + 1
    ws.set_column("A:A", 46); ws.set_column("B:B", 16); ws.set_column("C:C", 62); ws.set_column("D:D", 66)
    ws.write("A1", spec["title"], title)
    ws.write("A2", f"Yellow cells are inputs; change them and every answer updates. Raw data are on the {dsheet} sheet.", note)
    ws.merge_range("A4:D4", spec["inputs"], sec)
    for c, h in zip("ABCD", ["Quantity", "Value", "Note", "R object"]): ws.write(f"{c}5", h, hdr)
    for i, (a, b, c, d) in enumerate([
            ("Hypothesized mean difference (mu_D)", 0, "H0: no difference between the two conditions", "mu_d <- 0"),
            ("Alpha", 0.05, "Significance level", "alpha <- 0.05"),
            ("Confidence level", 0.95, "For the confidence interval", "conf.level <- 0.95")]):
        r = 6 + i; ws.write(f"A{r}", a, wrap); ws.write(f"B{r}", b, inp); ws.write(f"C{r}", c, wrap); ws.write(f"D{r}", d, wrap)
    MU, AL, CL = "$B$6", "$B$7", "$B$8"
    Dr = f"'{dsheet}'!$D$2:$D${last}"; X = f"'{dsheet}'!$B$2:$B${last}"; Y = f"'{dsheet}'!$C$2:$C${last}"
    xn, yn = spec["xname"], spec["yname"]

    def block(top, text):
        ws.merge_range(f"A{top}:D{top}", text, sec)
        for c, h in zip("ABCD", ["Step", "Result", "Explanation", "Equivalent in R (all arguments named)"]): ws.write(f"{c}{top+1}", h, hdr)
        return top + 2
    def line(r, step, formula, val, expl, rcode):
        ws.write(f"A{r}", step, wrap); ws.write_formula(f"B{r}", formula, num, val); ws.write(f"C{r}", expl, wrap); ws.write(f"D{r}", rcode, wrap)
    def decision(r, step, formula, val, expl, rcode):
        ws.write(f"A{r}", step, wrap); ws.write_formula(f"B{r}", formula, txt, val); ws.write(f"C{r}", expl, wrap); ws.write(f"D{r}", rcode, wrap)

    r = block(10, "1. By hand: difference scores, mean difference, s_D, standard error, t")
    q1 = r
    line(r, "Number of pairs (n)", f"=COUNT({Dr})", V["n"], f"Count of the {n} difference scores (D = {xn} - {yn}, column D of the data sheet)", f"n <- length(x = D)"); r += 1
    line(r, "Mean difference (D-bar)", f"=AVERAGE({Dr})", V["db"], "Sum of the D scores divided by n", "d_bar <- mean(x = D)"); r += 1
    line(r, "Sum of squared deviations", f"=DEVSQ({Dr})", V["ss"], "Each D's squared distance from D-bar, added up (numerator of s_D)", "sum((D - d_bar)^2)"); r += 1
    line(r, "SD of the differences (s_D)", f"=STDEV.S({Dr})", V["sd"], "s_D = sqrt(SS / (n - 1))", "s_d <- sd(x = D)"); r += 1
    line(r, "Standard error = s_D / sqrt(n)", f"=B{r-1}/SQRT(B{q1})", V["se"], "Estimated standard error of the mean difference", "se <- s_d / sqrt(x = n)"); r += 1
    line(r, "t = (D-bar - mu_D) / SE", f"=(B{q1+1}-{MU})/B{r-1}", V["t"], spec["t_expl"], "t <- d_bar / se"); tq = r; r += 2

    r = block(r, "2. Degrees of freedom and critical values")
    line(r, "Degrees of freedom", f"=B{q1}-1", V["df"], f"df = n - 1 = {n} - 1 = {n-1} (pairs, not scores)", "df <- n - 1"); dq = r; r += 1
    line(r, "Two-tailed critical t (+/-)", f"=T.INV.2T({AL},B{dq})", V["crit2"], "2.5% in each tail; reject if |t| is larger than this", "qt(p = 0.975, df = df, lower.tail = TRUE)"); c2 = r; r += 1
    if spec["direction"] == "greater":
        line(r, "One-tailed critical t (greater than)", f"=T.INV(1-{AL},B{dq})", V["crit1"], "All 5% in the upper tail; reject if t is above this", "qt(p = 0.95, df = df, lower.tail = TRUE)")
    c1 = r; r += 2

    r = block(r, "3. Two-tailed test")
    line(r, "Two-tailed p-value", f"=T.DIST.2T(ABS(B{tq}),B{dq})", V["p2"], "Area beyond |t| in BOTH tails", "2 * pt(q = abs(x = t), df = df, lower.tail = FALSE)"); p2 = r; r += 1
    rc = f't.test(x = {xn}, y = {yn}, paired = TRUE, alternative = "two.sided", conf.level = 0.95)$conf.int'
    line(r, "95% CI lower", f"=B{q1+1}-T.INV.2T(1-{CL},B{dq})*B{q1+4}", V["lo"], "D-bar - critical t x SE", rc); r += 1
    line(r, "95% CI upper", f"=B{q1+1}+T.INV.2T(1-{CL},B{dq})*B{q1+4}", V["hi"], spec["ci_expl"], "(same call as above)"); r += 1
    decision(r, "Decision (two-tailed)", f'=IF(ABS(B{tq})>B{c2},"Reject H0","Fail to reject H0")', spec["dec2"], spec["dec2_expl"], "abs(x = t) > t_crit_two"); r += 2

    if spec["direction"] == "greater":
        r = block(r, "4. One-tailed test (H_A: mu_D > 0)")
        line(r, "One-tailed p-value", f"=T.DIST.RT(B{tq},B{dq})", V["pg"], "Area above t in the upper tail only: half the two-tailed p when t is positive", "pt(q = t, df = df, lower.tail = FALSE)"); p1 = r; r += 1
        decision(r, "Decision (one-tailed)", f'=IF(B{tq}>B{c1},"Reject H0","Fail to reject H0")', "Reject H0", spec["dec1_expl"], f't.test(x = {xn}, y = {yn}, paired = TRUE, alternative = "greater", conf.level = 0.95)'); r += 2
        r = block(r, "5. Ignoring the pairing (independent-samples t-test)")
        ws.write(f"A{r}", "Independent-samples p-value", wrap)
        ws.write_formula(f"B{r}", f"=T.TEST({X},{Y},2,2)", num, V["pind"])
        ws.write(f"C{r}", f"Not significant: between-person differences swamp the effect. Pairing uses r = {V['r']:.2f} between the two conditions.", wrap)
        ws.write(f"D{r}", f't.test(x = {xn}, y = {yn}, paired = FALSE, var.equal = TRUE)', wrap); r += 1
        ws.write(f"A{r}", "Correlation between conditions (r)", wrap)
        ws.write_formula(f"B{r}", f"=CORREL({X},{Y})", num, V["r"])
        ws.write(f"C{r}", "The stronger r is, the more pairing helps.", wrap); ws.write(f"D{r}", f"cor(x = {yn}, y = {xn})", wrap); r += 2
        eff = "6. Effect size and APA sentence"
    else:
        r = block(r, "4. Ignoring the pairing (independent-samples t-test)")
        ws.write(f"A{r}", "Independent-samples p-value", wrap)
        ws.write_formula(f"B{r}", f"=T.TEST({X},{Y},2,2)", num, V["pind"])
        ws.write(f"C{r}", f"Not significant: between-person differences swamp the effect. Pairing uses r = {V['r']:.2f} between the two conditions.", wrap)
        ws.write(f"D{r}", f't.test(x = {xn}, y = {yn}, paired = FALSE, var.equal = TRUE)', wrap); r += 2
        eff = "5. Effect size and APA sentence"
    r = block(r, eff)
    line(r, "Cohen's d_z = D-bar / s_D", f"=B{q1+1}/B{q1+3}", V["dz"], spec["dz_expl"], "d_z <- d_bar / s_d"); dd = r; r += 1
    decision(r, "Size of the effect", f'=IF(ABS(B{dd})<0.2,"negligible",IF(ABS(B{dd})<0.5,"small",IF(ABS(B{dd})<0.8,"medium","large")))', spec["size"],
             "About 0.2 is small, 0.5 is medium, and 0.8 is large.", "abs(x = d_z)"); r += 1
    ws.write(f"A{r}", "APA sentence", wrap)
    pcell = f"B{p1}" if spec["direction"] == "greater" else f"B{p2}"
    tail = " (one-tailed)" if spec["direction"] == "greater" else ""
    f = (f'="{spec["apa_lead"]}"&", M_D = "&TEXT(B{q1+1},"0.00")&", SD_D = "&TEXT(B{q1+3},"0.00")'
         f'&", t("&B{dq}&") = "&TEXT(B{tq},"0.00")&", p = "&TEXT({pcell},"0.000")&"{tail}, d_z = "&TEXT(B{dd},"0.00")&"."')
    ws.merge_range(f"B{r}:D{r}", "", wrap); ws.write_formula(f"B{r}", f, wrap, spec["apa"]); ws.set_row(r - 1, 32); r += 2
    if spec.get("submit"):
        r = block(r, "Submit question")
        ws.write(f"A{r}", "Answer", wrap); ws.merge_range(f"B{r}:D{r}", spec["submit"], wrap); ws.set_row(r - 1, 48)
    # data sheet
    dw.set_column("A:A", 12); dw.set_column("B:D", 16)
    for c, h in zip("ABCD", ["Student", labels[0], labels[1], f"D = {xn} - {yn}"]): dw.write(f"{c}1", h, hdr)
    for i in range(n):
        dw.write(i + 1, 0, i + 1); dw.write(i + 1, 1, V["X"][i]); dw.write(i + 1, 2, V["Y"][i])
        dw.write_formula(i + 1, 3, f"=B{i+2}-C{i+2}", None, V["X"][i] - V["Y"][i])
    dw.write("F1", spec["rcode"], note)

R_ = SETS["RECALL"]; A_ = SETS["ANXIETY"]
build("Lab 6 Key", "Data", R_, ("away", "near"), dict(
    title="Lab 6: Instructions and Walkthrough (Excel version)",
    inputs="Inputs: words recalled, 10 students, D = away - near, H_A: mu_D > 0",
    n=10, xname="away", yname="near", direction="greater",
    t_expl="t = 1.9 / 0.690 = 2.75: the mean difference is about 2.75 standard errors ABOVE 0",
    ci_expl="D-bar + critical t x SE. The interval excludes 0.",
    dec2="Reject H0", dec2_expl="|t| = 2.75 > 2.262 and p = .022 < .05, so we reject H0 (two-tailed).",
    dec1_expl="2.75 > 1.833 and p = .011 < .05, so we reject H0. The decision does not change: p is exactly half of the two-tailed p.",
    dz_expl="d_z = 1.9 / 2.183 = 0.87", size="large",
    apa_lead="Students recalled significantly more words with their phone away (M = 16.10, SD = 3.35) than near (M = 14.20, SD = 2.30)",
    apa="Students recalled significantly more words with their phone away (M = 16.10, SD = 3.35) than near (M = 14.20, SD = 2.30), M_D = 1.90, SD_D = 2.18, t(9) = 2.75, p = .011 (one-tailed), d_z = 0.87.",
    submit="Once each person's two scores are subtracted there is one column of difference scores, and the paired test asks whether its mean differs from 0: a one-sample t-test with mu_0 = 0 and df = n - 1 pairs.",
    rcode="R: set.seed(seed = 133); near <- round(x = rnorm(n = 10, mean = 14, sd = 3), digits = 0); away <- round(x = near + 1.5 + rnorm(n = 10, mean = 0, sd = 2.2), digits = 0)"))
build("Worked Example", "Worked Data", A_, ("after", "before"), dict(
    title="Lab 6 worked example (Excel version): anxiety before and after a breathing exercise",
    inputs="Inputs: 12 students, D = after - before, H_A: mu_D != 0 (two-tailed)",
    n=12, xname="after", yname="before", direction="two",
    t_expl="t = -2.58 / 0.94 = -2.74: the mean difference is about 2.74 standard errors BELOW 0 (anxiety went down)",
    ci_expl="D-bar + critical t x SE. The interval does NOT contain 0.",
    dec2="Reject H0", dec2_expl="|t| = 2.74 > 2.201 and p = .019 < .05, so we reject H0. Anxiety changed after the exercise.",
    dec1_expl="", dz_expl="d_z = -2.58 / 3.26 = -0.79; the negative sign only says anxiety went down", size="medium",
    apa_lead="Anxiety decreased after the breathing exercise",
    apa="Anxiety decreased after the breathing exercise, M_D = -2.58, SD_D = 3.26, t(11) = -2.74, p = .019, d_z = -0.79.",
    rcode="R: set.seed(seed = 6); before <- round(x = rnorm(n = 12, mean = 20, sd = 4), digits = 0); after <- round(x = before - 3 + rnorm(n = 12, mean = 0, sd = 2.5), digits = 0)"))
wb.close()
