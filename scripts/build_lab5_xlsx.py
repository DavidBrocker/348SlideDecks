"""Rebuild lab5/lab5_instr.xlsx (sheets: Lab 5 Key, Data, Worked Example, Worked Data).

Run from the repo root:   python3 scripts/build_lab5_xlsx.py
Needs: xlsxwriter, and Rscript on the PATH. All statistics are computed by R (so they match
lab5.qmd exactly) and written into the workbook as cached values next to live Excel formulas.
"""
import subprocess, xlsxwriter

R_CODE = """
stats <- function(x, mu, alt) {
  n <- length(x); m <- mean(x); s <- sd(x); se <- s / sqrt(n); t <- (m - mu) / se; df <- n - 1
  tt <- t.test(x = x, mu = mu, alternative = "two.sided", conf.level = 0.95)
  tl <- t.test(x = x, mu = mu, alternative = "less", conf.level = 0.95)
  cat(sprintf("%s=%.16g", c("n","m","ss","s","se","t","df","crit2","crit1","p2","lo","hi","p1","d"),
      c(n, m, sum((x - m)^2), s, se, t, df, qt(0.975, df), qt(0.05, df), tt$p.value,
        tt$conf.int[1], tt$conf.int[2], tl$p.value, (m - mu) / s)), sep = "\n")
  cat("DATA=", paste(x, collapse = ","), "\n", sep = "")
}
set.seed(seed = 133); sleep_hours <- round(x = rnorm(n = 10, mean = 6.5, sd = 0.9), digits = 1)
cat("## SLEEP\n"); stats(sleep_hours, 7)
set.seed(seed = 348); study_hours <- round(x = rnorm(n = 12, mean = 12, sd = 4), digits = 0)
cat("## STUDY\n"); stats(study_hours, 9)
"""

def r_values():
    out = subprocess.run(["Rscript", "-e", R_CODE], capture_output=True, text=True, check=True).stdout
    sets, cur = {}, None
    for line in out.splitlines():
        if line.startswith("## "):
            cur = line[3:].strip(); sets[cur] = {}
        elif "=" in line and cur:
            k, v = line.split("=", 1)
            sets[cur][k.strip()] = [float(x) for x in v.split(",")] if k == "DATA" else float(v)
    return sets

SETS = r_values()
V = {k: v for k, v in SETS["SLEEP"].items() if k != "DATA"}
data = SETS["SLEEP"]["DATA"]
W = {k: v for k, v in SETS["STUDY"].items() if k != "DATA"}
wdata = SETS["STUDY"]["DATA"]
OUT = "lab5/lab5_instr.xlsx"
wb = xlsxwriter.Workbook(OUT)
ws=wb.add_worksheet("Lab 5 Key"); dw=wb.add_worksheet("Data")
title=wb.add_format({"bold":True,"font_size":14}); note=wb.add_format({"italic":True,"font_color":"#555555"})
sec=wb.add_format({"bold":True,"bg_color":"#5C715E","font_color":"white"})
hdr=wb.add_format({"bold":True,"bg_color":"#E4EAE4","border":1})
inp=wb.add_format({"bg_color":"#FFF2CC","border":1}); wrap=wb.add_format({"text_wrap":True,"valign":"top","border":1})
num=wb.add_format({"num_format":"0.0000","valign":"top","border":1}); txt=wb.add_format({"valign":"top","border":1,"bold":True,"text_wrap":True})
ws.set_column("A:A",46); ws.set_column("B:B",16); ws.set_column("C:C",62); ws.set_column("D:D",66)
ws.write("A1","Lab 5: Instructions and Walkthrough (Excel version)",title)
ws.write("A2","Yellow cells are inputs; change them and every answer below updates. Raw data are on the Data sheet.",note)
ws.merge_range("A4:D4","Inputs: hours of sleep, 10 students, H_A: mu < 7",sec)
for c,h in zip("ABCD",["Quantity","Value","Note","R object"]): ws.write(f"{c}5",h,hdr)
rows=[("Hypothesized mean (mu)",7,"Recommended hours of sleep","mu_0 <- 7"),
      ("Alpha",0.05,"Significance level","alpha <- 0.05"),
      ("Confidence level",0.95,"For the confidence interval","conf.level <- 0.95")]
for i,(a,b,c,d) in enumerate(rows):
    r=6+i; ws.write(f"A{r}",a,wrap); ws.write(f"B{r}",b,inp); ws.write(f"C{r}",c,wrap); ws.write(f"D{r}",d,wrap)
MU,AL,CL="$B$6","$B$7","$B$8"
R="Data!$B$2:$B$11"
def block(top,titletext):
    ws.merge_range(f"A{top}:D{top}",titletext,sec)
    for c,h in zip("ABCD",["Step","Result","Explanation","Equivalent in R (all arguments named)"]): ws.write(f"{c}{top+1}",h,hdr)
    return top+2
def line(r,step,formula,val,expl,rcode,fmt=num):
    ws.write(f"A{r}",step,wrap); ws.write_formula(f"B{r}",formula,fmt,val); ws.write(f"C{r}",expl,wrap); ws.write(f"D{r}",rcode,wrap)
# Q1
r=block(10,"Q1. By hand: mean, s, standard error, and t")
q1=r
line(r,"Sample size (n)",f"=COUNT({R})",V["n"],"Count of the 10 scores on the Data sheet","n <- length(x = sleep_hours)"); r+=1
line(r,"Sample mean",f"=AVERAGE({R})",V["m"],"Sum of the scores divided by n","m <- mean(x = sleep_hours)"); r+=1
line(r,"Sum of squared deviations",f"=DEVSQ({R})",V["ss"],"Each score's squared distance from the mean, added up (numerator of s)","sum((sleep_hours - m)^2)"); r+=1
line(r,"Sample SD (s)",f"=STDEV.S({R})",V["s"],"s = sqrt(4.165 / (10 - 1)) = 0.680","s <- sd(x = sleep_hours)"); r+=1
line(r,"Standard error = s / sqrt(n)",f"=B{r-1}/SQRT(B{q1})",V["se"],"SE = 0.680 / sqrt(10) = 0.215","se <- s / sqrt(x = n)"); r+=1
line(r,"t = (mean - mu) / SE",f"=(B{q1+1}-{MU})/B{r-1}",V["t"],"t = (6.55 - 7) / 0.215 = -2.09: the mean is about 2.09 standard errors BELOW 7","t <- (m - mu_0) / se"); tq=r; r+=2
# Q2
r=block(r,"Q2. Degrees of freedom and critical values")
line(r,"Degrees of freedom",f"=B{q1}-1",V["df"],"df = n - 1 = 10 - 1 = 9","df <- n - 1"); dq=r; r+=1
line(r,"Two-tailed critical t (+/-)",f"=T.INV.2T({AL},B{dq})",V["crit2"],"2.5% in each tail; reject if |t| is larger than this","qt(p = 0.975, df = df, lower.tail = TRUE)"); c2=r; r+=1
line(r,"One-tailed critical t (less than)",f"=T.INV({AL},B{dq})",V["crit1"],"All 5% in the lower tail; reject if t is below this","qt(p = 0.05, df = df, lower.tail = TRUE)"); c1=r; r+=2
# Q3
r=block(r,"Q3. Two-tailed test")
line(r,"Two-tailed p-value",f"=T.DIST.2T(ABS(B{tq}),B{dq})",V["p2"],"Area beyond |t| in BOTH tails","2 * pt(q = abs(x = t), df = df, lower.tail = FALSE)"); p2=r; r+=1
line(r,"95% CI lower",f"=B{q1+1}-T.INV.2T(1-{CL},B{dq})*B{q1+4}",V["lo"],"mean - critical t x SE","t.test(x = sleep_hours, mu = 7, alternative = \"two.sided\", conf.level = 0.95)$conf.int"); r+=1
line(r,"95% CI upper",f"=B{q1+1}+T.INV.2T(1-{CL},B{dq})*B{q1+4}",V["hi"],"mean + critical t x SE. The interval contains 7.","(same call as above)"); r+=1
ws.write(f"A{r}","Decision (two-tailed)",wrap)
ws.write_formula(f"B{r}",f'=IF(ABS(B{tq})>B{c2},"Reject H0","Fail to reject H0")',txt,"Fail to reject H0")
ws.write(f"C{r}","|t| = 2.09 < 2.262 and p > .05, so we fail to reject H0 (two-tailed).",wrap); ws.write(f"D{r}","abs(x = t) > t_crit_two",wrap); r+=2
# Q4
r=block(r,"Q4. One-tailed test (H_A: mu < 7)")
line(r,"One-tailed p-value",f"=T.DIST(B{tq},B{dq},TRUE)",V["p1"],"Area below t in the lower tail only: exactly half the two-tailed p","pt(q = t, df = df, lower.tail = TRUE)"); p1=r; r+=1
ws.write(f"A{r}","Decision (one-tailed)",wrap)
ws.write_formula(f"B{r}",f'=IF(B{tq}<B{c1},"Reject H0","Fail to reject H0")',txt,"Reject H0")
ws.write(f"C{r}","-2.09 < -1.833 and p = .033 < .05, so we reject H0. The decision changed only because all of alpha sits in one tail. The direction must be chosen BEFORE seeing the data.",wrap)
ws.write(f"D{r}","t.test(x = sleep_hours, mu = 7, alternative = \"less\", conf.level = 0.95)",wrap); r+=2
# Q5
r=block(r,"Q5. Effect size and APA sentence")
line(r,"Cohen's d = (mean - mu) / s",f"=(B{q1+1}-{MU})/B{q1+3}",V["d"],"d = (6.55 - 7) / 0.680 = -0.66","d <- (m - mu_0) / s"); dd=r; r+=1
ws.write(f"A{r}","Size of the effect",wrap)
ws.write_formula(f"B{r}",f'=IF(ABS(B{dd})<0.2,"negligible",IF(ABS(B{dd})<0.5,"small",IF(ABS(B{dd})<0.8,"medium","large")))',txt,"medium")
ws.write(f"C{r}","About 0.2 is small, 0.5 is medium, and 0.8 is large; 0.66 is medium-to-large.",wrap); ws.write(f"D{r}","abs(x = d)",wrap); r+=1
apa="Students slept significantly less than the recommended 7 hours, M = 6.55, SD = 0.68, t(9) = -2.09, p = .033 (one-tailed), d = -0.66."
ws.write(f"A{r}","APA sentence",wrap)
f=(f'="Students slept significantly less than the recommended "&TEXT({MU},"0")&" hours, M = "&TEXT(B{q1+1},"0.00")&", SD = "&TEXT(B{q1+3},"0.00")'
   f'&", t("&B{dq}&") = "&TEXT(B{tq},"0.00")&", p = "&TEXT(B{p1},"0.000")&" (one-tailed), d = "&TEXT(B{dd},"0.00")&"."')
ws.merge_range(f"B{r}:D{r}","",wrap); ws.write_formula(f"B{r}",f,wrap,apa); ws.set_row(r-1,32); r+=2
# Submit
r=block(r,"Submit question: why t instead of z?")
ws.write(f"A{r}","Answer",wrap); ws.merge_range(f"B{r}:D{r}","s is only an estimate of sigma and changes from sample to sample, so the t distribution has fatter tails than z to account for that extra uncertainty (especially when n is small). As n grows, t becomes nearly identical to z.",wrap); ws.set_row(r-1,48)
# Data sheet
dw.set_column("A:A",12); dw.set_column("B:B",14)
dw.write("A1","Student",hdr); dw.write("B1","Sleep hours",hdr)
for i,x in enumerate(data): dw.write(i+1,0,i+1); dw.write(i+1,1,x)
dw.write("D1","R: set.seed(seed = 133); sleep_hours <- round(rnorm(n = 10, mean = 6.5, sd = 0.9), digits = 1)",note)

# ===================== Worked Example sheet =====================
wk = wb.add_worksheet("Worked Example"); wd = wb.add_worksheet("Worked Data")
wk.set_column("A:A", 46); wk.set_column("B:B", 16); wk.set_column("C:C", 62); wk.set_column("D:D", 66)
wk.write("A1", "Lab 5 worked example (Excel version): weekly study hours", title)
wk.write("A2", "Yellow cells are inputs; change them and every answer updates. Raw data are on the Worked Data sheet.", note)
wk.merge_range("A4:D4", "Inputs: 12 students, H0: mu = 9, H_A: mu != 9 (two-tailed)", sec)
for c, h in zip("ABCD", ["Quantity", "Value", "Note", "R object"]): wk.write(f"{c}5", h, hdr)
for i, (a, b, c, d) in enumerate([
        ("Hypothesized mean (mu)", 9, "National average hours studied per week", "mu_0 <- 9"),
        ("Alpha", 0.05, "Significance level", "alpha <- 0.05"),
        ("Confidence level", 0.95, "For the confidence interval", "conf.level <- 0.95")]):
    r = 6 + i; wk.write(f"A{r}", a, wrap); wk.write(f"B{r}", b, inp); wk.write(f"C{r}", c, wrap); wk.write(f"D{r}", d, wrap)
WMU, WAL, WCL = "$B$6", "$B$7", "$B$8"
WR = "'Worked Data'!$B$2:$B$13"

def wblock(top, text):
    wk.merge_range(f"A{top}:D{top}", text, sec)
    for c, h in zip("ABCD", ["Step", "Result", "Explanation", "Equivalent in R (all arguments named)"]): wk.write(f"{c}{top+1}", h, hdr)
    return top + 2

def wline(r, step, formula, val, expl, rcode, fmt=num):
    wk.write(f"A{r}", step, wrap); wk.write_formula(f"B{r}", formula, fmt, val); wk.write(f"C{r}", expl, wrap); wk.write(f"D{r}", rcode, wrap)

r = wblock(10, "1. By hand: mean, s, standard error, and t")
w1 = r
wline(r, "Sample size (n)", f"=COUNT({WR})", W["n"], "Count of the 12 scores on the Worked Data sheet", "n <- length(x = study_hours)"); r += 1
wline(r, "Sample mean", f"=AVERAGE({WR})", W["m"], "Sum of the scores divided by n", "m <- mean(x = study_hours)"); r += 1
wline(r, "Sum of squared deviations", f"=DEVSQ({WR})", W["ss"], "Each score's squared distance from the mean, added up (numerator of s)", "sum((study_hours - m)^2)"); r += 1
wline(r, "Sample SD (s)", f"=STDEV.S({WR})", W["s"], "s = sqrt(SS / (n - 1))", "s <- sd(x = study_hours)"); r += 1
wline(r, "Standard error = s / sqrt(n)", f"=B{r-1}/SQRT(B{w1})", W["se"], "SE = 4.72 / sqrt(12) = 1.36", "se <- s / sqrt(x = n)"); r += 1
wline(r, "t = (mean - mu) / SE", f"=(B{w1+1}-{WMU})/B{r-1}", W["t"], "t = (12.67 - 9) / 1.36 = 2.69: the mean is about 2.69 standard errors ABOVE 9", "t <- (m - mu_0) / se"); wt = r; r += 2

r = wblock(r, "2. Degrees of freedom and critical value")
wline(r, "Degrees of freedom", f"=B{w1}-1", W["df"], "df = n - 1 = 12 - 1 = 11", "df <- n - 1"); wdf = r; r += 1
wline(r, "Two-tailed critical t (+/-)", f"=T.INV.2T({WAL},B{wdf})", W["crit2"], "2.5% in each tail; reject if |t| is larger than this", "qt(p = 0.975, df = df, lower.tail = TRUE)"); wc = r; r += 2

r = wblock(r, "3. p-value, confidence interval, and decision")
wline(r, "Two-tailed p-value", f"=T.DIST.2T(ABS(B{wt}),B{wdf})", W["p2"], "Area beyond |t| in BOTH tails", "2 * pt(q = abs(x = t), df = df, lower.tail = FALSE)"); wp = r; r += 1
wline(r, "95% CI lower", f"=B{w1+1}-T.INV.2T(1-{WCL},B{wdf})*B{w1+4}", W["lo"], "mean - critical t x SE", 't.test(x = study_hours, mu = 9, alternative = "two.sided", conf.level = 0.95)$conf.int'); r += 1
wline(r, "95% CI upper", f"=B{w1+1}+T.INV.2T(1-{WCL},B{wdf})*B{w1+4}", W["hi"], "mean + critical t x SE. The interval does NOT contain 9.", "(same call as above)"); r += 1
wk.write(f"A{r}", "Decision (two-tailed)", wrap)
wk.write_formula(f"B{r}", f'=IF(ABS(B{wt})>B{wc},"Reject H0","Fail to reject H0")', txt, "Reject H0")
wk.write(f"C{r}", "|t| = 2.69 > 2.201 and p = .021 < .05, so we reject H0. Students study more than the national average.", wrap)
wk.write(f"D{r}", "abs(x = t) > t_crit", wrap); r += 2

r = wblock(r, "4. Effect size and APA sentence")
wline(r, "Cohen's d = (mean - mu) / s", f"=(B{w1+1}-{WMU})/B{w1+3}", W["d"], "d = (12.67 - 9) / 4.72 = 0.78", "d <- (m - mu_0) / s"); wd_row = r; r += 1
wk.write(f"A{r}", "Size of the effect", wrap)
wk.write_formula(f"B{r}", f'=IF(ABS(B{wd_row})<0.2,"negligible",IF(ABS(B{wd_row})<0.5,"small",IF(ABS(B{wd_row})<0.8,"medium","large")))', txt, "medium")
wk.write(f"C{r}", "About 0.2 is small, 0.5 is medium, and 0.8 is large; 0.78 is at the medium/large boundary (the lab slides call it large, d ~ 0.8).", wrap)
wk.write(f"D{r}", "abs(x = d)", wrap); r += 1
wapa = "Students studied more than the national average of 9 hours, M = 12.67, SD = 4.72, t(11) = 2.69, p = .021, d = 0.78."
wk.write(f"A{r}", "APA sentence", wrap)
wf = (f'="Students studied more than the national average of "&TEXT({WMU},"0")&" hours, M = "&TEXT(B{w1+1},"0.00")&", SD = "&TEXT(B{w1+3},"0.00")'
      f'&", t("&B{wdf}&") = "&TEXT(B{wt},"0.00")&", p = "&TEXT(B{wp},"0.000")&", d = "&TEXT(B{wd_row},"0.00")&"."')
wk.merge_range(f"B{r}:D{r}", "", wrap); wk.write_formula(f"B{r}", wf, wrap, wapa); wk.set_row(r - 1, 32)

wd.set_column("A:A", 12); wd.set_column("B:B", 14)
wd.write("A1", "Student", hdr); wd.write("B1", "Study hours", hdr)
for i, x in enumerate(wdata): wd.write(i + 1, 0, i + 1); wd.write(i + 1, 1, x)
wd.write("D1", "R: set.seed(seed = 348); study_hours <- round(x = rnorm(n = 12, mean = 12, sd = 4), digits = 0)", note)
wb.close()
