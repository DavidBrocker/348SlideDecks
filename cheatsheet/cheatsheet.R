---
format: html
---

# One Sample T-Test

# Data Type

```{r}
t.test(x = mtcars$mpg, mu = 33)

```

# Interpretation:

# t(df) = t, p, CI\[ll,uu\]

# Report: Mean Difference \| Mean

# Paired Sample T-Test

```{r}
ptt <- 
  tibble(
    t1 = rnorm(25,5,2),
    t2 = rnorm(25,7,2)
  )

# Comma Method
t.test(ptt$t1,ptt$t2,paired = TRUE)

# Formula Method
t.test(Pair(t1,t2)  ~ 1, data = ptt)
```

# Independent Samples T-Test

# Data Type:

# Two Separate Data Samples

```{r}
# Comma Method
with(ptt,t.test(t2,t1))

# Function Method
# Make the Data Wide
ptt_long <- 
  ptt |> 
  + tidyr::pivot_longer(cols = everything(),
                        names_to = "time",
                        values_to = "score")

t.test(score~time, data = ptt_long)
```

# ANOVA

# Data Type

# IV: 3 Levels

```{r}
exp <- 
  tibble(
    grp = rep(c("SSLD","Placebo","Control"),each = 100),
    IQ = c(rnorm(100,120,5),
           rnorm(100,110,5),
           rnorm(100,110,5)) |> round(0)
  )
    
# Formula
# aov(DV ~ IV, data = data)

aov(IQ ~ grp, data = exp) |> 
  summary()
```

# ANOVA's are Linear Regressions!

```{r}
lm(IQ ~ grp, data = exp) |> 
  summary()

# Follow-Up Tests
# Save the model, NOT the summary!
aov(IQ ~ grp, data = exp) |> 
  TukeyHSD()
```
