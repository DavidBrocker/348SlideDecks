



# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Create the data
n <- 20
group <- rep(c("Classical", "Pop", "Silence"), each = n)
scores <- c(
  rnorm(n, mean = 75, sd = 10),  # Classical music group
  rnorm(n, mean = 70, sd = 10),  # Pop music group
  rnorm(n, mean = 65, sd = 10)   # Silence group
)

# Combine into a data frame
data <- data.frame(
  Group = factor(group, levels = c("Classical", "Pop", "Silence")),
  Score = scores
)

# Descriptive statistics
summary_stats <- data |> 
  group_by(Group) |> 
  summarize(
    Mean = mean(Score),
    SD = sd(Score),
    n = n()
  )
print(summary_stats)

# Visualize the data
ggplot(data, aes(x = Group, y = Score, fill = Group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Test Scores by Study Music Type",
    y = "Test Score",
    x = "Music Type"
  )

# Conduct one-way ANOVA
anova_result <- aov(Score ~ Group, data = data)
summary(anova_result)

# Post-hoc test (Tukey's HSD)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Plot Tukey's HSD results
plot(tukey_result)


tukey_result$Group |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "comparison") |> 
  ggplot(aes(comparison,diff)) + 
  geom_point(shape = "|",
             size = 5) + 
  ylim(-21,7) +
  coord_flip() +
  theme_minimal() + 
  geom_point(aes(y = lwr),
                 shape = "|",
                 size = 5) + 
  geom_point(aes(y = upr),
             shape = "|",
             size = 5) +
  geom_segment(
    aes(x = comparison,xend = comparison,
        y = lwr, yend = upr)
  ) + 
  geom_hline(yintercept = 0,
             lty = "dashed",
             color = "red") + 
  scale_y_continuous(breaks = seq(-20, 5,5)) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "95% Family-Wise Confidence Level",
    y = "\nDifferences in mean levels of Group",
    x = ""
  )
  
  
library(broom)
library(gt)
library(huxtable)

anova_result |> 
  tidy() |> 
  rename(
    SS = sumsq,
    MS = meansq,
    `F` = statistic,
    p = p.value
  ) |> 
  hux() |> 
  set_number_format(fmt_pretty(digits = 5))

# Questions -----------------------------------------------------------------------------------

# 1.	What are the null and alternative hypotheses for this ANOVA?
#  2.	Interpret the ANOVA summary table. Was there a significant effect of music type on test scores?
#  3.	If the ANOVA was significant, examine the Tukey’s HSD results. Which groups differ from each other?
#  4.	Based on the boxplot, what conclusions can you draw about the effect of background music on study performance?


