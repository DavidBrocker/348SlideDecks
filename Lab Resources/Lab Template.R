# Click 'Run'
install.packages("tidyverse")
install.packages("gt")
install.packages("afex")
install.packages("babynames")

# Click 'Run'
library(tidyverse)
library(gt)
library(afex)
library(babynames)

# Click on 'Code' and make sure 'Rainbow Parentheses' is checked
# Click 'Run'
set.seed(348)

# Click 'Run'
fake_class <- 
  # This creates a dataset of 26 students with an average score of
  # 90 and a standard deviation of 1 point
  tibble(
    students = LETTERS,
    scores = rnorm(n = 26, mean = 90, sd = 1)
  ) 

# Click 'Run'
p <- 
  ggplot(fake_class,aes(students, scores)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 90, color = "green") +
  geom_segment(aes(x = students, y = scores,xend = students, yend = 90)) +
  theme_minimal() +
  labs(
    x = "Students",
    y = "Exam Scores",
    title = "Exam Scores for PSY 348 First Test"
  ) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()
  )

# Highlight the highest scoring student in red
p + 
  geom_point(
    aes(
      color = 
        ifelse(scores == max(scores),
               "red",
               "black"))
    ) +
  scale_color_identity()

# Popularity of my name between 1880 - 2017
babynames |> 
  filter(name == "David") |> 
  filter(sex == "M") |> 
  ggplot(aes(year, n)) +
  geom_line() +
  theme_minimal() +
  ylim(0,100000)

