# Load required libraries
library(ggplot2)
library(dplyr)

# Create `df` with random normal values and their probability densities
df <- tibble(
  x = rnorm(1000, 20, 5),
  pd = dnorm(x, mean(x), sd(x))
)

# Plot the normal distribution and indicate the mean value
df |>
  ggplot(aes(x, pd)) +
  geom_line() + # plot density curve
  geom_segment( # add vertical dashed line at the mean
    x = 20,
    xend = 20,
    y = 0,
    yend = dnorm(mean(x), mean(x), sd(x)),
    lty = "dashed"
  ) +
  theme_minimal( # minimal theme
    paper = "wheat1",
    ink = "navy",
    base_size = 15
  ) +
  theme_sub_panel( # custom theming (likely from custom ggplot extensions)
    grid = element_blank()
  ) +
  labs(
    x = "\nScores",
    y = "Probability Density\n",
    title = "Distribution of Test Scores",
    subtitle = "Dashed line indicates mean"
  ) +
  scale_x_continuous( # set x-axis breaks to cover ±4 SDs, 9 ticks
    breaks = seq(
      (mean(x) - 4 * sd(x)) |> round(0),
      (mean(x) + 4 * sd(x)) |> round(0),
      length.out = 9
    )
  ) +
  theme_sub_plot( # further custom theming
    title.position = "plot",
    title = element_text(face = "bold"),
    subtitle = element_text(face = "italic")
  )







set.seed(123)
practice_df <- tibble(
  id       = 1:120,
  gender   = sample(c("Male", "Female", "Other"), 120, replace = TRUE, prob = c(0.45, 0.45, 0.1)),
  condition= sample(c("Control", "TreatmentA", "TreatmentB"), 120, replace = TRUE),
  age      = sample(18:65, 120, replace = TRUE),
  pretest  = rnorm(120, 50, 10),
  posttest = pretest + 
    if_else(condition == "Control", rnorm(120, 2, 4), 
            if_else(condition == "TreatmentA", rnorm(120, 8, 5), rnorm(120, 5, 5))),
  score1   = rnorm(120, 15, 4),
  score2   = rnorm(120, 100, 15),
  score3   = rnorm(120, 40, 7)
) %>%
  mutate(
    gender    = if_else(row_number() %% 30 == 0, NA_character_, gender),
    posttest  = if_else(row_number() %% 25 == 0, NA_real_, posttest),
    condition = if_else(row_number() %% 17 == 0, "Unknown", condition)
  )

practice_df |> write.csv("LV_RData.csv")


# 1. How many participants are in each experimental condition?
practice_df |> 
  count(condition)

# 2. What is the average posttest score for each gender group?
practice_df |> 
  group_by(gender) |> 
  summarize(
    avg = mean(posttest,na.rm = TRUE)
    )

# 3. What proportion of 'gender' or 'posttest' values are missing in this dataset?
practice_df |> 
  filter(is.na(gender) | is.na(posttest))

# 4. For participants aged 30 or less, what is their mean pretest score?
practice_df |> 
  filter(age <=30) |> 
  summarize(
    m_pre = mean(pretest))

# 5. Which condition, on average, improved the most from pretest to posttest?
practice_df |> 
  group_by(condition) |> 
  summarize(
    m_pre = mean(pretest, na.rm = TRUE), 
    m_post = mean(posttest, na.rm = TRUE)
    ) |> 
  mutate(
    diff = m_post-m_pre
  )



# For Research --------------------------------------------------------------------------------



library(RedditExtractoR)
library(purrr)
library(dplyr)
library(tidytext)

# Get Threads
sht <- find_thread_urls(subreddit = "selfharm")

# Subset
subsh <- 
  sht |> 
  filter(comments > 0)


# Handle Single Comment Threads
comment_coerce <- function(x){
  
  xx <- get_thread_content(x)
  
  xx$comments <- 
    xx$comments |> 
    mutate(comment_id = as.character(comment_id))
}

# Get Thread Content (Comments, Upvotes, etc.)
all_threads <- map_df(subsh$url[1:50],comment_coerce)

all_threads2 <- map_df(subsh$url[51:100],comment_coerce)

all_thrd <- 
  all_threads |> 
  bind_rows(all_threads2)

# Group by Url and tokenize
all_thrd |> 
  group_by(url,comment_id) |> 
  tidytext::unnest_tokens(word,comment) |> 
  anti_join(stop_words) |> 
  inner_join(get_sentiments(lexicon  = "bing")) |>
  mutate(id = row_number(url)) |> 
  group_by(id,sentiment) |> 
  count(sort = TRUE) |> 
  ggplot(aes(id,n,color = sentiment)) + 
  geom_point(pch = "|") + 
  theme_minimal() + 
  coord_flip()
  
