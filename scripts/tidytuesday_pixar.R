library(gtrendsR)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggtext)

sui <- gtrends("suicide", time = "all") # since 2004

sui <- 
  sui$interest_over_time %>%
  as_tibble() 

sui %>%
  mutate(hits = as.numeric(replace(hits, hits == "<1", "0"))) %>%
  mutate(diff_hits = hits - lag(hits)) %>% 
  ggplot(aes(date, hits, color = keyword)) +
  geom_line(color = "darkblue") +
  labs(
    x = "Date", 
    y = "Hits", 
    title = "Google Trends Over Time",
    subtitle = "For Keyword: Normal"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(face = "italic"),
    legend.position = "none"
  ) + 
  facet_wrap(~keyword)


pixar_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv')
public_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv')

library(showtext)


pixar_films %>%
  full_join(public_response, by = "film") %>%
  tidyr::pivot_longer(
    cols = c(rotten_tomatoes, metacritic, critics_choice),
    names_to = "rating_medium",
    values_to = "rating"
  ) %>%
  filter(!is.na(film)) %>%
  group_by(rating_medium) %>%
  mutate(
    mean_rating = mean(rating, na.rm = TRUE),
    rating_medium = case_when(
      rating_medium == "critics_choice" ~ "Critics Choice",
      rating_medium == "metacritic" ~ "Metacritic",
      rating_medium == "rotten_tomatoes" ~ "Rotten Tomatoes"
    )
  ) %>%
  ggplot(aes(rating, fct_rev(film), color = rating_medium, label = rating)) +
  geom_segment(aes(xend = mean_rating, yend = film), size = 1) +
  geom_point(size = 6) +
  geom_text(color = "white", size = 3) +
  geom_vline(aes(xintercept = mean_rating, color = rating_medium), linetype = "dashed", show.legend = FALSE) +
  facet_wrap(~rating_medium, scales = "free_x") +
  theme_minimal(base_size = 14) +
  labs(
    x = "\nFilm Rating",
    y = "",
    title = "Pixar Movie Ratings by Critic Source",
    subtitle = "Each movie's score from <span style = 'color: #7570B3;'>**Rotten Tomatoes**</span>, <br><span style = 'color:#D95F02;'>**Metacritic**</span>, and <span style = 'color: #1B9E77;'>**Critics Choice**</span> (dashed line = source mean)<br>",
    caption = "Data: #TidyTuesday"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_textbox_simple(face = "italic"),
    plot.caption = element_text(face = "italic", size = 10),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#F7F7F7", color = NA)
  ) +
  scale_color_brewer(palette = "Dark2")





# 3/25 Amazon Budget Reports----------------------------------------------------------------------------------------
library(arrow)
library(tidytext)
library(stringr)

url <- "https://raw.githubusercontent.com/GregoryVdvinne/gregoryvdvinne.github.io/main/Amazon_Budgets/Data/Intermediate/all_reports_ocr_uncleaned.feather"
destfile <- tempfile(fileext = ".feather")  # Temporary file path
download.file(url, destfile, mode = "wb")   # Download file
unclean_data <- read_feather(destfile)  # Read .feather file

# Tokenize into single words
report_words_clean <-  unclean_data |>
  unnest_tokens(
    word, # Name of column of words in new dataframe
    text  # Name of column containing text in original dataframe
  ) |>
  # Remove stop words
  anti_join(stop_words, by = "word") |>
  # Remove some additional 'words'
  dplyr::filter(!(word %in% c(letters, LETTERS)),  # Single letters
                !str_detect(word, "\\d"),          # Words containing a number
                !str_detect(word, "_"))

report_words_clean |> 
  inner_join(get_sentiments(lexicon = "bing")) |> 
  group_by(year,sentiment) |> 
  count() |> 
  ggplot(aes(year,n,color = sentiment)) + 
  geom_line(aes(group = sentiment)) + 
  theme_minimal()

report_words_clean %>%
  group_by(year, word) %>%
  count(sort = TRUE, name = "n") %>%
  arrange(desc(n)) %>%
  group_by(year) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  mutate(ismax = n == max(n),
         label = if_else(ismax, word, NA_character_)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(word, n), n, fill = ismax)) +
  geom_col(color = "black", width = 0.8, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = c(`TRUE` = "#CB444A", `FALSE` = "grey90")) +
  labs(
    x = NULL,
    y = NULL,
    title = "Most Frequent Words in Amazon Annual Reports (2005–2023)",
    subtitle = "Top 10 per year; words most used per year highlighted in red.",
    caption = "Source: Amazon Annual Reports OCR (2005–2023)"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(face = "italic", size = 13),
    plot.caption = element_text(face = "italic", size = 10),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.background = element_rect(colour = NA, fill = "grey95"),
    strip.text = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "#F8F8F8", color = NA),
    panel.spacing = unit(1, "lines")
  ) 


# 5/06 Grant Funding --------------------------------------------------------------------------

# Fetch data from the CSV download link at https://grant-watch.us/nsf-data.html
raw_nsf_terminations <- readr::read_csv("https://drive.usercontent.google.com/download?id=1TFoyowiiMFZm73iU4YORniydEeHhrsVz&export=download")

# Clean the data
nsf_terminations <- 
  raw_nsf_terminations |> 
  janitor::clean_names() |> 
  mutate(usaspending_obligated = readr::parse_number(usaspending_obligated)) |> 
  mutate(in_cruz_list = !is.na(in_cruz_list)) |>
  mutate(grant_number = as.character(grant_number)) 

nsf_terminations |> 
  summarise(all=sum(usaspending_obligated,na.rm = TRUE)) |> 
  mutate(all = all |> scales::comma())

         