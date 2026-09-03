library(dplyr)
library(gt)
library(ggplot2)
library(tidyr)
library(readr)

all_recipes = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/all_recipes.csv")
cuisines = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv")

all_recipes

all_recipes |> 
  ggplot(aes(total_ratings, calories)) + 
  geom_point(size = 2, fill = "black") + 
  theme_minimal(
    ink = "tomato",
    paper = "cornsilk"
  ) + 
  theme(
    
  )

