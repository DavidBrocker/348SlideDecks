# Load Libraries
library(dplyr, quietly = TRUE,warn.conflicts = FALSE)
library(ggplot2)
library(gt)
library(forcats)
# Read data file from CSV
billboard <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv',show_col_types = FALSE)

# Group by Artists
billboard |> 
  group_by(artist) |> 
  count(sort = TRUE) |> 
  ungroup() |> 
  top_n(10) 