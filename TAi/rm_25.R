library(gander)
library(ellmer)
library(readxl)
library(ggplot2)
library(dplyr)
library(janitor)
library(gt)
library(gtsummary)
library(air)
library(haven)

grp1 <- read_excel(file.choose(),sheet = 1)
grp2 <- read_excel(file.choose(),sheet = 2)
grp3 <- read_excel(file.choose(),sheet = 3)
grp4 <- read_excel(file.choose(),sheet = 4)
grp5 <- read_excel(file.choose(),sheet = 5)
grp6 <- read_excel(file.choose(),sheet = 6)


# Social Media Use and Generations
grp1_cln <- 
  clean_names(grp1) %>% 
  slice_sample(n = 100, replace = TRUE) |> 
  mutate(
    age = age + 5,
    across(ends_with(c("_linkedin", "_facebook")), ~ . + 1)
  )

# Tattoos and Perception
grp2_cln <- 
  clean_names(grp2) %>% 
  slice_sample(n = 100, replace = TRUE)

# Emotional Regulation
grp3_cln <- 
  clean_names(grp3) %>% 
  slice_sample(n = 100, replace = TRUE)

# Sleep Habits
grp4_cln <- 
  clean_names(grp4) %>% 
  slice_sample(n = 100, replace = TRUE)

# Social Media and Personal and Physical Comfort
grp5_cln <- 
  clean_names(grp5) %>% 
  slice_sample(n = 100, replace = TRUE)

# Color and Emotion
grp6_cln <- 
  clean_names(grp6) %>% 
  slice_sample(n = 100, replace = TRUE)

write_sav(grp1_cln, "grp1_cln.sav")
write_sav(grp2_cln, "grp2_cln.sav")
write_sav(grp3_cln, "grp3_cln.sav")
write_sav(grp4_cln, "grp4_cln.sav")
write_sav(grp5_cln, "grp5_cln.sav")
write_sav(grp6_cln, "grp6_cln.sav")

