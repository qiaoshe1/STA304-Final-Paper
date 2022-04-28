## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(janitor)
library(lubridate)
library(tidyverse)


## ---------------------------------------------------------------------------------------------------------------------------------
set.seed(895)

simulated_data <- tibble(
  X = seq(from = 1, to = 8112),
  id = seq(from = 50000, to = 58111),
  gender = sample(c('Male', 'Female', "NA"), 8112, replace = TRUE, prob = c(0.477, 0.522, 0.001)), 
  age = sample(c("20-24", "45-54", "35-44", "55-64", "25-34", "15-19", "65 and older"), 8112, replace = TRUE),
  province = sample(c("Nova Scotia", "British Columbia", "New Brunswick", "Prince Edward Island", "Quebec", "Ontario", "Alberta", "Saskatchewan", "Manitoba", "Newfoundland and Labrador"), 8112, replace = TRUE),
  smoked_cigarettes = sample(c('Yes', 'No', "NA"), 8112, replace = TRUE, prob = c(0.449, 0.550, 0.001)),
  age_first_time_cigarettes = sample(c("20-24", "Valid skip", "15-19", "10-14", "0-9", "25-29", "35-39", "30-34", "Not stated", "40 and over"), 8112, replace = TRUE),
  freq_cigarettes_30_days = sample(c("Not at all", "Valid skip", "Daily", "Less than daily, but at least once in the past month", "Not stated"), 8112, replace = TRUE),
  freq_little_cigars_30_days = sample(c("Not at all", NA, "Not stated", "Daily", "Less than daily, but at least once in the past month"), 8112, replace = TRUE, prob = c(0.975, 0.001, 0.01, 0.008, 0.006)),
  freq_cigars_30_days = sample(c("Not at all", "Less than once a week, but at least once in the past month", "Not stated", "At least once a week"), 8112, replace = TRUE, prob = c(0.975, 0.011, 0.006, 0.008)),
  freq_tabacco_pipe_30_days = sample(c("Not at all", "Less than once a week, but at least once in the past month", "Not stated", "At least once a week"), 8112, replace = TRUE, prob = c(0.975, 0.011, 0.006, 0.008)),
  freq_chewing_tabacco_30_days = sample(c("Not at all", "Less than once a week, but at least once in the past month", "Not stated", "At least once a week"), 8112, replace = TRUE, prob = c(0.975, 0.011, 0.006, 0.008)),
  freq_tabacco_waterpipe_30_days = sample(c("Not at all", "Less than once a week, but at least once in the past month", "Not stated", "At least once a week"), 8112, replace = TRUE, prob = c(0.975, 0.011, 0.006, 0.008)),
  when_quit_smoking = sample(c("Valid skip", "More than 5 years ago", "Less than one year ago", "1 to 2 years ago", "3 to 5 years ago", "Not Stated" ), 8112, replace = TRUE, prob = c(0.769, 0.08, 0.05, 0.091, 0.01, 0)),
  num_try_stop_smoking = sample(c("Valid skip", "0", "4 or more", "1", "2 or 3", "Not Stated" ), 8112, replace = TRUE),
  quit_smoking_by_vaping = sample(c("Valid skip", "No", "Yes", "Not Stated" ), 8112, replace = TRUE, prob = c(0.952, 0.032, 0.013, 0.003)),
  vaped = sample(c('Yes', 'No', NA), 8112, replace = TRUE, prob = c(0.449, 0.550, 0.001)),
  age_first_time_vape = sample(c("20-24", "Valid skip", "15-19", "10-14", "50-54", "25-29", "35-39", "30-34", "55-59", "Not stated", "40-44", "65 and over", "60-64", "45-49"), 8112, replace = TRUE),
  freq_vaped_30_days = sample(c("Valid skip", "Not at all", "Less than once a week, but at least once in the past month", "Not stated", "Daily", "Less than daily, but at least once in the past month"), 8112, replace = TRUE, prob = c(0.8, 0.14, 0.019, 0, 0.028, 0.013)),
  main_reason_vaping = sample(c("Valid skip", "Other", "To reduce stress", "Enjoy it", "To cut down on smoking cigarettes", "To avoid returning to smoking cigarettes", "Curiosity", "To use when cannot/not allowed to smoke cigarettes", "To quit smoking cigarettes", "Not stated"), 8112, replace = TRUE),
  num_try_stop_vaping = sample(c("Valid skip", "0", "4 or more", "1", "2 or 3", "Not Stated"), 8112, replace = TRUE),
  harm_vaping = sample(c("Somewhat less harmful than cigarettes", "About the same as cigarettes", "Don't know", "Much more harmful than cigarettes", "Somewhat more harmful than cigarettes", "Much less harmful than cigarettes", "Not stated"), 8112, replace = TRUE),
  smoked_cannabis = sample(c('Yes', 'No', NA), 8112, replace = TRUE, prob = c(0.449, 0.550, 0.001)),
  age_first_time_cannabis = sample(c("20-24", "Valid skip", "15-19", "10-14", "25-29", "35-39", "30-34", "Not stated", "40-44", "50 and over", "45-49"), 8112, replace = TRUE),
  freq_cannabis_30_days = sample(c("Valid skip", "Not at all", "Less than once a week, but at least once in the past month", "Not stated", "Daily", "Less than daily, but at least once in the past month"), 8112, replace = TRUE, prob = c(0.8, 0.14, 0.019, 0,  0.028, 0.013)),
  freq_cannabis_tobacco_mix_30_days = sample(c("Valid skip", "Not at all", "Less than once a week, but at least once in the past month", "Not stated", "Daily", "Less than daily, but at least once in the past month"), 8112, replace = TRUE, prob = c(0.8, 0.14, 0.019, 0,  0.028, 0.013)),
  first_product = sample(c("Valid skip", "Cannabis", "Cigarette", "An e-cigarette or vaping device", "Not stated"), 8112, replace = TRUE),
  freq_alcohol_30_days = sample(c("At least once a week", "Not at all", "Less than once a week, but at least once in the past month", "Not stated", "Daily"), 8112, replace = TRUE),
  smoking_status = sample(c("Never smoked", "Current smoker", "Former smoker", "Not stated"), 8112, replace = TRUE, prob = c(0.67, 0.098, 0.23, 0.002))
)

