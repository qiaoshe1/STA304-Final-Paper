## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(knitr)


## ----warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------
CTNS <- read.csv(file = "CTNS.csv")


## ----warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------
# Select variables

CTNS_new <- CTNS %>% 
  mutate(PUMFID, GENDER, TBC_05AR, TBC_05BR, TBC_10AR, TBC_10BR, TBC_20R, TBC_35R, TBC_40R, OTP_05A, OTP_05BR, OTP_05CR, OTP_05DR, OTP_05ER, VAP_05AR, VAP_05BR, VAP_10R, VAP_35R, VAP_45R, VAP_60, CAN_05AR, CAN_05BR, CAN_10AR, CAN_10BR, CAN_15AR, CAN_15BR, IU_05R, ALC_05, AGEGROUP, PROV_C, DV_SSR, DV_CN30R, DV_VP30R, DV_VC30R, DV_ALC30, FIRSTTRR)

CTNS_new <- CTNS_new %>% 
  rename(id = PUMFID, 
         gender = GENDER, 
         age = AGEGROUP,
         province = PROV_C,
         smoked_cigarettes = TBC_05AR, 
         age_first_time_cigarettes = TBC_05BR, 
         freq_cigarettes_30_days = TBC_10AR, 
         freq_little_cigars_30_days = OTP_05A,
         freq_cigars_30_days = OTP_05BR, 
         freq_tabacco_pipe_30_days = OTP_05CR, 
         freq_chewing_tabacco_30_days = OTP_05DR, 
         freq_tabacco_waterpipe_30_days = OTP_05ER,
         when_quit_smoking = TBC_20R, 
         num_try_stop_smoking = TBC_35R, 
         quit_smoking_by_vaping = TBC_40R, 
         vaped = VAP_05AR, 
         age_first_time_vape = VAP_05BR, 
         freq_vaped_30_days = VAP_10R, 
         main_reason_vaping = VAP_35R, 
         num_try_stop_vaping = VAP_45R, 
         harm_vaping = VAP_60, 
         smoked_cannabis = CAN_05AR, 
         age_first_time_cannabis = CAN_05BR, 
         freq_cannabis_30_days = CAN_10AR, 
         freq_cannabis_tobacco_mix_30_days = CAN_15AR, 
         first_product = IU_05R,
         freq_alcohol_30_days = ALC_05,
         smoking_status = DV_SSR
    ) 


## ----warning = FALSE, message = FALSE---------------------------------------------------------------------------------------------
# Change from strings into numbers

# Basic Information
CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(age = case_when(
    age == 01 ~ "15-19",
    age == 02 ~ "20-24",
    age == 03 ~ "25-34",
    age == 04 ~ "35-44",
    age == 05 ~ "45-54",
    age == 06 ~ "55-64",
    age == 07 ~ "65 and older")
    )

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(province = case_when(
    province == 10 ~ "Newfoundland and Labrador",
    province == 11 ~ "Prince Edward Island",
    province == 12 ~ "Nova Scotia",
    province == 13 ~ "New Brunswick",
    province == 24 ~ "Quebec",
    province == 35 ~ "Ontario",
    province == 46 ~ "Manitoba",
    province == 47 ~ "Saskatchewan",
    province == 48 ~ "Alberta",
    province == 59 ~ "British Columbia")
    ) 


# Tobacco
CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(smoked_cigarettes = case_when(
    smoked_cigarettes == 1 ~ "Yes",
    smoked_cigarettes == 2 ~ "No",
    smoked_cigarettes == 3 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(age_first_time_cigarettes = case_when(
    age_first_time_cigarettes == 01 ~ "0-9",
    age_first_time_cigarettes == 02 ~ "10-14",
    age_first_time_cigarettes == 03 ~ "15-19",
    age_first_time_cigarettes == 04 ~ "20-24",
    age_first_time_cigarettes == 05 ~ "25-29",
    age_first_time_cigarettes == 06 ~ "30-34",
    age_first_time_cigarettes == 07 ~ "35-39",
    age_first_time_cigarettes == 08 ~ "40 and over",
    age_first_time_cigarettes == 96 ~ "Valid skip",
    age_first_time_cigarettes == 99 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_cigarettes_30_days = case_when(
    freq_cigarettes_30_days == 1 ~ "Daily",
    freq_cigarettes_30_days == 2 ~ "Less than daily, but at least once in the past month",
    freq_cigarettes_30_days == 4 ~ "Not at all",
    freq_cigarettes_30_days == 6 ~ "Valid skip",
    freq_cigarettes_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_little_cigars_30_days = case_when(
    freq_little_cigars_30_days == 1 ~ "Daily",
    freq_little_cigars_30_days == 2 ~ "Less than daily, but at least once in the past month",
    freq_little_cigars_30_days == 4 ~ "Not at all",
    freq_little_cigars_30_days == 6 ~ "Valid skip",
    freq_little_cigars_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_cigars_30_days = case_when(
    freq_cigars_30_days == 2 ~ "At least once a week",
    freq_cigars_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_cigars_30_days == 4 ~ "Not at all",
    freq_cigars_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_tabacco_pipe_30_days = case_when(
    freq_tabacco_pipe_30_days == 2 ~ "At least once a week",
    freq_tabacco_pipe_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_tabacco_pipe_30_days == 4 ~ "Not at all",
    freq_tabacco_pipe_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_chewing_tabacco_30_days = case_when(
    freq_chewing_tabacco_30_days == 2 ~ "At least once a week",
    freq_chewing_tabacco_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_chewing_tabacco_30_days == 4 ~ "Not at all",
    freq_chewing_tabacco_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_tabacco_waterpipe_30_days = case_when(
    freq_tabacco_waterpipe_30_days == 2 ~ "At least once a week",
    freq_tabacco_waterpipe_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_tabacco_waterpipe_30_days == 4 ~ "Not at all",
    freq_tabacco_waterpipe_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(when_quit_smoking = case_when(
    when_quit_smoking == 1 ~ "Less than one year ago",
    when_quit_smoking == 2 ~ "1 to 2 years ago",
    when_quit_smoking == 3 ~ "3 to 5 years ago",
    when_quit_smoking == 4 ~ "More than 5 years ago",
    when_quit_smoking == 6 ~ "Valid skip",
    when_quit_smoking == 9 ~ "Not Stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(num_try_stop_smoking = case_when(
    num_try_stop_smoking == 1 ~ "0",
    num_try_stop_smoking == 2 ~ "1",
    num_try_stop_smoking == 3 ~ "2 or 3",
    num_try_stop_smoking == 4 ~ "4 or more",
    num_try_stop_smoking == 6 ~ "Valid skip",
    num_try_stop_smoking == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(quit_smoking_by_vaping = case_when(
    quit_smoking_by_vaping == 1 ~ "Yes",
    quit_smoking_by_vaping == 2 ~ "No",
    quit_smoking_by_vaping == 6 ~ "Valid skip",
    quit_smoking_by_vaping == 9 ~ "Not stated")
    ) 


# Vaping
CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(vaped = case_when(
    vaped == 1 ~ "Yes",
    vaped == 2 ~ "No",
    vaped == 3 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(age_first_time_vape = case_when(
    age_first_time_vape == 02 ~ "10-14",
    age_first_time_vape == 03 ~ "15-19",
    age_first_time_vape == 04 ~ "20-24",
    age_first_time_vape == 05 ~ "25-29",
    age_first_time_vape == 06 ~ "30-34",
    age_first_time_vape == 07 ~ "35-39",
    age_first_time_vape == 08 ~ "40-44",
    age_first_time_vape == 09 ~ "45-49",
    age_first_time_vape == 10 ~ "50-54",
    age_first_time_vape == 11 ~ "55-59",
    age_first_time_vape == 12 ~ "60-64",
    age_first_time_vape == 13 ~ "65 and over",
    age_first_time_vape == 96 ~ "Valid skip",
    age_first_time_vape == 99 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_vaped_30_days = case_when(
    freq_vaped_30_days == 1 ~ "Daily",
    freq_vaped_30_days == 2 ~ "Less than daily, but at least once in the past month",
    freq_vaped_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_vaped_30_days == 4 ~ "Not at all",
    freq_vaped_30_days == 6 ~ "Valid skip",
    freq_vaped_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(main_reason_vaping = case_when(
    main_reason_vaping == 01 ~ "Curiosity",
    main_reason_vaping == 02 ~ "Enjoy it",
    main_reason_vaping == 03 ~ "To reduce stress",
    main_reason_vaping == 04 ~ "To quit smoking cigarettes",
    main_reason_vaping == 05 ~ "To cut down on smoking cigarettes",
    main_reason_vaping == 06 ~ "To use when cannot/not allowed to smoke cigarettes",
    main_reason_vaping == 07 ~ "To avoid returning to smoking cigarettes",
    main_reason_vaping == 08 ~ "Other",
    main_reason_vaping == 96 ~ "Valid skip",
    main_reason_vaping == 99 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(num_try_stop_vaping = case_when(
    num_try_stop_vaping == 1 ~ "0",
    num_try_stop_vaping == 2 ~ "1",
    num_try_stop_vaping == 3 ~ "2 or 3",
    num_try_stop_vaping == 4 ~ "4 or more",
    num_try_stop_vaping == 6 ~ "Valid skip",
    num_try_stop_vaping == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(harm_vaping = case_when(
    harm_vaping == 1 ~ "Much less harmful than cigarettes",
    harm_vaping == 2 ~ "Somewhat less harmful than cigarettes",
    harm_vaping == 3 ~ "About the same as cigarettes",
    harm_vaping == 4 ~ "Somewhat more harmful than cigarettes",
    harm_vaping == 5 ~ "Much more harmful than cigarettes",
    harm_vaping == 7 ~ "Don't know",
    harm_vaping == 9 ~ "Not stated")
    ) 


#Cannabis
CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(smoked_cannabis = case_when(
    smoked_cannabis == 1 ~ "Yes",
    smoked_cannabis == 2 ~ "No",
    smoked_cannabis == 3 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(age_first_time_cannabis = case_when(
    age_first_time_cannabis == 01 ~ "0-9",
    age_first_time_cannabis == 02 ~ "10-14",
    age_first_time_cannabis == 03 ~ "15-19",
    age_first_time_cannabis == 04 ~ "20-24",
    age_first_time_cannabis == 05 ~ "25-29",
    age_first_time_cannabis == 06 ~ "30-34",
    age_first_time_cannabis == 07 ~ "35-39",
    age_first_time_cannabis == 08 ~ "40-44",
    age_first_time_cannabis == 09 ~ "45-49",
    age_first_time_cannabis == 10 ~ "50 and over",
    age_first_time_cannabis == 96 ~ "Valid skip",
    age_first_time_cannabis == 99 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_cannabis_30_days = case_when(
    freq_cannabis_30_days == 1 ~ "Daily",
    freq_cannabis_30_days == 2 ~ "Less than daily, but at least once a week",
    freq_cannabis_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_cannabis_30_days == 4 ~ "Not at all",
    freq_cannabis_30_days == 6 ~ "Valid skip",
    freq_cannabis_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_cannabis_tobacco_mix_30_days = case_when(
    freq_cannabis_tobacco_mix_30_days == 1 ~ "Daily",
    freq_cannabis_tobacco_mix_30_days == 2 ~ "Less than daily, but at least once a week",
    freq_cannabis_tobacco_mix_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_cannabis_tobacco_mix_30_days == 4 ~ "Not at all",
    freq_cannabis_tobacco_mix_30_days == 6 ~ "Valid skip",
    freq_cannabis_tobacco_mix_30_days == 9 ~ "Not stated")
    ) 


# Others
CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(first_product = case_when(
    first_product == 1 ~ "Cigarette",
    first_product == 2 ~ "An e-cigarette or vaping device",
    first_product == 3 ~ "Cannabis",
    first_product == 6 ~ "Valid skip",
    first_product == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(freq_alcohol_30_days = case_when(
    freq_alcohol_30_days == 1 ~ "Daily",
    freq_alcohol_30_days == 2 ~ "At least once a week", 
    freq_alcohol_30_days == 3 ~ "Less than once a week, but at least once in the past month",
    freq_alcohol_30_days == 4 ~ "Not at all",
    freq_alcohol_30_days == 9 ~ "Not stated")
    ) 

CTNS_new <- CTNS_new %>% 
  rowwise() %>% 
  mutate(smoking_status = case_when(
    smoking_status == 1 ~ "Current smoker",
    smoking_status == 2 ~ "Former smoker",
    smoking_status == 3 ~ "Never smoked",
    smoking_status == 9 ~ "Not stated")
    ) 


## ---------------------------------------------------------------------------------------------------------------------------------
CTNS_clean <- CTNS_new %>% 
  transmute(id, gender, age, province, smoked_cigarettes, age_first_time_cigarettes, freq_cigarettes_30_days, freq_little_cigars_30_days, freq_cigars_30_days, freq_tabacco_pipe_30_days, freq_chewing_tabacco_30_days, freq_tabacco_waterpipe_30_days, when_quit_smoking, num_try_stop_smoking, quit_smoking_by_vaping, vaped, age_first_time_vape, freq_vaped_30_days, main_reason_vaping, num_try_stop_vaping, harm_vaping, smoked_cannabis, age_first_time_cannabis, freq_cannabis_30_days, freq_cannabis_tobacco_mix_30_days, first_product, freq_alcohol_30_days, smoking_status)


## ---------------------------------------------------------------------------------------------------------------------------------
write.csv(CTNS_clean, file = "CTNS_clean.csv")

