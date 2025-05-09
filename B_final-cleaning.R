# TITLE: WA BRFSS 2020 - Final Data Cleaning
# Last Edited: 05-09-2025
# Description: In this script, we will clean and recode our dataset.


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#loading packages
library(tidyverse)
library(reactable)
library(haven)

#importing dataset
(brfss20 <- read_dta("brfss_2020.dta"))

#viewing all variable names, alphabetically
(brfss20_var <- sort(colnames(brfss20)))


### ---- DATA CLEANING ---- ###

#remove all mental health k6 items
brfss20 <- brfss20 %>%
  select(-misnervs, -mishopls, -misrstls, -misdeprd, -miseffrt, -miswtles, 
         -mistmnt)

#remove unused ace variables
brfss20 <- brfss20 %>%
  select(-acedeprs2, -acedrink, -acedrugs, -aceprisn, -acedivrc,
         -acetouch2, -acethem2, -acehvsx2)

#remove _ from variable names
names(brfss20) <- sub("^_", "", names(brfss20))



### ---- CREATE FACTOR VARIABLES ---- ###

#creating new factored age variable with 8 bins
brfss20 <- brfss20 %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 24 ~ "18-24",
    age >= 25 & age <= 34 ~ "25-34",
    age >= 35 & age <= 44 ~ "35-44",
    age >= 45 & age <= 54 ~ "45-54",
    age >= 55 & age <= 64 ~ "55-64",
    age >= 65 & age <= 74 ~ "65-74",
    age >= 75 & age <= 84 ~ "75-84",
    age >= 80             ~ "85+",
    TRUE ~ NA_character_
  )) %>%
  mutate(age_group = factor(age_group, levels = c("18-24", "25-34", "35-44", 
                                                  "45-54", "55-64", "65-74", 
                                                  "75-84", "85+")))


#creating new factored sex variable 
brfss20 <- brfss20 %>%
  mutate(sex_f = factor(sex, levels = c(1, 2), labels = c("Male", "Female")))



#creating new factored race/ethnicity variable
brfss20 <- brfss20 %>%
  mutate(
    raceth_f = case_when(
      hispanc == 1 ~ "Hispanic",
      hispanc == 2 & prace1 == 1 ~ "White, NH",
      hispanc == 2 & prace1 == 2 ~ "Black, NH",
      hispanc == 2 & prace1 == 3 ~ "Asian, NH",
      hispanc == 2 & prace1 == 4 ~ "AI/AN, NH",
      hispanc == 2 & prace1 == 6 ~ "Other race, NH",
      TRUE ~ NA_character_
    ),
    raceth_f = factor(raceth_f, levels = c(
      "White, NH", "Black, NH", "Asian, NH", "AI/AN, NH", "Other race, NH", "Hispanic"))
  )

#creating new factored marital status variable
brfss20 <- brfss20 %>%
  mutate(
    marital_f = case_when(
        marital_wa == 1 ~ "Married",
        marital_wa == 2 ~ "Divorced",
        marital_wa == 3 ~ "Widowed",
        marital_wa == 4 ~ "Seperated",
        marital_wa == 5 ~ "Never Married",
        marital_wa == 6 ~ "Unmarried Couple",
        marital_wa == 8 ~ "Domestic Partnership",
        TRUE ~ NA_character_
    ),
    marital_f = factor(marital_f, levels = c(
      "Married", "Domestic Partnership", "Unmarried Couple", "Never Married",
      "Seperated","Divorced", "Widowed"
      ))
)
  
#creating new factored variable for employment
brfss20 <- brfss20 %>%
  mutate(
    employ_f = case_when(
      employ1 == 1 ~ "Employed",
      employ1 == 2 ~ "Self-Employed",
      employ1 == 3 ~ "1+ Years Out of Work",
      employ1 == 4 ~ "<1 Year Out of Work",
      employ1 == 5 ~ "Homemaker",
      employ1 == 6 ~ "Student",
      employ1 == 7 ~ "Retired",
      employ1 == 8 ~ "Unable to Work",
      employ1 == 9 ~ "Refused",
      TRUE ~ NA_character_
    ),
    employ_f = factor(employ_f, levels = c(
      "Student", "Employed", "Self-Employed", "Homemaker", "Retired",
      "1+ Years Out of Work", "<1 Year Out of Work", "Unable to Work", "Refused"))
)

#creating new factored variable for physical health
brfss20 <- brfss20 %>%
  mutate(
    physhlth_f = case_when(
      physhlth == 88 ~ "None",
      physhlth >= 1 & physhlth <= 10 ~ "1-10 Days",
      physhlth >= 11 & physhlth <= 20 ~ "11-20 Days",
      physhlth >= 21 & physhlth <= 30 ~ "21-30 Days",
      TRUE ~ NA_character_
    ),
    physhlth_f = factor(physhlth_f, levels = c(
      "None", "1-10 Days", "11-20 Days", "21-30 Days"))
)

#creating new factored variable for sleep
brfss20 <- brfss20 %>%
  mutate(
    sleep_f = case_when(
      sleptim1 >= 1 & sleptim1 <= 6 ~ "Insufficient Sleep",
      sleptim1 >= 7 & sleptim1 <= 9 ~ "Sufficient Sleep",
      sleptim1 >= 10 & sleptim1 <= 18 ~ "Excessive Sleep",
      TRUE ~ NA_character_
    ),
    sleep_f = factor(sleep_f, levels = c(
      "Insufficient Sleep", "Sufficient Sleep", "Excessive Sleep"))
  )



### ---- RECODE VARIABLES ---- ###

#recode ace variables into 1,0, NAs
brfss20 <- brfss20 %>%
  mutate(
    acepunch2 = ifelse(acepunch2 %in% c(7, 9), NA, ifelse(acepunch2 == 1, 1, 0)), #household violence
    acehurt2  = ifelse(acehurt2 %in% c(7, 9), NA, ifelse(acehurt2 == 1, 1, 0)),   #physical abuse
    aceswear2 = ifelse(aceswear2 %in% c(7, 9), NA, ifelse(aceswear2 == 1, 1, 0))  #verbal abuse
  )


#label factor ace variables
#brfss20 <- brfss20 %>%
  #mutate(
    #acepunch_f = factor(acepunch2, levels = c(1, 0), labels = c("Yes", "No")),   #household violence
    #acehurt_f  = factor(acehurt2,  levels = c(1, 0), labels = c("Yes", "No")),   #physical abuse
    #aceswear_f = factor(aceswear2, levels = c(1, 0), labels = c("Yes", "No"))    #verbal abuse
  #)


#label all NAs as "Missing"
brfss20 <- brfss20 %>%
  mutate(
    across(c(sex_f, age_group, raceth_f,
    ), ~ fct_explicit_na(.x, na_level = "Missing")),  
   #across(c(acepunch_f, acehurt_f, aceswear_f,
   #), ~ fct_explicit_na(.x, na_level = "Don't Know/Refused"))
   )


#recode and label suicide variable
brfss20 <- brfss20 %>%
  mutate(
    suicide = case_when(
      suicide == 1 ~ 1,
      suicide == 2 ~ 0,
      suicide %in% c(7, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    suicide_f = factor(suicide, levels = c(1, 0), labels = c("Yes", "No"))
  )

### ---- CREATING NEW EXPOSURE VARIABLE ---- ###

#creating new variable that sums the aces
brfss20 <- brfss20 %>%
  mutate(
    aces_sum = rowSums(across(c(acepunch2, acehurt2, aceswear2)), na.rm = FALSE),
    aces_na = rowSums(is.na(across(c(acepunch2, acehurt2, aceswear2))))
    )

#checking quantities 
table(brfss20$aces_sum)
sum(is.na(brfss20$aces_sum))
    # 0 ACEs = 4260
    # 1 ACEs = 1998
    # 2 ACEs = 1079
    # 3 ACEs = 769
    # 1+ NAs = 259

#more information for the 259 with at least one NA
table(brfss20$aces_na)
    # Don't Know/Refused to 1 ACE = 166
    # Don't Know/Refused to 2 ACEs = 28
    # Don't Know/Refused to 3 ACEs = 65
    


### ---- EXPORTING NEW DATASET ---- ###

write_dta(brfss20, "brfss_2020.dta")
