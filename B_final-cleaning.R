# TITLE: WA BRFSS 2020 - Final Data Cleaning
# Last Edited: 05-6-2025
# Description: In this script, we will clean and recode our dataset.


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#loading packages
library(tidyverse)
library(reactable)
library(haven)


#importing dataset
brfss20 <- read_dta("brfss_2020.dta")
View(brfss20)



### ---- DATA CLEANING ---- ###

#remove all mental health k6 items
brfss20 <- brfss20 %>%
  select(-misnervs, -mishopls, -misrstls, -misdeprd, -miseffrt, -miswtles, 
         -mistmnt, -mi_score, -smi)

#remove unused ace variables
brfss20 <- brfss20 %>%
  select(-acedeprs2, -acedrink, -acedrugs, -aceprisn, -acedivrc,
         -acetouch2, -acethem2, -acehvsx2)

#remove _ from variable names
names(brfss20) <- sub("^_", "", names(brfss20))



### ---- CREATE FACTOR VARIABLES ---- ###

#categorize age into 8 bins
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


#label sex
brfss20 <- brfss20 %>%
  mutate(sex_f = factor(sex, levels = c(1, 2), labels = c("Male", "Female")))



#condense and label race/ethnicity
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


### ---- RECODE VARIABLES ---- ###

#recode ace variables into 1,0, NAs
brfss20 <- brfss20 %>%
  mutate(
    acepunch2 = ifelse(acepunch2 %in% c(7, 9), NA, ifelse(acepunch2 == 1, 1, 0)), #household violence
    acehurt2  = ifelse(acehurt2 %in% c(7, 9), NA, ifelse(acehurt2 == 1, 1, 0)),   #physical abuse
    aceswear2 = ifelse(aceswear2 %in% c(7, 9), NA, ifelse(aceswear2 == 1, 1, 0))  #verbal abuse
  )


#label factor ace variables
brfss20 <- brfss20 %>%
  mutate(
    acepunch_f = factor(acepunch2, levels = c(1, 0), labels = c("Yes", "No")),   #household violence
    acehurt_f  = factor(acehurt2,  levels = c(1, 0), labels = c("Yes", "No")),   #physical abuse
    aceswear_f = factor(aceswear2, levels = c(1, 0), labels = c("Yes", "No"))    #verbal abuse
  )


#label all NAs as "Missing"
brfss20 <- brfss20 %>%
  mutate(
    across(c(sex_f, age_group, raceth_f,
    ), ~ fct_explicit_na(.x, na_level = "Missing")),  
    across(c(acepunch_f, acehurt_f, aceswear_f,
    ), ~ fct_explicit_na(.x, na_level = "Don't Know/Refused")))


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

### ---- EXPORTING NEW DATASET ---- ###

write_dta(brfss20, "brfss_2020.dta")

