# TITLE: WA BRFSS 2020 - Table 1 
# Last Edited: 04-06-25
# Description: In this script, we will create a Table 1. 


### ---- SETUP & DATA IMPORT ---- ###
library(tidyverse)
library(haven)
library(gtsummary)
library(gt)
library(labelled)


#importing dataset
brfss20 <- read_dta("brfss_2020.dta")

brfss20 <- brfss20 %>%
  mutate(across(where(is.labelled), as_factor)) 


### ---- CREATE OUR TABLE 1 ---- ###
table1 <- brfss20 %>%
  select(suicide_f, sex_f, age_group, raceth_f, aces_f,
         marital_f, employ_f, physhlth_f, sleep_f) %>%
  tbl_summary(
    by = aces_f,
    missing = "no",
    percent = "column",
    label = list(
      suicide_f ~ "Active Suicidal Ideation",
      sex_f ~ "Sex",
      age_group ~ "Age (years)",
      marital_f ~ "Marital Status",
      employ_f ~ "Employment Status",
      physhlth_f ~ "Poor Physical Health",
      sleep_f ~ "Average Sleep",
      raceth_f ~ "Race/Ethnicity"
    ),
    type = list(suicide_f ~ "categorical")
  )  %>%
  modify_caption("**Table 1** Sociodemographic Characteristics of Adults with Adverse Childhood Experience(s) by Active Suicidal Ideation in the Last 12 Months - 2020 Washington State, Behavioral Risk Factor Surveillance Survey (N = 8,365)") %>%
  modify_spanning_header(all_stat_cols() ~ 
                           "**Adverse Childhood Experiences**<br>*Household Violence, Physical Abuse, and Verbal Abuse*") |>
  remove_footnote_header(columns = all_stat_cols()) |>
  
  modify_footnote_spanning_header(
    footnote = "*Did your parents or adults in your home ever slap, hit, kick, punch or beat each other up?*<br>
                &nbsp; *Did your parents or adult in your home ever hit, beat, kick, or physically hurt you in any way?*<br>
                &nbsp; *Did your parents or adults in your home ever swear at you, insult you, or put you down?*",
    columns = all_stat_cols(),
    replace = FALSE
  ) |>
  modify_footnote_body(
    footnote = "NH = Non-Hispanic",
    columns = "label",
    rows = variable == "raceth_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "*At any time in the past 12 months did you seriously think about trying to kill yourself?*",
    columns = "label",
    rows = variable == "suicide_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "*Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?*",
    columns = "label",
    rows = variable == "physhlth_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "*On average, how many hours of sleep do you get in a 24-hour period?*",
    columns = "label",
    rows = variable == "sleep_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "<7 Hours = Insufficient Sleep <br>&nbsp;  7-9 Hours = Sufficient Sleep <br>&nbsp;  >9 Hours = Excessive Sleep",
    columns = "label",
    rows = variable == "sleep_f" & row_type == "label", replace = FALSE
  ) %>%
  modify_table_body(
    ~ .x %>%
      filter(!(variable == "suicide_f" & !label %in% c("Active Suicidal Ideation", "Yes", "No"))))

#view table1
table1
