# TITLE: WA BRFSS 2020 - Table 2 
# Last Edited: 05-12-25
# Description: In this script, we will create a Table 2. 


### ---- SETUP & DATA IMPORT ---- ###
library(tidyverse)
library(haven)
library(gtsummary)
library(gt)
library(labelled)
library(dplyr)

#importing dataset
brfss20 <- read_dta("brfss_2020.dta")
brfss20 <- brfss20 %>%
  mutate(across(where(is.labelled), as_factor)) 

#clean up data to remove individuals who did not answer our categories
brfss20 <- brfss20 %>%
  filter(!is.na(suicide_f), !is.na(aces_f))
brfss20 <- brfss20 %>%
  filter(suicide_f != "")         # there were some individuals w/o category?

#recode suicide_f to binary factor variable
brfss20$suicide_f <- factor(brfss20$suicide_f, levels = c("No", "Yes"))




#####----CHI SQUARE TABLE (unadjusted) -----#####

unadj.table <- with(brfss20, table(aces_f, suicide_f), na.rm = TRUE)
View(unadj.table)
chisq.test(unadj.table)
#     Pearson's Chi-squared test
#     data:  unadj.table
#     X-squared = 208.51, df = 3, p-value < 2.2e-16
 


####---- TABLE DATA (adjusted) -----#####

#Note: is it only possible to do this with logistic regression?
adj.table <- glm(suicide_f ~ aces_f + age_group + raceth_f + marital_f + 
                   employ_f + physhlth_f + sleep_f, data = brfss20, family = binomial)
summary(adj.table)

#Find OR
exp(coef(adj.table))                

#Key findings from this (recall reference group is first coded)
#           Folks with 1 ACE showed 4.13 times the odds (compared to 0 ACE)
#           Folks with 3 ACE showed 8.88 times the odds!
#           OR decrease with age (meaning 18-24 age group highest odds)
#           AI/AN and Other race higher odds (compared to white, but not stat significant)
#           Domestic partnership and never married, and widowed higher odds (compared to married)
#           Over one year out of work and unable to work higher odds (compared to student)
#           Confusing physical health predictor, but all are higher odds (compared to zero poor health days)

summary(adj.table)


####---- TABLE 2 CREATION (LOG REGRESSION) ----####

table2 <- tbl_regression(
  adj.table,
  exponentiate = TRUE, 
  label = list(
    aces_f = "Adverse Childhood Experiences",
    age_group = "Age Group",
    raceth_f = "Race/Ethnicity",
    marital_f = "Marital Status",
    employ_f = "Employment",
    physhlth_f = "Days of Poor Physical Health", 
    sleep_f = "Sleep Duration"
  )
) %>%
  add_global_p() %>%        
  bold_labels() %>%
  modify_caption("**Table 2. Adjusted Associations between Suicidal Ideation by ACEs and Covariates**") %>%
  as_gt() %>%
  cols_width(
    OR ~ px(250)
  )



#view table 2
table2

########################################

####-----CREATING TABLE 2 w/ EPI.2BY2-------#######


#recode ACE factors into binaries (0 ACEs vs any ACE)

brfss20 <- brfss20 %>% mutate(
  aces_bin = case_when(
    aces_f %in% c("1 ACE", "2 ACEs", "3 ACEs") ~ 2,
    aces_f == "No ACEs" ~ 1,
    TRUE ~ NA_real_
  )
)

# Run 10% test to assess for confounding vs effect modification




#Run unadjusted analysis

first.2by2 <- with(brfss20, table(aces_bin, suicide_f))
first.2by2.output <- epi.2by2(first.2by2, method = 'cross.sectional')                   
first.2by2.output

#Run adjusted analysis (adjust for sex and race)
second.2by2 <- xtabs(~ aces_bin + suicide_f + sex_f + raceth_f, data = brfss20)
second.array <- array(second.2by2,
                     dim = c(2, 2, 6),
                     dimnames = list(
                       suicide_f = c('Yes', 'No'),
                       sex_f = c('Male', 'Female'),
                       raceth_f = c('White, NH', 'Black, NH', 'Asian, NH', 
                                    'AI/AN, NH', 'Other race, NH', 'Hispanic')))

second.output <- epi.2by2(dat = second.array, method = 'cross.sectional')
second.output
