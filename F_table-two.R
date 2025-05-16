# TITLE: WA BRFSS 2020 - Table 2 
# Last Edited: 05-16-25
# Description: In this script, we will create a Table 2. 


### ---- SETUP & DATA IMPORT ---- ###
library(tidyverse)
library(haven)
library(gtsummary)
library(gt)
library(labelled)
library(dplyr)
library(epiR)

#importing dataset
brfss20 <- read_dta("brfss_2020.dta")
brfss20 <- brfss20 %>%
  mutate(across(where(is.labelled), as_factor)) 

#clean up data to remove individuals who did not answer our categories
brfss20 <- brfss20 %>%
  filter(!is.na(suicide_f), !is.na(aces_f))
#brfss20 <- brfss20 %>%            # this line only leaves 207 people
#  filter(suicide_f != "")         # there were some individuals w/o category?
                                   # yah, weird bug but the only way i could create table1

#recode suicide_f to binary factor variable
brfss20$suicide_f <- factor(brfss20$suicide_f, levels = c("No", "Yes"))

#checking # of observations
table(brfss20$aces_f)
table(brfss20$suicide_f)



#####----CHI SQUARE TABLE (unadjusted) -----#####

unadj.table <- with(brfss20, table(aces_f, suicide_f), na.rm = TRUE)
View(unadj.table)
chisq.test(unadj.table)
#     Pearson's Chi-squared test
#     data:  unadj.table
#     X-squared = 208.51, df = 3, p-value < 2.2e-16
 
#at least one ace chisq test
unadj.table.atl1 <- with(brfss20, table(ace_atleast1, suicide_f), na.rm = TRUE)
View(unadj.table.atl1)
chisq.test(unadj.table.atl1)
#     Pearson's Chi-squared test
#     X-squared = 138.04, df = 1, p-value < 2.2e-16


####----- RUN 10% TEST ASSESING COVARIATES-------#######

# Run 10% test to assess for confounding vs effect modification




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


####-----CREATING TABLE 2 w/ EPI.2BY2-------#######

#running unadjusted analysis for no aces and at least 1 ace
one_ace2x2 <- with(brfss20, 
                   table(ace_atleast1_12, suicide_12))
one_ace2x2
epi.2by2(one_ace2x2, method = 'cross.sectional')
  #prevalence ratio = 8.07 (95% CI: 5.32, 12.23)


####-------STRATIFIED ANALYSIS-------#######

#effect modification, stratified analysis for sex
strat_f <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = sex_f == "Female")
strat_m <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = sex_f == "Male")

#naming our margins 
array_f <- array(strat_f,
                 dim = c(2,2), 
                 list(exposure = c('1+ ACEs', 'No ACEs'), 
                      outcomes = c('Suicidal Ideation', 'No Suicidal Ideation'))) 

array_m <- array(strat_m,
                 dim = c(2,2), 
                 list(exposure =c('1+ ACEs', 'No ACEs'), 
                      outcomes = c('Suicidal Ideation', 'No Suicidal Ideation'))) 

#running epi.2by2 analysis
epi.2by2(array_f, method = 'cross.sectional') #females
  #prevalence ratio = 11.38 (95% CI: 5.77, 22.44)
epi.2by2(array_m, method = 'cross.sectional') #males
 #prevalence ratio = 6.22 (95% CI: 3.65, 10.59)



