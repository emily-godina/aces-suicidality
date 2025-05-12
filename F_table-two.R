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
#Large association shown with unadjusted analysis 


####---- TABLE (adjusted) -----#####

#Note: is it only possible to do this with logistic regression?
adj.table <- glm(suicide_f ~ aces_f + age_group + raceth_f + marital_f + 
                   employ_f + physhlth_f + sleep_f, data = brfss20, family = binomial)
summary(adj.table)

#Find OR
exp(coef(adj.table))                

#Key findings from this
#           Folks with 1 ACE showed 4.13 times the odds
#           Folks with 3 ACE showed 8.88 times the odds!
#           OR decrease with age until 85+
#           AI/AN and Other race higher odds
#           Domestic partnership and never married, and widowed higher odds
#           Over one year out of work and unable to work higher odds
#           Confusing physical health predictor, but all are higher odds

summary(adj.table)
