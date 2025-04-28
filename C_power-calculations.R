
# TITLE: WA BRFSS 2020 - Power Calculation 
# Last Edited: 04-27-25
# Description: In this script, we will conduct a power calculation to determine


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#loading packages
library(tidyverse)
library(reactable)
library(gt)
library(epiR)

#importing dataset
brfss20 <- read_dta("brfss_2020.dta")


### ---- BASICS OF POWER CALCULATIONS ---- ###

#power calculations solve for the unknown below

# power = 80%, 85%, 90%
# alpha = 0.05
# n = 8,365
# P(Y|A_0) = 0.0979 [3] 0.00497 [4]
        # = ended up not selecting 0.0039 [1], 0.008 [2]
    # prevalence of suicidality in unexposed group
# MDPR = unknown
    # minimally detectable prevalence ratio


### ---- POWER CALCULATIONS #1 ---- ###

pc.49.8 <- epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = .00497, #prevalence in unexposed group
  n = 8365, #number of total participants (already cleaned)
  power = 0.8, #fixing power to 80%
  r = 4105/4260 #ratio of exposed to unexposed
)
#minimally detectable prevalence ratio
pc.49.8$pr


### ---- POWER CALCULATIONS #2 ---- ###

pc.49.85 <- epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = .00497, #prevalence in unexposed group
  n = 8365, #number of total participants (already cleaned)
  power = 0.85, #fixing power to 85%
  r = 4105/4260 #ratio of exposed to unexposed
)
#minimally detectable prevalence ratio
pc.49.85$pr


### ---- POWER CALCULATIONS #3 ---- ###

pc.49.9 <- epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = .00497, #prevalence in unexposed group
  n = 8365, #number of total participants (already cleaned)
  power = 0.90, #fixing power to 90%
  r = 4105/4260 #ratio of exposed to unexposed
)
#minimally detectable prevalence ratio
pc.49.9$pr


### ---- POWER CALCULATIONS #4 ---- ###

pc.9.8 <- epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = .0979, #prevalence in unexposed group
  n = 8365, #number of total participants (already cleaned)
  power = 0.8, #fixing power to 90%
  r = 4105/4260 #ratio of exposed to unexposed
)
#minimally detectable prevalence ratio
pc.9.8$pr


### ---- POWER CALCULATIONS #5 ---- ###

pc.9.85 <- epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = .0979, #prevalence in unexposed group
  n = 8365, #number of total participants (already cleaned)
  power = 0.85, #fixing power to 90%
  r = 4105/4260 #ratio of exposed to unexposed
)
#minimally detectable prevalence ratio
pc.9.85$pr


### ---- POWER CALCULATIONS #6 ---- ###

pc.9.9 <- epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = .0979, #prevalence in unexposed group
  n = 8365, #number of total participants (already cleaned)
  power = 0.9, #fixing power to 90%
  r = 4105/4260 #ratio of exposed to unexposed
)
#minimally detectable prevalence ratio
pc.9.9$pr


### ---- MERGING & TABLES ---- ###

#merging seperate power calculations together
pc.49 <- rbind(pc.49.8, pc.49.85, pc.49.9)
pc.9 <- rbind(pc.9.8, pc.9.85, pc.9.9)

power_calculations <- rbind(pc.49, pc.9)

#cleaning 
power_calculations <- as.data.frame(power_calculations) %>%
  mutate(MDPR = str_extract_all(pr, "[0-9\\.]+")) %>%
  unnest_wider(MDPR, names_sep = "_")

power_calculations$MDPR_1 <- round(as.double(power_calculations$MDPR_1), digits = 3)
power_calculations$MDPR_2 <- round(as.double(power_calculations$MDPR_2), digits = 3)
power_calculations$or <- NULL
power_calculations$pr <- NULL
power_calculations$pdexp0 <- c(0.497, 0.497, 0.497, 9.790, 9.790, 9.790)


reactable(power_calculations %>% select(power, pdexp0, MDPR_1, MDPR_2),
          columns = list(power = colDef(name = "Fixed Power (%)"),
                         pdexp0 = colDef(name = "Prevalence in Unexposed Group"),
                         MDPR_1 = colDef(name = "MDPR (Protective)"),
                         MDPR_2 = colDef(name = "MDPR (Risk)")),
          bordered = T, outlined = T, striped = T,
          defaultColDef = colDef(align = "center"))


# --- PRIMARY AIM --- #


# --- SECONDARY AIM --- #



#[1] presence of suicidal ideation with no depression (Angst et al.)
#1. Angst J, Hengartner MP, Rogers J, et al. Suicidality in the prospective Zurich study: prevalence, risk factors and gender. Eur Arch Psychiatry Clin Neurosci. 2014;264(7):557-565. doi:10.1007/s00406-014-0500-1

#[2] presence of suicide attempt with no ACEs adults (Dube et al.)
#2. Dube SR, Anda RF, Felitti VJ, et al. Childhood Abuse, Household Dysfunction, and the Risk of Attempted Suicide Throughout the Life Span: Findings From the Adverse Childhood Experiences Study. JAMA. 2001;286(24):3089–3096. doi:10.1001/jama.286.24.3089

#[3] exact same suicidality question, with no ACEs for 15-17 year olds (Jia et al.)
#3. Jia Z, Wen X, Chen F, et al. Cumulative Exposure to Adverse Childhood Experience: Depressive Symptoms, Suicide Intensions and Suicide Plans among Senior High School Students in Nanchang City of China. IJERPH. 2020;17(13):4718. doi:10.3390/ijerph17134718

#[4] participants aged 65+, suicidal ideation since 60 years old (Sachs-Ericsson et al.)
#4. Sachs-Ericsson N, Corsentino E, Rushing NC, et al. Early childhood abuse and late-life suicidal ideation. Aging & Mental Health. 2013;17(4):489-494. doi:10.1080/13607863.2012.758236



