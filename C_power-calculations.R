
# TITLE: WA BRFSS 2020 - Power Calculation 
# Last Edited: 04-27-25
# Description: In this script, we will conduct a power calculation to determine


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#loading packages
library(tidyverse)
library(epiR)


### ---- QUANTIFYING EXPOSURES ---- ###



### ---- BASICS OF POWER CALCULATIONS ---- ###

#power calculations solve for the unknown below

# power
# alpha
# n
# P(Y|A_0)
# MDPR

### ---- POWER CALCULATIONS #1 ---- ###







### ---- CLASS EXAMPLE BELOW ---- ###


#EXAMPLE 1
#number exp = 6500
#number unexp = 30000

#calc min detectable prevalence ratio
epi.ssxsectn(
  pdexp1 = NA, #expected prevalence of outcome in exp
  pdexp0 = .2, #expected prevalence of outcome in unexp 
  n = 36500, #(if accounting for missing data this would be 337438 individuals)
  power = 0.8, #fix power at 80%
  r = 6500/30000 #ratio of exp to unexp (this may change if accounting for missing data)
)


#calc min detectable prevalence ratio
epi.ssxsectn(
  pdexp1 = NA, #expected prevalence of outcome in exp
  pdexp0 = .2, #expected prevalence of outcome in unexp 
  n = 36500, #(if accounting for missing data this would be 337438 individuals)
  power = 0.8, #fix power at 80%
  r = 6500/30000 #ratio of exp to unexp (this may change if accounting for missing data)
)


#OUR DATA
epi.ssxsectn(
  pdexp1 = NA, #expected prevalence of outcome in exp
  pdexp0 = 0.4, #expected prevalence of outcome in unexp
  n = 12500, #total 5750*2
  power = 0.8, #fix power at 80%
)