
# TITLE: WA BRFSS 2020 - Basic Data Cleaning 
# Last Edited: 05-07-25
# Description: In this script, we will create a subset of our variables of 
      # interest to use for our future data manipulation and data analysis.


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#importing 2020 brfss dataset
brfss20 <- read_dta("WA_BRFSS_2020.dta")

#loading packages
library(tidyverse)
library(haven)


### ---- DATA MANAGEMENT & CLEANUP ---- ###

#viewing all variable names, alphabetically
(brfss20_var <- sort(colnames(brfss20)))

#subsetting only desired variables
brfss20 <- dplyr::select(brfss20,
              #adverse childhood experiences not using
                acedeprs2, acedrink, acedrugs, aceprisn, acedivrc, acetouch2, 
                acethem2, acehvsx2, acescor1, acescor2, aceindx2,
              #adverse childhood experiences exposures
              acepunch2, acehurt2, aceswear2,
              #potential covariates to consider and demographics
              sex, age,
              "_prace1","_mrace1", "_m_race", hispanc3, "_hispanc",
              misnervs, mishopls, misrstls, misdeprd, miseffrt, miswtles,  
              mistmnt, menthlth, "_ment14d",
              marital_wa, employ1, physhlth, "_phys14d", poorhlth, sleptim1,
              #outcome of interest
              suicide,
              #potential weighted variables to use
              "_llcpwt", agestd)

#important notes:
#this question does not exist in this dataset (2020)
    #how often do you get the social and emotional support that you need?
#although in codebook, these variables did not exist
    # "_age_g2", "_race1", "_impsex", "_impage"


### ---- ESTABLISHING SAMPLE SIZE ---- ###

#removing individuals that don't meet the requirements 

#there are 12,902 participants
brfss20 <- brfss20 %>%
  drop_na(acepunch2, acehurt2, aceswear2, sex, age)
#there are now 10,705 participants

#removing participants who experienced other ACEs but not our ACEs
brfss20 <- brfss20 %>%
  filter(!((acedeprs2 == 1 | acedrink == 1 | acedrugs == 1 | aceprisn == 1 |
          acedivrc == 1  | acetouch2 == 1 | acethem2 == 1 | acehvsx2 == 1) & 
          (acepunch2 == 2 & acehurt2 == 2 & aceswear2 == 2)))
#there are now 8,365 participants 


### ---- QUANTIFYING EXPOSRES & OUTCOME ---- ###

#subset for just exposure and outcome variables
exp_out <- dplyr::select(brfss20,
                         acepunch2, acehurt2, aceswear2,
                         suicide)


# EXPOSURE QUESTION ONE
  table(exp_out$acepunch2)
  # Did your parents or adults in your home ever slap, 
  # hit, kick, punch or beat each other up?
    # Yes = 1442
    # No = 6781
    # Don't Know = 50
    # Refused = 92

  
# EXPOSURE QUESTION TWO
  table(exp_out$acehurt2)
  # Did your parents or adult in your home ever hit, 
  # beat, kick, or physically hurt you in any way?
    # Yes = 1718 
    # No = 6535
    # Don't Know = 16
    # Refused = 96
 
   
# EXPOSURE QUESTION THREE
  table(exp_out$aceswear2)
  # Did your parents or adults in your home ever swear at you, 
   # insult you, or put you down?
     # Yes = 3410 
     # No = 4792
     # Don't Know = 53
     # Refused = 110
   
  
# OUTCOME QUESTION ONE
  table(exp_out$suicide)
  # At any time in the past 12 months did you seriously
  # think about trying to kill yourself?
      # Yes = 212 
      # No = 8127
      # Don't Know = 3
      # Refused = 23

# NO ACES OF INTEREST
  table((exp_out$acehurt2 == 2 & exp_out$acepunch2 == 2 & exp_out$aceswear2 == 2))
  # Individuals that did not experience household violence, physical abuse,
  # or verbal abuse
      # TRUE = 4260
      # FALSE = 4105
  

  ### ---- EXPORTING NEW DATASET ---- ###
  
  write_dta(brfss20, "brfss_2020.dta")

