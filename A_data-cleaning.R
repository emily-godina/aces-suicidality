
# TITLE: WA BRFSS 2020 - Basic Data Cleaning 
# Last Edited: 04-27-25
# Description: In this script, we will create a subset of our variables of 
      # interest to use for our future data manipulation and data analysis.


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#importing 2020 brfss dataset
brfss20 <- read_dta("WA_BRFSS_2020.dta")

#loading packages
library(tidyverse)


### ---- DATA MANAGEMENT & CLEANUP ---- ###

#viewing all variable names, alphabetically
(brfss20_var <- sort(colnames(brfss20)))

#subsetting only desired variables
brfss20 <- dplyr::select(brfss20,
              #adverse childhood experiences
              acedeprs2, acedrink, acedrugs, aceprisn, acedivrc, acepunch2, 
              acehurt2, aceswear2, acetouch2, acethem2, acehvsx2, 
              acescor1, acescor2, aceindx2,
              #potential covariates to consider
              sex, age,
              "_prace1","_mrace1", "_m_race", hispanc3, "_hispanc",
              misnervs, mishopls, misrstls, misdeprd, miseffrt, miswtles,  
              mistmnt, mi_score, smi,
              #outcome of interest
              suicide,
              #potential weighted variables to use
              "_llcpwt", agestd)

#important notes:
#this question does not exist in this dataset (2020)
    #how often do you get the social and emotional support that you need?
#although in codebook, these variables did not exist
    # "_age_g2", "_race1", "_impsex", "_impage"



### ---- QUANTIFYING EXPOSRES & OUTCOME ---- ###

#subset for just exposure and outcome variables
exp_out <- dplyr::select(brfss20,
                         acepunch2, acehurt2, aceswear2,
                         suicide)


# EXPOSURE QUESTION ONE
  table(exp_out$acepunch2)
  # Did your parents or adults in your home ever slap, 
  # hit, kick, punch or beat each other up?
    # Yes = 1443 
    # No = 9127
    # Don't Know = 50
    # Refused = 93

  
# EXPOSURE QUESTION TWO
  table(exp_out$acehurt2)
  # Did your parents or adult in your home ever hit, 
  # beat, kick, or physically hurt you in any way?
    # Yes = 1718 
    # No = 8879
    # Don't Know = 16
    # Refused = 96
 
   
# EXPOSURE QUESTION THREE
  table(exp_out$aceswear2)
  # Did your parents or adults in your home ever swear at you, 
   # insult you, or put you down?
     # Yes = 3410 
     # No = 7132
     # Don't Know = 53
     # Refused = 110
   
  
# OUTCOME QUESTION ONE
  table(exp_out$suicide)
  # At any time in the past 12 months did you seriously
  # think about trying to kill yourself?
      # Yes = 263 
      # No = 10525
      # Don't Know = 3
      # Refused = 26


  ### ---- EXPORTING NEW DATASET ---- ###
  
  saveRDS(brfss20, file = "brfss_wa_2020.rds")

