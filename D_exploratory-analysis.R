# TITLE: WA BRFSS 2020 - Exploratory Analysis 
# Last Edited: 04-29-25
# Description: In this script, we will conduct exploratory analysis and 
    # create a Table 1. 


### ---- SETUP & DATA IMPORT ---- ###

#clearing workspace
rm(list = ls())

#loading packages
library(tidyverse)
library(reactable)
library(gt)

#importing dataset
brfss20 <- read_dta("brfss_2020.dta")
View(brfss20)
#new edit 
