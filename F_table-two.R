# TITLE: WA BRFSS 2020 - Table 2 
# Last Edited: 05-19-25
# Description: In this script, we will conduct MH stratified analysis and 
   # create a Table 2. 


### ---- SETUP & DATA IMPORT ---- ###
library(tidyverse)
library(haven)
library(labelled)
library(dplyr)
library(epiR)
library(huxtable)

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
#View(unadj.table)
chisq.test(unadj.table)
#     Pearson's Chi-squared test
#     data:  unadj.table
#     X-squared = 208.51, df = 3, p-value < 2.2e-16
 
#at least one ace chisq test
unadj.table.atl1 <- with(brfss20, table(ace_atleast1, suicide_f), na.rm = TRUE)
#View(unadj.table.atl1)
chisq.test(unadj.table.atl1)
#     Pearson's Chi-squared test
#     X-squared = 138.04, df = 1, p-value < 2.2e-16


####----- RUN 10% TEST ASSESING COVARIATES-------#######

# Run 10% test to assess for confounding vs effect modification



####-----CREATING TABLE 2 w/ EPI.2BY2-------#######

#running unadjusted analysis for no aces and at least 1 ace
one_ace2x2 <- with(brfss20, 
                   table(ace_atleast1_12, suicide_12))
one_ace2x2
PR <- epi.2by2(one_ace2x2, method = 'cross.sectional')
  #prevalence ratio = 8.07 (95% CI: 5.32, 12.23)


####-------STRATIFIED ANALYSIS-------#######

#effect modification, stratified analysis for sex
strat_f <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = sex_f == "Female")
strat_m <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = sex_f == "Male")

#naming our margins 
array_f <- array(strat_f,
                 dim = c(2,2), 
                 list(exposure = c('1+ PVWs', 'No PVWs'), 
                      outcomes = c('Suicidal Ideation', 'No Suicidal Ideation'))) 

array_m <- array(strat_m,
                 dim = c(2,2), 
                 list(exposure =c('1+ PVWs', 'No PVWs'), 
                      outcomes = c('Suicidal Ideation', 'No Suicidal Ideation'))) 

#running epi.2by2 analysis
PR.f <- epi.2by2(array_f, method = 'cross.sectional') #females
  #prevalence ratio = 11.38 (95% CI: 5.77, 22.44)
PR.m <- epi.2by2(array_m, method = 'cross.sectional') #males
 #prevalence ratio = 6.22 (95% CI: 3.65, 10.59)


#effect modification, stratified analysis for race/ethnicity
(table(brfss20$raceth_f, brfss20$suicide_f))
#creating stratification
strat_w1 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_wht == 1) #white
strat_w2 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_wht == 2) #bipoc
strat_b1 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_blk == 1) #black
strat_as1 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_asn == 1) #asian
strat_ai1 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_aian == 1) #ai/an
strat_o1 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_oth == 1) #other
strat_h1 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = race_hisp == 1) #hispanic

#naming our margins and creating array
make_array <- function(strat, out_name = deparse(substitute(strat))) {
  arr <- array(strat,
               dim = c(2, 2),
               list(exposure = c("1+ PVWs", "No PVWs"),
               outcomes = c("Suicidal Ideation", "No Suicidal Ideation")))
  assign(sub("strat_", "array_", out_name), arr, envir = .GlobalEnv)
}

#continuity correction for 0 people were not exposed and experienced the outcome
strat_b1 <- strat_b1 + 0.5
strat_as1 <- strat_as1 + 0.5

#running epi.2by2 analysis
(PR.white <- epi.2by2(make_array(strat_w1), method = 'cross.sectional')) #white
#prevalence ratio = 7.63 (95% CI: 4.72, 12.32)
(PR.bipoc <- epi.2by2(make_array(strat_w2), method = 'cross.sectional')) #bipoc
#prevalence ratio = 9.32 (95% CI: 3.72, 23.34)
(PR.black <- epi.2by2(make_array(strat_b1), method = 'cross.sectional')) #black
#prevalence ratio = 7.56 (95% CI: 0.43, 134.46) -- TOO LARGE CI
(PR.asian <- epi.2by2(make_array(strat_as1), method = 'cross.sectional')) #asian
#prevalence ratio = 4.43 (95% CI: 0.25, 77.92) -- TOO LARGE CI
(PR.aian <- epi.2by2(make_array(strat_ai1), method = 'cross.sectional')) #ai/an
#prevalence ratio = 6.96 (95% CI: 1.98, 24.52) 
(PR.other <- epi.2by2(make_array(strat_o1), method = 'cross.sectional')) #other
#prevalence ratio = 6.12 (95% CI: 0.76, 49.04) -- TOO LARGE CI
(PR.hispanic <- epi.2by2(make_array(strat_h1), method = 'cross.sectional')) #hispanic
#prevalence ratio = 17.22 (95% CI: 2.32, 128.11) -- TOO LARGE CI


#effect modification, stratified analysis for age 
(table(brfss20$age_group, brfss20$suicide_f))
#creating stratification
strat_1824 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_1824 == 1) #18-24
strat_2534 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_2534 == 1) #25-34
strat_3544 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_3544 == 1) #35-44
strat_4554 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_4554 == 1) #45-54
strat_5564 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_5564 == 1) #55-64
strat_6574 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_6574 == 1) #65-74
strat_7584 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_7584 == 1) #75-84
strat_85ov <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_85ov == 1) #85+
#new age stratification
strat_1834 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_1834 == 1) #18-34
strat_3554 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_3554 == 1) #35-54
strat_5574 <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_5574 == 1) #55-74
strat_75ov <- xtabs(~ace_atleast1_12 + suicide_12, data = brfss20, subset = age_75ov == 1) #75+


#running epi.2by2 analysis
(PR_1824 <- epi.2by2(make_array(strat_1824), method = 'cross.sectional')) #18-24
#prevalence ratio = 6.76 (95% CI: 2.74, 16.70)
(PR_2534 <- epi.2by2(make_array(strat_2534), method = 'cross.sectional')) #25-34
#prevalence ratio = 22.49 (95% CI: 3.10, 163.14) -- TOO LARGE CI
(PR_3544 <- epi.2by2(make_array(strat_3544), method = 'cross.sectional')) #35-44
#prevalence ratio = 4.75 (95% CI: 1.85, 12.20) 
(PR_4554 <- epi.2by2(make_array(strat_4554), method = 'cross.sectional')) #45-54
#prevalence ratio = 4.12 (95% CI: 1.60, 10.60) 
(PR_5564 <- epi.2by2(make_array(strat_5564), method = 'cross.sectional')) #55-64
#prevalence ratio = 22.86 (95% CI: 3.10, 168.60)  -- TOO LARGE CI
(PR_6574 <- epi.2by2(make_array(strat_6574), method = 'cross.sectional')) #65-74
#prevalence ratio = 2.52 (95% CI: 0.92, 6.91)
(PR_7584 <- epi.2by2(make_array(strat_7584), method = 'cross.sectional')) #75-84
#prevalence ratio = 10.63 (95% CI: 1.25, 90.61) -- TOO LARGE CI
(PR_85ov <- epi.2by2(make_array(strat_85ov), method = 'cross.sectional')) #85+
#prevalence ratio = 11.60 (95% CI: 1.23, 109.54) -- TOO LARGE CI
#--------------#
(PR_1834 <- epi.2by2(make_array(strat_1834), method = 'cross.sectional')) #18-34
#prevalence ratio = 9.13 (95% CI: 4.02, 20.75)
(PR_3554 <- epi.2by2(make_array(strat_3554), method = 'cross.sectional')) #35-54
#prevalence ratio = 4.11 (95% CI: 2.33, 8.59)
(PR_5574 <- epi.2by2(make_array(strat_5574), method = 'cross.sectional')) #55-74
#prevalence ratio = 5.89 (95% CI: 2.62, 13.24) 
(PR_75ov <- epi.2by2(make_array(strat_75ov), method = 'cross.sectional')) #75+
#prevalence ratio = 9.59 (95% CI: 2.04, 44.93) 


p_value <- map_dbl(
  list(PR, PR.m, PR.f, PR_1824, PR_2534, PR_3544, PR_4554, PR_5564, PR_6574, PR_7584,
       PR_85ov, PR.white, PR.black, PR.asian, PR.aian, PR.other, PR.hispanic, PR.bipoc,
       PR_1834, PR_3554, PR_5574, PR_75ov),
  ~ .x$massoc.detail$chi2.strata.fisher$p.value.2s)  

####------CREATING STATUM SUMS-------####
(sum_Y <- sum(one_ace2x2[1:2,1])) #ASI overall
(sum_N <- sum(one_ace2x2[1:2,2])) #overall
(sum_mY <- sum(strat_m[1:2,1])) #ASI male
(sum_mN <- sum(strat_m[1:2,2])) #male
(sum_fY <- sum(strat_f[1:2,1])) #ASI female
(sum_fN <- sum(strat_f[1:2,2])) #female
(sum_18Y <- sum(strat_1824[1:2,1])) #ASI 18-24
(sum_18N <- sum(strat_1824[1:2,2])) #18-24
(sum_25Y <- sum(strat_2534[1:2,1])) #ASI 25-34
(sum_25N <- sum(strat_2534[1:2,2])) #25-34
(sumsum18Y <- sum_18Y + sum_25Y)
(sumsum18N <- sum_18N + sum_25N)
(sum_35Y <- sum(strat_3544[1:2,1])) #ASI 35-44
(sum_35N <- sum(strat_3544[1:2,2])) #35-44
(sum_45Y <- sum(strat_4554[1:2,1])) #ASI 45-54
(sum_45N <- sum(strat_4554[1:2,2])) #45-54
(sumsum35Y <- sum_35Y + sum_45Y)
(sumsum35N <- sum_35N + sum_45N)
(sum_55Y <- sum(strat_5564[1:2,1])) #ASI 55-64
(sum_55N <- sum(strat_5564[1:2,2])) #55-64
(sum_65Y <- sum(strat_6574[1:2,1])) #ASI 65-74
(sum_65N <- sum(strat_6574[1:2,2])) #65-74
(sumsum55Y <- sum_55Y + sum_65Y)
(sumsum55N <- sum_55N + sum_65N)
(sum_75Y <- sum(strat_7584[1:2,1])) #ASI 75-84
(sum_75N <- sum(strat_7584[1:2,2])) #75-84
(sum_85Y <- sum(strat_85ov[1:2,1])) #ASI 85+
(sum_85N <- sum(strat_85ov[1:2,2])) #85+
(sumsum75Y <- sum_75Y + sum_85Y)
(sumsum75N <- sum_75N + sum_85N)
(sum_w1Y <- sum(strat_w1[1:2,1])) #ASI white
(sum_w1N <- sum(strat_w1[1:2,2])) #white
(sum_w2Y <- sum(strat_w2[1:2,1])) #ASI bipoc
(sum_w2N <- sum(strat_w2[1:2,2])) #bipoc
(sum_bY <- sum(strat_b1[1:2,1])) #ASI black
(sum_bN <- sum(strat_b1[1:2,2])) #black
(sum_asY <- sum(strat_as1[1:2,1])) #ASI asian
(sum_asN <- sum(strat_as1[1:2,2])) #asian
(sum_aiY <- sum(strat_ai1[1:2,1])) #ASI ai/an
(sum_aiN <- sum(strat_ai1[1:2,2])) #ai/an
(sum_oY <- sum(strat_o1[1:2,1])) #ASI other
(sum_oN <- sum(strat_o1[1:2,2])) #other
(sum_hY <- sum(strat_h1[1:2,1])) #ASI hispanic
(sum_hN <- sum(strat_h1[1:2,2])) #hispanic


####------COMBINING INTO TABLE 2-------####
table2_bind <- bind_rows(
  Overall     = as.data.frame(t(c(sum_Y, sum_N, PR$massoc.summary[1, ],  p_value = p_value[1]))),
  Male        = as.data.frame(t(c(sum_mY, sum_mN, PR.m$massoc.summary[1, ], p_value = p_value[2]))),
  Female      = as.data.frame(t(c(sum_fY, sum_fN, PR.f$massoc.summary[1, ], p_value = p_value[3]))),
  #"Age 18-24" = as.data.frame(t(c(sum_18Y, sum_18N, PR_1824$massoc.summary[1, ], p_value = p_value[4]))),
  #"Age 25-34" = as.data.frame(t(c(sum_25Y, sum_25N, PR_2534$massoc.summary[1, ], p_value = p_value[5]))),
  #"Age 35-44" = as.data.frame(t(c(sum_35Y, sum_35N, PR_3544$massoc.summary[1, ], p_value = p_value[6]))),
  #"Age 45-54" = as.data.frame(t(c(sum_45Y, sum_45N, PR_4554$massoc.summary[1, ], p_value = p_value[7]))),
  #"Age 55-64" = as.data.frame(t(c(sum_55Y, sum_55N, PR_5564$massoc.summary[1, ], p_value = p_value[8]))),
  #"Age 65-74" = as.data.frame(t(c(sum_65Y, sum_65N, PR_6574$massoc.summary[1, ], p_value = p_value[9]))),
  #"Age 75-84" = as.data.frame(t(c(sum_75Y, sum_75N, PR_7584$massoc.summary[1, ], p_value = p_value[10]))),
  #"Age 85+"   = as.data.frame(t(c(sum_85Y, sum_85N, PR_85ov$massoc.summary[1, ], p_value = p_value[11]))),
  "Age 18-34" = as.data.frame(t(c(sumsum18Y, sumsum18N, PR_1834$massoc.summary[1, ], p_value = p_value[19]))),
  "Age 35-54" = as.data.frame(t(c(sumsum35Y, sumsum35N, PR_3554$massoc.summary[1, ], p_value = p_value[20]))),
  "Age 55-74" = as.data.frame(t(c(sumsum55Y, sumsum55N, PR_5574$massoc.summary[1, ], p_value = p_value[21]))),
  "Age 75+" = as.data.frame(t(c(sumsum75Y, sumsum75N, PR_75ov$massoc.summary[1, ], p_value = p_value[22]))),
  "White, NH" = as.data.frame(t(c(sum_w1Y, sum_w1N, PR.white$massoc.summary[1, ], p_value = p_value[12]))),
  "Black, NH" = as.data.frame(t(c(sum_bY, sum_bN, PR.black$massoc.summary[1, ], p_value = p_value[13]))),
  "Asian, NH" = as.data.frame(t(c(sum_asY, sum_asN, PR.asian$massoc.summary[1, ], p_value = p_value[14]))),
  "AI/AN, NH" = as.data.frame(t(c(sum_aiY, sum_aiN, PR.aian$massoc.summary[1, ], p_value = p_value[15]))),
  Hispanic    = as.data.frame(t(c(sum_hY, sum_hN, PR.hispanic$massoc.summary[1, ], p_value = p_value[17]))),
  BIPOC       = as.data.frame(t(c(sum_w2Y, sum_w2N, PR.bipoc$massoc.summary[1, ], p_value = p_value[18]))),
  "Another Race, NH"  = as.data.frame(t(c(sum_oY, sum_oN, PR.other$massoc.summary[1, ], p_value = p_value[16]))),
  .id = "Stratum"
) |>
  mutate(across(-Stratum, 
                ~ {x <- round(as.numeric(.x), 3)
                ifelse(x == 0 | is.na(x), "<0.001", format(x, nsmall = 0))}
  )) 

#make into a huxtable
table2 <- as_hux(table2_bind, add_colnames = TRUE)
#remove unwanted column
table2 <- table2[, -4]

#creating function for inserting new blank rows
adding_row <- function(table, label, after) {
  table <- insert_row(table, rep("", ncol(table)), after = after)
  new_row <- after + 1 
  table[new_row, 1] <- label
  table <- merge_cells(table, new_row, 1:ncol(table))
}
#labeling row names and using the above function
table2 <- adding_row(table2, "Sex", after = 2)
table2 <- adding_row(table2, "Age Group (years)", after = 5)  
table2 <- adding_row(table2, "Race/Ethnicity", after = 10)
#table2 <- adding_row(table2, "", after = 22)

#hard coding cell colors 
table2 <- set_background_color(table2, c(2, 4:5, 7:10, 12, 15:17), 2:7, "#E0F3CA")
table2 <- set_background_color(table2, c(13:14, 18), 2:7, "#FCE6E2")
table2 <- set_background_color(table2, c(2:18), 1, "#D6E7F1")

#
table2 <- adding_row(table2, "Prevalence Ratios of WA Adults with PVWs that Experienced Active Suicidal Ideation", after = 0)
table2 <- set_background_color(table2, 1, 1, "#3C5E7E")

#setting heading color and overall theme
(table2 <- theme_bright(
  table2,
  header_rows = TRUE,
  header_cols = FALSE,
  colors = c("#3C5E7E")
))

#modifying other settings for our table 2
table2 <- add_footnote(table2, "BIPOC = Black, Indigenous, and People of Color \n *Unadjusted (no confounders found) and unweighted prevalence ratios")
table2 <- set_caption(table2, "Table 2. Prevalence Ratios of Active Suicidal Ideation Among Adults With 0 vs ≥1 PVWs — 2020 Washington State, BRFSS (N = 8,106)")
table2 <- set_header_cols(table2, 1, T)
table2 <- style_headers(table2, bold = T)
table2 <- set_bold(table2, nrow(table2), 1:ncol(table2), FALSE)
table2 <- set_italic(table2, nrow(table2), 1:ncol(table2), TRUE)
table2 <- set_text_color(table2, 1, 1:ncol(table2),"white")
table2 <- set_align(table2, 1:19, value = "center")
table2 <- set_contents(table2, 2, 1:7, c("Stratum", "With ASI", "Without ASI", "PR", "Lower CI", "Upper CI", "p-value")) 

#exporting into docx file
quick_docx(table2, file = "table2v3.docx")


####------Data Visualization of PR w/ CIs-------####

ggplot_data <- data.frame(
  var = c("Overall", "Male", "Female", "Age: 18-34", "Age: 35-54", 
          "Age: 55-74", "Age: 75+",  "White, NH", "BIPOC",
          "*Black, NH", "*Asian, NH", "AI/AN, NH", "Hispanic", "*Another \nRace, NH"),
  est = c(PR$massoc.summary[1, "est"],
          PR.m$massoc.summary[1, "est"],
          PR.f$massoc.summary[1, "est"],
          PR_1834$massoc.summary[1, "est"],
          PR_3554$massoc.summary[1, "est"],
          PR_5574$massoc.summary[1, "est"],
          PR_75ov$massoc.summary[1, "est"],
          PR.white$massoc.summary[1, "est"],
          PR.bipoc$massoc.summary[1, "est"],
          PR.black$massoc.summary[1, "est"],
          PR.asian$massoc.summary[1, "est"],
          PR.aian$massoc.summary[1, "est"],
          PR.hispanic$massoc.summary[1, "est"],
          PR.other$massoc.summary[1, "est"]),
  lower = c(PR$massoc.summary[1, "lower"],
            PR.m$massoc.summary[1, "lower"],
            PR.f$massoc.summary[1, "lower"],
            PR_1834$massoc.summary[1, "lower"],
            PR_3554$massoc.summary[1, "lower"],
            PR_5574$massoc.summary[1, "lower"],
            PR_75ov$massoc.summary[1, "lower"],
            PR.white$massoc.summary[1, "lower"],
            PR.bipoc$massoc.summary[1, "lower"],
            PR.black$massoc.summary[1, "lower"],
            PR.asian$massoc.summary[1, "lower"],
            PR.aian$massoc.summary[1, "lower"],
            PR.hispanic$massoc.summary[1, "lower"],
            PR.other$massoc.summary[1, "lower"]),
  upper = c(PR$massoc.summary[1, "upper"],
            PR.m$massoc.summary[1, "upper"],
            PR.f$massoc.summary[1, "upper"],
            PR_1834$massoc.summary[1, "upper"],
            PR_3554$massoc.summary[1, "upper"],
            PR_5574$massoc.summary[1, "upper"],
            PR_75ov$massoc.summary[1, "upper"],
            PR.white$massoc.summary[1, "upper"],
            PR.bipoc$massoc.summary[1, "upper"],
            PR.black$massoc.summary[1, "upper"],
            PR.asian$massoc.summary[1, "upper"],
            PR.aian$massoc.summary[1, "upper"],
            PR.hispanic$massoc.summary[1, "upper"],
            PR.other$massoc.summary[1, "upper"])
)

ggplot_data$var <- factor(ggplot_data$var, levels = ggplot_data$var)

ggplot(ggplot_data, aes(x = var, y = est, ymin = lower, ymax = upper)) +
geom_rect(data = subset(ggplot_data, var == "Overall"),
          aes(xmin = as.numeric(var) - 0.5, 
              xmax = as.numeric(var) + 0.5,
              alpha = 0.3, ymin = -Inf, ymax = Inf),
          fill = "black", inherit.aes = FALSE) +
  geom_pointrange(shape = 18, size = 1.1, aes(color = var)) +  
  scale_color_manual(values = c(
    "Overall" = "white",
    "Male" = "deeppink3",
    "Female" = "deeppink3",
    "Age: 18-34" = "steelblue",
    "Age: 35-54" = "steelblue",
    "Age: 55-74" = "steelblue",
    "Age: 75+" = "steelblue",
    "White, NH" = "purple2",
    "*Black, NH" = "slateblue4",
    "*Asian, NH" = "slateblue4",
    "AI/AN, NH" = "purple2",
    "Hispanic" = "purple2",
    "BIPOC" = "purple2",
    "*Another \nRace, NH" = "slateblue4"
  )) +
  labs(
    x = "Demographic Subgroup",
    y = "Prevalence Ratio (95% CI)",
    title = "Stratified Analyses of Active Suicidal Ideation Among Adults 
    With 0 vs ≥1 PVWs — 2020 Washington State, BRFSS (N = 8,106)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 140, 10),  
    limits = c(0, 135)         
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold")
  ) + theme(legend.position = "none")

