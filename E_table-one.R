
### ---- CREATE OUR TABLE 1 ---- ###

#create table 1
#need to decide title/caption, what we're doing with NAs "unknowns"
table1 <- brfss20 %>%
  select(suicide_f, sex_f, age_group, raceth_f,
         acepunch_f, acehurt_f, aceswear_f) %>%
  tbl_summary(
    by = suicide_f,
    missing = "ifany",
    missing_text = "Missing",
    percent = "column",
    label = list(
      sex_f ~ "Sex",
      age_group ~ "Age (years)",
      raceth_f ~ "Race/Ethnicity", 
      acepunch_f ~ "Witnessed Household Violence (Hit/Hurt)",
      acehurt_f ~ "Physical Abuse (Hit/Hurt)",
      aceswear_f ~ "Verbal Abuse (Sworn/Insulted)"
    )
  )  %>%
  modify_caption("**Table 1** Sociodemographic Characteristics of Adults with Adverse Childhood Experience(s) by Active Suicidal Ideation in the Last 12 Months - 2020 Washington State, Behavioral Risk Factor Surveillance Survey (N = 8,365)") %>%
  modify_spanning_header(all_stat_cols() ~ 
                           "**Active Suicidal Ideation**<br>*in the last 12 months*") |>
  remove_footnote_header(columns = all_stat_cols()) |>
  
  modify_footnote_spanning_header(
    footnote = "*At any time in the past 12 months did you seriously think about trying to kill yourself?*",
    columns = all_stat_cols(),
    replace = FALSE
  ) |>
  modify_footnote_header(
    footnote = "Additional 26 Individuals: N = 3 for \"Don\'t Know\" and N = 23 for \"Refuse\"",
    columns = all_stat_cols(),
    replace = FALSE
  ) |>
  modify_footnote_body(
    footnote = "NH = Non-Hispanic",
    columns = "label",
    rows = variable == "raceth_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "*Did your parents or adults in your home ever slap, hit, kick, punch or beat each other up?*",
    columns = "label",
    rows = variable == "acepunch_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "*Did your parents or adult in your home ever hit, beat, kick, or physically hurt you in any way?*",
    columns = "label",
    rows = variable == "acehurt_f" & row_type == "label"
  ) |>
  modify_footnote_body(
    footnote = "*Did your parents or adults in your home ever swear at you, insult you, or put you down?*",
    columns = "label",
    rows = variable == "aceswear_f" & row_type == "label"
  ) 

#view table1
table1


