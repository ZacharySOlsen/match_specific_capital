# Clearing the environment
rm(list =ls())

# Setting Working Directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Packages
library(data.table)
library(readr)
library(tidyverse)
library(kableExtra)

# Defining Columns we want to import from SIPP

# Column start positions for 2008 variables.
start_position_04 = c(897, 974, 888, 965, 921, 998, 906, 983, 934, 1011, 859, 950,
                      1027, 955, 1032, 786, 795, 1620, 1626, 855, 801, 849, 836,
                      579, 518, 520, 522, 648, 528, 2345, 26, 28, 6, 500, 503)

# Column end positions for 2008 variables.
end_position_04 = c(904, 981, 895, 972, 923, 1000, 907, 984, 938, 1015, 860, 953,
                    1030, 958, 1035, 787, 796, 1624, 1630, 856, 802, 853, 837, 580,
                    518, 520, 523, 655, 529, 2346, 27, 31, 17, 502, 506)

# Variable Names
column_names = c("end_date_job_1_TEJDATE1", "end_date_job_2_TEJDATE2",
                 "start_date_job_1_TSJDATE1", "start_date_job_2_TSJDATE2",
                 "job_duration_1_EOCCTIM1", "job_duration_2_EOCCTIM2",
                 "reason_separation_ERSEND1", "reason_separation_ERSEND2",
                 "job_earnings_TPMSUM1", "job_earnings_TPMSUM2",
                 "employment_status_RMESR", "ind_code_EJBIND1", "ind_code_EJBIND2",
                 "occup_code_TJBOCC1", "occup_code_TJBOCC2", "highest_educ_EEDUCATE",
                 "vocation_RCOLLVOC", "amount_state_unemp_T05AMT",
                 "amount_supp_unemp_T06AMT", "unknown_job_date_EBFLAG",
                 "labor_force_imputation_EPPFLAG", "income_from_extra_job_TMLMSUM",
                 "time_layoff_ELAYOFF", "age_TAGE", "sex_ESEX", "race_ERACE",
                 "hispanic_EORIGIN", "total_person_income_TPTOTINC", "citizen_ECITIZEN",
                 "war_month_LGTMON", "calendar_month_RHCALMN",
                 "calendar_year_RHCALYR", "SSUID", "EENTAID", "EPPPNUM")

columns = fwf_positions(start = start_position_04, end_position_04, 
                        col_names = column_names)

data_files = c()
for (i in 1:12) {
  dat_stor = paste("/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2004_wave/l04puw", sep = "", i)
  dat_stor_2 = paste(dat_stor, sep = "/", "l04puw")
  dat_stor_3 = paste(dat_stor_2, sep = "", i)
  data_files[i] = paste(dat_stor_3, sep = ".", "dat")
  rm(dat_stor, dat_stor_2, dat_stor_3)
}


# For loop to upload all of the data
job_data_04 = tibble()

for (i in seq(data_files)) {
  
  # Uploading the data set.
  data_file = data_files[[i]]
  
  data_stor = read_fwf(data_file, col_positions = columns)
  
  data_stor = as.tibble(sapply(data_stor, as.numeric))
  
  job_data_04 = bind_rows(job_data_04, data_stor)
  
  # Clearing variable to save memory.
  rm(data_stor, data_file)
}
rm(data_files)

# job_data_08 = job_data_08 |> filter(job_earnings_TPMSUM1 < 12500)

# Splitting data into observations with job end dates and job start dates.
end_date_obs = job_data_04 |> filter(end_date_job_1_TEJDATE1 != -1) |> filter(reason_separation_ERSEND1 %in% c(8, 9))
start_date_obs = job_data_04 |> filter(start_date_job_1_TSJDATE1 > 20001001)

# Lists of unique person identifiers for those who have a end job date that was do to some undesired separation.
end_distinct_SSUID = end_date_obs |> distinct(SSUID)
end_distinct_EENTAID = end_date_obs |> distinct(EENTAID)
end_distinct_EPPPNUM = end_date_obs |> distinct(EPPPNUM)

# Filtering out starts that don't match unique person identifiers in the end job group.
start_date_obs_filtered = start_date_obs |> filter(SSUID %in% end_distinct_SSUID$SSUID) |> filter(EENTAID %in% end_distinct_EENTAID$EENTAID) |> filter(EPPPNUM %in% end_distinct_EPPPNUM$EPPPNUM)

rm(start_date_obs)
# Lists of unique person identifiers for those who started a job after 01/01/2008.

start_distinct_SSUID = start_date_obs_filtered |> distinct(SSUID)
start_distinct_EENTAID = start_date_obs_filtered |> distinct(EENTAID)
start_distinct_EPPPNUM = start_date_obs_filtered |> distinct(EPPPNUM)

# Filtering out observations in the end job group that don't have unique person identifiers for those in the filtered start group.
end_date_obs_filtered = end_date_obs |> filter(SSUID %in% start_distinct_SSUID$SSUID) |> filter(EENTAID %in% start_distinct_EENTAID$EENTAID) |> filter(EPPPNUM %in% start_distinct_EPPPNUM$EPPPNUM)

# Now we have the intersection of the sets of those who lost a job during the time period and those who got a job during the time period of the interviews.

rm(end_date_obs)

# Cartesian Product of the two data sets.
combined_starts_ends = inner_join(end_date_obs_filtered, start_date_obs_filtered, by = c("SSUID", "EENTAID", "EPPPNUM"), relationship = "many-to-many", suffix = c("_old", "_new"))

# Distance between start date and end date.
combined_starts_ends = combined_starts_ends |> mutate(date_difference = start_date_job_1_TSJDATE1_new - end_date_job_1_TEJDATE1_old) |> filter(date_difference >= 0)

# Filtering difference
nearest_date = combined_starts_ends |> group_by(end_date_job_1_TEJDATE1_old, SSUID, EENTAID, EPPPNUM) |> filter(date_difference == min(date_difference)) |> ungroup()

# Working to keep only observation in the same month as when they lost their old job and got a new one.
nearest_date = nearest_date |> mutate(end_date_job_1_TEJDATE1_old = as.Date(as.character(end_date_job_1_TEJDATE1_old), format = "%Y%m%d")) |> mutate(end_year = year(as.character(end_date_job_1_TEJDATE1_old))) |> mutate(end_month = month(as.character(end_date_job_1_TEJDATE1_old)))

nearest_date = nearest_date |> mutate(start_date_job_1_TSJDATE1_new = as.Date(as.character(start_date_job_1_TSJDATE1_new), format = "%Y%m%d")) |> mutate(start_year = year(as.character(start_date_job_1_TSJDATE1_new))) |> mutate(start_month = month(as.character(start_date_job_1_TSJDATE1_new)))

nearest_date = nearest_date |> filter(calendar_year_RHCALYR_old == end_year) |> filter(calendar_month_RHCALMN_old == end_month) |> filter(calendar_year_RHCALYR_new == start_year) |> filter(calendar_month_RHCALMN_new == start_month)

nearest_date = nearest_date |> distinct(SSUID, EENTAID, EPPPNUM, end_date_job_1_TEJDATE1_old, .keep_all = T)

# And it is cleaned!!!

# Adding month before wage
smaller_job_data = job_data_04 |> select(job_earnings_TPMSUM1, ind_code_EJBIND1, occup_code_TJBOCC1, calendar_month_RHCALMN, calendar_year_RHCALYR, SSUID, EENTAID, EPPPNUM)

nearest_date = nearest_date |> mutate(month_before = calendar_month_RHCALMN_old - 1) |> 
  mutate(end_year = if_else(month_before == 0, end_year - 1, end_year)) |> 
  mutate(month_before = if_else(month_before == 0, 12, month_before))

nearest_date = inner_join(nearest_date, smaller_job_data, by = c("SSUID", "EENTAID", "EPPPNUM", "end_year" = "calendar_year_RHCALYR", "month_before" = "calendar_month_RHCALMN", "ind_code_EJBIND1_old" = "ind_code_EJBIND1", "occup_code_TJBOCC1_old" = "occup_code_TJBOCC1"), suffix = c("_exact", "_monthB")) |> rename("earnings_m_b_TPSUM1" = "job_earnings_TPMSUM1")

#Adding month after wage
nearest_date = nearest_date |> mutate(month_after = calendar_month_RHCALMN_new + 1) |> 
  mutate(end_year_a = if_else(month_after == 13, start_year + 1, start_year)) |>
  mutate(month_after = if_else(month_after == 13, 1, month_after))

nearest_date = inner_join(nearest_date, smaller_job_data, by = c("SSUID", "EENTAID", "EPPPNUM", "start_year" = "calendar_year_RHCALYR", "month_after" = "calendar_month_RHCALMN", "ind_code_EJBIND1_new" = "ind_code_EJBIND1", "occup_code_TJBOCC1_new" = "occup_code_TJBOCC1"), suffix = c("_exact", "_monthB")) |> rename("earnings_m_a_TPSUM1" = "job_earnings_TPMSUM1")

# Summary Stats and change percent change in wages.

nearest_date = nearest_date |> filter(earnings_m_b_TPSUM1 != 0) |> filter(earnings_m_a_TPSUM1 != 0)

nearest_date = nearest_date |> mutate(percent_change_earnings = (earnings_m_a_TPSUM1 - earnings_m_b_TPSUM1)/earnings_m_b_TPSUM1) 
nearest_date = nearest_date |> mutate(sex_ESEX_old = if_else(sex_ESEX_old == 2, 1, 0)) |> rename(female = sex_ESEX_old)
nearest_date = nearest_date |> mutate(black = if_else(race_ERACE_old == 2, 1, 0))
nearest_date = nearest_date |> mutate(asian = if_else(race_ERACE_old == 3, 1, 0))
nearest_date = nearest_date |> mutate(other = if_else(race_ERACE_old == 4, 1, 0))
nearest_date = nearest_date |> mutate(hispanic_EORIGIN_old = if_else(hispanic_EORIGIN_old == 1, 1, 0))
nearest_date = nearest_date |> mutate(citizen_ECITIZEN_old = if_else(citizen_ECITIZEN_old == 1, 1, 0))

nearest_date = nearest_date |> mutate(highschool = if_else(highest_educ_EEDUCATE_old == 39, 1, 0))
nearest_date = nearest_date |> mutate(some_college = if_else(highest_educ_EEDUCATE_old == 40, 1, 0))
nearest_date = nearest_date |> mutate(trade_vocation = if_else(highest_educ_EEDUCATE_old == 41, 1, 0))
nearest_date = nearest_date |> mutate(associates = if_else(highest_educ_EEDUCATE_old == 43, 1, 0))
nearest_date = nearest_date |> mutate(no_highschool = if_else(highest_educ_EEDUCATE_old < 39, 1, 0))
nearest_date = nearest_date |> mutate(bachelors = if_else(highest_educ_EEDUCATE_old == 44, 1, 0))
nearest_date = nearest_date |> mutate(master_professional = if_else(highest_educ_EEDUCATE_old > 44, 1, 0))

summary_stat_demographics = nearest_date |> select(no_highschool, highschool, trade_vocation, some_college, associates, bachelors, master_professional, female, black, asian, other, hispanic_EORIGIN_old, age_TAGE_old, citizen_ECITIZEN_old)

summary_demographics = summary_stat_demographics |> summarize(across(everything(), mean))

demographic_names = c("No High School Diploma" = "no_highschool",
                      "High School Diploma" = "highschool",
                      "Some College" = "some_college",
                      "Associates Degree" = "associates",
                      "Bachelors Degree" = "bachelors",
                      "Masters, PhD, or Professional Degree" = "master_professional",
                      "Female" = "female", "African American" = "black",
                      "Asian" = "asian", "Other Race" = "other",
                      "Hispanic" = "hispanic_EORIGIN_old", "Age" = "age_TAGE_old",
                      "Is a Citizen of the US" = "citizen_ECITIZEN_old",
                      "Trade, Technical, or Vocational Certification" = "trade_vocation")

summary_demographics = rename(summary_demographics, all_of(demographic_names)) |>
  kbl(caption = "Demographics for 2004 SIPP Sample that Lost and Gained a Job") |>
  kable_classic_2(html_font = "Times New Roman") |> footnote(general = "n = 584")

save_kable(summary_demographics, "demographics_04.html")

print(summary_demographics)

earning_names = c("Old Job Monthly Earnings" = "earnings_m_b_TPSUM1",
                  "New Job Monthly Earnings" = "earnings_m_a_TPSUM1",
                  "Percent Change in Monthly Earnings" = "percent_change_earnings")

earning_summary_means = nearest_date |> 
  select(earnings_m_b_TPSUM1, earnings_m_a_TPSUM1, percent_change_earnings) |>
  summarize(across(everything(), mean)) |> rename(any_of(earning_names)) |>
  pivot_longer(everything()) |> rename("Mean" = value)

earning_summary_sd = nearest_date |> 
  select(earnings_m_b_TPSUM1, earnings_m_a_TPSUM1, percent_change_earnings) |>
  summarize(across(everything(), sd)) |> rename(any_of(earning_names)) |>
  pivot_longer(everything()) |> rename("Standard Deviation" = value)

earning_summary_stats = inner_join(earning_summary_means, earning_summary_sd, by = "name") |>
  kbl(caption = "Summary Statistics for Earnings of 2004 SIPP Sample that Lost and Gained a Job", col.names = c("", "Mean", "Standard Deviation")) |>
  kable_classic_2(html_font = "Times New Roman") |> footnote(general = "n = 584")

save_kable(earning_summary_stats, "earning_table_04.html")

print(earning_summary_stats)

# Creating tables


pre_ind = nearest_date |> count(ind_code_EJBIND1_old, sort = T) |>  head(10)
post_ind = nearest_date |> count(ind_code_EJBIND1_new, sort = T) |> head(10)

pre_occup = nearest_date |> count(occup_code_TJBOCC1_old, sort = T) |> head(10)
post_occup = nearest_date |> count(occup_code_TJBOCC1_new, sort = T) |> head(10)