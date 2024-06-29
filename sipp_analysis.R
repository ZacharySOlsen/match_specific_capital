# Clearing the environment
rm(list =ls())

# Setting Working Directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Packages
library(data.table)
library(readr)
library(tidyverse)

# Defining Columns we want to import from SIPP

# Column start positions for 2008 Wave 1 variables
start_position_08 = c(897, 974, 888, 965, 921, 998, 906, 983, 934, 1011, 859, 950,
                      1027, 955, 1032, 786, 795, 1617, 1623, 855, 801, 849, 836,
                      579, 518, 520, 522, 648, 528, 2336, 26, 28, 6, 500, 503)

# Column end positions for 2008 Wave 1 variables.
end_position_08 = c(904, 981, 895, 972, 923, 1000, 907, 984, 938, 1015, 860, 953,
                    1030, 958, 1035, 787, 796, 1621, 1627, 856, 802, 853, 837,
                    580, 518, 520, 523, 655, 529, 2337, 27, 31, 17, 502, 506)





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
                 "longitudinal_month_LGTMON", "calendar_month_RHCALMN",
                 "calendar_year_RHCALYR", "SSUID", "EENTAID", "EPPPNUM")

columns = fwf_positions(start = start_position_08, end_position_08, 
                        col_names = column_names)

# File Paths to the 16 data sets
data_files = list("/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw1/l08puw1.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw2/l08puw2.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw3/l08puw3.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw4/l08puw4.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw5/l08puw5.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw6/l08puw6.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw7/l08puw7.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw8/l08puw8.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw9/l08puw9.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw10/l08puw10.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw11/l08puw11.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw12/l08puw12.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw13/l08puw13.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw14/l08puw14.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw15/l08puw15.dat",
                  "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw16/l08puw16.dat")

# For loop to upload all of the data
job_data_08 = tibble()

for (i in seq(data_files)) {
  
  # Uploading the data set.
  data_file = data_files[[i]]
  
  data_stor = read_fwf(data_file, col_positions = columns)
  
  data_stor = as.tibble(sapply(data_stor, as.numeric))
  
  job_data_08 = bind_rows(job_data_08, data_stor)
  
  # Clearing variable to save memory.
  rm(data_stor, data_file)
}
rm(data_files)

# Splitting data into observations with job end dates and job start dates.
end_date_obs = job_data_08 |> filter(end_date_job_1_TEJDATE1 != -1) |> filter(reason_separation_ERSEND1 %in% c(8, 9))
start_date_obs = job_data_08 |> filter(start_date_job_1_TSJDATE1 > 20080101)

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
combined_starts_ends = inner_join(end_date_obs_filtered, start_date_obs_filtered, by = c("SSUID", "EENTAID", "EPPPNUM"), relationship = "many-to-many")

# Distance between start date and end date.
combined_starts_ends = combined_starts_ends |> mutate(date_difference = start_date_job_1_TSJDATE1.y - end_date_job_1_TEJDATE1.x) |> filter(date_difference >= 0)

# Filtering difference
nearest_date = combined_starts_ends |> group_by(date_difference) |> filter(date_difference == min(date_difference)) |> ungroup()

# Working to keep only observation in the same month as when they lost their old job and got a new one.
nearest_date = nearest_date |> mutate(end_date_job_1_TEJDATE1.x = as.Date(as.character(end_date_job_1_TEJDATE1.x), format = "%Y%m%d")) |> mutate(end_year = year(as.character(end_date_job_1_TEJDATE1.x))) |> mutate(end_month = month(as.character(end_date_job_1_TEJDATE1.x)))

nearest_date = nearest_date |> mutate(start_date_job_1_TSJDATE1.y = as.Date(as.character(start_date_job_1_TSJDATE1.y), format = "%Y%m%d")) |> mutate(start_year = year(as.character(start_date_job_1_TSJDATE1.y))) |> mutate(start_month = month(as.character(start_date_job_1_TSJDATE1.y)))

nearest_date = nearest_date |> filter(calendar_year_RHCALYR.x == end_year) |> filter(calendar_month_RHCALMN.x == end_month) |> filter(calendar_year_RHCALYR.y == start_year) |> filter(calendar_month_RHCALMN.y == start_month)

nearest_date = nearest_date |> distinct(SSUID, EENTAID, EPPPNUM, end_date_job_1_TEJDATE1.x, .keep_all = T)

# And it is cleaned!!!

# Summary Stats and change percent change in wages.

nearest_date = nearest_date |> rename(job_earnings_TPMSUM1_old  = job_earnings_TPMSUM1.x) |> rename(job_earnings_TPMSUM1_new = job_earnings_TPMSUM1.y)

nearest_date = nearest_date |> filter(job_earnings_TPMSUM1_old != 0) |> filter(job_earnings_TPMSUM1_new != 0)

nearest_date = nearest_date |> mutate(percent_change_earnings = (job_earnings_TPMSUM1_new - job_earnings_TPMSUM1_old)/job_earnings_TPMSUM1_old) 
nearest_date = nearest_date |> mutate(sex_ESEX.x = if_else(sex_ESEX.x == 2, 1, 0)) |> rename(female = sex_ESEX.x)
nearest_date = nearest_date |> mutate(black = if_else(race_ERACE.x == 2, 1, 0))
nearest_date = nearest_date |> mutate(asian = if_else(race_ERACE.x == 3, 1, 0))
nearest_date = nearest_date |> mutate(other = if_else(race_ERACE.x == 4, 1, 0))
nearest_date = nearest_date |> mutate(hispanic_EORIGIN.x = if_else(hispanic_EORIGIN.x == 1, 1, 0))
nearest_date = nearest_date |> mutate(citizen_ECITIZEN.x = if_else(citizen_ECITIZEN.x == 1, 1, 0))

nearest_date = nearest_date |> mutate(highschool = if_else(highest_educ_EEDUCATE.x == 39, 1, 0))
nearest_date = nearest_date |> mutate(some_college = if_else(highest_educ_EEDUCATE.x == 40, 1, 0))
nearest_date = nearest_date |> mutate(associates = if_else(highest_educ_EEDUCATE.x == 43, 1, 0))
nearest_date = nearest_date |> mutate(no_highschool = if_else(highest_educ_EEDUCATE.x < 39, 1, 0))
nearest_date = nearest_date |> mutate(bachelors = if_else(highest_educ_EEDUCATE.x == 44, 1, 0))
nearest_date = nearest_date |> mutate(master_professional = if_else(highest_educ_EEDUCATE.x > 44, 1, 0))

summary_stat_demographics = nearest_date |> select(no_highschool, highschool, some_college, associates, bachelors, master_professional, female, black, asian, other, hispanic_EORIGIN.x, age_TAGE.x, citizen_ECITIZEN.x)

summary_demographics = summary_stat_demographics |> summarize(across(everything(), mean))

demographic_names = c("No High School Diploma" = "no_highschool",
                      "High School Diploma" = "highschool",
                      "Some College" = "some_college",
                      "Associates Degree" = "associates",
                      "Bachelors Degree" = "bachelors",
                      "Masters, PhD, or Professional Degree" = "master_professional",
                      "Female" = "female", "African American" = "black",
                      "Asian" = "asian", "Other Race" = "other",
                      "Hispanic" = "hispanic_EORIGIN.x", "Age" = "age_TAGE.x",
                      "Is a Citizen of the US" = "citizen_ECITIZEN.x")

summary_demographics = rename(summary_demographics, all_of(demographic_names)) |>
  kbl(caption = "Demographics for 2008 SIPP Sample that Lost and Gained a Job") |>
  kable_classic_2(html_font = "Times New Roman") |> footnote(general = "n = 929") |>
  save_kable("demographics_08.html")

print(summary_demographics)

earning_names = c("Old Job Monthly Earnings" = "job_earnings_TPMSUM1_old",
                  "New Job Monthly Earnings" = "job_earnings_TPMSUM1_new",
                  "Percent Change in Monthly Earnings" = "percent_change_earnings")

earning_summary_means = nearest_date |> 
  select(job_earnings_TPMSUM1_old, job_earnings_TPMSUM1_new, percent_change_earnings) |>
  summarize(across(everything(), mean)) |> rename(any_of(earning_names)) |>
  pivot_longer(everything()) |> rename("Mean" = value)

earning_summary_sd = nearest_date |> 
  select(job_earnings_TPMSUM1_old, job_earnings_TPMSUM1_new, percent_change_earnings) |>
  summarize(across(everything(), sd)) |> rename(any_of(earning_names)) |>
  pivot_longer(everything()) |> rename("Standard Deviation" = value)

earning_summary_stats = inner_join(earning_summary_means, earning_summary_sd, by = "name") |>
  kbl(caption = "Summary Statistics for Earnings of 2008 SIPP Sample that Lost and Gained a Job", col.names = c("", "Mean", "Standard Deviation")) |>
  kable_classic_2(html_font = "Times New Roman") |> footnote(general = "n = 929") |>
  save_kable("earning_table_08.html")

print(earning_summary_stats)

pre_ind = nearest_date |> count(ind_code_EJBIND1.x, sort = T) |>  head(10)
post_ind = nearest_date |> count(ind_code_EJBIND1.y, sort = T) |> head(10)

pre_occup = nearest_date |> count(occup_code_TJBOCC1.x, sort = T) |> head(10)
post_occup = nearest_date |> count(occup_code_TJBOCC1.y, sort = T) |> head(10)