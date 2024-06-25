# Clearing the environment
rm(list =ls())

# Setting Working Directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Packages
library(data.table)
library(readr)
library(tidyverse)

# Defining Columns we want to import from SIPP

# Column start positions for 2008 variables.
start_position_04 = c(887, 963, 878, 954, 911, 987, 896, 972, 925, 1001, 849, 941,
                      1017, 945, 1021, 780, 1486, 1492, 845, 794, 839, 829, 575,
                      531, 533, 535, 644, 2188, 26, 28, 6, 513, 516)

# Column end positions for 2008 variables.
end_position_04 = c(894, 970, 885, 961, 914, 990, 897, 973, 929, 1005, 850, 943,
                    1019, 947, 1023, 781, 1490, 1496, 846, 795, 843, 830, 576, 531,
                    533, 536, 651, 2189, 27, 31, 17, 515, 519)

# Variable Names
column_names = c("end_date_job_1_TEJDATE1", "end_date_job_2_TEJDATE2",
                 "start_date_job_1_TSJDATE1", "start_date_job_2_TSJDATE2",
                 "job_duration_1_EOCCTIM1", "job_duration_2_EOCCTIM2",
                 "reason_separation_ERSEND1", "reason_separation_ERSEND2",
                 "job_earnings_TPMSUM1", "job_earnings_TPMSUM2",
                 "employment_status_RMESR", "ind_code_EJBIND1", "ind_code_EJBIND2",
                 "occup_code_TJBOCC1", "occup_code_TJBOCC2", "highest_educ_EEDUCATE",
                 "amount_state_unemp_T05AMT", "amount_other_unemp_T07AMT",
                 "unknown_job_date_EBFLAG", "labor_force_imputation_EPPFLAG",
                 "income_from_extra_job_TMLMSUM", "time_layoff_ELAYOFF", "age_TAGE",
                 "sex_ESEX", "race_ERACE", "hispanic_EORIGIN", 
                 "total_person_income_TPTOTINC",
                 "longitudinal_month_LGTMON", "calendar_month_RHCALMN",
                 "calendar_year_RHCALYR", "SSUID", "EENTAID", "EPPPNUM")

columns = fwf_positions(start = start_position_04, end_position_04, 
                        col_names = column_names)

# Loading file path names
data_files = c()
for (i in 1:9) {
  dat_stor = paste("/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2001_wave/l01puw", sep = "", i)
  dat_stor2 = paste(dat_stor, sep = "/", "l01puw")
  dat_stor_3 = paste(dat_stor2, sep = "", i)
  data_files[i] = paste(dat_stor_3, sep = ".", "dat")
}

# For loop to upload all of the data
job_data_01 = tibble()

for (i in seq(data_files)) {
  
  # Uploading the data set.
  data_file = data_files[[i]]
  
  data_stor = read_fwf(data_file, col_positions = columns)
  
  data_stor = as.tibble(sapply(data_stor, as.numeric))
  
  job_data_01 = bind_rows(job_data_01, data_stor)
  
  # Clearing variable to save memory.
  rm(data_stor)
}


# Splitting data into observations with job end dates and job start dates.
end_date_obs = job_data_01 |> filter(end_date_job_1_TEJDATE1 != -1) |> filter(reason_separation_ERSEND1 %in% c(8, 9))
start_date_obs = job_data_01 |> filter(start_date_job_1_TSJDATE1 > 20001001)

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

nearest_date = nearest_date |> mutate(percent_change_earnings = (job_earnings_TPMSUM1_old - job_earnings_TPMSUM1_new)/job_earnings_TPMSUM1_old)

summary_stats = mean(nearest_date$percent_change_earnings)
