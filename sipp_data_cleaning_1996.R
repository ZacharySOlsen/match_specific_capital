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
start_position_04 = c(866, 942, 857, 933, 890, 875, 951, 904, 966, 828, 920, 982,
                      924, 986, 769, 1446, 1452, 824, 818, 808, 568, 524, 526, 528,
                      633, 26, 28, 6, 506, 509)

# Column end positions for 2008 variables.
end_position_04 = c(873, 949, 864, 940, 893, 876, 952, 908, 970, 829, 922, 984,
                    926, 988, 770, 1450, 1456, 825, 822, 809, 569, 524, 526, 529, 
                    640, 27, 31, 17, 508, 512)

# Variable Names
column_names = c("end_date_job_1_TEJDATE1", "end_date_job_2_TEJDATE2",
                 "start_date_job_1_TSJDATE1", "start_date_job_2_TSJDATE2",
                 "job_duration_1_EOCCTIM1",
                 "reason_separation_ERSEND1", "reason_separation_ERSEND2",
                 "job_earnings_TPMSUM1", "job_earnings_TPMSUM2",
                 "employment_status_RMESR", "ind_code_EJBIND1", "ind_code_EJBIND2",
                 "occup_code_TJBOCC1", "occup_code_TJBOCC2", "highest_educ_EEDUCATE",
                 "amount_state_unemp_T05AMT", "amount_other_unemp_T07AMT",
                 "unknown_job_date_EBFLAG", 
                 "income_from_extra_job_TMLMSUM", "time_layoff_ELAYOFF", "age_TAGE",
                 "sex_ESEX", "race_ERACE", "hispanic_EORIGIN", 
                 "total_person_other_income_TPTOTINC", "calendar_month_RHCALMN",
                 "calendar_year_RHCALYR", "SSUID", "EENTAID", "EPPPNUM")

columns = fwf_positions(start = start_position_04, end_position_04, 
                        col_names = column_names)

# Loading file path names
data_files = c()
for (i in 1:12) {
  dat_stor = paste("/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/1996_wave/l96puw", sep = "", i)
  dat_stor2 = paste(dat_stor, sep = "/", "l96puw")
  dat_stor_3 = paste(dat_stor2, sep = "", i)
  data_files[i] = paste(dat_stor_3, sep = ".", "dat")
}

# For loop to upload all of the data
job_data_96 = tibble()

for (i in seq(data_files)) {
  
  # Uploading the data set.
  data_file = data_files[[i]]
  
  data_stor = read_fwf(data_file, col_positions = columns)
  
  data_stor = as.tibble(sapply(data_stor, as.numeric))
  
  job_data_96 = bind_rows(job_data_96, data_stor)
  
  # Clearing variable to save memory.
  rm(data_stor)
}
# There is something wrong with the data upload process here. Will need to fix.

# job_data_08 = job_data_08 |> filter(job_earnings_TPMSUM1 < 12500)


# Splitting data into observations with job end dates and job start dates.
end_date_obs = job_data_96 |> filter(!is.na(end_date_job_1_TEJDATE1)) #|> filter(reason_separation_ERSEND1 %in% c(8, 9))
start_date_obs = job_data_96 |> filter(start_date_job_1_TSJDATE1 > 20001001)

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
combined_starts_ends = combined_starts_ends |> mutate(date_difference = start_date_job_1_TSJDATE1.y - end_date_job_1_TEJDATE1.x) |> filter(date_difference >= 0)

# Filtering difference
nearest_date = combined_starts_ends |> group_by(end_date_job_1_TEJDATE1.x, SSUID, EENTAID, EPPPNUM) |> filter(date_difference == min(date_difference)) |> ungroup()

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

nearest_date = nearest_date |> mutate(highschool = if_else(highest_educ_EEDUCATE.x == 39, 1, 0))
nearest_date = nearest_date |> mutate(some_college = if_else(highest_educ_EEDUCATE.x == 40, 1, 0))
nearest_date = nearest_date |> mutate(trade_vocation = if_else(highest_educ_EEDUCATE.x == 41, 1, 0))
nearest_date = nearest_date |> mutate(associates = if_else(highest_educ_EEDUCATE.x == 43, 1, 0))
nearest_date = nearest_date |> mutate(no_highschool = if_else(highest_educ_EEDUCATE.x < 39, 1, 0))
nearest_date = nearest_date |> mutate(bachelors = if_else(highest_educ_EEDUCATE.x == 44, 1, 0))
nearest_date = nearest_date |> mutate(master_professional = if_else(highest_educ_EEDUCATE.x > 44, 1, 0))

summary_stat_demographics = nearest_date |> select(no_highschool, highschool, trade_vocation, some_college, associates, bachelors, master_professional, female, black, asian, other, hispanic_EORIGIN.x, age_TAGE.x)

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
                      "Trade, Technical, or Vocational Certification" = "trade_vocation")

summary_demographics = rename(summary_demographics, all_of(demographic_names)) |>
  kbl(caption = "Demographics for 1996 SIPP Sample that Lost and Gained a Job") |>
  kable_classic_2(html_font = "Times New Roman") |> footnote(general = "n = 657") |>
  save_kable("demographics_04.html")

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
  kbl(caption = "Summary Statistics for Earnings of 2004 SIPP Sample that Lost and Gained a Job", col.names = c("", "Mean", "Standard Deviation")) |>
  kable_classic_2(html_font = "Times New Roman") |> footnote(general = "n = 657") |>
  save_kable("earning_table_04.html")

print(earning_summary_stats)

# Creating tables


pre_ind = nearest_date |> count(ind_code_EJBIND1.x, sort = T) |>  head(10)
post_ind = nearest_date |> count(ind_code_EJBIND1.y, sort = T) |> head(10)

pre_occup = nearest_date |> count(occup_code_TJBOCC1.x, sort = T) |> head(10)
post_occup = nearest_date |> count(occup_code_TJBOCC1.y, sort = T) |> head(10)