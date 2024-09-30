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
column_names = c("SSUID", "EENTAID", "EPPPNUM", "RHCALYR", "RHCALMN", "LGTMON",
                 "EENO1", "EENO2", "RWKESR1", "RWKESR2", "RWKESR3", "RWKESR4", 
                 "RWKESR5", "ELAYOFF", "TEJDATE1", "TEJDATE2", "TSJDATE1", "TSJDATE2", 
                 "ERSEND1", "ERSEND2", "EOCCTIM1", "EOCCTIM2", "EJBIND1", "EJBIND2",
                 "TJBOCC1", "TJBOCC2", "TPMSUM1", "TPMSUM2", "TMLMSUM", "T05AMT",
                 "TPTOTINC", "EBFLAG", "EPPFLAG", "EEDUCATE", "ESEX", "ERACE", 
                 "EORIGIN", "TAGE")

# Column start positions for 2008 Wave 1 variables
start_position = c(6, 500, 503, 28, 26, 2336, 
                   883, 960, 861, 863, 865, 867, 
                   869, 836, 897, 974, 888, 965, 
                   906, 983, 921, 998, 950, 1027, 
                   955, 1032, 934, 1011, 849, 1617, 
                   648, 855, 801, 786, 518, 520, 
                   522, 579)

# Column end positions for 2008 Wave 1 variables.
end_position = c(17, 502, 506, 31, 27, 2337, 
                 884, 961, 862, 864, 866, 868, 
                 870, 837, 904, 981, 895, 972, 
                 907, 984, 923, 1000, 953, 1030, 
                 958, 1035, 938, 1015, 853, 1621, 
                 655, 856, 802, 787, 518, 520, 
                 523, 580)

columns = fwf_positions(start = start_position, end_position, col_names = column_names)

# File Paths to the 16 data sets.
data_files = c()
for (i in 1:16) {
  dat_stor = paste("/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2008_wave/l08puw", sep = "", i)
  dat_stor_2 = paste(dat_stor, sep = "/", "l08puw")
  dat_stor_3 = paste(dat_stor_2, sep = "", i)
  data_files[i] = paste(dat_stor_3, sep = ".", "dat")
  rm(dat_stor, dat_stor_2, dat_stor_3)
}

# For loop to upload all of the data
job_data = tibble()

for (i in seq(data_files)) {
  
  # Uploading the data set.
  data_file = data_files[[i]]
  
  data_stor = read_fwf(data_file, col_positions = columns)
  
  data_stor = as.tibble(sapply(data_stor, as.numeric))
  
  job_data = bind_rows(job_data, data_stor)
  
  # Clearing variable to save memory.
  rm(data_stor, data_file)
}

# Clearing variables that will no longer be used to reduce clutter.
rm(data_files, columns, data_files, column_names, end_position, i, start_position)

# job_data_08 = job_data_08 |> filter(job_earnings_TPMSUM1 < 12500)

# Filtering for observations that have some kind of unemployment.
unemployed = job_data |> filter( (RWKESR1 == 4) | (RWKESR2 == 4) | (RWKESR3 == 4) | (RWKESR4 == 4) | (RWKESR5 == 4) ) |>
  filter( (ERSEND1 == 9) | (ERSEND2 == 9))

# Taking out those laid off to deal with later.
unemployed_layoff = unemployed |> filter(ELAYOFF == 1)

unemployed = unemployed |> filter(ELAYOFF == 2)

# Dealing with various potential two job variations.

# These are those who lost a job and acquired a new one sometime during the same wave.
no_first_job_end_date = unemployed |> filter(TEJDATE1 == -1) |> filter(TSJDATE1 > TEJDATE2)

# Only had one job. Didn't get a new one during that wave.
no_second_job = unemployed |> filter(EENO2 == -1)

# Who's left. For visualization purposes.
leftovers = unemployed |> filter(TEJDATE1 != -1) |> filter(EENO2 != -1)

# Those who did lose a second job.
lose_second_job = leftovers |> filter(TEJDATE2 != -1)

lose_second_job1 = lose_second_job |> filter(TEJDATE1 > TEJDATE2)

lose_second_job2 = lose_second_job |> filter(TEJDATE2 > TEJDATE1)

# Fine leaving them out for now.
oddball = job_data |> filter(SSUID == 365925202729) |> filter(EENTAID == 11) |> filter(EPPPNUM == 101)

# Narrowing down why they lost their job for the last job they held.
lose_second_job1 = lose_second_job1 |> filter(ERSEND1 == 9)

lose_second_job2 = lose_second_job2 |> filter(ERSEND2 == 9)

# Nothing I can get from lose_second_job2 after checking the person below.
oddball2 = job_data |> filter(SSUID == 739133667334) |> filter(EENTAID == 41) |>
  filter(EPPPNUM == 403)

# Those who didn't lose a second job.
no_second_job_end_date = leftovers |> filter(TEJDATE2 == -1) |> filter(TSJDATE2 > TEJDATE1)

# Okay. Need to work with lose_second_job1, no_first_job_end_date, no_second_job, 
# and no_second_job_end_date.

# Finding before/after wages for no_first_job_end_date.
no_first_job_end_date = no_first_job_end_date |> mutate(TEJDATE2 = ymd(TEJDATE2)) |>
  mutate(TSJDATE1 = ymd(TSJDATE1)) |> mutate(TSJDATE2 = ymd(TSJDATE2))

# Wow! They have a nonexistent date. June 31st is truly a day that exists. \s
# I'll just change that June 30th.
no_first_job_end_date = no_first_job_end_date |> mutate(TSJDATE2 = if_else(is.na(TSJDATE2), ymd(20080630), TSJDATE2))

# Creating variables for the month and year they left their old job and started
# their new one. Can use this to join and get month before and month after income.
no_first_job_end_date = no_first_job_end_date |> mutate(year_left = year(TEJDATE2)) |>
  mutate(month_left = month(TEJDATE2)) |> mutate(year_start = year(TSJDATE1)) |>
  mutate(month_start = month(TSJDATE1)) |> mutate(smallest_diff = as.numeric(TSJDATE1 - TEJDATE2))

nfjed_key = no_first_job_end_date |> 
  distinct(SSUID, EENTAID, EPPPNUM, year_left, month_left , year_start, month_start, smallest_diff)

nfjed_key = nfjed_key |> mutate(month_before = month_left - 1) |> 
  mutate(year_before = if_else(month_before == 0, year_left - 1, year_left)) |>
  mutate(month_before = if_else(month_before == 0, 12, month_before))

nfjed_key = nfjed_key |> mutate(month_after = month_start + 1) |>
  mutate(year_after = if_else(month_after == 13, year_start + 1, year_start)) |>
  mutate(month_after = if_else(month_after == 13, 1, month_after))


nfjed_data = inner_join(nfjed_key, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_before" = "RHCALMN", "year_before" = "RHCALYR"))
nfjed_data = inner_join(nfjed_data, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_after" = "RHCALMN", "year_after" = "RHCALYR"), suffix = c("_old", "new"))

rm(no_first_job_end_date, nfjed_key)

# Finding before/after wages for no_second_job_end_date.
no_second_job_end_date = no_second_job_end_date |> mutate(TEJDATE1 = ymd(TEJDATE1)) |>
  mutate(TSJDATE1 = ymd(TSJDATE1)) |> mutate(TSJDATE2 = ymd(TSJDATE2))

# Creating variables for the month and year they left their old job and started
# their new one. Can use this to join and get month before and month after income.
no_second_job_end_date = no_second_job_end_date |> mutate(year_left = year(TEJDATE1)) |>
  mutate(month_left = month(TEJDATE1)) |> mutate(year_start = year(TSJDATE2)) |>
  mutate(month_start = month(TSJDATE2)) |> mutate(smallest_diff = as.numeric(TSJDATE2 - TEJDATE1))

# Creating key.
nsjed_key = no_second_job_end_date |> 
  distinct(SSUID, EENTAID, EPPPNUM, year_left, month_left , year_start, month_start, smallest_diff)

nsjed_key = nsjed_key |> mutate(month_before = month_left - 1) |> 
  mutate(year_before = if_else(month_before == 0, year_left - 1, year_left)) |>
  mutate(month_before = if_else(month_before == 0, 12, month_before))

nsjed_key = nsjed_key |> mutate(month_after = month_start + 1) |>
  mutate(year_after = if_else(month_after == 13, year_start + 1, year_start)) |>
  mutate(month_after = if_else(month_after == 13, 1, month_after))

nsjed_data = inner_join(nsjed_key, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_before" = "RHCALMN", "year_before" = "RHCALYR"))
nsjed_data = inner_join(nsjed_data, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_after" = "RHCALMN", "year_after" = "RHCALYR"), suffix = c("_old", "new"))

rm(no_second_job_end_date, nsjed_key)

# Now working on no_second_job. First I have to actually find when they get their
# new job.
find_start_key = no_second_job |> distinct(SSUID, EENTAID, EPPPNUM, TEJDATE1)

# Only observations with a start.
starts = job_data |> filter( (TEJDATE1 != -1) | (TEJDATE2 != -1) )

# Joining
find_starts = inner_join(find_start_key, starts, by = c("SSUID", "EENTAID", "EPPPNUM"), relationship = "many-to-many", suffix = c("", "_y"))

# Changing Variables to a date format.
find_starts = find_starts |> mutate(TSJDATE1 = ymd(TSJDATE1)) |> 
  mutate(TSJDATE2 = ymd(TSJDATE2)) |> mutate(TEJDATE1 = ymd(TEJDATE1))

# Getting duration between end and start dates for observations.
find_starts = find_starts |> mutate(date_diff1 = TSJDATE1 - TEJDATE1) |>
  mutate(date_diff2 = TSJDATE2 - TEJDATE1)

# Filtering out observations where the start dates are before the end date.
find_starts = find_starts |> filter( (date_diff1 >= 0) | (date_diff2 >= 0))

# Finding the lowest date diff observations.
find_starts = find_starts |> mutate(smallest_diff = ifelse( (date_diff2 > date_diff1) & (date_diff1 > 0), date_diff1, date_diff2))

# Records NAs as smaller than any date. NAs only happen for the second job date.
# So just impute the first date difference.
find_starts = find_starts |> mutate(smallest_diff = if_else(is.na(smallest_diff), as.numeric(date_diff1), as.numeric(smallest_diff)))

find_starts = find_starts |> group_by(SSUID, EENTAID, EPPPNUM, TEJDATE1) |> 
  filter(smallest_diff == min(smallest_diff)) |> ungroup()

find_starts = find_starts |> distinct(SSUID, EENTAID, EPPPNUM, TEJDATE1, .keep_all = T)

find_starts = find_starts |> mutate(date_diff1 = as.numeric(date_diff1)) |> 
  mutate(date_diff2 = as.numeric(date_diff2))

find_starts = find_starts |> mutate(year_start = if_else(smallest_diff == date_diff1, year(TSJDATE1), year(TSJDATE2))) |>
  mutate(month_start = if_else(smallest_diff == date_diff1, month(TSJDATE1), month(TSJDATE2)))

find_starts = find_starts |> mutate(year_left = year(TEJDATE1)) |> 
  mutate(month_left = month(TEJDATE1))

# Adding dates before and dates after.
find_starts = find_starts |> mutate(month_before = month_left - 1) |> 
  mutate(year_before = if_else(month_before == 0, year_left - 1, year_left)) |>
  mutate(month_before = if_else(month_before == 0, 12, month_before))

find_starts = find_starts |> mutate(month_after = month_start + 1) |>
  mutate(year_after = if_else(month_after == 13, year_start + 1, year_start)) |>
  mutate(month_after = if_else(month_after == 13, 1, month_after))

# Keeping only the needed things for joining.
keeps = c("SSUID", "EENTAID", "EPPPNUM", "smallest_diff", "year_start", "month_start",
          "year_left", "month_left", "month_before", "year_before", "month_after", "year_after")

nsj_key = find_starts |> select(all_of(keeps))

nsj_data = inner_join(nsj_key, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_before" = "RHCALMN", "year_before" = "RHCALYR"))
nsj_data = inner_join(nsj_data, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_after" = "RHCALMN", "year_after" = "RHCALYR"), suffix = c("_old", "new"))

# Final group. lose_second_job1.

find_start_key2 = lose_second_job1 |> distinct(SSUID, EENTAID, EPPPNUM, TEJDATE1)

# Only observations with a start.
starts = job_data |> filter( (TEJDATE1 != -1) | (TEJDATE2 != -1) )

# Joining
find_starts2 = inner_join(find_start_key2, starts, by = c("SSUID", "EENTAID", "EPPPNUM"), relationship = "many-to-many", suffix = c("", "_y"))

# Changing Variables to a date format.
find_starts2 = find_starts2 |> mutate(TSJDATE1 = ymd(TSJDATE1)) |> 
  mutate(TSJDATE2 = ymd(TSJDATE2)) |> mutate(TEJDATE1 = ymd(TEJDATE1))

# Getting duration between end and start dates for observations.
find_starts2 = find_starts2 |> mutate(date_diff1 = TSJDATE1 - TEJDATE1) |>
  mutate(date_diff2 = TSJDATE2 - TEJDATE1)

# Filtering out observations where the start dates are before the end date.
find_starts2 = find_starts2 |> filter( (date_diff1 >= 0) | (date_diff2 >= 0))

# Finding the lowest date diff observations.
find_starts2 = find_starts2 |> mutate(smallest_diff = ifelse( (date_diff2 > date_diff1) & (date_diff1 > 0), date_diff1, date_diff2))

# Records NAs as smaller than any date. NAs only happen for the second job date.
# So just impute the first date difference.
find_starts2 = find_starts2 |> mutate(smallest_diff = if_else(is.na(smallest_diff), as.numeric(date_diff1), as.numeric(smallest_diff)))

find_starts2 = find_starts2 |> group_by(SSUID, EENTAID, EPPPNUM, TEJDATE1) |> 
  filter(smallest_diff == min(smallest_diff)) |> ungroup()

find_starts2 = find_starts2 |> distinct(SSUID, EENTAID, EPPPNUM, TEJDATE1, .keep_all = T)

find_starts2 = find_starts2 |> mutate(date_diff1 = as.numeric(date_diff1)) |> 
  mutate(date_diff2 = as.numeric(date_diff2))

find_starts2 = find_starts2 |> mutate(year_start = if_else(smallest_diff == date_diff1, year(TSJDATE1), year(TSJDATE2))) |>
  mutate(month_start = if_else(smallest_diff == date_diff1, month(TSJDATE1), month(TSJDATE2)))

find_starts2 = find_starts2 |> mutate(year_left = year(TEJDATE1)) |> 
  mutate(month_left = month(TEJDATE1))

# Adding dates before and dates after.
find_starts2 = find_starts2 |> mutate(month_before = month_left - 1) |> 
  mutate(year_before = if_else(month_before == 0, year_left - 1, year_left)) |>
  mutate(month_before = if_else(month_before == 0, 12, month_before))

find_starts2 = find_starts2 |> mutate(month_after = month_start + 1) |>
  mutate(year_after = if_else(month_after == 13, year_start + 1, year_start)) |>
  mutate(month_after = if_else(month_after == 13, 1, month_after))

# Keeping only the needed things for joining.
keeps = c("SSUID", "EENTAID", "EPPPNUM", "smallest_diff", "year_start", "month_start",
          "year_left", "month_left", "month_before", "year_before", "month_after", "year_after")

lsj1_key = find_starts2 |> select(all_of(keeps))

lsj1_data = inner_join(lsj1_key, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_before" = "RHCALMN", "year_before" = "RHCALYR"))
lsj1_data = inner_join(lsj1_data, job_data, by = c("SSUID", "EPPPNUM", "EENTAID", "month_after" = "RHCALMN", "year_after" = "RHCALYR"), suffix = c("_old", "new"))

# Done sorting. Now will bind these together.
cleaned_08 = nsj_data |> bind_rows(nsjed_data) |> bind_rows(nfjed_data) |> 
  bind_rows(lsj1_data)

write_csv(cleaned_08, "2008_cleaned_data.csv")
