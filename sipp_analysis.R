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
start_position_08 = c(934, 1011, 897, 974, 950, 1027, 921, 998, 906, 983, 955,
                         1032, 888, 965, 885, 962, 786, 795, 1617, 1623, 855,
                         836, 2336, 6, 500, 503, 579, 859)

# Column end positions for 2008 Wave 1 variables.
end_position_08 = c(938, 1015, 904, 981, 953, 1030, 923, 1000, 907, 984, 958,
                       1035, 895, 972, 886, 963, 787, 796, 1621, 1627, 856, 837,
                       2337, 17, 502, 506, 580, 860)

# Variable Names
column_names = c("month_earnings_1", "month_earnings_2", "job_end_date_1", 
                 "job_end_date_2", "industry_code_1", "industry_code_2", 
                 "job_duration_1", "job_duration_2", "reason_separation_1", 
                 "reason_separation_2", "occupation_classification_code_1", 
                 "occupation_classification_code_2", "job_start_date_1", 
                 "job_start_date_2", "still_employed_1", "still_employed_2", "educ",
                 "vocational", "amount_state_unemp", "amount_supplement_unemp", 
                 "unknown_job_dates", "time_layoff_job", "longitudinal_month", 
                 "SSUID", "EENTAID", "EPPPNUM", "age", "empolyment_status")

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
  rm(data_stor)
}


# Creating Column Names
num_suffixes = 64

# Taking out identification variables.

column_names_no_id = column_names[ -c(23, 24, 25, 26)]

column_names_month = lapply(1:num_suffixes, function(i) {paste(column_names_no_id, i, sep = "_m")}) |> unlist()

# Filtering observations by month.
for (i in 1:64) {
  
  # Filter Month
  month = i
  
  # Vector name index
  start_num = 1 + (i - 1) * 24
  
  end_num = 24 + (i - 1) * 24
  data_stor = job_data_08 |> filter(longitudinal_month == month) |> select(!longitudinal_month) |> rename_with(.fn = ~ column_names_month[start_num:end_num], .cols = column_names_no_id)
  
  var_name = paste("job_data_08", i, sep = "_m")
  assign(var_name, data_stor)
}

# Joining together tables
by = join_by(SSUID, EENTAID, EPPPNUM)

job_data_08 = job_data_08_m1

for (i in 2:64) {
  
  var_name = paste("job_data_08", i, sep = "_m")
  
  data_stor = get(var_name)
  
  job_data_08 = job_data_08 |> full_join(data_stor, by)
  
  rm(data_stor)
  rm(list = var_name)
}

