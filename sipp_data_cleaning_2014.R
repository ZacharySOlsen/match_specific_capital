# Clearing the environment
rm(list =ls())

# Setting Working Directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Packages
library(data.table)
library(readr)
library(tidyverse)
library(janitor)

# Loading data
EJB_RSEND_ALL = c()
EJB_JOBID_ALL = c()
TJB_IND_ALL = c()
TJB_IND_ALL_ALT = c()
TJB_OCC_ALL = c()
TJB_OCC_ALL_ALT = c()
TJB_MSUM_ALL = c()
EJB_BMONTH_ALL = c()
EJB_EMONTH_ALL = c()
TJB_STRTYR_ALL = c()

# Creating all variable names that have 7 versions.
for (i in 1:7) {
  
  EJB = paste("EJB", sep = "", i)
  EJB_BMONTH = paste(EJB, sep = "_", "BMONTH")
  EJB_EMONTH = paste(EJB, sep = "_", "EMONTH")
  EJB_RSEND = paste(EJB, sep = "_", "RSEND")
  EJB_JOBID = paste(EJB, sep = "_", "JOBID")
  TJB = paste("TJB", sep = "", i)
  tjb = paste("tjb", sep = "", i)
  TJB_IND = paste(tjb, sep = "_", "ind")
  TJB_OCC = paste(tjb, sep = "_", "occ")
  TJB_STRTYR = paste(TJB, sep = "_", "STRTYR")
  TJB_MSUM = paste(TJB, sep = "_", "MSUM")
  TJB_IND_alt = paste(TJB, sep="_", "IND")
  TJB_OCC_alt = paste(TJB, sep = "_", "OCC")
  
  EJB_RSEND_ALL[i] = EJB_RSEND
  EJB_JOBID_ALL[i] = EJB_JOBID
  EJB_BMONTH_ALL[i] = EJB_BMONTH
  EJB_EMONTH_ALL[i] = EJB_EMONTH
  TJB_IND_ALL[i] = TJB_IND
  TJB_IND_ALL_ALT[i] = TJB_IND_alt
  TJB_OCC_ALL[i] = TJB_OCC
  TJB_OCC_ALL_ALT[i] = TJB_OCC_alt
  TJB_MSUM_ALL[i] = TJB_MSUM
  TJB_STRTYR_ALL[i] = TJB_STRTYR
}

# Other Variables.
column_names = c("ENJ_BMONTH", "ENJ_EMONTH", "ENJ_NOWRK8", "RMNUMJOBS", "ENJ_LAYOFF", "TUC1AMT", "TUC2AMT", "TPTOTINC", "EEDUC", "TAGE", "ESEX", "ERACE", "EORIGIN", "ECITIZEN", "monthcode", "ssuid", "PNUM")

column_names_alt = c("ENJ_BMONTH", "ENJ_EMONTH", "ENJ_NOWRK8", "RMNUMJOBS", "ENJ_LAYOFF", "TUC1AMT", "TUC2AMT", "TPTOTINC", "EEDUC", "TAGE", "ESEX", "ERACE", "EORIGIN", "ECITIZEN", "MONTHCODE", "SSUID", "PNUM")

# Appending all variable names together.
column_names = append(column_names, TJB_MSUM_ALL, 5) |> append(TJB_OCC_ALL, 5) |> append(TJB_IND_ALL, 5) |> append(EJB_JOBID_ALL, 5) |> append(EJB_RSEND_ALL, 5) |> append(TJB_STRTYR_ALL, 5) |> append(EJB_EMONTH_ALL, 5) |> append(EJB_BMONTH_ALL, 5)

column_names_alt = append(column_names_alt, TJB_MSUM_ALL, 5) |> append(TJB_OCC_ALL_ALT, 5) |> append(TJB_IND_ALL_ALT, 5) |> append(EJB_JOBID_ALL, 5) |> append(EJB_RSEND_ALL, 5) |> append(TJB_STRTYR_ALL, 5) |> append(EJB_EMONTH_ALL, 5) |> append(EJB_BMONTH_ALL, 5)

# File address for the data.
address = c()

for (i in 1:4) {
  address_1 = paste("/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2014_wave/pu2014w", sep = "", i)
  address_2 = paste(address_1, sep = "/PU2014W", i)
  address[i] = paste(address_2, sep = "", ".csv")
  rm(address_1, address_2)
}

# Loading the data.
job_data_14_WAVE1 = fread(address[1], sep = "|", select = column_names) |> 
  clean_names()

job_data_14_WAVE2 = fread(address[2], sep = "|", select = column_names_alt) |> 
  clean_names()

job_data_14_WAVE3 = fread(address[3], sep = "|", select = column_names_alt) |> 
  clean_names()

job_data_14_WAVE4 = fread(address[4], sep = "|", select = column_names_alt) |> 
  clean_names()

# Adding a "date" to all of the survey waves so that I can bind them together
# and know from which wave they are from. Also will hopefully allow linking.
job_data_14_WAVE1 = job_data_14_WAVE1 |> mutate(month_year = paste(monthcode, sep = "/", "2013"))

job_data_14_WAVE2 = job_data_14_WAVE2 |> mutate(month_year = paste(monthcode, sep = "/", "2014"))

job_data_14_WAVE3 = job_data_14_WAVE3 |> mutate(month_year = paste(monthcode, sep = "/", "2015"))

job_data_14_WAVE4 = job_data_14_WAVE4 |> mutate(month_year = paste(monthcode, sep = "/", "2016"))

# Vector denoting which reasons for separation we want.
reason_separation = c(1, 2, 3, 5, 6)

# Filtering out for an unemployment spell. Then why they lost their job.
no_work_1 = job_data_14_WAVE1 |> filter(!is.na(enj_bmonth))

no_work_1 = no_work_1 |> filter( (ejb1_rsend %in% reason_separation) | (ejb2_rsend %in% reason_separation) | (ejb3_rsend %in% reason_separation) | (ejb4_rsend %in% reason_separation) | (ejb5_rsend %in% reason_separation)  | (ejb6_rsend %in% reason_separation) | (ejb7_rsend %in% reason_separation) )

no_work_2 = job_data_14_WAVE2 |> filter(!is.na(enj_bmonth))

no_work_2 = no_work_2 |> filter( (ejb1_rsend %in% reason_separation) | (ejb2_rsend %in% reason_separation) | (ejb3_rsend %in% reason_separation) | (ejb4_rsend %in% reason_separation) | (ejb5_rsend %in% reason_separation)  | (ejb6_rsend %in% reason_separation) | (ejb7_rsend %in% reason_separation) )

no_work_3 = job_data_14_WAVE3 |> filter(!is.na(enj_bmonth))

no_work_3 = no_work_3 |> filter( (ejb1_rsend %in% reason_separation) | (ejb2_rsend %in% reason_separation) | (ejb3_rsend %in% reason_separation) | (ejb4_rsend %in% reason_separation) | (ejb5_rsend %in% reason_separation)  | (ejb6_rsend %in% reason_separation) | (ejb7_rsend %in% reason_separation) )

no_work_4 = job_data_14_WAVE4 |> filter(!is.na(enj_bmonth))

no_work_4 = no_work_4 |> filter( (ejb1_rsend %in% reason_separation) | (ejb2_rsend %in% reason_separation) | (ejb3_rsend %in% reason_separation) | (ejb4_rsend %in% reason_separation) | (ejb5_rsend %in% reason_separation)  | (ejb6_rsend %in% reason_separation) | (ejb7_rsend %in% reason_separation) )

# Matching job end date with job start date.
match_date_1 = no_work_1 |> filter( (enj_emonth == ejb1_bmonth) | (enj_emonth == ejb2_bmonth) | (enj_emonth == ejb3_bmonth) | (enj_emonth == ejb4_bmonth) | (enj_emonth == ejb5_bmonth) | (enj_emonth == ejb6_bmonth) | (enj_emonth == ejb7_bmonth)) #|> filter(enj_emonth == monthcode)

match_date_2 = no_work_2 |> filter( (enj_emonth == ejb1_bmonth) | (enj_emonth == ejb2_bmonth) | (enj_emonth == ejb3_bmonth) | (enj_emonth == ejb4_bmonth) | (enj_emonth == ejb5_bmonth) | (enj_emonth == ejb6_bmonth) | (enj_emonth == ejb7_bmonth)) #|> filter(enj_emonth == monthcode)

match_date_3 = no_work_3 |> filter( (enj_emonth == ejb1_bmonth) | (enj_emonth == ejb2_bmonth) | (enj_emonth == ejb3_bmonth) | (enj_emonth == ejb4_bmonth) | (enj_emonth == ejb5_bmonth) | (enj_emonth == ejb6_bmonth) | (enj_emonth == ejb7_bmonth)) #|> filter(enj_emonth == monthcode)

match_date_4 = no_work_4 |> filter( (enj_emonth == ejb1_bmonth) | (enj_emonth == ejb2_bmonth) | (enj_emonth == ejb3_bmonth) | (enj_emonth == ejb4_bmonth) | (enj_emonth == ejb5_bmonth) | (enj_emonth == ejb6_bmonth) | (enj_emonth == ejb7_bmonth)) #|> filter(enj_emonth == monthcode)

# Filtering out observations I don't have a before month wage to check.
match_date_1 = match_date_1 |> filter(enj_bmonth != 1)

# Filtering out those at the end of the last observation wave since we won't have
# month after wages for them.
match_date_4 = match_date_4 |> filter(enj_emonth < 12)

# Filtering out those who weren't working due to reasons other than not being able
# to find a job.
match_date_1 = match_date_1 |> filter(enj_nowrk8 == 1)
match_date_2 = match_date_2 |> filter(enj_nowrk8 == 1)
match_date_3 = match_date_3 |> filter(enj_nowrk8 == 1)
match_date_4 = match_date_4 |> filter(enj_nowrk8 == 1)

# Now identifying those who could potentially have interyear unemployment.
inter_year_1 = no_work_1 |> filter(enj_emonth == 12)
inter_year_2 = no_work_2 |> filter(enj_emonth == 12)
inter_year_3 = no_work_3 |> filter(enj_emonth == 12)

# Need a link to month 1.
month_1_wave2 = match_date_2 |> filter(enj_bmonth == 1)
month_1_wave3 = match_date_3 |> filter(enj_bmonth == 1)
month_1_wave4 = match_date_4 |> filter(enj_bmonth == 1)

ssuid_wave2 = month_1_wave2 |> distinct(ssuid)
ssuid_wave3 = month_1_wave3 |> distinct(ssuid)
ssuid_wave4 = month_1_wave4 |> distinct(ssuid)

pnum_wave2 = month_1_wave2 |> distinct(pnum)
pnum_wave3 = month_1_wave3 |> distinct(pnum)
pnum_wave4 = month_1_wave4 |> distinct(pnum)

# Filtering by id for interyear groups.
year1_year2 = inter_year_1 |> filter(pnum %in% pnum_wave2$pnum) |> 
  filter(ssuid %in% ssuid_wave2$ssuid)

year1_year3 = inter_year_1 |> filter(pnum %in% pnum_wave4$pnum) |> 
  filter(ssuid %in% ssuid_wave4$ssuid)

ssuid_1_3 = year1_year3 |> distinct(ssuid)
pnum_1_3 = year1_year3 |> distinct(pnum)

test = month_1_wave2 |> filter(pnum %in% pnum_1_3$pnum) |> filter(ssuid %in% ssuid_1_3$ssuid)

test2 = job_data_14_WAVE3 |> filter(pnum %in% test$pnum) |> filter(ssuid %in% test$ssuid)
