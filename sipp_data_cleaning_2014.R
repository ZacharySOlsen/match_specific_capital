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
column_names_ind = c("ENJ_BMONTH", "ENJ_EMONTH", "ENJ_NOWRK8", "RMNUMJOBS", "monthcode", "ssuid", "PNUM")

column_names_alt = c("ENJ_BMONTH", "ENJ_EMONTH", "ENJ_NOWRK8", "RMNUMJOBS", "MONTHCODE", "SSUID", "PNUM")

# Appending all variable names together.
column_names = append(column_names_ind, TJB_MSUM_ALL, 5) |> append(TJB_OCC_ALL, 5) |> append(TJB_IND_ALL, 5) |> append(EJB_JOBID_ALL, 5) |> append(EJB_RSEND_ALL, 5) |> append(TJB_STRTYR_ALL, 5) |> append(EJB_EMONTH_ALL, 5) |> append(EJB_BMONTH_ALL, 5)

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

# Dropping tjb6_strtyr and tjb7_strtyr. Are suppressed in the data so all are 
# NA.
job_data_14_WAVE1 = job_data_14_WAVE1 |> select(!tjb6_strtyr) |> select(!tjb7_strtyr)
job_data_14_WAVE2 = job_data_14_WAVE2 |> select(!tjb6_strtyr) |> select(!tjb7_strtyr)
job_data_14_WAVE3 = job_data_14_WAVE3 |> select(!tjb6_strtyr) |> select(!tjb7_strtyr)
job_data_14_WAVE4 = job_data_14_WAVE4 |> select(!tjb6_strtyr) |> select(!tjb7_strtyr)

# Creating a longitudinal month variables to have time index across waves.
job_data_14_WAVE1 = job_data_14_WAVE1 |> mutate(longitudinal_month = monthcode)
job_data_14_WAVE2 = job_data_14_WAVE2 |> mutate(longitudinal_month = monthcode + 12)
job_data_14_WAVE3 = job_data_14_WAVE3 |> mutate(longitudinal_month = monthcode + 24)
job_data_14_WAVE4 = job_data_14_WAVE4 |> mutate(longitudinal_month = monthcode + 36)

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

# Creating distance metrics
no_work_1 = no_work_1 |> mutate(distance_jb1 = if_else(is.na(tjb1_strtyr), ejb1_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb2 = if_else(is.na(tjb2_strtyr), ejb2_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb3 = if_else(is.na(tjb3_strtyr), ejb3_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb4 = if_else(is.na(tjb4_strtyr), ejb4_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb5 = if_else(is.na(tjb5_strtyr), ejb5_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb6 = ejb6_bmonth - enj_emonth) |>
  mutate(distance_jb7 = ejb7_bmonth - enj_emonth)

no_work_2 = no_work_2 |> mutate(distance_jb1 = if_else(is.na(tjb1_strtyr), ejb1_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb2 = if_else(is.na(tjb2_strtyr), ejb2_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb3 = if_else(is.na(tjb3_strtyr), ejb3_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb4 = if_else(is.na(tjb4_strtyr), ejb4_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb5 = if_else(is.na(tjb5_strtyr), ejb5_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb6 = ejb6_bmonth - enj_emonth) |>
  mutate(distance_jb7 = ejb7_bmonth - enj_emonth)

no_work_3 = no_work_3 |> mutate(distance_jb1 = if_else(is.na(tjb1_strtyr), ejb1_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb2 = if_else(is.na(tjb2_strtyr), ejb2_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb3 = if_else(is.na(tjb3_strtyr), ejb3_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb4 = if_else(is.na(tjb4_strtyr), ejb4_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb5 = if_else(is.na(tjb5_strtyr), ejb5_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb6 = ejb6_bmonth - enj_emonth) |>
  mutate(distance_jb7 = ejb7_bmonth - enj_emonth)

no_work_4 = no_work_4 |> mutate(distance_jb1 = if_else(is.na(tjb1_strtyr), ejb1_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb2 = if_else(is.na(tjb2_strtyr), ejb2_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb3 = if_else(is.na(tjb3_strtyr), ejb3_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb4 = if_else(is.na(tjb4_strtyr), ejb4_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb5 = if_else(is.na(tjb5_strtyr), ejb5_bmonth - enj_emonth, NA)) |>
  mutate(distance_jb6 = ejb6_bmonth - enj_emonth) |>
  mutate(distance_jb7 = ejb7_bmonth - enj_emonth)

# Setting negative distances to NA.

no_work_1 = no_work_1 |> mutate(distance_jb1 = if_else(distance_jb1 < 0, NA, distance_jb1)) |>
  mutate(distance_jb2 = if_else(distance_jb2 < 0, NA, distance_jb2)) |>
  mutate(distance_jb3 = if_else(distance_jb3 < 0, NA, distance_jb3)) |>
  mutate(distance_jb4 = if_else(distance_jb4 < 0, NA, distance_jb4)) |>
  mutate(distance_jb5 = if_else(distance_jb5 < 0, NA, distance_jb5)) |>
  mutate(distance_jb6 = if_else(distance_jb6 < 0, NA, distance_jb6)) |>
  mutate(distance_jb7 = if_else(distance_jb7 < 0, NA, distance_jb7))

no_work_2 = no_work_2 |> mutate(distance_jb1 = if_else(distance_jb1 < 0, NA, distance_jb1)) |>
  mutate(distance_jb2 = if_else(distance_jb2 < 0, NA, distance_jb2)) |>
  mutate(distance_jb3 = if_else(distance_jb3 < 0, NA, distance_jb3)) |>
  mutate(distance_jb4 = if_else(distance_jb4 < 0, NA, distance_jb4)) |>
  mutate(distance_jb5 = if_else(distance_jb5 < 0, NA, distance_jb5)) |>
  mutate(distance_jb6 = if_else(distance_jb6 < 0, NA, distance_jb6)) |>
  mutate(distance_jb7 = if_else(distance_jb7 < 0, NA, distance_jb7))

no_work_3 = no_work_3 |> mutate(distance_jb1 = if_else(distance_jb1 < 0, NA, distance_jb1)) |>
  mutate(distance_jb2 = if_else(distance_jb2 < 0, NA, distance_jb2)) |>
  mutate(distance_jb3 = if_else(distance_jb3 < 0, NA, distance_jb3)) |>
  mutate(distance_jb4 = if_else(distance_jb4 < 0, NA, distance_jb4)) |>
  mutate(distance_jb5 = if_else(distance_jb5 < 0, NA, distance_jb5)) |>
  mutate(distance_jb6 = if_else(distance_jb6 < 0, NA, distance_jb6)) |>
  mutate(distance_jb7 = if_else(distance_jb7 < 0, NA, distance_jb7))

no_work_4 = no_work_4 |> mutate(distance_jb1 = if_else(distance_jb1 < 0, NA, distance_jb1)) |>
  mutate(distance_jb2 = if_else(distance_jb2 < 0, NA, distance_jb2)) |>
  mutate(distance_jb3 = if_else(distance_jb3 < 0, NA, distance_jb3)) |>
  mutate(distance_jb4 = if_else(distance_jb4 < 0, NA, distance_jb4)) |>
  mutate(distance_jb5 = if_else(distance_jb5 < 0, NA, distance_jb5)) |>
  mutate(distance_jb6 = if_else(distance_jb6 < 0, NA, distance_jb6)) |>
  mutate(distance_jb7 = if_else(distance_jb7 < 0, NA, distance_jb7))

# Filtering the first month of wave 1 and the last month of wave 4 out because
# I want a month before and month after for earnings.
no_work_1 = no_work_1 |> filter(enj_bmonth != 1)
no_work_4 = no_work_4 |> filter(enj_emonth != 12)

# Just gonna filter manually. Don't feel like spending time creating a loop.


# Wave 1 job variables 1-7
dst1_w1 = no_work_1 |> filter(!is.na(distance_jb1))
dst2_w1 = no_work_1 |> filter(!is.na(distance_jb2))
dst3_w1 = no_work_1 |> filter(!is.na(distance_jb3))
dst4_w1 = no_work_1 |> filter(!is.na(distance_jb4))
dst5_w1 = no_work_1 |> filter(!is.na(distance_jb5))
dst6_w1 = no_work_1 |> filter(!is.na(distance_jb6))
dst7_w1 = no_work_1 |> filter(!is.na(distance_jb7))

# Wave 2 job variables 1-7
dst1_w2 = no_work_2 |> filter(!is.na(distance_jb1))
dst2_w2 = no_work_2 |> filter(!is.na(distance_jb2))
dst3_w2 = no_work_2 |> filter(!is.na(distance_jb3))
dst4_w2 = no_work_2 |> filter(!is.na(distance_jb4))
dst5_w2 = no_work_2 |> filter(!is.na(distance_jb5))
dst6_w2 = no_work_2 |> filter(!is.na(distance_jb6))
dst7_w2 = no_work_2 |> filter(!is.na(distance_jb7))

# Wave 3 job variables 1-7
dst1_w3 = no_work_3 |> filter(!is.na(distance_jb1))
dst2_w3 = no_work_3 |> filter(!is.na(distance_jb2))
dst3_w3 = no_work_3 |> filter(!is.na(distance_jb3))
dst4_w3 = no_work_3 |> filter(!is.na(distance_jb4))
dst5_w3 = no_work_3 |> filter(!is.na(distance_jb5))
dst6_w3 = no_work_3 |> filter(!is.na(distance_jb6))
dst7_w3 = no_work_3 |> filter(!is.na(distance_jb7))

# Wave 4 job variables 1-7
dst1_w4 = no_work_4 |> filter(!is.na(distance_jb1)) 
dst2_w4 = no_work_4 |> filter(!is.na(distance_jb2))
dst3_w4 = no_work_4 |> filter(!is.na(distance_jb3))
dst4_w4 = no_work_4 |> filter(!is.na(distance_jb4))
dst5_w4 = no_work_4 |> filter(!is.na(distance_jb5))
dst6_w4 = no_work_4 |> filter(!is.na(distance_jb6))
dst7_w4 = no_work_4 |> filter(!is.na(distance_jb7))

# All 5, 6, and 7 variables are empty. Can delete and not focus on them.
rm(dst5_w1, dst5_w2, dst5_w3, dst5_w4, dst6_w1, dst6_w2, dst6_w3, dst6_w4, dst7_w1,
   dst7_w2, dst7_w3, dst7_w4)

# Along with them dst4_w3 is empty. Will remove as well.
rm(dst4_w3)

# Filtering out those who got multiple jobs in the same month. Not clear how I
# would compare the wage change. Can always be added back in later.
dst1_w1 = dst1_w1 |> filter(is.na(distance_jb2)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst1_w2 = dst1_w2 |> filter(is.na(distance_jb2)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst1_w3 = dst1_w3 |> filter(is.na(distance_jb2)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst1_w4 = dst1_w4 |> filter(is.na(distance_jb2)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst2_w1 = dst2_w1 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst2_w2 = dst2_w2 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst2_w3 = dst2_w3 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst2_w4 = dst2_w4 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb3)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst3_w1 = dst3_w1 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst3_w2 = dst3_w2 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst3_w3 = dst3_w3 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst3_w4 = dst3_w4 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb4)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst4_w1 = dst4_w1 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb3)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst4_w2 = dst4_w2 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb3)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))

dst4_w4 = dst4_w4 |> filter(is.na(distance_jb1)) |> filter(is.na(distance_jb2)) |> 
  filter(is.na(distance_jb3)) |> filter(is.na(distance_jb5)) |> filter(is.na(distance_jb6)) |> 
  filter(is.na(distance_jb7))


# Inter-Year

ssuid_no_work_1 = no_work_1 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |> 
  distinct(ssuid)

ssuid_no_work_2 = no_work_2 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |> 
  distinct(ssuid)

ssuid_no_work_3 = no_work_3 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |>
  distinct(ssuid)


pnum_no_work_1 = no_work_1 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |>
  distinct(pnum)

pnum_no_work_2 = no_work_2 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |> 
  distinct(pnum)

pnum_no_work_3 = no_work_3 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |>
  distinct(pnum)

# Filtering the dsts.

# SSUID
# Wave 1 to 2.
ssuid_dst1_w2 = dst1_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(ssuid)

ssuid_dst2_w2 = dst2_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(ssuid)

ssuid_dst3_w2 = dst3_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(ssuid)

ssuid_dst4_w2 = dst4_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(ssuid)

ssuid_dst_w2 = ssuid_dst1_w2 |> bind_rows(ssuid_dst2_w2) |> bind_rows(ssuid_dst3_w2) |>
  bind_rows(ssuid_dst4_w2)

# Wave 2 to 3.
ssuid_dst1_w3 = dst1_w3 |> filter(pnum %in% pnum_no_work_2$pnum) |>
  filter(ssuid %in% ssuid_no_work_2$ssuid) |> distinct(ssuid)

ssuid_dst2_w3 = dst2_w3 |> filter(pnum %in% pnum_no_work_2$pnum) |>
  filter(ssuid %in% ssuid_no_work_2$ssuid) |> distinct(ssuid)

ssuid_dst3_w3 = dst3_w3 |> filter(pnum %in% pnum_no_work_2$pnum) |>
  filter(ssuid %in% ssuid_no_work_2$ssuid) |> distinct(ssuid)

ssuid_dst_w3 = ssuid_dst1_w3 |> bind_rows(ssuid_dst2_w3) |> bind_rows(ssuid_dst3_w3)

# Wave 3 to 4.
ssuid_dst1_w4 = dst1_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(ssuid)

ssuid_dst2_w4 = dst2_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(ssuid)

ssuid_dst3_w4 = dst3_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(ssuid)

ssuid_dst4_w4 = dst4_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(ssuid)

ssuid_dst_w4 = ssuid_dst1_w4 |> bind_rows(ssuid_dst2_w4) |> bind_rows(ssuid_dst3_w4) |>
  bind_rows(ssuid_dst4_w4)

# PNUM
# Wave 1 to 2.
pnum_dst1_w2 = dst1_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(pnum)

pnum_dst2_w2 = dst2_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(pnum)

pnum_dst3_w2 = dst3_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(pnum)

pnum_dst4_w2 = dst4_w2 |> filter(pnum %in% pnum_no_work_1$pnum) |>
  filter(ssuid %in% ssuid_no_work_1$ssuid) |> distinct(pnum)

pnum_dst_w2 = pnum_dst1_w2 |> bind_rows(pnum_dst2_w2) |> bind_rows(pnum_dst3_w2) |>
  bind_rows(pnum_dst4_w2)

# Wave 2 to 3.
pnum_dst1_w3 = dst1_w3 |> filter(pnum %in% pnum_no_work_2$pnum) |>
  filter(ssuid %in% ssuid_no_work_2$ssuid) |> distinct(pnum)

pnum_dst2_w3 = dst2_w3 |> filter(pnum %in% pnum_no_work_2$pnum) |>
  filter(ssuid %in% ssuid_no_work_2$ssuid) |> distinct(pnum)

pnum_dst3_w3 = dst3_w3 |> filter(pnum %in% pnum_no_work_2$pnum) |>
  filter(ssuid %in% ssuid_no_work_2$ssuid) |> distinct(pnum)

pnum_dst_w3 = pnum_dst1_w3 |> bind_rows(pnum_dst2_w3) |> bind_rows(pnum_dst3_w3)

# Wave 3 to 4
pnum_dst1_w4 = dst1_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(pnum)

pnum_dst2_w4 = dst2_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(pnum)

pnum_dst3_w4 = dst3_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(pnum)

pnum_dst4_w4 = dst4_w4 |> filter(pnum %in% pnum_no_work_3$pnum) |>
  filter(ssuid %in% ssuid_no_work_3$ssuid) |> distinct(pnum)

pnum_dst_w4 = pnum_dst1_w4 |> bind_rows(pnum_dst2_w4) |> bind_rows(pnum_dst3_w4) |>
  bind_rows(pnum_dst4_w4)

# Clearing variables.
rm(pnum_dst1_w2, pnum_dst1_w3, pnum_dst1_w4, pnum_dst2_w2, pnum_dst2_w3, pnum_dst2_w4,
   pnum_dst3_w2, pnum_dst3_w3, pnum_dst3_w4, pnum_dst4_w2, pnum_dst4_w4, pnum_no_work_1, 
   pnum_no_work_2, pnum_no_work_3, ssuid_dst1_w2, ssuid_dst1_w3, ssuid_dst1_w4, 
   ssuid_dst2_w2, ssuid_dst2_w3, ssuid_dst2_w4, ssuid_dst3_w2, ssuid_dst3_w3, 
   ssuid_dst3_w4, ssuid_dst4_w2, ssuid_dst4_w4, ssuid_no_work_1, ssuid_no_work_2, 
   ssuid_no_work_3)

# Final filtering.
matches_no_work_1 = no_work_1 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |> 
  filter(pnum %in% pnum_dst_w2$pnum) |> filter(ssuid %in% ssuid_dst_w2$ssuid)

matches_no_work_2 = no_work_2 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |> 
  filter(pnum %in% pnum_dst_w3$pnum) |> filter(ssuid %in% ssuid_dst_w3$ssuid)

matches_no_work_3 = no_work_3 |> filter(enj_emonth == 12) |>
  filter(is.na(distance_jb1) & is.na(distance_jb2) & is.na(distance_jb3) & is.na(distance_jb4) & is.na(distance_jb5) & is.na(distance_jb6) & is.na(distance_jb7)) |> 
  filter(pnum %in% pnum_dst_w4$pnum) |> filter(ssuid %in% ssuid_dst_w4$ssuid)

# Selecting job variables to keep.

dst_w2 = dst1_w2 |> bind_rows(dst2_w2) |> bind_rows(dst3_w2) |> bind_rows(dst4_w2)

dst_w3 = dst1_w3 |> bind_rows(dst2_w3) |> bind_rows(dst3_w3)

dst_w4 = dst1_w4 |> bind_rows(dst2_w4) |> bind_rows(dst3_w4) |> bind_rows(dst4_w4)

# Joining.
year_one_two_match = matches_no_work_1 |> inner_join(dst_w2, by = c("pnum", "ssuid"), suffix = c("_one", "_two"))

year_two_three_match = matches_no_work_2 |> inner_join(dst_w3, by = c("pnum", "ssuid"), suffix = c("_two", "_three"))

year_three_four_match = matches_no_work_3 |> inner_join(dst_w4, by = c("pnum", "ssuid"), suffix = c("_three", "_four"))


# Creating columns of true start dates.
year_one_two_match = year_one_two_match |> mutate(end_old_job = enj_bmonth_one) |> mutate(begin_new_job = enj_emonth_two + 12)

year_two_three_match = year_two_three_match |> mutate(end_old_job = enj_bmonth_two + 12) |> mutate(begin_new_job = enj_emonth_three + 24)

year_three_four_match = year_three_four_match |> mutate(end_old_job = enj_bmonth_three + 24) |> mutate(begin_new_job = enj_emonth_four + 36)

#Now will separate by which have distance values by variable.

# Year-one-two
year_one_two_match_dst1 = year_one_two_match |> filter(!is.na(distance_jb1_two)) |> 
  select(!ends_with("_one")) |> select(!contains("2") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |> 
  select(!contains("7"))

year_one_two_match_dst2 = year_one_two_match |>
  filter(!is.na(distance_jb2_two)) |> select(!ends_with("_one")) |>  select(!contains("1") ) |> 
  select(!contains("3") ) |> select(!contains("4")) |> select(!contains("5")) |> 
  select(!contains("6")) |> select(!contains("7"))

year_one_two_match_dst3 = year_one_two_match |> filter(!is.na(distance_jb3_two)) |>
  select(!ends_with("_one")) |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_one_two_match_dst4 = year_one_two_match |> filter(!is.na(distance_jb4_two)) |>
  select(!ends_with("_one"))|> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("3")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

# Year-two-three
year_two_three_match_dst1 = year_two_three_match |> filter(!is.na(distance_jb1_three)) |>
  select(!ends_with("_two")) |> select(!contains("2")) |> select(!contains("3")) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_two_three_match_dst2 = year_two_three_match |> filter(!is.na(distance_jb2_three)) |>
  select(!ends_with("_two")) |> select(!contains("1") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_two_three_match_dst3 = year_two_three_match |> filter(!is.na(distance_jb3_three)) |>
  select(!ends_with("_two")) |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_two_three_match_dst4 = year_two_three_match |> filter(!is.na(distance_jb4_three)) |>
  select(!ends_with("_two")) |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("3")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

# Year-three-four
year_three_four_match_dst1 = year_three_four_match |> filter(!is.na(distance_jb1_four)) |>
  select(!ends_with("_three")) |> select(!contains("2")) |> select(!contains("3")) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_three_four_match_dst2 = year_three_four_match |> filter(!is.na(distance_jb2_four)) |>
  select(!ends_with("_three")) |> select(!contains("1") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_three_four_match_dst3 = year_three_four_match |> filter(!is.na(distance_jb3_four)) |>
  select(!ends_with("_three")) |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

year_three_four_match_dst4 = year_three_four_match |> filter(!is.na(distance_jb4_four)) |>
  select(!ends_with("_three")) |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("3")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))


# 1 variables
dst1_w1 = dst1_w1 |> select(!contains("2")) |> select(!contains("3")) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst1_w2 = dst1_w2 |> select(!contains("2") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst1_w3 = dst1_w3 |> select(!contains("2") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst1_w4 = dst1_w4 |> select(!contains("2") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

# 2 variables
dst2_w1 = dst2_w1 |> select(!contains("1") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst2_w2 = dst2_w2 |> select(!contains("1") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst2_w3 = dst2_w3 |> select(!contains("1") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst2_w4 = dst2_w4 |> select(!contains("1") ) |> select(!contains("3") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

# 3 variables
dst3_w1 = dst3_w1 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst3_w2 = dst3_w2 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst3_w3 = dst3_w3 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst3_w4 = dst3_w4 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("4")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

# 4 variables
dst4_w1 = dst4_w1 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("3")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst4_w2 = dst4_w2 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("3")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

dst4_w4 = dst4_w4 |> select(!contains("1") ) |> select(!contains("2") ) |> 
  select(!contains("3")) |> select(!contains("5")) |> select(!contains("6")) |>
  select(!contains("7"))

# Rename variables.
dst1_labels = c("ejb_bmonth" = "ejb1_bmonth", "ejb_emonth" = "ejb1_emonth", "tjb_strtyr" = "tjb1_strtyr", "ejb_rsend" = "ejb1_rsend", "ejb_jobid" = "ejb1_jobid", "tjb_ind" = "tjb1_ind", "tjb_occ" = "tjb1_occ", "tjb_msum" = "tjb1_msum", "distance" = "distance_jb1")

dst2_labels = c("ejb_bmonth" = "ejb2_bmonth", "ejb_emonth" = "ejb2_emonth", "tjb_strtyr" = "tjb2_strtyr", "ejb_rsend" = "ejb2_rsend", "ejb_jobid" = "ejb2_jobid", "tjb_ind" = "tjb2_ind", "tjb_occ" = "tjb2_occ", "tjb_msum" = "tjb2_msum", "distance" = "distance_jb2")

dst3_labels = c("ejb_bmonth" = "ejb3_bmonth", "ejb_emonth" = "ejb3_emonth", "tjb_strtyr" = "tjb3_strtyr", "ejb_rsend" = "ejb3_rsend", "ejb_jobid" = "ejb3_jobid", "tjb_ind" = "tjb3_ind", "tjb_occ" = "tjb3_occ", "tjb_msum" = "tjb3_msum", "distance" = "distance_jb3")

dst4_labels = c("ejb_bmonth" = "ejb4_bmonth", "ejb_emonth" = "ejb4_emonth", "tjb_strtyr" = "tjb4_strtyr", "ejb_rsend" = "ejb4_rsend", "ejb_jobid" = "ejb4_jobid", "tjb_ind" = "tjb4_ind", "tjb_occ" = "tjb4_occ", "tjb_msum" = "tjb4_msum", "distance" = "distance_jb4")

y_one_two_labels_dst1 = c("enj_bmonth" = "enj_bmonth_two", "enj_emonth" = "enj_emonth_two", "enj_nowrk8" = "enj_nowrk8_two", "rmnumjobs" = "rmnumjobs_two", "monthcode" = "monthcode_two", "ejb_bmonth" = "ejb1_bmonth_two", "ejb_emonth" = "ejb1_emonth_two", "tjb_strtyr" = "tjb1_strtyr_two", "ejb_rsend" = "ejb1_rsend_two", "ejb_jobid" = "ejb1_jobid_two", "tjb_ind" = "tjb1_ind_two", "tjb_occ" = "tjb1_occ_two", "tjb_msum" = "tjb1_msum_two", "distance" = "distance_jb1_two", "longitudinal_month" = "longitudinal_month_two")

y_one_two_labels_dst2 = c("enj_bmonth" = "enj_bmonth_two", "enj_emonth" = "enj_emonth_two", "enj_nowrk8" = "enj_nowrk8_two", "rmnumjobs" = "rmnumjobs_two", "monthcode" = "monthcode_two", "ejb_bmonth" = "ejb2_bmonth_two", "ejb_emonth" = "ejb2_emonth_two", "tjb_strtyr" = "tjb2_strtyr_two", "ejb_rsend" = "ejb2_rsend_two", "ejb_jobid" = "ejb2_jobid_two", "tjb_ind" = "tjb2_ind_two", "tjb_occ" = "tjb2_occ_two", "tjb_msum" = "tjb2_msum_two", "distance" = "distance_jb2_two", "longitudinal_month" = "longitudinal_month_two")

y_one_two_labels_dst3 = c("enj_bmonth" = "enj_bmonth_two", "enj_emonth" = "enj_emonth_two", "enj_nowrk8" = "enj_nowrk8_two", "rmnumjobs" = "rmnumjobs_two", "monthcode" = "monthcode_two", "ejb_bmonth" = "ejb3_bmonth_two", "ejb_emonth" = "ejb3_emonth_two", "tjb_strtyr" = "tjb3_strtyr_two", "ejb_rsend" = "ejb3_rsend_two", "ejb_jobid" = "ejb3_jobid_two", "tjb_ind" = "tjb3_ind_two", "tjb_occ" = "tjb3_occ_two", "tjb_msum" = "tjb3_msum_two", "distance" = "distance_jb3_two", "longitudinal_month" = "longitudinal_month_two")

y_one_two_labels_dst4 = c("enj_bmonth" = "enj_bmonth_two", "enj_emonth" = "enj_emonth_two", "enj_nowrk8" = "enj_nowrk8_two", "rmnumjobs" = "rmnumjobs_two", "monthcode" = "monthcode_two", "ejb_bmonth" = "ejb4_bmonth_two", "ejb_emonth" = "ejb4_emonth_two", "tjb_strtyr" = "tjb4_strtyr_two", "ejb_rsend" = "ejb4_rsend_two", "ejb_jobid" = "ejb4_jobid_two", "tjb_ind" = "tjb4_ind_two", "tjb_occ" = "tjb4_occ_two", "tjb_msum" = "tjb4_msum_two", "distance" = "distance_jb4_two", "longitudinal_month" = "longitudinal_month_two")

y_two_three_labels_dst1 = c("enj_bmonth" = "enj_bmonth_three", "enj_emonth" = "enj_emonth_three", "enj_nowrk8" = "enj_nowrk8_three", "rmnumjobs" = "rmnumjobs_three", "monthcode" = "monthcode_three", "ejb_bmonth" = "ejb1_bmonth_three", "ejb_emonth" = "ejb1_emonth_three", "tjb_strtyr" = "tjb1_strtyr_three", "ejb_rsend" = "ejb1_rsend_three", "ejb_jobid" = "ejb1_jobid_three", "tjb_ind" = "tjb1_ind_three", "tjb_occ" = "tjb1_occ_three", "tjb_msum" = "tjb1_msum_three", "distance" = "distance_jb1_three", "longitudinal_month" = "longitudinal_month_three")

y_two_three_labels_dst2 = c("enj_bmonth" = "enj_bmonth_three", "enj_emonth" = "enj_emonth_three", "enj_nowrk8" = "enj_nowrk8_three", "rmnumjobs" = "rmnumjobs_three", "monthcode" = "monthcode_three", "ejb_bmonth" = "ejb2_bmonth_three", "ejb_emonth" = "ejb2_emonth_three", "tjb_strtyr" = "tjb2_strtyr_three", "ejb_rsend" = "ejb2_rsend_three", "ejb_jobid" = "ejb2_jobid_three", "tjb_ind" = "tjb2_ind_three", "tjb_occ" = "tjb2_occ_three", "tjb_msum" = "tjb2_msum_three", "distance" = "distance_jb2_three", "longitudinal_month" = "longitudinal_month_three")

y_three_four_labels_dst1 = c("enj_bmonth" = "enj_bmonth_four", "enj_emonth" = "enj_emonth_four", "enj_nowrk8" = "enj_nowrk8_four", "rmnumjobs" = "rmnumjobs_four", "monthcode" = "monthcode_four", "ejb_bmonth" = "ejb1_bmonth_four", "ejb_emonth" = "ejb1_emonth_four", "tjb_strtyr" = "tjb1_strtyr_four", "ejb_rsend" = "ejb1_rsend_four", "ejb_jobid" = "ejb1_jobid_four", "tjb_ind" = "tjb1_ind_four", "tjb_occ" = "tjb1_occ_four", "tjb_msum" = "tjb1_msum_four", "distance" = "distance_jb1_four", "longitudinal_month" = "longitudinal_month_four")

y_three_four_labels_dst2 = c("enj_bmonth" = "enj_bmonth_four", "enj_emonth" = "enj_emonth_four", "enj_nowrk8" = "enj_nowrk8_four", "rmnumjobs" = "rmnumjobs_four", "monthcode" = "monthcode_four", "ejb_bmonth" = "ejb2_bmonth_four", "ejb_emonth" = "ejb2_emonth_four", "tjb_strtyr" = "tjb2_strtyr_four", "ejb_rsend" = "ejb2_rsend_four", "ejb_jobid" = "ejb2_jobid_four", "tjb_ind" = "tjb2_ind_four", "tjb_occ" = "tjb2_occ_four", "tjb_msum" = "tjb2_msum_four", "distance" = "distance_jb2_four", "longitudinal_month" = "longitudinal_month_four")

# Renaming
dst1_w1 = dst1_w1 |> rename(any_of(dst1_labels))
dst1_w2 = dst1_w2 |> rename(any_of(dst1_labels))
dst1_w3 = dst1_w3 |> rename(any_of(dst1_labels))
dst1_w4 = dst1_w4 |> rename(any_of(dst1_labels))

dst2_w1 = dst2_w1 |> rename(any_of(dst2_labels))
dst2_w2 = dst2_w2 |> rename(any_of(dst2_labels))
dst2_w3 = dst2_w3 |> rename(any_of(dst2_labels))
dst2_w4 = dst2_w4 |> rename(any_of(dst2_labels))

dst3_w1 = dst3_w1 |> rename(any_of(dst3_labels))
dst3_w2 = dst3_w2 |> rename(any_of(dst3_labels))
dst3_w3 = dst3_w3 |> rename(any_of(dst3_labels))
dst3_w4 = dst3_w4 |> rename(any_of(dst3_labels))

dst4_w1 = dst4_w1 |> rename(any_of(dst4_labels))
dst4_w2 = dst4_w2 |> rename(any_of(dst4_labels))
dst4_w4 = dst4_w4 |> rename(any_of(dst4_labels))

# Binding back together.
wave_1_no_work = dst1_w1 |> bind_rows(dst2_w1) |> bind_rows(dst3_w1) |> bind_rows(dst4_w1)
wave_2_no_work = dst1_w2 |> bind_rows(dst2_w2) |> bind_rows(dst3_w2) |> bind_rows(dst4_w2)
wave_3_no_work = dst1_w3 |> bind_rows(dst2_w3) |> bind_rows(dst3_w3)
wave_4_no_work = dst1_w4 |> bind_rows(dst2_w4) |> bind_rows(dst3_w4) |> bind_rows(dst4_w4)

rm(dst_w2, dst_w3, dst_w4, dst1_w1, dst1_w2, dst1_w3, dst1_w4, dst2_w1, dst2_w2, 
   dst2_w3, dst2_w4, dst3_w1, dst3_w2, dst3_w3, dst3_w4, dst4_w1, dst4_w2, dst4_w4)

rm(year_two_three_match_dst3, year_two_three_match_dst4, year_three_four_match_dst3, year_three_four_match_dst4)
rm(pnum_dst_w2, pnum_dst_w3, pnum_dst_w4, ssuid_dst_w2, ssuid_dst_w3, ssuid_dst_w4)

# Renaming inter year unemployment
year_one_two_match_dst1 = year_one_two_match_dst1 |> rename(any_of(y_one_two_labels_dst1))
year_one_two_match_dst2 = year_one_two_match_dst2 |> rename(any_of(y_one_two_labels_dst2))
year_one_two_match_dst3 = year_one_two_match_dst3 |> rename(any_of(y_one_two_labels_dst3))
year_one_two_match_dst4 = year_one_two_match_dst4 |> rename(any_of(y_one_two_labels_dst4))

year_two_three_match_dst1 = year_two_three_match_dst1 |> rename(any_of(y_two_three_labels_dst1))
year_two_three_match_dst2 = year_two_three_match_dst2 |> rename(any_of(y_two_three_labels_dst2))

year_three_four_match_dst1 = year_three_four_match_dst1 |> rename(any_of(y_three_four_labels_dst1))
year_three_four_match_dst2 = year_three_four_match_dst2 |> rename(any_of(y_three_four_labels_dst2))

# Binding inter year.
year_one_two_match = year_one_two_match_dst1 |> bind_rows(year_one_two_match_dst2) |>
  bind_rows(year_one_two_match_dst3) |> bind_rows(year_one_two_match_dst4)

year_two_three_match = year_two_three_match_dst1 |> bind_rows(year_two_three_match_dst2)

year_three_four_match = year_three_four_match_dst1 |> bind_rows(year_three_four_match_dst2)

rm(year_one_two_match_dst1, year_one_two_match_dst2, year_one_two_match_dst3, year_one_two_match_dst4,
   year_two_three_match_dst1, year_two_three_match_dst2, year_three_four_match_dst1,
   year_three_four_match_dst2)
rm(matches_no_work_1, matches_no_work_2, matches_no_work_3)
rm(no_work_1, no_work_2, no_work_3, no_work_4)

# Adding end_old_job and begin_new_job to wave_no_work variables.
wave_1_no_work = wave_1_no_work |> mutate(end_old_job = enj_bmonth) |> mutate(begin_new_job = ejb_bmonth)
wave_2_no_work = wave_2_no_work |> mutate(end_old_job = enj_bmonth) |> mutate(begin_new_job = ejb_bmonth)
wave_3_no_work = wave_3_no_work |> mutate(end_old_job = enj_bmonth) |> mutate(begin_new_job = ejb_bmonth)
wave_4_no_work = wave_4_no_work |> mutate(end_old_job = enj_bmonth) |> mutate(begin_new_job = ejb_bmonth)

# Doing some quick filtering to join the inter year unemployment.
ssuid_y_1_2 = year_one_two_match |> distinct(ssuid)
ssuid_y_2_3 = year_two_three_match |> distinct(ssuid)
ssuid_y_3_4 = year_three_four_match |> distinct(ssuid)

pnum_y_1_2 = year_one_two_match |> distinct(pnum)
pnum_y_2_3 = year_two_three_match |> distinct(pnum)
pnum_y_3_4 = year_three_four_match |> distinct(pnum)

longitudinal_month_1_2 = year_one_two_match |> distinct(longitudinal_month)
longitudinal_month_2_3 = year_two_three_match |> distinct(longitudinal_month)
longitudinal_month_3_4 = year_three_four_match |> distinct(longitudinal_month)

wave_2_no_work = wave_2_no_work |> filter( !( (ssuid %in% ssuid_y_1_2$ssuid) & (pnum %in% pnum_y_1_2$pnum) & (longitudinal_month %in% longitudinal_month_1_2$longitudinal_month)) )
wave_2_no_work = wave_2_no_work |> bind_rows(year_one_two_match)

wave_3_no_work = wave_3_no_work |> filter( !( (ssuid %in% ssuid_y_2_3$ssuid) & (pnum %in% pnum_y_2_3$pnum) & (longitudinal_month %in% longitudinal_month_2_3$longitudinal_month)) )
wave_3_no_work = wave_3_no_work |> bind_rows(year_two_three_match)

wave_4_no_work = wave_4_no_work |> filter( !( (ssuid %in% ssuid_y_3_4$ssuid) & (pnum %in% pnum_y_3_4$pnum) & (longitudinal_month %in% longitudinal_month_3_4$longitudinal_month)) )
wave_4_no_work = wave_4_no_work |> bind_rows(year_three_four_match)

# Joining with full sample to get month before and after wages.

job_data_14 = job_data_14_WAVE1 |> bind_rows(job_data_14_WAVE2) |> 
  bind_rows(job_data_14_WAVE3) |> bind_rows(job_data_14_WAVE4)

# Binding all of the no work wave back together.
no_work_all_waves = wave_1_no_work |> bind_rows(wave_2_no_work) |> bind_rows(wave_3_no_work) |>
  bind_rows(wave_4_no_work)

# Adding month before/month after columns.
no_work_all_waves = no_work_all_waves |> mutate(month_before = end_old_job - 1) |>
  mutate(month_after = begin_new_job + 1)

# Joining together on basis of month before.
no_work_all_waves = no_work_all_waves |> inner_join(job_data_14, by = c("ssuid", "pnum", "month_before" = "longitudinal_month"), suffix = c("", "_before")) |>
  inner_join(job_data_14, by = c("ssuid", "pnum", "month_after" = "longitudinal_month"), suffix = c("", "_after"))

# Filtering and renaming, and binding to get before wage into one column.
no_work_all_waves_1 = no_work_all_waves |> filter(!is.na(tjb1_msum))
no_work_all_waves_2 = no_work_all_waves |> filter(!is.na(tjb2_msum))
no_work_all_waves_3 = no_work_all_waves |> filter(!is.na(tjb3_msum))
no_work_all_waves_4 = no_work_all_waves |> filter(!is.na(tjb4_msum))

# Filtering out those who had multiple jobs before. Again, for comparison reasons.
# When I do hours I might bring them back in.
no_work_all_waves_1 = no_work_all_waves_1 |> filter(is.na(tjb2_ind)) |> 
  filter(is.na(tjb3_ind)) |> filter(is.na(tjb4_ind)) |> filter(is.na(tjb5_ind)) |> 
  filter(is.na(tjb6_ind)) |> filter(is.na(tjb7_ind))

no_work_all_waves_2 = no_work_all_waves_2 |> filter(is.na(tjb1_ind)) |> 
  filter(is.na(tjb3_ind)) |> filter(is.na(tjb4_ind)) |> filter(is.na(tjb5_ind)) |> 
  filter(is.na(tjb6_ind)) |> filter(is.na(tjb7_ind))

no_work_all_waves_3 = no_work_all_waves_3 |> filter(is.na(tjb1_ind)) |> 
  filter(is.na(tjb2_ind)) |> filter(is.na(tjb4_ind)) |> filter(is.na(tjb5_ind)) |> 
  filter(is.na(tjb6_ind)) |> filter(is.na(tjb7_ind))



# Column Selection 

no_work_all_waves_1_1 = no_work_all_waves_1 |> filter(!is.na(tjb1_msum_after))
no_work_all_waves_1_2 = no_work_all_waves_1 |> filter(!is.na(tjb2_msum_after))
no_work_all_waves_1_3 = no_work_all_waves_1 |> filter(!is.na(tjb3_msum_after))
no_work_all_waves_1_4 = no_work_all_waves_1 |> filter(!is.na(tjb4_msum_after))

no_work_all_waves_2_1 = no_work_all_waves_2 |> filter(!is.na(tjb1_msum_after))
no_work_all_waves_2_2 = no_work_all_waves_2 |> filter(!is.na(tjb2_msum_after))
no_work_all_waves_2_3 = no_work_all_waves_2 |> filter(!is.na(tjb3_msum_after))
no_work_all_waves_2_4 = no_work_all_waves_2 |> filter(!is.na(tjb4_msum_after))

no_work_all_waves_3_1 = no_work_all_waves_3 |> filter(!is.na(tjb1_msum_after))
no_work_all_waves_3_2 = no_work_all_waves_3 |> filter(!is.na(tjb2_msum_after))
no_work_all_waves_3_3 = no_work_all_waves_3 |> filter(!is.na(tjb3_msum_after))
no_work_all_waves_3_4 = no_work_all_waves_3 |> filter(!is.na(tjb4_msum_after))

# Filtering out unneeded columns.
remove_all = c("tjb_strtyr", "enj_bmonth_before", "enj_emonth_before", 
               "rmnumjobs_before", "ejb_bmonth", "ejb1_bmonth", 
               "ejb2_bmonth", "ejb3_bmonth", "ejb4_bmonth", "ejb5_bmonth", "ejb6_bmonth", 
               "ejb7_bmonth", "ejb_emonth", "ejb1_emonth", "ejb2_emonth", "ejb3_emonth", 
               "ejb4_emonth", "ejb5_emonth", "ejb6_emonth", "ejb7_emonth", "tjb1_strtyr",
               "tjb2_strtyr", "tjb3_strtyr", "tjb4_strtyr", "tjb5_strtyr", "ejb1_rsend", 
               "ejb2_rsend", "ejb3_rsend", "ejb4_rsend", "ejb5_rsend", "ejb6_rsend",
               "ejb7_rsend", "enj_bmonth_after", "enj_emonth_after", "rmnumjobs_after",
               "ejb1_bmonth_after", "ejb2_bmonth_after", 
               "ejb3_bmonth_after", "ejb4_bmonth_after", "ejb5_bmonth_after", 
               "ejb6_bmonth_after", "ejb7_bmonth_after", "ejb1_emonth_after", 
               "ejb2_emonth_after", "ejb3_emonth_after", "ejb4_emonth_after", 
               "ejb5_emonth_after", "ejb6_emonth_after", "ejb7_emonth_after",
               "tjb1_strtyr_after", "tjb2_strtyr_after", "tjb3_strtyr_after", 
               "tjb4_strtyr_after", "tjb5_strtyr_after", "ejb1_rsend_after", 
               "ejb2_rsend_after", "ejb3_rsend_after", "ejb4_rsend_after", 
               "ejb5_rsend_after", "ejb6_rsend_after", "ejb7_rsend_after",
               "ejb4_jobid", "ejb5_jobid", "ejb6_jobid", "ejb7_jobid", "tjb4_ind",
               "tjb5_ind", "tjb6_ind", "tjb7_ind", "tjb4_occ", "tjb5_occ", "tjb6_occ", 
               "tjb7_occ", "tjb4_msum", "tjb5_msum", "tjb6_msum", "tjb7_msum", 
               "ejb5_jobid_after", "ejb6_jobid_after", "ejb7_jobid_after", "tjb5_ind_after", 
               "tjb6_ind_after", "tjb7_ind_after", "tjb5_occ_after", "tjb6_occ_after", 
               "tjb7_occ_after", "tjb5_msum_after", "tjb6_msum_after", "tjb7_msum_after")

no_work_all_waves_1_1 = no_work_all_waves_1_1 |> select(!all_of(remove_all))
no_work_all_waves_1_2 = no_work_all_waves_1_2 |> select(!all_of(remove_all))
no_work_all_waves_1_3 = no_work_all_waves_1_3 |> select(!all_of(remove_all))
no_work_all_waves_1_4 = no_work_all_waves_1_4 |> select(!all_of(remove_all))

no_work_all_waves_2_1 = no_work_all_waves_2_1 |> select(!all_of(remove_all))
no_work_all_waves_2_2 = no_work_all_waves_2_2 |> select(!all_of(remove_all))
no_work_all_waves_2_3 = no_work_all_waves_2_3 |> select(!all_of(remove_all))
no_work_all_waves_2_4 = no_work_all_waves_2_4 |> select(!all_of(remove_all))

no_work_all_waves_3_1 = no_work_all_waves_3_1 |> select(!all_of(remove_all))
no_work_all_waves_3_2 = no_work_all_waves_3_2 |> select(!all_of(remove_all))
no_work_all_waves_3_3 = no_work_all_waves_3_3 |> select(!all_of(remove_all))
no_work_all_waves_3_4 = no_work_all_waves_3_4 |> select(!all_of(remove_all))

# Other columns to get rid of
before_1 = c("ejb2_jobid", "ejb3_jobid", "tjb2_ind", "tjb3_ind", "tjb2_occ", "tjb3_occ", 
             "tjb2_msum", "tjb3_msum")

before_2 = c("ejb1_jobid", "ejb3_jobid", "tjb1_ind", "tjb3_ind", "tjb1_occ", "tjb3_occ", 
             "tjb1_msum", "tjb3_msum")

before_3 = c("ejb1_jobid", "ejb2_jobid", "tjb1_ind", "tjb2_ind", "tjb1_occ", "tjb2_occ", 
             "tjb1_msum", "tjb2_msum")

no_work_all_waves_1_1 = no_work_all_waves_1_1 |> select(!all_of(before_1))
no_work_all_waves_1_2 = no_work_all_waves_1_2 |> select(!all_of(before_1))
no_work_all_waves_1_3 = no_work_all_waves_1_3 |> select(!all_of(before_1))
no_work_all_waves_1_4 = no_work_all_waves_1_4 |> select(!all_of(before_1))

no_work_all_waves_2_1 = no_work_all_waves_2_1 |> select(!all_of(before_2))
no_work_all_waves_2_2 = no_work_all_waves_2_2 |> select(!all_of(before_2))
no_work_all_waves_2_3 = no_work_all_waves_2_3 |> select(!all_of(before_2))
no_work_all_waves_2_4 = no_work_all_waves_2_4 |> select(!all_of(before_2))

no_work_all_waves_3_1 = no_work_all_waves_3_1 |> select(!all_of(before_3))
no_work_all_waves_3_4 = no_work_all_waves_3_4 |> select(!all_of(before_3))

# Filtering out odd two month after jobs
no_work_all_waves_1_1 = no_work_all_waves_1_1 |> filter(is.na(ejb2_jobid_after))
no_work_all_waves_1_2 = no_work_all_waves_1_2 |> filter(is.na(ejb1_jobid_after))

after_1 = c("ejb2_jobid_after", "ejb3_jobid_after", "ejb4_jobid_after", "tjb2_ind_after",
            "tjb3_ind_after", "tjb4_ind_after", "tjb2_occ_after", "tjb3_occ_after", 
            "tjb4_occ_after", "tjb2_msum_after", "tjb3_msum_after", "tjb4_msum_after", 
            "enj_nowrk8_before")

after_2 = c("ejb1_jobid_after", "ejb3_jobid_after", "ejb4_jobid_after", "tjb1_ind_after",
            "tjb3_ind_after", "tjb4_ind_after", "tjb1_occ_after", "tjb3_occ_after", 
            "tjb4_occ_after", "tjb1_msum_after", "tjb3_msum_after", "tjb4_msum_after")

after_3 = c("ejb1_jobid_after", "ejb2_jobid_after", "ejb4_jobid_after", "tjb1_ind_after",
           "tjb2_ind_after", "tjb4_ind_after", "tjb1_occ_after", "tjb2_occ_after", 
           "tjb4_occ_after", "tjb1_msum_after", "tjb2_msum_after", "tjb4_msum_after")

after_4 = c("ejb1_jobid_after", "ejb2_jobid_after", "ejb3_jobid_after", "tjb1_ind_after",
            "tjb2_ind_after", "tjb3_ind_after", "tjb1_occ_after", "tjb2_occ_after", 
            "tjb3_occ_after", "tjb1_msum_after", "tjb2_msum_after", "tjb3_msum_after")

no_work_all_waves_1_1 = no_work_all_waves_1_1 |> select(!all_of(after_1))

no_work_all_waves_2_1 = no_work_all_waves_2_1 |> select(!all_of(after_1))

no_work_all_waves_3_1 = no_work_all_waves_3_1 |> select(!all_of(after_1))

no_work_all_waves_1_2 = no_work_all_waves_1_2 |> select(!all_of(after_2))

no_work_all_waves_2_2 = no_work_all_waves_2_2 |> select(!all_of(after_2))

no_work_all_waves_1_3 = no_work_all_waves_1_3 |> select(!all_of(after_3))

no_work_all_waves_2_3 = no_work_all_waves_2_3 |> select(!all_of(after_3))

no_work_all_waves_1_4 = no_work_all_waves_1_4 |> select(!all_of(after_4))

no_work_all_waves_2_4 = no_work_all_waves_2_4 |> select(!all_of(after_4))

no_work_all_waves_3_4 = no_work_all_waves_3_4 |> select(!all_of(after_4))

# Final rename.
before_label_1 = c("ejb_jobid_before" = "ejb1_jobid", "tjb_ind_before" = "tjb1_ind", 
                   "tjb_occ_before" = "tjb1_occ", "tjb_msum_before" = "tjb1_msum")

before_label_2 = c("ejb_jobid_before" = "ejb2_jobid", "tjb_ind_before" = "tjb2_ind", 
                   "tjb_occ_before" = "tjb2_occ", "tjb_msum_before" = "tjb2_msum")

before_label_3 = c("ejb_jobid_before" = "ejb3_jobid", "tjb_ind_before" = "tjb3_ind", 
                   "tjb_occ_before" = "tjb3_occ", "tjb_msum_before" = "tjb3_msum")

no_work_all_waves_1_1 = no_work_all_waves_1_1 |> rename(any_of(before_label_1))
no_work_all_waves_1_2 = no_work_all_waves_1_2 |> rename(any_of(before_label_1))
no_work_all_waves_1_3 = no_work_all_waves_1_3 |> rename(any_of(before_label_1))
no_work_all_waves_1_4 = no_work_all_waves_1_4 |> rename(any_of(before_label_1))

no_work_all_waves_2_1 = no_work_all_waves_2_1 |> rename(any_of(before_label_2))
no_work_all_waves_2_2 = no_work_all_waves_2_2 |> rename(any_of(before_label_2))
no_work_all_waves_2_3 = no_work_all_waves_2_3 |> rename(any_of(before_label_2))
no_work_all_waves_2_4 = no_work_all_waves_2_4 |> rename(any_of(before_label_2))

no_work_all_waves_3_1 = no_work_all_waves_3_1 |> rename(any_of(before_label_3))
no_work_all_waves_3_4 = no_work_all_waves_3_4 |> rename(any_of(before_label_3))

after_label_1 = c("ejb_jobid_after" = "ejb1_jobid_after", "tjb_ind_after" = "tjb1_ind_after", 
                  "tjb_occ_after" = "tjb1_occ_after", "tjb_msum_after" = "tjb1_msum_after")

after_label_2 = c("ejb_jobid_after" = "ejb2_jobid_after", "tjb_ind_after" = "tjb2_ind_after", 
                  "tjb_occ_after" = "tjb2_occ_after", "tjb_msum_after" = "tjb2_msum_after")

after_label_3 = c("ejb_jobid_after" = "ejb3_jobid_after", "tjb_ind_after" = "tjb3_ind_after", 
                  "tjb_occ_after" = "tjb3_occ_after", "tjb_msum_after" = "tjb3_msum_after")

after_label_4 = c("ejb_jobid_after" = "ejb4_jobid_after", "tjb_ind_after" = "tjb4_ind_after", 
                  "tjb_occ_after" = "tjb4_occ_after", "tjb_msum_after" = "tjb4_msum_after")

no_work_all_waves_1_1 = no_work_all_waves_1_1 |> rename(any_of(after_label_1))
no_work_all_waves_2_1 = no_work_all_waves_2_1 |> rename(any_of(after_label_1))
no_work_all_waves_3_1 = no_work_all_waves_3_1 |> rename(any_of(after_label_1))

no_work_all_waves_1_2 = no_work_all_waves_1_2 |> rename(any_of(after_label_2))
no_work_all_waves_2_2 = no_work_all_waves_2_2 |> rename(any_of(after_label_2))

no_work_all_waves_1_3 = no_work_all_waves_1_3 |> rename(any_of(after_label_3))
no_work_all_waves_2_3 = no_work_all_waves_2_3 |> rename(any_of(after_label_3))

no_work_all_waves_1_4 = no_work_all_waves_1_4 |> rename(any_of(after_label_4))
no_work_all_waves_2_4 = no_work_all_waves_2_4 |> rename(any_of(after_label_4))
no_work_all_waves_3_4 = no_work_all_waves_3_4 |> rename(any_of(after_label_4))

final_table = no_work_all_waves_1_1 |> bind_rows(no_work_all_waves_1_2) |> 
  bind_rows(no_work_all_waves_1_3) |> bind_rows(no_work_all_waves_1_4) |>
  bind_rows(no_work_all_waves_2_1) |> bind_rows(no_work_all_waves_2_2) |>
  bind_rows(no_work_all_waves_2_3) |> bind_rows(no_work_all_waves_2_4) |>
  bind_rows(no_work_all_waves_3_1) |> bind_rows(no_work_all_waves_3_4)
