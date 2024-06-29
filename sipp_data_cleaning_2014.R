# Clearing the environment
rm(list =ls())

# Setting Working Directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Packages
library(data.table)
library(readr)
library(tidyverse)

# Loading data
EJB_RSEND_ALL = c()
EJB_JOBID_ALL = c()
TJB_IND_ALL = c()
TJB_OCC_ALL = c()
TJB_MSUM_ALL = c()

# Creating all variable names that have 7 versions.
for (i in 1:7) {
  
  EJB = paste("EJB", sep = "", i)
  EJB_RSEND = paste(EJB, sep = "_", "RSEND")
  EJB_JOBID = paste(EJB, sep = "_", "JOBID")
  TJB = paste("TJB", sep = "", i)
  tjb = paste("tjb", sep = "", i)
  TJB_IND = paste(tjb, sep = "_", "ind")
  TJB_OCC = paste(tjb, sep = "_", "occ")
  TJB_MSUM = paste(TJB, sep = "_", "MSUM")
  
  EJB_RSEND_ALL[i] = EJB_RSEND
  EJB_JOBID_ALL[i] = EJB_JOBID
  TJB_IND_ALL[i] = TJB_IND
  TJB_OCC_ALL[i] = TJB_OCC
  TJB_MSUM_ALL[i] = TJB_MSUM
}

# Other Variables.
column_names = c("ENJ_BMONTH", "ENJ_EMONTH", "ENJ_NOWRK8", "RMNUMJOBS", "rmser", "ENJ_LAYOFF", "TUC1AMT", "TUC2AMT", "TPTOTINC", "EEDUC", "TAGE", "ESEX", "ERACE", "EORIGIN", "ECITIZEN", "monthcode", "ssuid", "PNUM")

# Appending all variable names together.
column_names = append(column_names, TJB_MSUM_ALL, 6) |> append(TJB_OCC_ALL, 6) |> append(TJB_IND_ALL, 6) |> append(EJB_JOBID_ALL, 6) |> append(EJB_RSEND_ALL, 6)

# File address for the data.
address = "/Users/zs0ls/Documents/Grad School/Econ 290/SIPP Data Analysis/2014_wave/pu2014w1/PU2014W1.csv"

# Loading the data.
job_data_14_WAVE1 = fread(address, sep = "|", select = column_names)

testing = fread(address, sep = "|", select = c("RMSER"))
