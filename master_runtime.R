# master_runtime.R
# -----------------
# This script loads required packages, prompts the user for the folder path using a file explorer,
# and then runs the SIPP processing.

# Load required packages
library(tcltk)
library(tidyverse)
library(data.table)
# Prompt the user for a folder path using a file explorer
#folder_path <- tcltk::tk_choose.dir(default = getwd(), caption = "Select Folder Containing SIPP CSV Files")
#if (is.na(folder_path) || folder_path == "") {
#  stop("No folder selected. Exiting.")
#}
folder_path <- "C:/Users/dhoward/Documents/0196 Econ Thesis/SIPP"
message("Selected folder: ", folder_path)

# Monthly folder
setwd(rstudioapi::getActiveProject())
setwd("./Scripts/Monthly")
source("11 monthly_processing.R")
source("12 wrangle.R")

rm(list = setdiff(ls(),c("unemployment", "folder_path", "monthly_SIPP_wrangled")))

# Annual folder
setwd(rstudioapi::getActiveProject())
setwd("./Scripts/Annual")
source("01 annual_processing.R")
source("02 wrangle.R")
source("03 savings.R")
source("04 weight.R")
source("05 plotting.R")
source("06 statistics.R")
source("07 analysis.R")

# Very intensive process, run optionally to view monthly unemployment data
# source("13 weight.R")
# source("14 plotting.R")

rm(list = setdiff(ls(),"unemployment", "folder_path", "monthly_sipp_wrangled", "unemp.svy", "unemp"))