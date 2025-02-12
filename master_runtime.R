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

# Source and run the wrangle.R script
source("sipp_processing.R")
source("wrangle.R")
source("savings.R")
source("summary_statistics.R")
source("plotting.R")