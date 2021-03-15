###############################################################################
############################## MAIN SCRIPT ####################################
###############################################################################

#================================ LIBRARIES ====================================
library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)   # devtools::install_github("abresler/nbastatR")
library(future)
library(dplyr)
library(readr)
library(glmnet)
library(caret)
library(ggplot2)
# ==============================================================================




# Download data (only run first time)
#source("src/00_DataDownload.R")

# Load Data 
source("src/01_ReadData.R")

# Define functions for computing lineup transition matrices
source("src/02_Transition_Matrices.R")

# Run the regressions
source("src/03_Regressions")