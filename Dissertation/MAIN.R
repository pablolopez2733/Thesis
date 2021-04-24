###############################################################################
############################## MAIN SCRIPT ####################################
###############################################################################

#================================ LIBRARIES ====================================
library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)   # devtools::install_github("abresler/nbastatR")
library(future)
library(glmnet)
library(caret)
library(data.table)
library(ggdark)
library(gt)
# ==============================================================================




# Download data (only run first time)
#source("src/00_DataDownload.R")

# Load Data 
source("src/01_ReadData.R")

# Define functions for computing lineup transition matrices
source("src/02_TransitionMatrices.R")

# Define functions for running regressions
source("src/03_Regressions")
