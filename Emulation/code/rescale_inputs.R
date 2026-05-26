## Script for normalising inputs
## Run once then save input_df_rescaled in main code
## TODO refine set up code
# Load libraries ----

library(R.matlab)
library(ggplot2)
library(reshape2)
library(viridis)
library(cowplot)
library(fields)
library(dplyr)
library(scales)
library(reshape2)
library(tidyverse)
library(gtools)
library(Matrix)
library(patchwork)
library(tidyr)
library(lhs)
library(stargazer)
library(jsonlite)
library(this.path)


# Set up working directory and source imported code ---------------------------------------------------------

script_dir <- this.path::this.dir()
wd <- dirname(dirname(script_dir))
# set the working directory as FTT folder
setwd(wd)

## Source functions taken from other repositories
## Functons and scripts copied from ExeterUQ https://github.com/BayesExeter/ExeterUQ &
## https://github.com/JSalter90/UQ
# source code paths
uq_path <- "Emulation/code/imported_code/UQ/Gasp.R"
source(uq_path) ### Created by James Salter
# reset working dir
setwd(wd)

# Set up config file & paths ---------------------------------------------------

# import config
config_path <- "Emulation/code/config/config.json"
config <- jsonlite::fromJSON(config_path)

# main paths TODO: are any unnecessary
data_path <- "Emulation/data/"
input_path <- paste0(data_path, "lhc_scenarios.csv")
#output_path <- paste0(data_path, "emulator_inputs.csv")
output_path <- paste0(data_path, "cumulative_emissions_clean.csv")


input_path_rescaled <- paste0(substr(input_path, 1, nchar(input_path)-4), "_rescaled.csv")

## load input
input_df <- read.csv(input_path)
#input_df_rescaled <- read.csv(input_path_rescaled)


# Normalise inputs (only first time, after use input_df_rescaled) MOVE TO SEPERATE SCRIPT?--------------------------------------------------------

# get input names and remove scenario numbers
col_names <- names(input_df[2:length(names(input_df))])
# categorical vars which don't need normalising
vars_ignore <- c("scenario", "reb_switch")
col_names <- col_names[!col_names %in% vars_ignore]

# load input parameter ranges and format
ranges <- lapply(config$ranges, function(x) list(min = x[1], max = x[2]))

# Copy input_df for rescaling
input_df_rescaled <- input_df

# Function for rescaling
rescale_column <- function(column, range) {
  (column - range$min) / (range$max - range$min)
}

## TODO CHECK THAT COLS_TECH IS NOT A TYPO IN ORIGINAL CODE
# Rescale specified columns and overwrite the original columns
for (col in col_names) {
  input_df_rescaled[[col]] <- rescale_column(input_df_rescaled[[col]], ranges[[col]])
}


### Invert certain parameters for readability
# Function to invert the values in a column
invert_values <- function(values) {
  return(1 - values)
}

input_df_rescaled$cr_solar <- invert_values(input_df_rescaled$cr_solar)
input_df_rescaled$cr_wind <- invert_values(input_df_rescaled$cr_wind)


# this check needs editing to consider scenario id
# check for values out of range
numeric_cols <- sapply(input_df_rescaled, is.numeric)
# Create a logical vector identifying rows with any numeric value outside 0-1
rows_outside <- apply(input_df_rescaled[, numeric_cols], 1, function(row) any(row < 0 | row > 1, na.rm = TRUE))
input_df_rescaled[rows_outside, ] ## should be 0
# Error message if ranges out of bounds
if(nrow(input_df_rescaled[rows_outside, ]) != 0) {
  stop("Error: Rows outside bounds detected!")
}

# save checked table
write.csv(input_df_rescaled, input_path_rescaled, row.names = FALSE)


