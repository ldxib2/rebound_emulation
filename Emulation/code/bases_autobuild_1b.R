####### Build Bases - separate and summed

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

# Set up paths ---------------------------------------------------

# main paths TODO: are any unnecessary
data_path <- "Emulation/data/"
input_path <- paste0(data_path, "lhc_scenarios.csv")

input_path_rescaled <- paste0(substr(input_path, 1, nchar(input_path)-4), "_rescaled.csv")

# Data paths
emul_path <- paste0(data_path, "emulators/")
valid_plot_path <- paste0(data_path, "validation/plots/")
valid_model_path <- paste0(data_path, "validation/models/")
design_path <- paste0(data_path, "designs/")
bases_path <- paste0(data_path, "bases/")

# create dirs if doesn't exist
paths <- c(emul_path, valid_plot_path, valid_model_path, bases_path)
invisible(lapply(paths, function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}))


# Set up vars for emulators -----------------------------------------------

# output vars of interest
# if subsetting comment out and make new object preserving key
output_vars <- c('energy_demand_no', 'energy_demand_rb') #, 

# Load input & output matrix -------------------------------------------------------

## load output
full_output <- read.csv(output_path, header = T)
colnames(full_output)[1] <- 'scenario'

colnames(full_output)[2:length(colnames(full_output))] <- c(2010:2050)

full_output <- full_output %>%
  mutate(
    rebound = if_else(grepl("^RB_", scenario), 1L, 0L),
    scenario = as.integer(sub("^(RB_|No_RB_)", "", scenario))
  ) %>%   relocate(rebound, .after = scenario) 

full_output <- full_output[order(full_output$rebound, full_output$scenario), ]
rownames(full_output) <- NULL


## load input
#input_df <- read.csv(input_path)
input_df_rescaled <- read.csv(input_path_rescaled)
input_df_rescaled <- subset(input_df_rescaled[c(1:100),], select = -c(reb_switch))

# Create bases  -----------------------------------------------------------

# Reload the dictionary to store DataBasis_ftt and q for each combination
DataBasis_path <- paste0(data_path, "designs/DataBasis_dict.rds")

DataBasis_dict <- list()

# Loop through each combination of vars, techs, and regions
for (var in output_vars) {

      # Create a unique key based on the current combination
      key <- var

      # Filter output_df 
      if (endsWith(var, "_no")) {
        output_df <- full_output[full_output$rebound == 0,]
      } else {
        output_df <- full_output[full_output$rebound == 1,]
      }

      # Convert to wide format 
      rownames(output_df) <- output_df$scenario
      # Remove 'scenario' column and store it
      #scenarios <- output_df$scenario
      # Transpose the year columns
      wide_data <- as.data.frame(t(output_df[ , -which(names(output_df) %in% c("scenario", "rebound"))]))
      # Set column names to scenario values
      #colnames(wide_data) <- scenarios
      # Add year as a column
      wide_data$year <- rownames(wide_data)
      # Optional: move 'year' to first column
      wide_data <- wide_data[ , c("year", setdiff(names(wide_data), "year"))]

      
      # Reorder columns of wide_data to match input_df_rescaled scenarios
      wide_data <- wide_data[, match(input_df_rescaled$scenario, colnames(wide_data))]

      # Create the basis object
      DataBasis_ftt <- MakeDataBasis(as.matrix(wide_data))

      # Calculate the number of vectors to explain 99.99% of the variability
      q <- ExplainT(DataBasis_ftt, vtot = 0.9999)

      # Store both DataBasis_ftt and q in the dictionary
      DataBasis_dict[[key]] <- list(DataBasis_ftt = DataBasis_ftt, q = q)
    }
  

# Save dictionary
saveRDS(DataBasis_dict, file = paste0(bases_path, "DataBasis_dict.rds"))



# Build emulators ---------------------------------------------------------
DataBasis_dict <- readRDS(paste0(design_path, "DataBasis_dict.rds"))


# # Define whether to split data for validation
val <- "Y"  # Set to "Y" for validation split, "N" for full dataset training

# Initialize or reload the dictionary to store validation data
# ValidationData_path <- paste0(file_path, "/data/designs/DataBasis_dict.rds")
# ValidationData_dict <- readRDS(ValidationData_path)

ValidationData_dict <- list()

# TODO Create if statement for summed vars etc
# Iterate over each key in DataBasis_dict
for (key in names(DataBasis_dict)) {

  # Retrieve DataBasis_ftt and q from DataBasis_dict
  DataBasis_ftt <- DataBasis_dict[[key]]$DataBasis_ftt
  q <- DataBasis_dict[[key]]$q

  # Data for coefficients, predicted by inputs, with Coeffs determining the projection
  Coeffs <- Project(data = DataBasis_ftt$CentredField,
                    basis = DataBasis_ftt$tBasis[,1:q])
  colnames(Coeffs)[1:q] <- paste("C", 1:q, sep = "")

  # Combine coefficients with design and add noise vector
  input_coeffs <- data.frame(input_df_rescaled[,-1],  # Use the scaled version from before
                             Noise = runif(nrow(input_df_rescaled), -1, 1),
                             Coeffs)

  ## Create train/test data if validation is needed
  if (val == "Y") {
    set.seed(321)
    inds <- sample(1:nrow(input_df_rescaled), nrow(input_df_rescaled))

    # Calculate 90% for training and 10% for validation
    train_size <- round(0.9 * nrow(input_df_rescaled))
    train_inds <- inds[1:train_size]
    val_inds <- inds[(train_size + 1):nrow(input_df_rescaled)]

    train_data <- input_coeffs[train_inds,]
    val_data <- input_coeffs[val_inds,]

    # Store the validation data in the ValidationData_dict with the same key
    ValidationData_dict[[key]] <- val_data
  } else {
    # Use the entire dataset for training if validation is not required
    train_data <- input_coeffs
  }

  # Build emulator on training data
  emulator <- BasisEmulators(train_data, q, mean_fn = 'step', maxdf = 5, training_prop = 1)

  # Save the emulator using the current var, tech_short, and region
  saveRDS(emulator, file = paste0(emul_path, "em_", key, "_base.rds"))

  # Save initial validation plots
  if (val == "Y") {
    for (i in seq(1, length(emulator), by = 1)) {

      png(paste0(valid_plot_path,"base_", key,"_", i, ".png"), width = 1200, height = 900, res = 150)
      par(mfrow = c(1,1), mar = c(4,2,2,2))
      ValidateGasp(emulator[[i]], val_data)
      dev.off()  # Close the graphics device

    }
  }
  }

# # Save validation data
saveRDS(ValidationData_dict, file = paste0(data_path, "validation/ValidationData_dict.rds"))
input_dfs <- readRDS(paste0(data_path, "predictions/input_dfs_GBL_pw_fixed.RData"))



