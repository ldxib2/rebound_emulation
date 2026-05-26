
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
sim_data <- paste0(data_path, "simulation_data/")
input_path <- paste0(data_path, "lhc_scenarios.csv")
#output_path <- paste0(data_path, "emulator_inputs.csv")
output_path <- paste0(sim_data, "raw_data_wide.csv")


input_path_rescaled <- paste0(substr(input_path, 1, nchar(input_path)-4), "_rescaled.csv")

# Data paths
emul_path <- paste0(data_path, "emulators/")
valid_plot_path <- paste0(data_path, "validation/plots/")
valid_model_path <- paste0(data_path, "validation/models/")

# create dirs if doesn't exist
paths <- c(emul_path, valid_plot_path, valid_model_path)
invisible(lapply(paths, function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}))


# Set up vars for emulators -----------------------------------------------

# regions
regions <- c('GBL') # CN, US, RGN, RGS
years <- c(2050)


# Load input & output matrix -------------------------------------------------------

## load output
full_output <- read.csv(output_path)

# convert to long format 
# Could have a RB variable to avoid confusing names?
full_output <- full_output |>
  pivot_longer(
    cols = names(full_output[2:length(names(full_output))]),
    names_to  = "variable",            
    values_to = "value"            
  ) %>% 
  arrange(variable, scenario) # reorder

# output vars of interest
output_vars <- unique(full_output$variable) 


# Emulator design ----------------------------------------------

# Reload if needed
input_df_rescaled <- read.csv(input_path_rescaled)

# Set seed for function
seed_it = 5000

############## Function for building and saving emulators
build_and_save_emulator <- function(output_data, key, seed = seed_it) {
  print(paste('Building emulator: ', key))
  # Merge with inputs for training
  train_data <- merge(input_df_rescaled, output_data[, c("scenario", "value")], by = "scenario", all = FALSE)
  
  # Add essential terms
  em_data <- data.frame(train_data[, 2:(ncol(train_data) - 1)],
                        Noise = runif(nrow(train_data), -1, 1),
                        value = train_data$value)
  
  # Build emulator with seed for training set
  set.seed(seed_it)
  em <- BuildGasp('value', em_data, mean_fn = 'step')
  
  # Save emulator
  saveRDS(em, file = paste0(emul_path, "em_", key, ".rds"))
  
  # Generate validation plots
  png(paste0(valid_plot_path, key, ".png"), width = 1200, height = 900, res = 150)
  par(mfrow = c(5, 4), mar = c(4, 2, 2, 2))
  ValidateGasp(em, IndivPars = TRUE)
  dev.off()
  
  # Save regression analysis
  html_output <- paste0(valid_model_path, key, "_lm_summary.html")
  stargazer(em$lm$linModel, type = "html", out = html_output)
  print(paste('Emulator: ', key, ', validation plot and model saved'))
  return(em)  # Return the emulator object in case further analysis is needed
}



# Execute -----------------------------------------------------------------

# Loop through vars
for (var in unique(output_vars)) {

        output_data <- subset(full_output, 
                                variable == var)
        build_and_save_emulator(output_data, var)
      }
  





