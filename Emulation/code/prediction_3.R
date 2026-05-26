
# Load libraries ----------------------------------------------------------

library(ggplot2)
library(tidyr)
library(lhs)

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
scen_path <- paste0(data_path, "scenarios/")
input_path <- config$scen_levels_path
input_path_rescaled <- paste0(substr(input_path, 1, nchar(input_path)-4), "_rescaled.csv")


# Data paths
emul_path <- "Emulation/data/emulators/single/"

## Output paths
output_dir <- paste0(data_path, "/predictions/")

# Scenario file names
output_scenarios_gbl <- "input_dfs_GBL_lhs.RData" 
out_scen_pw_fixed <- "input_dfs_GBL_pw_fixed.RData"
out_scen_rb_fixed <- "input_dfs_GBL_rb_fixed.RData"


# Prediction file names
out_gbl_preds <- "preds_GBL_lhs.csv"
out_pw_fix_preds <- "preds_GBL_pw_fixed.csv"
out_rb_fix_preds <- "preds_GBL_rb_fixed.csv"

# Load input data and basic functions ---------------------------------------------------------
input_df_rescaled <- read.csv(input_path_rescaled)

params <- names(input_df_rescaled[2:10])
power_params <- params[c(5,6,7,8,9)]
rebound_params <- params[c(1,2,3,4)]
fixed_params <- rebound_params  # Switch as desired with below
lhc_params <- power_params


# Emulator Selections  ------------------------------------------------

# option for using emulator mean or sampling from uncertainty, mean by default
emulator_uncertainty <- "NO" # "YES"

### Global
# List of emulator selections to run
emulators_all <- tools::file_path_sans_ext(list.files(paste0(data_path, "emulators/single/")))
emulator_selections <- emulators_all


# Scenario design - GLOBAL --------------------------------------------------

# Define grid values for scenarios
grid_vals <- rbind(
  # set power scens - limited range
  # c(0.3, 0.3, 0.3, 0.5, 0.3),
  # c(0.7, 0.7, 0.7, 0.5, 0.7)
  # set power scens FULL
  # c(0, 0, 0, 0.5, 0),
  # c(0.5, 0.5, 0.5, 0.5, 0.5),
  # c(1, 1, 1, 0.5, 1)
  # set rebound scens FULL
  c(0, 0, 0, 0),
  c(0.5, 0.5, 0.5, 0.5),
  c(1, 1, 1, 1)
  # set rebound scens - limited range
  # c(0.3, 0.3, 0.3, 0.3),
  # c(0.7, 0.7, 0.7, 0.7)
  
)


# Convert to dataframe and assign column names
fixed_df  <- as.data.frame(grid_vals)
names(fixed_df) <- fixed_params

n_iterations <- 20000

# Scenario creation - GLOBAL ------------------------------------------------

# Initialize list to store each row's data frame of samples
input_dfs <- list()

for (i in 1:nrow(fixed_df)) {
  
  ## variables that are fixed
  fixed_vars <- fixed_df[i, ]

  ## variables that vary
  lhs_use <- lhc_params
  
  ## Prepare empty list
  lhs_sample <- NULL
  normal_sample <- NULL
  
  # LHS sampling for specific columns
  if (length(lhs_use) > 0) {
    var_min_lhs <- rep(0, length(lhs_use))
    var_max_lhs <- rep(1, length(lhs_use))
    
    lhs_matrix <- randomLHS(n_iterations, length(lhs_use))
    
    lhs_sample <- as.data.frame(lhs_matrix)
    colnames(lhs_sample) <- lhs_use
  }
  

  # Combine with fixed parameters
  input_df_sample <- cbind(
    as.data.frame(matrix(rep(unlist(fixed_vars), n_iterations), nrow = n_iterations, byrow = TRUE)),
    lhs_sample
  )
  
  colnames(input_df_sample)[1:length(fixed_vars)] <- names(fixed_df)
  
  input_dfs[[paste0("id_", i)]] <- input_df_sample
}


# Save input scenarios
saveRDS(input_dfs, paste0(output_dir, out_scen_rb_fixed))

## Input Sample distribution plot
# Pick one of the generated data frames
df_check <- input_dfs[["id_1"]]

# Melt into long format for faceting
df_long <- df_check %>%
  pivot_longer(cols = all_of(params), names_to = "variable", values_to = "value")

# Plot histograms for each variable
p <- ggplot(df_long, aes(x = value)) +
  geom_histogram(binwidth = 0.02, boundary = 0, fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Sampled Variables", x = "Value", y = "Count")
p
# Save to figures folder
ggsave(paste0(data_path, "/figures/scen_input_dist_GBL.png"), p, 
       width = 8, height = 6, dpi = 100)


# Emulator predictions - GLOBAL ---------------------------------------------

# Reload fileinput_dfs <- readRDS(paste0(output_dir, output_scenarios_gbl))
#input_dfs <- readRDS("C:/Users/ib400/Github/FTT_StandAlone/Emulation/data/predictions/input_dfs_GBL_emiss.RData")

# combine into df
all_inputs_df <- do.call(rbind, input_dfs)

# Optionally reset row names
rownames(all_inputs_df) <- NULL

# Convert to df (only type allowed by emulators)
all_inputs_mat <- data.frame(all_inputs_df)

# List to store all results
all_results <- list()

# Loop over each emulator
for (selection in emulator_selections) {
  
  # Load emulator
  emulator_path <- paste0(emul_path, selection, ".rds")
  emulator <- readRDS(emulator_path)
  
  
  # Loop over each input set
  for (id_name in names(input_dfs)) {
    
    input_df <- input_dfs[[id_name]]
    
    # Predict
    pred <- PredictGasp(input_df, emulator)
    pred_mean <- pred$mean
    
    # option to sample from emulator uncertainty
    samp <- rnorm(length(pred$mean), mean = pred$mean, sd = pred$sd)
    
    if (emulator_uncertainty == "YES") {
      result_df <- cbind(input_df, prediction = samp)
    
      } else {
        # Combine with inputs
        result_df <- cbind(input_df, prediction = pred_mean)
    }
    
    
    # Add metadata
    result_df$emulator   <- selection
    result_df$id         <- id_name
    result_df$sample_id  <- seq_len(nrow(input_df))
    
    # Store result
    all_results[[paste0(selection, "_", id_name)]] <- result_df
  }
}

# Combine all into a single data.frame
final_results_df <- do.call(rbind, all_results)

# Optional: reset rownames
rownames(final_results_df) <- NULL

# Save
write.csv(final_results_df, 
          file = paste0(output_dir, out_rb_fix_preds),
          row.names = F)



