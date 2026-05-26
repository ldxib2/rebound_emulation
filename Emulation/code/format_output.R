## Script for formatting E3ME output

# Libraries
library(dplyr)
library(tidyr)

# Load input & output matrix -------------------------------------------------------
data_path <- "Emulation/data/"
output_path <- paste0(data_path, "simulation_data/FR0_PJ_2050_wide.csv")
formatted_path <- paste0(data_path, "simulation_data/energy_demand_long.csv")
# edit variable name
output_var <- "EngDem"

## load output
full_output <- read.csv(output_path)

# If output is in raw form and not using long format use this
names(full_output) <- c("scenario", "value")
scenarios <- full_output[[1]]

# Extract the number and type from the scenario name
full_output$scenario_num <- as.numeric(gsub("^(RB_|No_RB_)", "", full_output$scenario))

full_output$variable <- ifelse(
  grepl("^RB_", full_output$scenario),
  paste0("RB_", output_var),
  paste0("No_RB_", output_var)
)


# Drop the original scenario column and rename
full_output$scenario <- full_output$scenario_num
full_output$scenario_num <- NULL

# Reorder
full_output <- full_output[order(full_output$scenario), c("scenario", "variable", "value")]

# save formated data
write.csv(full_output, formatted_path, row.names = FALSE)



