
# Load libraries ----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(emtools)


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
output_path <- paste0(data_path, "emulator_inputs.csv")


input_path_rescaled <- paste0(substr(input_path, 1, nchar(input_path)-4), "_rescaled.csv")

# Data paths
emul_path <- "Emulation/data/emulators/"
valid_plot_path <- "Emulation/data/validation/plots/"
valid_model_path <- "Emulation/data/validation/models/"
figures_path <- "Emulation/data/figures/"
output_dir <- "Emulation/data/predictions/"

# create dirs if doesn't exist
paths <- c(emul_path, valid_plot_path, valid_model_path, figures_path, output_dir)
invisible(lapply(paths, function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}))




# Global
gbl_sa_filename <- "sa_gbl.csv"

gbl_plot_title <- "sa_gbl.png"
gbl_plot_path <- paste0(figures_path, gbl_plot_title)



# Load input data ---------------------------------------------------------

input_df_rescaled <- read.csv(input_path_rescaled)
inputs <- input_df_rescaled[,2:length(names(input_df_rescaled))]

# Indentify inputs to constrain e.g. cannibalisation rates
inputs_red <- c()
# set the bounds somewhere between 0-1
inputs_red_bounds <- list(min = 0.45, max = 0.55)


# Set up functions --------------------------------------------------------

# Function for plotting
rotate <- function(x) t(apply(x, 2, rev))

# A clean OAT‐sens function
oat_sens_pretrained <- function(ref, emulator, n = 30, lower = 0, upper = 1) {
  # ref: named numeric vector length d, e.g. rep(0.5, d)
  # emulator: what you pass to PredictGasp()
  # n: grid points per parameter
  d <- length(ref)
  varnames <- names(ref)
  sens <- numeric(d)
  names(sens) <- varnames
  
  # for each parameter i
  for(i in seq_len(d)) {
    # make an n×d matrix all = ref
    X_oat <- matrix(ref, nrow = n, ncol = d, byrow = TRUE)
    
    # choose bounds for this variable
    if (varnames[i] %in% inputs_red) {
      low_i  <- inputs_red_bounds$min
      high_i <- inputs_red_bounds$max
    } else {
      low_i  <- lower
      high_i <- upper
    }
  
    # replace column i by its own grid
    X_oat[, i] <- seq(low_i, high_i, length.out = n)
    colnames(X_oat) <- varnames
    
    # predict: PredictGasp often returns a vector or a list with $mean
    pred <- PredictGasp(as.data.frame(X_oat), emulator)
    m <- pred$mean
    
    # compute variance
    sens[i] <- var(m)
  }
  sens
}

# oat SA plot function
oaatSensvarSummaryPlot <- function(oat_sens_mat,
                                   threshold = 0.01) {
  
  # normalize & average
  normsens <- normalize(t(oat_sens_mat))  # rows: params, cols: outputs
  normsens_mean <- rowMeans(normsens)
  

  # 1) now extract names from the filtered matrix
  ynames <- rownames(oat_sens_mat)    # outputs
  xnames <- colnames(oat_sens_mat)    # parameters
  
  # 2) normalize & average
  normsens      <- normalize(t(oat_sens_mat))
  normsens_mean <- rowMeans(normsens)
  
  # 3) sort by importance
  sort_ix <- sort(normsens_mean, decreasing = TRUE, index.return = TRUE)$ix
  
  # 4) two‐panel layout with bigger left margin
  par(mar = c(15, 20, 5, 1), mfrow = c(1,2))
  layout(matrix(c(1,1,2), ncol = 3, nrow = 1))
  
  # 5) LEFT: heatmap
  image(rotate(normsens[sort_ix, ]), axes = FALSE, col = colorRampPalette(c("white", "darkgreen"))(100))
  axis(1,
       at     = seq(0,1,length.out = length(ynames)),
       labels = ynames,
       las    = 3, cex.axis = 1.6) # HERE
  axis(2,
       at     = seq(1,0,length.out = length(xnames)),
       labels = xnames[sort_ix],
       las    = 1, cex.axis = 1.6) #1.4
  # mtext('One-at-a-time sensitivity', side = 3, adj = 0, line = 2, cex = 1)
  
  # 6) RIGHT: dot‐plot
  lab_ix <- seq_along(xnames) - 0.5
  par(yaxs = 'i', mar = c(15,1,5,5))
  plot(rev(normsens_mean[sort_ix]), lab_ix,
       xlab = 'mean oaat variance (normalized)', ylab = '',
       ylim = c(0,length(xnames)), type = 'n', yaxt = 'n',
       cex.lab = 1.5,
       cex.axis = 1.5)
  abline(h = lab_ix, col = 'grey', lty = 'dashed')
  points(rev(normsens_mean[sort_ix]), lab_ix,
         col = "darkgreen", pch = 19, cex = 1.5)
  image.plot(legend.only = TRUE,
             zlim = c(0,1),
             col = colorRampPalette(c("white", "darkgreen"))(100),
             horizontal = TRUE,
             legend.args = list(text = 'Relative sensitivity',
                                side = 3, line = 1, cex = 1), 
             cex.axis = 3)
}



# Set up params and files for SA ------------------------------------------

# Reference grid, default at mean (0.5)
# alter these fixed values to perform SA elsewhere in input space
fixed_value <- 0.5
ref <- setNames(rep(fixed_value, 9), colnames(inputs)) 

# List all your .rds files
rds_files_all <- list.files(emul_path, pattern = "\\.rds$", full.names = TRUE)


# Global sensitivity analysis ---------------------------------------------
global_param_files <- rds_files_all

# Read them into a named list
global_params_list <- setNames(
  lapply(global_param_files, readRDS),
  tools::file_path_sans_ext(basename(global_param_files))
)

# Run OAT sensitivity on each emulator
global_sens_list <- lapply(global_params_list, function(em) {
  oat_sens_pretrained(ref, em, n = 30)
})

# Stack into a matrix - THIS NEEDS EDITING IF VARS ARE EDITED
global_sens_mat <- do.call(rbind, global_sens_list)
# rownames(global_sens_mat) <- c("Emissions 2030", "Emissions 2040")
# colnames(global_sens_mat) <- c("US phaseouts", "US subsidies")

# Save sensitivity matrix
write.csv(global_sens_mat,
          file = paste0(output_dir, gbl_sa_filename),
          row.names = F)

# Plot and save - FIG 2
png(gbl_plot_path, width = 1400, height = 1100, res = 150)
oaatSensvarSummaryPlot(global_sens_mat, threshold = 0.01)
dev.off()





