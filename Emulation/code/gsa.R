## Script for performing global sensitivity analysis (GSA) with existing emulators
library(mgcv)
library(dplyr)
library(plotly)

# Set up root
setwd('~/Github/rebound_emulation')

# Read and wrangle data
data <- read.csv("Emulation/data/predictions/preds_combined_fac.csv")
# subset and remove factors
set.seed(123) #With 1000 rows and many tensor terms, 
# individual ti() indices will be noisy. 
# If you want to know how stable the rankings are, 
# refit on a few different random samples (different seeds) 
# and see whether the top terms stay the same

# subset for testing
dat <- data[data$emulator == 'em_RB_GtCO2', 1:10]
samp <- dat
#samp <- samp[sample(nrow(samp), 100000), ]

# create function for inputs to prediction
x <- setdiff(names(samp), "prediction") 
f <- reformulate(
  c(sprintf("s(%s)", x),
    combn(x,2,FUN=\(z) sprintf("ti(%s,%s)", z[1],  z[2]))),
  response = "prediction"
)

# formula for calculating sobol indices from model
sobol <- function(model) {
  th   <- as.data.frame(predict(model, type = "terms"))
  yhat <- predict(model, type = "response")
  sapply(th, var) / var(yhat)
}


b <- bam(f, data = samp, method = "fREML", discrete = TRUE,
         select = TRUE, nthreads = parallel::detectCores() - 1,
         control = gam.control(trace = TRUE))
# Find a way of comparing, sample size that works
g <- gam(f, data = samp, method = "REML", select = TRUE,
        control = gam.control(trace = TRUE))

g <- readRDS("Emulation/data/emulators/gam_model.rds")
# check model fit
summary(g)$r.sq        # adjusted R-squared
summary(g)$dev.expl    # deviance explained
summary(b)$r.sq        # adjusted R-squared
summary(b)$dev.expl    # deviance explained

# manual sobol calculation, same as sobol function
term_hat <- as.data.frame(predict(n, type = "terms"))
yhat <- predict(n, type = "response")
approx_sobol <- sapply(term_hat, var) / var(yhat)

# generate sobols through function
S_gam <- sobol(g)   # gam
S_bam <- sobol(b)   # bam

saveRDS(b, file = "Emulation/data/emulators/bam_model.rds")
saveRDS(g, file = "Emulation/data/emulators/gam_model.rds")

b <- readRDS("Emulation/data/emulators/bam_model.rds")
g <- readRDS("Emulation/data/emulators/gam_model.rds")

# put both indices side by side
res <- data.frame(term = names(S_gam), gam = S_gam, bam = S_bam)
#res <- data.frame(term = names(S_bam), bam = S_bam)
res <- res[order(-res$bam), ] 
View(head(res, 15))          # most important terms

# split into first/second order effects
main <- res[grepl("^s\\(", res$term), ]
int  <- res[grepl("^ti\\(", res$term), ]
colSums(main[, c("gam","bam")])   # total first-order share
colSums(int[,  c("gam","bam")])   # total interaction share

# check aggreement between methods
cor(res$gam, res$bam)
plot(res$gam, res$bam); abline(0, 1) # good alignment between methods
max(abs(res$gam - res$bam)) # biggest difference is still marginal

### sanity checks

sum(S_gam)   # should be close to 1 
# but at 0.62 could this be a sample error?
sum(S_bam) # 0.63
"""
If it’s noticeably off 1, the inputs are probably correlated, 
or the terms aren’t orthogonal over your sample. 
The decomposition is then only approximate. 
If your inputs were sampled independently (for example from a design), 
the sum should be close to 1.
These are shares of the variance of the model’s predictions, 
not of the true output. Check the model fit as well.

Model fit checks are performed above and seem good so more info needed - sample error?
"""

# extract info from model objects
summary(b)             # table of terms: edf, F, p-value; plus R-sq, deviance explained
s <- summary(b)$s.table
wig <- s[order(-s[, "edf"]), ]   # terms with the most wiggliness

sp <- b$sp                # estimated smoothing parameters (huge values = heavily shrunk)
b$edf                      # edf per coefficient
sum(b$edf)                 # total effective df of the model
b$gcv.ubre                 # final REML score (compare gam vs bam only loosely)
b$converged                # did it converge?

# plotting effects
plot(b, pages = 6, scale = 0)        # all smooths (may be crowded with 55 terms)

# contours
vis.gam(b, view = c("lead_solar","lead_onshore"), plot.type = "contour")  # an interaction surface
vis.gam(b, view = c("cr_solar","lead_onshore"), plot.type = "persp")  # an interaction surface
vis.gam(b, view = c("lead_solar","cr_solar"), plot.type = "contour")  # an interaction surface
vis.gam(b, view = c("lead_solar","cr_solar"), plot.type = "persp")  # an interaction surface
vis.gam(b, view = c("lead_onshore","trans_rebound"), plot.type = "contour")  # an interaction surface
vis.gam(b, view = c("lead_onshore","trans_rebound"), plot.type = "persp")  # an interaction surface
vis.gam(b, view = c("cr_solar","trans_rebound"), plot.type = "contour")  # an interaction surface
vis.gam(b, view = c("cr_solar","trans_rebound"), plot.type = "persp")  # an interaction surface

vis.gam(b, view = c("lead_solar","trans_rebound"), plot.type = "contour")  # an interaction surface
vis.gam(b, view = c("lead_solar","trans_rebound"), plot.type = "persp")  # an interaction surface
vis.gam(b, view = c("lead_onshore","trans_turnover"), plot.type = "contour")  # an interaction surface
vis.gam(b, view = c("lead_onshore","trans_turnover"), plot.type = "persp")  # an interaction surface



# Visual adjustments
vis.gam(b, view = c("lead_solar", "lead_onshore"),
        plot.type = "persp",
        theta = 90, phi = 30)

vis.gam(b, view = c("lead_solar", "lead_onshore"),
        plot.type = "persp",
        theta = 45, phi = 20,
        r = 2,              # distance of the viewer (smaller = more distorted, larger = flatter)
        d = 1,              # strength of perspective effect
        expand = 0.8,       # shrinks or stretches the vertical (z) axis
        ticktype = "detailed",  # shows numeric tick marks on axes
        color = "topo",
        se = -1)            # -1 adds a pair of surfaces showing the +/- confidence bounds

g <- expand.grid(lead_solar = seq(min(samp$lead_solar), max(samp$lead_solar), length = 50),
                 lead_onshore = seq(min(samp$lead_onshore), max(samp$lead_onshore), length = 50))
# fill remaining variables at their medians
for (v in setdiff(x, names(g))) g[[v]] <- median(samp[[v]])

# add prediction col from data
g$pred <- predict(b, newdata = g)

# plot predictions vs inputs
plot_ly(x = unique(g$lead_solar), y = unique(g$lead_onshore),
        z = matrix(g$pred, 50, 50, byrow = TRUE)) |> add_surface()

# plotting top terms
n_terms <- 10
top <- match(head(res$term, n_terms), sapply(b$smooth, `[[`, "label"))
plot(b)
plot(b, select = top[1])

# diagnostics
gam.check(b) # residual plots and k-index / edf table for each term


####### Higher order effects

# general formula builder
make_fanova_formula <- function(data, response = "prediction", max_order = 2) {
  x <- setdiff(names(data), response)
  
  stopifnot(
    max_order >= 1,
    max_order <= length(x)
  )
  
  # First-order smooths
  terms <- sprintf("s(%s)", x)
  
  # Higher-order pure interactions: ti(x1, x2), ti(x1, x2, x3), ...
  if (max_order >= 2) {
    for (k in 2:max_order) {
      terms <- c(
        terms,
        combn(x, k, FUN = function(z) {
          sprintf("ti(%s)", paste(z, collapse = ", "))
        })
      )
    }
  }
  
  reformulate(terms, response = response)
}


# fit GAMs

max_order <- 3   # choose 1, 2, 3, ...

f <- make_fanova_formula(
  data = samp,
  response = "prediction",
  max_order = max_order
)

b <- bam(
  f,
  data = samp,
  method = "fREML",
  discrete = TRUE,
  select = TRUE,
  nthreads = parallel::detectCores() - 1,
  control = gam.control(trace = TRUE)
)




