## Script for performing global sensitivity analysis (GSA) with existing emulators
library(mgcv)

# Read and wrangle data
data <- read.csv("Emulation/data/predictions/preds_combined_fac.csv")

# subset and remove factors
set.seed(123) #With 1000 rows and many tensor terms, 
# individual ti() indices will be noisy. 
# If you want to know how stable the rankings are, 
# refit on a few different random samples (different seeds) 
# and see whether the top terms stay the same

dat <- subset(data, select = -c(12:19)) #, lead_offshore)) # remove factors and offshore
samp <- dat[dat$emulator == 'em_RB_GtCO2', -c(11)] # -c(10)
samp <- samp[sample(nrow(samp), 100000), ]

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

n <- gam(f, data = samp, method = "REML", select = TRUE,
        control = gam.control(trace = TRUE))

n <- readRDS("Emulation/data/emulators/gam_model.rds")
# check model fit
summary(m)$r.sq        # adjusted R-squared
summary(m)$dev.expl    # deviance explained
summary(n)$r.sq        # adjusted R-squared
summary(n)$dev.expl    # deviance explained

# manual sobol calculation, same as sobol function
term_hat <- as.data.frame(predict(n, type = "terms"))
yhat <- predict(n, type = "response")
approx_sobol <- sapply(term_hat, var) / var(yhat)

# generate sobols through function
S_gam <- sobol(n)   # gam
S_bam <- sobol(m)   # bam

m <- readRDS("Emulation/data/emulators/bam_model.rds")
n <- readRDS("Emulation/data/emulators/gam_model.rds")


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
max(abs(res$gam - res$bam))

### sanity checks

sum(S_gam)   # should be close to 1 
# but at 0.62 
sum(S_bam) # 0.63
"""
If it’s noticeably off 1, the inputs are probably correlated, 
or the terms aren’t orthogonal over your sample. 
The decomposition is then only approximate. 
If your inputs were sampled independently (for example from a design), 
the sum should be close to 1.
These are shares of the variance of the model’s predictions, 
not of the true output. Check the model fit as well.

Model fit checks are performed above and seem good so more info needed
"""

# extract info from model objects
summary(m)             # table of terms: edf, F, p-value; plus R-sq, deviance explained
s <- summary(n)$s.table
s[order(-s[, "edf"]), ]   # terms with the most wiggliness

sp <- n$sp                # estimated smoothing parameters (huge values = heavily shrunk)
n$edf                      # edf per coefficient
sum(n$edf)                 # total effective df of the model
n$gcv.ubre                 # final REML score (compare gam vs bam only loosely)
n$converged                # did it converge?

# plotting effects
plot(n, pages = 1, scale = 0)        # all smooths (may be crowded with 55 terms)
plot(n, select = 1)                  # one term
vis.gam(n, view = c("lead_solar","lead_onshore"), plot.type = "contour")  # an interaction surface
vis.gam(m, view = c("lead_solar","lead_onshore"), plot.type = "contour")  # an interaction surface
vis.gam(m, view = c("lead_solar","cr_solar"), plot.type = "contour")  # an interaction surface

# plotting top terms
n_terms <- 10
top <- match(head(res$term, n_terms), sapply(n$smooth, `[[`, "label"))
plot(n, select = top[1])

# diagnostics
gam.check(b) # residual plots and k-index / edf table for each term

saveRDS(m, file = "Emulation/data/emulators/bam_model.rds")
saveRDS(n, file = "Emulation/data/emulators/gam_model.rds")
