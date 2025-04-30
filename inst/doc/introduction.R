## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(AccSamplingDesign)

## ----eval=FALSE---------------------------------------------------------------
#  # Install from GitHub
#  devtools::install_github("vietha/AccSamplingDesign")
#  
#  # Load package
#  library(AccSamplingDesign)

## -----------------------------------------------------------------------------
plan_attr <- optAttrPlan(
  PRQ = 0.01,   # Acceptable Quality Level (1% defects)
  CRQ = 0.05,   # Rejectable Quality Level (5% defects)
  alpha = 0.05, # Producer's risk
  beta = 0.10   # Consumer's risk
)

## -----------------------------------------------------------------------------
summary(plan_attr)

## -----------------------------------------------------------------------------
# Probability of accepting 3% defective lots
accProb(plan_attr, 0.03)

## -----------------------------------------------------------------------------
plot(plan_attr)

## -----------------------------------------------------------------------------
norm_plan <- optVarPlan(
  PRQ = 0.025,       # Acceptable quality level (% nonconforming)
  CRQ = 0.1,         # Rejectable quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "normal",
  sigma_type = "known"
)

summary(norm_plan)

# Generate OC curve data
oc_data_normal <- OCdata(norm_plan)
# show data of Proportion Nonconforming (x_p) vs Probability Acceptance (y)
#head(oc_data_normal, 15) 
plot(norm_plan)

## -----------------------------------------------------------------------------
p1 = 0.005
p2 = 0.03
alpha = 0.05
beta = 0.1

# known sigma plan
plan1 <- optVarPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "know")
summary(plan1)
plot(plan1)

# unknown sigma plan
plan2 <- optVarPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "unknow")
summary(plan2)
plot(plan2)

# Generate OC curve data
oc_data1 <- OCdata(plan1)
oc_data2 <- OCdata(plan2)

# Plot the first OC curve (solid red line)
plot(oc_data1@pd, oc_data1@paccept, type = "l", col = "red", lwd = 2,
     main = "Operating Characteristic (OC) Curve", 
     xlab = "Proportion Nonconforming", 
     ylab = "P(accept)")

# Add the second OC curve (dashed blue line)
lines(oc_data2@pd, oc_data2@paccept, col = "blue", lwd = 2, lty = 2)

abline(v = c(p1, p2), lty = 1, col = "gray")
abline(h = c(1 - alpha, beta), lty = 1, col = "gray")

legend1 = paste0("Known Sigma (n=", plan1$sample_size, ", k=", plan1$k, ")")
legend2 = paste0("Unknown Sigma (n=", plan2$sample_size, ", k=", plan2$k, ")")
# Add a legend to distinguish the two curves
legend("topright", legend = c(legend1, legend2), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# Add a grid
grid()

## -----------------------------------------------------------------------------
beta_plan <- optVarPlan(
  PRQ = 0.05,        # Target quality level (% nonconforming)
  CRQ = 0.2,         # Minimum quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "beta",
  theta = 44000000,
  theta_type = "known",
  LSL = 0.00001
)
summary(beta_plan)

# Plot OC use plot function
plot(beta_plan)

# Generate OC data
p_seq <- seq(0.005, 0.5, by = 0.005)
oc_data <- OCdata(beta_plan, pd = p_seq)
#head(oc_data)

# plot use S3 method
plot(oc_data)

# Plot the OC curve with Mean Level (x_m) and Probability of Acceptance (y)
plot(oc_data@pd, oc_data@paccept, type = "l", col = "blue", lwd = 2,
     main = "OC Curve by the mean levels (plot by data)", xlab = "Mean Levels",
     ylab = "P(accept)")
grid()

## -----------------------------------------------------------------------------
p1 = 0.005
p2 = 0.03
alpha = 0.05
beta = 0.1
spec_limit = 0.05 # use for Beta distribution
theta = 500

# My package for beta plan
beta_plan1 <- optVarPlan(
  PRQ = p1,       # Target quality level (% nonconforming)
  CRQ = p2,       # Minimum quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "beta",
  theta = theta,
  theta_type = "known",
  USL = spec_limit
)
summary(beta_plan1)

beta_plan2 <- optVarPlan(
  PRQ = p1,       # Target quality level (% nonconforming)
  CRQ = p2,       # Minimum quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "beta",
  theta = theta,
  theta_type = "unknown",
  USL = spec_limit
)
summary(beta_plan2)

# Generate OC curve data
oc_beta_data1 <- OCdata(beta_plan1)
oc_beta_data2 <- OCdata(beta_plan2)

# Plot the first OC curve (solid red line)
plot(oc_beta_data1@pd, oc_beta_data1@paccept, type = "l", col = "red", lwd = 2,
     main = "Operating Characteristic (OC) Curve", 
     xlab = "Proportion Nonconforming", 
     ylab = "P(accept)")

# Add the second OC curve (dashed blue line)
lines(oc_beta_data2@pd, oc_beta_data2@paccept, col = "blue", lwd = 2, lty = 2)

abline(v = c(p1, p2), lty = 1, col = "gray")
abline(h = c(1 - alpha, beta), lty = 1, col = "gray")

legend1 = paste0("Known Theta (n=", beta_plan1$sample_size, ", k=", beta_plan1$k, ")")
legend2 = paste0("Unknown Theta (n=", beta_plan2$sample_size, ", k=", beta_plan2$k, ")")
# Add a legend to distinguish the two curves
legend("topright", legend = c(legend1, legend2), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# Add a grid
grid()

## -----------------------------------------------------------------------------
# Probability of accepting 10% defective
accProb(norm_plan, 0.1)

# Probability of accepting 5% defective
accProb(beta_plan, 0.05)

