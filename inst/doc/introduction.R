## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(AccSamplingDesign)

## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github("vietha/AccSamplingDesign")

## -----------------------------------------------------------------------------
plan_attr <- optPlan(
  PRQ = 0.01,   # Acceptable Quality Level (1% defects)
  CRQ = 0.05,   # Rejectable Quality Level (5% defects)
  alpha = 0.02, # Producer's risk
  beta = 0.15,  # Consumer's risk
  distribution = "binomial"
)

## -----------------------------------------------------------------------------
summary(plan_attr)

## -----------------------------------------------------------------------------
# Probability of accepting 3% defective lots
accProb(plan_attr, 0.03)

## -----------------------------------------------------------------------------
plot(plan_attr)

## -----------------------------------------------------------------------------
# Step1: Find an optimal Attributes Sampling plan
optimal_plan <- optPlan(PRQ = 0.01, CRQ = 0.05, alpha = 0.02, beta = 0.15,
                        distribution = "binomial") # could try "poisson" too
# Summarize the plan
summary(optimal_plan)

# Step2: Compare the optimal plan with two alternative plans 
pd <- seq(0, 0.15, by = 0.001)
# Generate OC data from the optimal plan
oc_opt <- OCdata(plan = optimal_plan, pd = pd)
# Create and generate OC data for manual plan1
mplan1 <- manualPlan(n = optimal_plan$n, c = optimal_plan$c - 1,
                  distribution = "binomial")
oc_alt1 <- OCdata(plan = mplan1, pd = pd)
# Create and generate OC data for manual plan2
mplan2 <- manualPlan(n = optimal_plan$n, c = optimal_plan$c + 1,
                  distribution = "binomial")
oc_alt2 <- OCdata(plan = mplan2, pd = pd)

# Step3: Visualize results
plot(pd, oc_opt$paccept, type = "l", col = "blue", lwd = 2,
     xlab = "Proportion Defective", ylab = "Probability of Acceptance",
     main = "Attributes Sampling - OC Curves Comparison",
     xlim = c(0, 0.15), ylim = c(0, 1))
lines(pd, oc_alt1$paccept, col = "red", lwd = 2, lty = 2)
lines(pd, oc_alt2$paccept, col = "green", lwd = 2, lty = 3)
abline(v = c(0.01, 0.05), col = "gray50", lty = 2)
abline(h = c(1 - 0.02, 0.15), col = "gray50", lty = 2)
legend("topright", legend = c(sprintf("Optimal Plan (n = %d, c = %d)", 
       optimal_plan$n, optimal_plan$c),
       sprintf("Alt 1 (c = %d)", optimal_plan$c - 1),
       sprintf("Alt 2 (c = %d)", optimal_plan$c + 1)),
       col = c("blue", "red", "green"),
       lty = c(1, 2, 3), lwd = 2)

## -----------------------------------------------------------------------------
# Predefine parameters
PRQ <- 0.025
CRQ <- 0.1        
alpha <- 0.05 
beta <- 0.1

norm_plan <- optPlan(
  PRQ = PRQ,       # Acceptable quality level (% nonconforming)
  CRQ = CRQ,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "known"
)

# Summary plan
summary(norm_plan)

# Probability of accepting 10% defective
accProb(norm_plan, 0.1)

# plot OC 
plot(norm_plan)

## -----------------------------------------------------------------------------
# Setup a pd range to make sure all plans have use same pd range
pd <- seq(0, 0.2, by = 0.001)

# Generate OC curve data for designed plan
opt_pdata <- OCdata(norm_plan, pd = pd)

# Evaluated Plan 1: n + 6
eval1 <- manualPlan(n = norm_plan$sample_size + 10, k = norm_plan$k,
                       distribution = "beta", LSL = 5.65e-6, theta = 6.6e8)
eval1_pdata <- OCdata(eval1, pd = pd)
# Evaluated Plan 2: k + 0.1
eval2 <- manualPlan(n = norm_plan$sample_size, k = norm_plan$k + 0.1,
                       distribution = "beta", LSL = 5.65e-6, theta = 6.6e8)
eval2_pdata <- OCdata(eval2, pd = pd)

# Plot base
plot(100 *  pd, 100 * opt_pdata$paccept,
     type = "l", lwd = 2, col = "blue",
     xlab = "Percentage Nonconforming (%)",
     ylab = "Probability of Acceptance (%)",
     main = "Normal Variables Sampling - Designed Plan with Evaluated Plans")

# Add evaluated plan 1: n + 6
lines(100 * pd, 100 * eval1_pdata$paccept,
      col = "red", lty = "longdash", lwd = 2)

# Add evaluated plan 2: k + 0.1
lines(100 * pd, 100 * eval2_pdata$paccept,
      col = "forestgreen", lty = "dashed", lwd = 2)

# Add vertical dashed lines at PRQ and CRQ
abline(v = 100 * PRQ, col = "gray60", lty = "dashed")
abline(v = 100 * CRQ, col = "gray60", lty = "dashed")

# Add horizontal dashed lines at 1 - alpha and beta
abline(h = 100 * (1 - alpha), col = "gray60", lty = "dashed")
abline(h = 100 * beta, col = "gray60", lty = "dashed")

# Add legend
legend("topright",
       legend = c(paste0("Designed Plan: n = ", norm_plan$sample_size, ", k = ", round(norm_plan$k, 2)), 
                  "Evaluated Plan: n + 6", 
                  "Evaluated Plan: k + 0.1"),
       col = c("blue", "red", "forestgreen"),
       lty = c("solid", "longdash", "dashed"),
       lwd = 2,
       bty = "n")

## -----------------------------------------------------------------------------
p1 = 0.005
p2 = 0.03
alpha = 0.05
beta = 0.1

# known sigma plan
plan1 <- optPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "know")
summary(plan1)
plot(plan1)

# unknown sigma plan
plan2 <- optPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "unknow")
summary(plan2)
plot(plan2)

## -----------------------------------------------------------------------------
beta_plan <- optPlan(
  PRQ = 0.05,        # Target quality level (% nonconforming)
  CRQ = 0.2,         # Minimum quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "beta",
  theta = 44000000,
  theta_type = "known",
  LSL = 0.00001
)
# Summary Beta plan
summary(beta_plan)
# Probability of accepting 5% defective
accProb(beta_plan, 0.05)

# Plot OC use plot function
plot(beta_plan)

## -----------------------------------------------------------------------------
# plot use S3 method by default (defective rate)
plot(beta_plan)
# plot use S3 method by default by mean levels
plot(beta_plan, by = "mean")

