library(VGAM)

# 1. Fit a VGAM model (example using simulated data)
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- rbinom(n, 1, plogis(1 + 2 * x1 - 1.5 * x2))
my_data <- data.frame(y, x1, x2)

# Fit a logistic regression model using vglm
fit_vglm <- vglm(
  y ~ x1 + x2,
  family = binomialff,
  data = my_data,
)

# --- CORRECTED ROBUST SE CALCULATION ---

# Step 1: Get the "Bread"
bread <- vcov(fit_vglm)

# Step 2: Get the scores with respect to the LINEAR PREDICTOR (eta)
scores_wrt_eta <- weights(fit_vglm, type = "working", deriv.arg = TRUE)$deriv

# Step 3: Get the VLM model matrix
X_vlm <- model.matrix(fit_vglm, type = "vlm")

# Step 4: Use the chain rule to get scores with respect to COEFFICIENTS (beta)
# This is the crucial new step
scores_wrt_beta <- scores_wrt_eta * X_vlm # Element-wise multiplication

# Step 5: Calculate the "Meat" using the correct scores
# The result will now be a 3x3 matrix
meat_non_clustered <- crossprod(scores_wrt_beta)

# Step 6: Assemble the sandwich
vcov_robust_non_clustered <- bread %*% meat_non_clustered %*% bread

# Step 7: Extract the standard errors and compare
se_robust_non_clustered <- sqrt(diag(vcov_robust_non_clustered))

comparison <- data.frame(
  Original_SE = sqrt(diag(vcov(fit_vglm))),
  Robust_SE = se_robust_non_clustered
)

print(comparison)
