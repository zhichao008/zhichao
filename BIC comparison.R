# Read return data
returns <- read.csv("C:/1/CLEANED/results/strategy_output/returns_series.csv",
                    stringsAsFactors = FALSE)
returns$Date <- as.Date(returns$Date)
colnames(returns)[2:4] <- c("KOR", "IND", "GBR")

# Function to read uncertainty data
read_uncertainty <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  return(df)
}

# Read three types of uncertainty data for UK
epu <- read_uncertainty("C:/1/CLEANED/KOR_EPU_stock_cleaned.csv")
tpu <- read_uncertainty("C:/1/CLEANED/KOR_TPU_stock_cleaned.csv")
cpu <- read_uncertainty("C:/1/CLEANED/KOR_CPU_stock_cleaned.csv")

# Extract return series
returns_series <- returns$GBR
returns_series <- na.omit(returns_series)

# Extract uncertainty series (assuming second column contains values)
epu_series <- epu[, 2]
tpu_series <- tpu[, 2]
cpu_series <- cpu[, 2]

# Ensure consistent length
min_len <- min(length(returns_series), length(epu_series),
               length(tpu_series), length(cpu_series))
returns_series <- returns_series[1:min_len]
epu_series <- epu_series[1:min_len]
tpu_series <- tpu_series[1:min_len]
cpu_series <- cpu_series[1:min_len]

# 3. Calculate four aggregation methods
# ============================================================

# 3.1 Equal-weight average
equal_weight <- (epu_series + tpu_series + cpu_series) / 3

# 3.2 First Principal Component
pca_data <- data.frame(EPU = epu_series, TPU = tpu_series, CPU = cpu_series)
pca_result <- prcomp(pca_data, scale. = TRUE)
pc1 <- pca_result$x[, 1]

# 3.3 Additive specification
additive <- epu_series + tpu_series + cpu_series

# 3.4 MC_Weighted (using PCA loadings)
# UK PCA loadings: EPU=0.668, TPU=0.562, CPU=0.487
loadings <- pca_result$rotation[, 1]
mc_weighted <- loadings[1] * epu_series + loadings[2] * tpu_series + loadings[3] * cpu_series

# 4. BIC calculation functions (using linear regression models)
# ============================================================

# Method 1: Using linear regression (uncertainty as explanatory variable)
calculate_bic_lm <- function(y, x, model_name) {
  # Build dataframe
  df <- data.frame(y = y, x = x)

  # Fit linear regression model
  fit <- lm(y ~ x, data = df)

  # Extract BIC
  bic <- BIC(fit)

  return(bic)
}

# Method 2: Using AR model (accounting for time series autocorrelation)
calculate_bic_ar <- function(y, x, model_name, lag = 1) {
  # Build lagged variables
  n <- length(y)
  y_lag <- y[1:(n-lag)]
  x_aligned <- x[(lag+1):n]
  y_aligned <- y[(lag+1):n]

  # Build dataframe
  df <- data.frame(y = y_aligned, x = x_aligned, y_lag = y_lag)

  # Fit AR(1) + exogenous variable model
  fit <- lm(y ~ y_lag + x, data = df)

  # Extract BIC
  bic <- BIC(fit)

  return(bic)
}

# Method 3: Using simplified GARCH-type variance model (squared log returns)
calculate_bic_variance <- function(y, x, model_name) {
  # Calculate squared log returns (volatility proxy)
  y_sq <- y^2

  # Build dataframe
  df <- data.frame(y_sq = y_sq, x = x)

  # Fit variance model
  fit <- lm(y_sq ~ x, data = df)

  # Extract BIC
  bic <- BIC(fit)

  return(bic)
}

# 5. Choose one method for calculation
# ============================================================

cat("\n========== Calculating BIC Values ==========\n\n")

# Using AR(1) model (recommended)
cat("Calculating BIC using AR(1) + exogenous variable model...\n")

bic_eq <- calculate_bic_ar(returns_series, equal_weight, "Equal-Weight")
bic_pc1 <- calculate_bic_ar(returns_series, pc1, "PC1")
bic_add <- calculate_bic_ar(returns_series, additive, "Additive")
bic_mcw <- calculate_bic_ar(returns_series, mc_weighted, "MC_Weighted")

# 6. Summarize results
# ============================================================

results <- data.frame(
  Model = c("Equal-Weight", "PC1", "Additive", "MC_Weighted"),
  BIC = c(bic_eq, bic_pc1, bic_add, bic_mcw)
)

# Sort by BIC (lower is better)
results <- results[order(results$BIC), ]
results$Rank <- 1:nrow(results)

# 7. Print results
# ============================================================

cat("\n========== BIC Comparison Results ==========\n\n")
print(results)

cat("\n========== Conclusion ==========\n")
cat("Best model:", results$Model[1], "\n")
cat("Minimum BIC:", round(results$BIC[1], 2), "\n")

if (nrow(results) > 1) {
  cat("\nBIC differences:\n")
  for (i in 2:nrow(results)) {
    diff <- results$BIC[i] - results$BIC[1]
    cat("  ", results$Model[i], ": ", round(diff, 2), "\n")
  }
}

# 8. Visualization
# ============================================================

p_bic <- ggplot(results, aes(x = reorder(Model, BIC), y = BIC, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(BIC, 2)), vjust = -0.5, size = 4) +
  labs(title = "BIC Comparison of Four Models (United Kingdom)",
       subtitle = "Lower BIC indicates better model fit",
       x = "Model", y = "BIC") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Equal-Weight" = "steelblue",
                               "PC1" = "darkorange",
                               "Additive" = "forestgreen",
                               "MC_Weighted" = "red"))

print(p_bic)

# Save plot
ggsave("bic_comparison_uk.png", p_bic, width = 8, height = 6, dpi = 300)

# 9. Export results
# ============================================================
write.csv(results, "bic_comparison_results_uk.csv", row.names = FALSE)
cat("\nResults saved to bic_comparison_results_uk.csv\n")

# 10. Comparison of three BIC calculation methods (optional)
# ============================================================

cat("\n========== Comparison of Three BIC Calculation Methods ==========\n\n")

# Linear regression method
bic_lm_eq <- calculate_bic_lm(returns_series, equal_weight, "Equal-Weight")
bic_lm_pc1 <- calculate_bic_lm(returns_series, pc1, "PC1")
bic_lm_add <- calculate_bic_lm(returns_series, additive, "Additive")
bic_lm_mcw <- calculate_bic_lm(returns_series, mc_weighted, "MC_Weighted")

# Variance model method
bic_var_eq <- calculate_bic_variance(returns_series, equal_weight, "Equal-Weight")
bic_var_pc1 <- calculate_bic_variance(returns_series, pc1, "PC1")
bic_var_add <- calculate_bic_variance(returns_series, additive, "Additive")
bic_var_mcw <- calculate_bic_variance(returns_series, mc_weighted, "MC_Weighted")

results_lm <- data.frame(
  Model = c("Equal-Weight", "PC1", "Additive", "MC_Weighted"),
  LM_BIC = c(bic_lm_eq, bic_lm_pc1, bic_lm_add, bic_lm_mcw),
  AR_BIC = c(bic_eq, bic_pc1, bic_add, bic_mcw),
  Variance_BIC = c(bic_var_eq, bic_var_pc1, bic_var_add, bic_var_mcw)
)

print(results_lm)

# Save comparison results of three methods
write.csv(results_lm, "bic_comparison_all_methods.csv", row.names = FALSE)
