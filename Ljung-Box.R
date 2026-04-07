# 1. Load necessary packages (all base or commonly used packages)
# ============================================================
library(tseries)      # Ljung-Box test
library(ggplot2)      # Plotting
library(dplyr)        # Data manipulation

# 2. Data input
# ============================================================

returns <- read.csv("C:/1/CLEANED/results/strategy_output/returns_series.csv")$Mixed
returns <- na.omit(returns)


# Method C: If you already have model residuals, assign directly
# residuals <- c(...)  # Your residual vector

# 3. If no existing residuals, first fit a simple GARCH model to obtain residuals
# ============================================================
# Use R's built-in arima function to fit mean equation, then calculate residuals

# Method 1: Simple demean (subtract mean)
mu <- mean(returns)
residuals_raw <- returns - mu

# Method 2: Use ARMA model (optional)
# arma_fit <- arima(returns, order = c(1, 0, 1))
# residuals_raw <- residuals(arma_fit)

# If no volatility model, standardized residuals = raw residuals / rolling standard deviation
# Calculate rolling standard deviation as an approximation of conditional variance
window_size <- 20
conditional_sd <- zoo::rollapply(residuals_raw, window_size, sd,
                                 fill = NA, align = "right")
standardized_residuals <- residuals_raw / conditional_sd
standardized_residuals <- na.omit(standardized_residuals)

# If you already have model residuals, use directly:
# standardized_residuals <- your_residuals

cat("Standardized residuals length:", length(standardized_residuals), "\n")
cat("Standardized residuals summary:\n")
print(summary(standardized_residuals))

# 4. Ljung-Box test (core part)
# ============================================================

# Define lag orders
lags <- c(5, 10, 15, 20)

# Initialize results matrix
lb_results <- data.frame(
  Lag = integer(),
  Type = character(),
  LB_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

cat("\n========== Ljung-Box Test Results ==========\n\n")

for (lag in lags) {
  # Test standardized residuals
  test_raw <- Box.test(standardized_residuals, lag = lag, type = "Ljung-Box")
  lb_results <- rbind(lb_results, data.frame(
    Lag = lag,
    Type = "Raw Residuals",
    LB_Statistic = round(test_raw$statistic, 3),
    P_Value = round(test_raw$p.value, 4)
  ))

  # Test squared residuals (to check ARCH effects)
  test_sq <- Box.test(standardized_residuals^2, lag = lag, type = "Ljung-Box")
  lb_results <- rbind(lb_results, data.frame(
    Lag = lag,
    Type = "Squared Residuals",
    LB_Statistic = round(test_sq$statistic, 3),
    P_Value = round(test_sq$p.value, 4)
  ))
}

# Print results table
print(lb_results)

# 5. Diagnostic conclusions
# ============================================================

cat("\n========== Diagnostic Conclusions ==========\n\n")

# Check raw residuals
raw_pvalues <- lb_results %>%
  filter(Type == "Raw Residuals") %>%
  pull(P_Value)

raw_pass <- all(raw_pvalues > 0.05)

if (raw_pass) {
  cat("✓ Raw residuals Ljung-Box test passed\n")
  cat("  All lag p-values > 0.05, residuals have no significant autocorrelation\n")
} else {
  cat("✗ Raw residuals Ljung-Box test failed\n")
  cat("  There are lags with p-value < 0.05, residuals have autocorrelation\n")
}

# Check squared residuals
sq_pvalues <- lb_results %>%
  filter(Type == "Squared Residuals") %>%
  pull(P_Value)

sq_pass <- all(sq_pvalues > 0.05)

if (sq_pass) {
  cat("\n✓ Squared residuals Ljung-Box test passed\n")
  cat("  All lag p-values > 0.05, no significant ARCH effects\n")
} else {
  cat("\n✗ Squared residuals Ljung-Box test failed\n")
  cat("  There are lags with p-value < 0.05, ARCH effects exist\n")
}

# 6. Diagnostic visualization plots
# ============================================================

# Plot 1: Standardized residuals time series
p1 <- ggplot(data.frame(t = 1:length(standardized_residuals),
                        res = standardized_residuals),
             aes(x = t, y = res)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Standardized Residuals", x = "Time", y = "Residuals") +
  theme_minimal()

# Plot 2: ACF of raw residuals
acf_raw <- acf(standardized_residuals, lag.max = 30, plot = FALSE)
acf_raw_df <- data.frame(Lag = acf_raw$lag[-1], ACF = acf_raw$acf[-1])

p2 <- ggplot(acf_raw_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "red") +
  labs(title = "ACF of Raw Residuals", y = "Autocorrelation") +
  theme_minimal()

# Plot 3: ACF of squared residuals
acf_sq <- acf(standardized_residuals^2, lag.max = 30, plot = FALSE)
acf_sq_df <- data.frame(Lag = acf_sq$lag[-1], ACF = acf_sq$acf[-1])

p3 <- ggplot(acf_sq_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "red") +
  labs(title = "ACF of Squared Residuals", y = "Autocorrelation") +
  theme_minimal()

# Plot 4: Histogram with density curve
p4 <- ggplot(data.frame(res = standardized_residuals), aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(title = "Residual Distribution", x = "Residuals", y = "Density") +
  theme_minimal()

# Combine plots
combined_plot <- (p1 | p2) / (p3 | p4)
print(combined_plot)

# Save plot
ggsave("residual_diagnostics.png", combined_plot, width = 12, height = 10, dpi = 300)

# 7. Export test results
# ============================================================
write.csv(lb_results, "ljung_box_results.csv", row.names = FALSE)
cat("\nTest results saved to ljung_box_results.csv\n")
cat("Diagnostic plot saved to residual_diagnostics.png\n")

# 8. Print final summary table
# ============================================================
cat("\n========== Final Results Summary ==========\n\n")
print(lb_results)
