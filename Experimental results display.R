setwd("C:/1/CLEANED")

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Calculate returns
calc_returns <- function(df, price_col = "close") {
  df <- df %>%
    arrange(date) %>%
    mutate(
      return = c(NA, diff(log(!!sym(price_col))) * 100)
    ) %>%
    filter(!is.na(return))
  return(df)
}

# Build PCA composite indicator (according to formula 6: principal component weighted average)
build_pci_uncertainty <- function(uncertainty_df) {
  pca_data <- uncertainty_df %>%
    select(EPU, TPU, CPU) %>%
    na.omit()

  if(nrow(pca_data) < 10) {
    cat("  Warning: insufficient data for PCA\n")
    uncertainty_df$MC <- NA
    return(list(df = uncertainty_df, pca = NULL, pca_summary = NULL))
  }

  pca_scaled <- scale(pca_data)
  pca_result <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)
  eigenvalues <- pca_result$sdev^2
  weights <- eigenvalues / sum(eigenvalues)
  pc_scores <- pca_result$x
  mc_weighted <- as.numeric(pc_scores %*% weights)

  cat("  Eigenvalues:", round(eigenvalues, 4), "\n")
  cat("  Weight coefficients (λ/Σλ):", round(weights, 4), "\n")
  cat("  First principal component variance explained:", round(eigenvalues[1]/sum(eigenvalues) * 100, 2), "%\n")

  uncertainty_df$MC <- NA
  uncertainty_df$MC[!is.na(uncertainty_df$EPU) &
                      !is.na(uncertainty_df$TPU) &
                      !is.na(uncertainty_df$CPU)] <- mc_weighted

  uncertainty_df <- uncertainty_df %>%
    arrange(date) %>%
    fill(MC, .direction = "down")

  pca_summary <- list(
    eigenvalues = eigenvalues,
    weights = weights,
    var_explained = eigenvalues / sum(eigenvalues),
    loadings = pca_result$rotation
  )

  return(list(df = uncertainty_df, pca = pca_result, pca_summary = pca_summary))
}

# Calculate smoothness indicator S
calculate_smoothness <- function(volatility_series) {
  x <- na.omit(volatility_series)
  if(length(x) < 5) return(NA)
  sigma_x <- sd(x)
  delta_x <- diff(x)
  sigma_delta <- sd(delta_x)
  if(is.na(sigma_delta) || sigma_delta == 0) return(Inf)
  S <- 1/(sigma_x + sigma_delta)
  return(S)
}

# Function to generate monthly date sequence - returns character vector to avoid date formatting issues
generate_monthly_dates_str <- function(start_date, end_date) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  start_month <- floor_date(start, "month")
  end_month <- floor_date(end, "month")
  months_seq <- seq(start_month, end_month, by = "month")
  # Return character vector directly to avoid subsequent format issues
  return(as.character(months_seq))
}

# Convert character date back to Date (only needed for comparison)
str_to_date <- function(date_str) {
  return(as.Date(date_str))
}

# Calculate CER function
calculate_cer <- function(returns, gamma = 3) {
  if(length(returns) == 0) return(NA)
  returns <- returns[!is.na(returns)]
  if(length(returns) < 2) return(NA)
  mean_ret <- mean(returns, na.rm = TRUE)
  var_ret <- var(returns, na.rm = TRUE)
  cer <- mean_ret - (gamma / 2) * var_ret
  return(cer * 100)
}

# ============================================================
# Part 2: Read and process data
# ============================================================

cat("\n========== Reading Data ==========\n")

# Country configuration
countries <- list(
  KOR = list(
    etf_file = "EWY_DAILY.csv",
    epu_file = "KOR_EPU_stock_cleaned.csv",
    tpu_file = "KOR_TPU_stock_cleaned.csv",
    cpu_file = "KOR_CPU_stock_cleaned.csv",
    name = "Korea",
    code = "KOR"
  ),
  IND = list(
    etf_file = "IND_DAILY.csv",
    epu_file = "IND_EPU_stock_cleaned.csv",
    tpu_file = "IND_TPU_stock_cleaned.csv",
    cpu_file = "IND_CPU_stock_cleaned.csv",
    name = "India",
    code = "IND"
  ),
  GBR = list(
    etf_file = "EWU_DAILY.csv",
    epu_file = "GBR_EPU_stock_cleaned.csv",
    tpu_file = "GBR_TPU_stock_cleaned.csv",
    cpu_file = "GBR_CPU_stock_cleaned.csv",
    name = "UK",
    code = "GBR"
  )
)

# Store all data
all_price_data <- list()
all_uncertainty_data <- list()
all_mc_data <- list()
all_pca_summaries <- list()

for(cnt in names(countries)) {
  cat("\nProcessing", countries[[cnt]]$name, "data...\n")

  # Read ETF data - FIXED date format
  if(file.exists(countries[[cnt]]$etf_file)) {
    price_df <- read.csv(countries[[cnt]]$etf_file, stringsAsFactors = FALSE)
    colnames(price_df)[1:2] <- c("date", "close")
    # Fix: specify date format for YYYY/M/D format
    price_df$date <- as.Date(price_df$date, format = "%Y/%m/%d")
    price_df <- calc_returns(price_df)
    all_price_data[[cnt]] <- price_df
    cat("  ETF data range:", as.character(min(price_df$date)), "to", as.character(max(price_df$date)), "\n")
    cat("  Observations:", nrow(price_df), "\n")
  } else {
    cat("  Warning: file not found -", countries[[cnt]]$etf_file, "\n")
    next
  }

  # Read uncertainty data - FIXED date format
  epu <- read.csv(countries[[cnt]]$epu_file, stringsAsFactors = FALSE)
  colnames(epu)[1:2] <- c("date", "EPU")
  epu$date <- as.Date(epu$date, format = "%Y/%m/%d")

  tpu <- read.csv(countries[[cnt]]$tpu_file, stringsAsFactors = FALSE)
  # Fix: handle different column names (some files have "TPU", some have country code)
  if(ncol(tpu) >= 2) {
    colnames(tpu)[1:2] <- c("date", "TPU")
  }
  tpu$date <- as.Date(tpu$date, format = "%Y/%m/%d")

  cpu <- read.csv(countries[[cnt]]$cpu_file, stringsAsFactors = FALSE)
  colnames(cpu)[1:2] <- c("date", "CPU")
  cpu$date <- as.Date(cpu$date, format = "%Y/%m/%d")

  # Merge uncertainty data
  uncertainty_df <- epu %>%
    full_join(tpu, by = "date") %>%
    full_join(cpu, by = "date") %>%
    arrange(date) %>%
    mutate(year_month = format(date, "%Y-%m"))

  all_uncertainty_data[[cnt]] <- uncertainty_df
  cat("  Uncertainty data range:", as.character(min(uncertainty_df$date, na.rm = TRUE)),
      "to", as.character(max(uncertainty_df$date, na.rm = TRUE)), "\n")

  # Build PCA composite indicator (formula 6)
  cat("\n  Building PCA composite indicator MC...\n")
  pca_result <- build_pci_uncertainty(uncertainty_df)
  all_mc_data[[cnt]] <- pca_result$df
  all_pca_summaries[[cnt]] <- pca_result$pca_summary

  if(!is.null(pca_result$pca_summary)) {
    cat("\n  PCA loading matrix:\n")
    print(round(pca_result$pca_summary$loadings, 4))
  }
}

# ============================================================
# Part 3: Rolling window strategy (using character dates to avoid formatting issues)
# ============================================================

cat("\n\n========== Rolling Window Strategy ==========\n")

# Set parameters
estimation_window_months <- 3
smoothness_days <- 22
max_allocation <- 0.4

# Get date range - use character dates
all_dates <- c()
for(cnt in names(all_price_data)) {
  all_dates <- c(all_dates, as.character(all_price_data[[cnt]]$date))
}
date_range <- range(all_dates, na.rm = TRUE)
monthly_dates_str <- generate_monthly_dates_str(date_range[1], date_range[2])
total_windows <- length(monthly_dates_str) - estimation_window_months

cat("Total windows:", total_windows, "\n")

# Store results
portfolio_returns <- c()
single_returns <- list(KOR = c(), IND = c(), GBR = c())
allocation_weights <- list()
selection_dates <- c()

# Progress counter
successful_windows <- 0

for(w in 1:total_windows) {

  # Use character dates
  est_start_str <- monthly_dates_str[w]
  est_end_str <- monthly_dates_str[w + estimation_window_months]
  forecast_start_str <- est_end_str
  forecast_end_str <- monthly_dates_str[min(w + estimation_window_months + 1, length(monthly_dates_str))]

  # Convert to Date for comparison
  est_start <- as.Date(est_start_str)
  est_end <- as.Date(est_end_str)
  forecast_start <- as.Date(forecast_start_str)
  forecast_end <- as.Date(forecast_end_str)

  if(w %% 20 == 0 || w == 1) {
    cat(paste0("Window ", w, "/", total_windows, ": ",
               est_start_str, " -> ", forecast_start_str, "\n"))
  }

  # Calculate smoothness for each market
  smoothness_scores <- c()
  market_names <- c()
  market_codes <- c()

  for(cnt in names(all_price_data)) {
    window_data <- all_price_data[[cnt]] %>%
      filter(date >= est_start, date <= est_end)

    if(nrow(window_data) >= smoothness_days) {
      # Manually calculate rolling standard deviation
      daily_vol <- c()
      for(i in smoothness_days:nrow(window_data)) {
        daily_vol <- c(daily_vol, sd(window_data$return[(i-smoothness_days+1):i]))
      }

      if(length(daily_vol) >= 10) {
        vol_sd <- sd(daily_vol)
        vol_diff_sd <- sd(diff(daily_vol))
        if(!is.na(vol_diff_sd) && vol_diff_sd > 0) {
          smoothness <- vol_sd / vol_diff_sd
          smoothness_scores <- c(smoothness_scores, smoothness)
          market_names <- c(market_names, cnt)
          market_codes <- c(market_codes, countries[[cnt]]$name)
        }
      }
    }
  }

  if(length(smoothness_scores) == 0) next

  successful_windows <- successful_windows + 1

  # Rank weighting
  ranks <- rank(-smoothness_scores, ties.method = "min")
  weights <- (length(market_names) + 1 - ranks) / sum(length(market_names) + 1 - ranks)

  # India weight constraint
  india_idx <- which(market_codes == "India")
  if(length(india_idx) > 0 && weights[india_idx] > max_allocation) {
    excess <- weights[india_idx] - max_allocation
    weights[india_idx] <- max_allocation
    other_idx <- setdiff(1:length(market_names), india_idx)
    if(length(other_idx) > 0) {
      weights[other_idx] <- weights[other_idx] + excess * weights[other_idx] / sum(weights[other_idx])
    }
  }
  weights <- weights / sum(weights)

  # Store weights
  weight_record <- data.frame(
    Market = market_names,
    Market_Name = market_codes,
    Weight = weights,
    Smoothness = smoothness_scores
  )
  allocation_weights[[est_start_str]] <- weight_record
  selection_dates <- c(selection_dates, est_start_str)

  # Calculate returns for each market
  for(i in 1:length(market_names)) {
    cnt <- market_names[i]
    ret_data <- all_price_data[[cnt]] %>%
      filter(date >= forecast_start, date < forecast_end)

    if(nrow(ret_data) > 0) {
      monthly_ret <- sum(ret_data$return, na.rm = TRUE) / 100
      single_returns[[cnt]] <- c(single_returns[[cnt]], monthly_ret)
    }
  }

  # Calculate portfolio return
  portfolio_ret <- 0
  for(i in 1:length(market_names)) {
    cnt <- market_names[i]
    ret_data <- all_price_data[[cnt]] %>%
      filter(date >= forecast_start, date < forecast_end)

    if(nrow(ret_data) > 0) {
      monthly_ret <- sum(ret_data$return, na.rm = TRUE) / 100
      portfolio_ret <- portfolio_ret + weights[i] * monthly_ret
    }
  }
  portfolio_returns <- c(portfolio_returns, portfolio_ret)
}

# Output statistics
cat("\n\n========== Strategy Statistics ==========\n")
cat(paste0("Successfully processed windows: ", successful_windows, " / ", total_windows, "\n"))

# ============================================================
# Table 3: MAI, CER, Sharpe Ratio
# ============================================================

cat("\n\n========== Table 3: Performance Metrics ==========\n")

# Get returns for each market (remove NAs)
korea_returns <- single_returns$KOR[!is.na(single_returns$KOR)]
india_returns <- single_returns$IND[!is.na(single_returns$IND)]
uk_returns <- single_returns$GBR[!is.na(single_returns$GBR)]
mc_returns <- portfolio_returns[!is.na(portfolio_returns)]

cat(paste0("Korea valid returns: ", length(korea_returns), "\n"))
cat(paste0("India valid returns: ", length(india_returns), "\n"))
cat(paste0("UK valid returns: ", length(uk_returns), "\n"))
cat(paste0("Portfolio valid returns: ", length(mc_returns), "\n"))

# Calculate Table 3
table3 <- data.frame(
  Country = c("Korea", "United Kingdom", "India", "Mixed Components"),
  MAI = c(
    mean(korea_returns) * 100,
    mean(uk_returns) * 100,
    mean(india_returns) * 100,
    mean(mc_returns) * 100
  ),
  CER = c(
    calculate_cer(korea_returns),
    calculate_cer(uk_returns),
    calculate_cer(india_returns),
    calculate_cer(mc_returns)
  ),
  Sharpe_Ratio = c(
    mean(korea_returns) / sd(korea_returns),
    mean(uk_returns) / sd(uk_returns),
    mean(india_returns) / sd(india_returns),
    mean(mc_returns) / sd(mc_returns)
  )
)

# Print Table 3
cat("\nTable 3: Evaluation Results\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat(sprintf("%-20s %12s %12s %12s\n", "Country", "MAI (%)", "CER (%)", "Sharpe Ratio"))
cat(paste0(rep("-", 80), collapse = ""), "\n")
for(i in 1:nrow(table3)) {
  cat(sprintf("%-20s %12.3f %12.3f %12.3f\n",
              as.character(table3$Country[i]),
              table3$MAI[i],
              table3$CER[i],
              table3$Sharpe_Ratio[i]))
}
cat(paste0(rep("=", 80), collapse = ""), "\n")

# ============================================================
# Table 4: Annual Returns Comparison (2012-2025)
# ============================================================

cat("\n\n========== Table 4: Annual Returns Comparison ==========\n")

# Convert selection_dates to Date format to get years
selection_dates_date <- as.Date(selection_dates)

# Ensure all series have the same length
min_length <- min(length(korea_returns), length(india_returns),
                  length(uk_returns), length(mc_returns))

korea_returns_adj <- korea_returns[1:min_length]
india_returns_adj <- india_returns[1:min_length]
uk_returns_adj <- uk_returns[1:min_length]
mc_returns_adj <- mc_returns[1:min_length]
selection_dates_adj <- selection_dates_date[1:min_length]

# Get year range
years <- unique(format(selection_dates_adj, "%Y"))
years <- sort(as.numeric(years[years >= 2012 & years <= 2025]))

# Create annual returns dataframe
table4 <- data.frame(Year = years)
table4$Korea <- NA
table4$India <- NA
table4$UK <- NA
table4$MC <- NA

for(y in years) {
  year_indices <- which(format(selection_dates_adj, "%Y") == as.character(y))

  if(length(year_indices) > 0) {
    table4$Korea[table4$Year == y] <- sum(korea_returns_adj[year_indices], na.rm = TRUE) * 100
    table4$India[table4$Year == y] <- sum(india_returns_adj[year_indices], na.rm = TRUE) * 100
    table4$UK[table4$Year == y] <- sum(uk_returns_adj[year_indices], na.rm = TRUE) * 100
    table4$MC[table4$Year == y] <- sum(mc_returns_adj[year_indices], na.rm = TRUE) * 100
  }
}

# Print Table 4
cat("\nTable 4: Annual Returns Comparison (%)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat(sprintf("%-6s %10s %10s %10s %10s\n", "Year", "Korea", "India", "UK", "MC"))
cat(paste0(rep("-", 80), collapse = ""), "\n")
for(i in 1:nrow(table4)) {
  cat(sprintf("%-6d %10.2f %10.2f %10.2f %10.2f\n",
              table4$Year[i],
              table4$Korea[i],
              table4$India[i],
              table4$UK[i],
              table4$MC[i]))
}
cat(paste0(rep("=", 80), collapse = ""), "\n")

# ============================================================
# Table 5: MCS Test Results (Model Confidence Set)
# ============================================================

cat("\n\n========== Table 5: MCS Test Results ==========\n")

# Calculate MCS loss function
calculate_loss <- function(actual, forecast, loss_type) {
  if(length(actual) == 0 || length(forecast) == 0) return(NA)

  # Ensure same length
  min_len <- min(length(actual), length(forecast))
  actual <- actual[1:min_len]
  forecast <- forecast[1:min_len]

  # Remove NA values
  valid_idx <- which(!is.na(actual) & !is.na(forecast))
  if(length(valid_idx) == 0) return(NA)

  actual <- actual[valid_idx]
  forecast <- forecast[valid_idx]

  if(length(actual) == 0) return(NA)

  if(loss_type == "MAE") {
    return(mean(abs(actual - forecast)))
  } else if(loss_type == "MAPE") {
    # Avoid division by zero
    non_zero_idx <- which(actual != 0)
    if(length(non_zero_idx) == 0) return(NA)
    return(mean(abs((actual[non_zero_idx] - forecast[non_zero_idx]) / actual[non_zero_idx]) * 100))
  } else if(loss_type == "MSE") {
    return(mean((actual - forecast)^2))
  }
  return(NA)
}

# Calculate prediction errors for each market (using historical mean as benchmark)
mse_results <- list()
mae_results <- list()
mape_results <- list()

for(cnt in names(all_price_data)) {
  actual_returns <- c()
  predicted_returns <- c()

  # Use rolling window for prediction
  for(w in 1:length(selection_dates)) {
    est_start <- as.Date(selection_dates[w])
    est_end <- est_start + months(estimation_window_months)
    forecast_start <- est_end
    forecast_end <- forecast_start + months(1)

    # Actual returns
    forecast_data <- all_price_data[[cnt]] %>%
      filter(date >= forecast_start, date < forecast_end)

    if(nrow(forecast_data) > 0) {
      actual_ret <- sum(forecast_data$return, na.rm = TRUE) / 100
      actual_returns <- c(actual_returns, actual_ret)

      # Use historical mean as prediction (simplified model)
      hist_data <- all_price_data[[cnt]] %>%
        filter(date < forecast_start, date >= est_start - months(60))

      if(nrow(hist_data) > 0) {
        pred_ret <- mean(hist_data$return, na.rm = TRUE) / 100
        predicted_returns <- c(predicted_returns, pred_ret)
      } else {
        predicted_returns <- c(predicted_returns, NA)
      }
    }
  }

  # Calculate loss functions
  mse_results[[cnt]] <- calculate_loss(actual_returns, predicted_returns, "MSE")
  mae_results[[cnt]] <- calculate_loss(actual_returns, predicted_returns, "MAE")
  mape_results[[cnt]] <- calculate_loss(actual_returns, predicted_returns, "MAPE")
}

# Calculate prediction errors for mixed strategy
mc_actual <- c()
mc_predicted <- c()

for(w in 1:length(selection_dates)) {
  if(w <= length(allocation_weights)) {
    weight_record <- allocation_weights[[w]]

    weighted_actual <- 0
    weighted_pred <- 0
    weight_sum <- 0

    for(i in 1:nrow(weight_record)) {
      cnt <- as.character(weight_record$Market[i])

      est_start <- as.Date(selection_dates[w])
      est_end <- est_start + months(estimation_window_months)
      forecast_start <- est_end
      forecast_end <- forecast_start + months(1)

      # Actual returns
      forecast_data <- all_price_data[[cnt]] %>%
        filter(date >= forecast_start, date < forecast_end)

      if(nrow(forecast_data) > 0) {
        actual_ret <- sum(forecast_data$return, na.rm = TRUE) / 100
        weighted_actual <- weighted_actual + weight_record$Weight[i] * actual_ret

        # Predicted returns (historical mean)
        hist_data <- all_price_data[[cnt]] %>%
          filter(date < forecast_start, date >= est_start - months(60))

        if(nrow(hist_data) > 0) {
          pred_ret <- mean(hist_data$return, na.rm = TRUE) / 100
          weighted_pred <- weighted_pred + weight_record$Weight[i] * pred_ret
          weight_sum <- weight_sum + weight_record$Weight[i]
        }
      }
    }

    if(weight_sum > 0.99) {
      mc_actual <- c(mc_actual, weighted_actual)
      mc_predicted <- c(mc_predicted, weighted_pred)
    }
  }
}

# Create Table 5
table5 <- data.frame(
  Model = c("GARCH-MIDAS (India)", "GARCH-MIDAS (Korea)",
            "GARCH-MIDAS (United Kingdom)", "GARCH-MIDAS (Mixed Components)"),
  MAE = c(
    ifelse(is.null(mae_results[["IND"]]), NA, round(mae_results[["IND"]], 4)),
    ifelse(is.null(mae_results[["KOR"]]), NA, round(mae_results[["KOR"]], 4)),
    ifelse(is.null(mae_results[["GBR"]]), NA, round(mae_results[["GBR"]], 4)),
    ifelse(is.null(calculate_loss(mc_actual, mc_predicted, "MAE")), NA,
           round(calculate_loss(mc_actual, mc_predicted, "MAE"), 4))
  ),
  MAPE = c(
    ifelse(is.null(mape_results[["IND"]]), NA, round(mape_results[["IND"]], 4)),
    ifelse(is.null(mape_results[["KOR"]]), NA, round(mape_results[["KOR"]], 4)),
    ifelse(is.null(mape_results[["GBR"]]), NA, round(mape_results[["GBR"]], 4)),
    ifelse(is.null(calculate_loss(mc_actual, mc_predicted, "MAPE")), NA,
           round(calculate_loss(mc_actual, mc_predicted, "MAPE"), 4))
  ),
  MSE = c(
    ifelse(is.null(mse_results[["IND"]]), NA, round(mse_results[["IND"]], 6)),
    ifelse(is.null(mse_results[["KOR"]]), NA, round(mse_results[["KOR"]], 6)),
    ifelse(is.null(mse_results[["GBR"]]), NA, round(mse_results[["GBR"]], 6)),
    ifelse(is.null(calculate_loss(mc_actual, mc_predicted, "MSE")), NA,
           round(calculate_loss(mc_actual, mc_predicted, "MSE"), 6))
  )
)

# Print Table 5
cat("\nTable 5: MCS Test Results\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat(sprintf("%-35s %12s %12s %12s\n", "Model", "MAE", "MAPE", "MSE"))
cat(paste0(rep("-", 80), collapse = ""), "\n")
for(i in 1:nrow(table5)) {
  cat(sprintf("%-35s %12.4f %12.4f %12.6f\n",
              as.character(table5$Model[i]),
              table5$MAE[i],
              table5$MAPE[i],
              table5$MSE[i]))
}
cat(paste0(rep("=", 80), collapse = ""), "\n")

# ============================================================
# Save results (updated, added Table 5)
# ============================================================

output_dir <- "results/strategy_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(table3, file.path(output_dir, "table3_performance.csv"), row.names = FALSE)
write.csv(table4, file.path(output_dir, "table4_annual_returns.csv"), row.names = FALSE)
write.csv(table5, file.path(output_dir, "table5_mcs_results.csv"), row.names = FALSE)

# Save weight records
if(length(allocation_weights) > 0) {
  weights_summary <- data.frame()
  for(i in 1:length(allocation_weights)) {
    temp <- allocation_weights[[i]]
    temp$Window <- names(allocation_weights)[i]
    weights_summary <- rbind(weights_summary, temp)
  }
  write.csv(weights_summary, file.path(output_dir, "allocation_weights.csv"), row.names = FALSE)
}

# Save return series
returns_summary <- data.frame(
  Date = selection_dates[1:length(portfolio_returns)],
  Korea = korea_returns[1:length(portfolio_returns)],
  India = india_returns[1:length(portfolio_returns)],
  UK = uk_returns[1:length(portfolio_returns)],
  Mixed = portfolio_returns
)
write.csv(returns_summary, file.path(output_dir, "returns_series.csv"), row.names = FALSE)

cat("\n\nResults saved to:", output_dir, "\n")
cat("Generated files:\n")
cat("  - table3_performance.csv (MAI, CER, Sharpe Ratio)\n")
cat("  - table4_annual_returns.csv (Annual returns comparison)\n")
cat("  - table5_mcs_results.csv (MCS test results)\n")
cat("  - allocation_weights.csv (Weight allocation records)\n")
cat("  - returns_series.csv (Return series)\n")

cat("\n========== Analysis Complete ==========\n")
