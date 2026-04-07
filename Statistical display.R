# ============================================================
# Part 1: Define helper functions
# ============================================================
library(dplyr)
library(lubridate)  # provides floor_date function
library(zoo)
library(tidyr)
setwd("C:/1/CLEANED")
# Check and load rugarch (for GARCH-MIDAS)

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
# MC = (λ1*PC1 + λ2*PC2 + λ3*PC3) / (λ1+λ2+λ3)
build_pci_uncertainty <- function(uncertainty_df) {
  # Prepare PCA data
  pca_data <- uncertainty_df %>%
    select(EPU, TPU, CPU) %>%
    na.omit()

  if(nrow(pca_data) < 10) {
    cat("  Warning: insufficient data for PCA\n")
    uncertainty_df$MC <- NA
    return(list(df = uncertainty_df, pca = NULL, pca_summary = NULL))
  }

  # Standardize
  pca_scaled <- scale(pca_data)

  # Perform PCA
  pca_result <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)

  # Get eigenvalues
  eigenvalues <- pca_result$sdev^2

  # Calculate weighted principal component according to formula (6)
  # MC = (λ1*PC1 + λ2*PC2 + λ3*PC3) / (λ1+λ2+λ3)
  weights <- eigenvalues / sum(eigenvalues)
  pc_scores <- pca_result$x
  mc_weighted <- as.numeric(pc_scores %*% weights)

  cat("  Eigenvalues:", round(eigenvalues, 4), "\n")
  cat("  Weight coefficients (λ/Σλ):", round(weights, 4), "\n")
  cat("  First principal component variance explained:", round(eigenvalues[1]/sum(eigenvalues) * 100, 2), "%\n")

  # Add MC to dataframe
  uncertainty_df$MC <- NA
  uncertainty_df$MC[!is.na(uncertainty_df$EPU) &
                      !is.na(uncertainty_df$TPU) &
                      !is.na(uncertainty_df$CPU)] <- mc_weighted

  # Forward fill missing values
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
  S <- 1 / (sigma_delta + sigma_x)  # Only change
  return(S)
}

# Function to generate monthly date sequence
generate_monthly_dates <- function(start_date, end_date) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  start_month <- floor_date(start, "month")
  end_month <- floor_date(end, "month")
  months_seq <- seq(start_month, end_month, by = "month")
  return(months_seq)
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

  # Read ETF data
  if(file.exists(countries[[cnt]]$etf_file)) {
    price_df <- read.csv(countries[[cnt]]$etf_file, stringsAsFactors = FALSE)
    colnames(price_df)[1:2] <- c("date", "close")
    price_df$date <- as.Date(price_df$date)
    price_df <- calc_returns(price_df)
    all_price_data[[cnt]] <- price_df
    cat("  ETF data range:", min(price_df$date), "to", max(price_df$date), "\n")
    cat("  Observations:", nrow(price_df), "\n")
  } else {
    cat("  Warning: file not found -", countries[[cnt]]$etf_file, "\n")
    next
  }

  # Read uncertainty data
  epu <- read.csv(countries[[cnt]]$epu_file, stringsAsFactors = FALSE)
  colnames(epu)[1:2] <- c("date", "EPU")
  epu$date <- as.Date(epu$date)

  tpu <- read.csv(countries[[cnt]]$tpu_file, stringsAsFactors = FALSE)
  colnames(tpu)[1:2] <- c("date", "TPU")
  tpu$date <- as.Date(tpu$date)

  cpu <- read.csv(countries[[cnt]]$cpu_file, stringsAsFactors = FALSE)
  colnames(cpu)[1:2] <- c("date", "CPU")
  cpu$date <- as.Date(cpu$date)

  # Merge uncertainty data
  uncertainty_df <- epu %>%
    full_join(tpu, by = "date") %>%
    full_join(cpu, by = "date") %>%
    arrange(date) %>%
    mutate(year_month = format(date, "%Y-%m"))

  all_uncertainty_data[[cnt]] <- uncertainty_df
  cat("  Uncertainty data range:", min(uncertainty_df$date, na.rm = TRUE),
      "to", max(uncertainty_df$date, na.rm = TRUE), "\n")

  # Build PCA composite indicator (formula 6)
  cat("\n  Building PCA composite indicator MC...\n")
  pca_result <- build_pci_uncertainty(uncertainty_df)
  all_mc_data[[cnt]] <- pca_result$df
  all_pca_summaries[[cnt]] <- pca_result$pca_summary

  cat("\n  PCA loading matrix:\n")
  print(round(pca_result$pca_summary$loadings, 4))
}

# ============================================================
# Part 3: Rolling window strategy (GARCH-MIDAS + weight constraints)
# ============================================================

cat("\n\n========== Rolling Window Strategy ==========\n")
cat("Based on formula (6) composite indicator MC\n")
cat("Using GARCH-MIDAS to estimate volatility\n\n")

# Set parameters
estimation_window_months <- 3  # Estimation window months (5 years)
midas_lag <- 12  # MIDAS lag order k = 12
smoothness_days <- 22  # Smoothness calculation window
max_allocation <- 0.4  # Maximum weight constraint for India market

# Get date range for all markets
all_dates <- c()
for(cnt in names(all_price_data)) {
  all_dates <- c(all_dates, all_price_data[[cnt]]$date)
}

date_range <- range(all_dates, na.rm = TRUE)
monthly_dates <- generate_monthly_dates(date_range[1], date_range[2])

total_windows <- length(monthly_dates) - estimation_window_months

cat("Monthly points:", length(monthly_dates), "\n")
cat("Estimation window (months):", estimation_window_months, "\n")
cat("MIDAS lag order k:", midas_lag, "\n")
cat("India market max weight:", max_allocation * 100, "%\n")
cat("Total windows:", total_windows, "\n\n")

# Store results
portfolio_returns <- c()
allocation_weights <- list()
selection_dates <- c()

for(w in 1:total_windows) {

  # Estimation window
  est_start <- monthly_dates[w]
  est_end <- monthly_dates[w + estimation_window_months]

  # Forecast window (next month)
  forecast_start <- est_end
  forecast_end <- monthly_dates[min(w + estimation_window_months + 1, length(monthly_dates))]

  if(w %% 5 == 0 || w == 1) {
    cat(sprintf("\nWindow %d/%d:\n", w, total_windows))
    cat(sprintf("  Estimation period: %s to %s\n", format(est_start, "%Y-%m"), format(est_end, "%Y-%m")))
    cat(sprintf("  Forecast period: %s\n", format(forecast_start, "%Y-%m")))
  }

  # Calculate smoothness for each market
  smoothness_scores <- c()
  market_names <- c()
  market_codes <- c()

  for(cnt in names(all_price_data)) {

    # Get estimation window data
    price_data <- all_price_data[[cnt]]
    window_data <- price_data %>%
      filter(date >= est_start, date <= est_end)

    if(nrow(window_data) < smoothness_days) {
      smoothness_scores <- c(smoothness_scores, NA)
      market_names <- c(market_names, cnt)
      market_codes <- c(market_codes, countries[[cnt]]$name)
      next
    }

    # Get MC data
    mc_data <- all_mc_data[[cnt]]
    window_mc <- mc_data %>%
      filter(date >= est_start, date <= est_end)

    if(nrow(window_mc) < midas_lag) {
      smoothness_scores <- c(smoothness_scores, NA)
      market_names <- c(market_names, cnt)
      market_codes <- c(market_codes, countries[[cnt]]$name)
      next
    }

    # ========== GARCH-MIDAS volatility estimation ==========
    tryCatch({
      # Prepare data
      daily_for_model <- window_data %>%
        mutate(year_month = format(date, "%Y-%m"))

      monthly_for_model <- window_mc %>%
        select(year_month, value = MC)

      model_data <- daily_for_model %>%
        left_join(monthly_for_model, by = "year_month") %>%
        filter(!is.na(value)) %>%
        mutate(
          low_freq_date = as.Date(paste0(year_month, "-01")),
          x = as.numeric(scale(value))
        )

      n_months <- length(unique(model_data$low_freq_date))
      if(n_months < midas_lag || nrow(model_data) < 200) {
        stop("Insufficient data")
      }

      # GARCH-MIDAS estimation
      mfgarch_data <- data.frame(
        date = model_data$date,
        return = model_data$return,
        x = model_data$x,
        low_freq = model_data$low_freq_date
      )

      fit <- fit_mfgarch(
        data = mfgarch_data,
        y = "return",
        x = "x",
        K = midas_lag,
        low.freq = "low_freq",
        gamma = TRUE,
        weighting = "beta.restricted",
        control = list(maxit = 500, trace = 0)
      )

      # Extract tau
      if("tau" %in% names(fit) && length(fit$tau) > 0) {
        tau_df <- data.frame(
          year_month = names(fit$tau),
          tau = as.numeric(fit$tau)
        )

        tau_daily <- model_data %>%
          left_join(tau_df, by = "year_month") %>%
          pull(tau)

        tau_daily <- tau_daily[!is.na(tau_daily)]

        if(length(tau_daily) >= 10) {
          smoothness <- calculate_smoothness(tau_daily)
          smoothness_scores <- c(smoothness_scores, smoothness)
          market_names <- c(market_names, cnt)
          market_codes <- c(market_codes, countries[[cnt]]$name)
          next
        }
      }

      # Fallback: rolling standard deviation
      daily_vol <- zoo::rollapply(window_data$return, width = smoothness_days,
                                  FUN = sd, fill = NA, align = "right")
      daily_vol <- daily_vol[!is.na(daily_vol)]

      if(length(daily_vol) >= 10) {
        smoothness <- calculate_smoothness(daily_vol)
        smoothness_scores <- c(smoothness_scores, smoothness)
        market_names <- c(market_names, cnt)
        market_codes <- c(market_codes, countries[[cnt]]$name)
      } else {
        smoothness_scores <- c(smoothness_scores, NA)
        market_names <- c(market_names, cnt)
        market_codes <- c(market_codes, countries[[cnt]]$name)
      }

    }, error = function(e) {
      # Fallback to rolling standard deviation on error
      daily_vol <- zoo::rollapply(window_data$return, width = smoothness_days,
                                  FUN = sd, fill = NA, align = "right")
      daily_vol <- daily_vol[!is.na(daily_vol)]

      if(length(daily_vol) >= 10) {
        smoothness <- calculate_smoothness(daily_vol)
        smoothness_scores <- c(smoothness_scores, smoothness)
        market_names <- c(market_names, cnt)
        market_codes <- c(market_codes, countries[[cnt]]$name)
      } else {
        smoothness_scores <- c(smoothness_scores, NA)
        market_names <- c(market_names, cnt)
        market_codes <- c(market_codes, countries[[cnt]]$name)
      }
    })
  }

  # Calculate weights
  valid_idx <- which(!is.na(smoothness_scores))
  if(length(valid_idx) == 0) {
    next
  }

  valid_scores <- smoothness_scores[valid_idx]
  valid_markets <- market_names[valid_idx]
  valid_codes <- market_codes[valid_idx]

  # Rank weighting
  ranks <- rank(-valid_scores, ties.method = "min")
  raw_weights <- (length(valid_markets) + 1 - ranks) / sum(length(valid_markets) + 1 - ranks)

  # India weight constraint
  india_idx <- which(valid_codes == "India")
  if(length(india_idx) > 0 && raw_weights[india_idx] > max_allocation) {
    excess <- raw_weights[india_idx] - max_allocation
    raw_weights[india_idx] <- max_allocation
    other_idx <- setdiff(1:length(valid_markets), india_idx)
    if(length(other_idx) > 0 && sum(raw_weights[other_idx]) > 0) {
      ratio <- raw_weights[other_idx] / sum(raw_weights[other_idx])
      raw_weights[other_idx] <- raw_weights[other_idx] + excess * ratio
    }
  }

  final_weights <- raw_weights / sum(raw_weights)

  # Store weights
  weight_record <- data.frame(
    Market = valid_markets,
    Weight = final_weights,
    Smoothness = valid_scores
  )
  allocation_weights[[as.character(est_start)]] <- weight_record
  selection_dates <- c(selection_dates, est_start)

  if(w %% 10 == 0 || w == 1) {
    cat("  Weight allocation:\n")
    for(i in 1:nrow(weight_record)) {
      cat(sprintf("    %s: %.1f%% (Smoothness=%.4f)\n",
                  countries[[weight_record$Market[i]]]$name,
                  weight_record$Weight[i] * 100,
                  weight_record$Smoothness[i]))
    }
  }

  # Calculate portfolio return
  portfolio_return <- 0
  for(i in 1:nrow(weight_record)) {
    cnt <- weight_record$Market[i]
    forecast_data <- all_price_data[[cnt]] %>%
      filter(date >= forecast_start, date < forecast_end)

    if(nrow(forecast_data) > 0) {
      monthly_return <- sum(forecast_data$return, na.rm = TRUE) / 100
      portfolio_return <- portfolio_return + weight_record$Weight[i] * monthly_return
    }
  }
  portfolio_returns <- c(portfolio_returns, portfolio_return)
}
# ============================================================
# Add debug information at the beginning of Part 3
# ============================================================

cat("\n\n========== Rolling Window Strategy ==========\n")
cat("Based on formula (6) composite indicator MC\n")
cat("Using GARCH-MIDAS to estimate volatility\n\n")

# Set parameters
estimation_window_months <- 60  # Estimation window months (5 years)
midas_lag <- 12  # MIDAS lag order k = 12
smoothness_days <- 22  # Smoothness calculation window
max_allocation <- 0.4  # Maximum weight constraint for India market

# Get date range for all markets
all_dates <- c()
for(cnt in names(all_price_data)) {
  all_dates <- c(all_dates, all_price_data[[cnt]]$date)
}

date_range <- range(all_dates, na.rm = TRUE)
cat("\nData range check:\n")
cat("  Earliest date:", as.character(date_range[1]), "\n")
cat("  Latest date:", as.character(date_range[2]), "\n")

monthly_dates <- generate_monthly_dates(date_range[1], date_range[2])
cat("  Monthly points:", length(monthly_dates), "\n")
cat("  First 6 monthly points:", as.character(head(monthly_dates, 6)), "\n")
cat("  Last 6 monthly points:", as.character(tail(monthly_dates, 6)), "\n")

total_windows <- length(monthly_dates) - estimation_window_months
cat("  Total windows:", total_windows, "\n")

if(total_windows <= 0) {
  cat("Error: Number of windows is zero or negative, please check data range\n")
  stop("Insufficient windows")
}

# Check data volume for each market
cat("\nData volume check by market:\n")
for(cnt in names(all_price_data)) {
  cat(sprintf("  %s: Daily data %d rows, MC data %d rows\n",
              countries[[cnt]]$name,
              nrow(all_price_data[[cnt]]),
              nrow(all_mc_data[[cnt]])))
}

# Store results
portfolio_returns <- c()
allocation_weights <- list()
selection_dates <- c()

# Add counters
successful_windows <- 0
failed_windows <- 0

cat("\nStarting rolling window analysis...\n")

for(w in 1:min(total_windows, 10)) {  # Test first 10 windows

  # Estimation window
  est_start <- monthly_dates[w]
  est_end <- monthly_dates[w + estimation_window_months]

  # Forecast window (next month)
  forecast_start <- est_end
  forecast_end <- monthly_dates[min(w + estimation_window_months + 1, length(monthly_dates))]

  cat(sprintf("\nWindow %d/%d:\n", w, total_windows))
  cat(sprintf("  Estimation period: %s to %s\n", format(est_start, "%Y-%m"), format(est_end, "%Y-%m")))
  cat(sprintf("  Forecast period: %s\n", format(forecast_start, "%Y-%m")))

  # Calculate smoothness for each market
  smoothness_scores <- c()
  market_names <- c()
  market_codes <- c()

  for(cnt in names(all_price_data)) {

    # Get estimation window data
    price_data <- all_price_data[[cnt]]
    window_data <- price_data %>%
      filter(date >= est_start, date <= est_end)

    cat(sprintf("    %s: Daily data in window %d days\n", countries[[cnt]]$name, nrow(window_data)))

    if(nrow(window_data) < smoothness_days) {
      cat(sprintf("      Insufficient data (%d < %d)\n", nrow(window_data), smoothness_days))
      smoothness_scores <- c(smoothness_scores, NA)
      market_names <- c(market_names, cnt)
      market_codes <- c(market_codes, countries[[cnt]]$name)
      next
    }

    # Get MC data
    mc_data <- all_mc_data[[cnt]]
    window_mc <- mc_data %>%
      filter(date >= est_start, date <= est_end)

    cat(sprintf("      MC data in window %d months\n", nrow(window_mc)))

    if(nrow(window_mc) < midas_lag) {
      cat(sprintf("      Insufficient MC data (%d < %d)\n", nrow(window_mc), midas_lag))
      smoothness_scores <- c(smoothness_scores, NA)
      market_names <- c(market_names, cnt)
      market_codes <- c(market_codes, countries[[cnt]]$name)
      next
    }

    # ========== Use rolling standard deviation (test simple method first) ==========
    # Test with simple method first to ensure logic is correct
    daily_vol <- zoo::rollapply(window_data$return, width = smoothness_days,
                                FUN = sd, fill = NA, align = "right")
    daily_vol <- daily_vol[!is.na(daily_vol)]

    if(length(daily_vol) >= 10) {
      smoothness <- calculate_smoothness(daily_vol)
      smoothness_scores <- c(smoothness_scores, smoothness)
      market_names <- c(market_names, cnt)
      market_codes <- c(market_codes, countries[[cnt]]$name)
      cat(sprintf("      Smoothness = %.4f (using rolling standard deviation)\n", smoothness))
    } else {
      cat(sprintf("      Insufficient volatility data (%d < 10)\n", length(daily_vol)))
      smoothness_scores <- c(smoothness_scores, NA)
      market_names <- c(market_names, cnt)
      market_codes <- c(market_codes, countries[[cnt]]$name)
    }
  }

  # Calculate weights
  valid_idx <- which(!is.na(smoothness_scores))
  cat(sprintf("  Valid smoothness scores: %d\n", length(valid_idx)))

  if(length(valid_idx) == 0) {
    cat("  No valid scores, skipping this window\n")
    failed_windows <- failed_windows + 1
    next
  }

  successful_windows <- successful_windows + 1

  valid_scores <- smoothness_scores[valid_idx]
  valid_markets <- market_names[valid_idx]
  valid_codes <- market_codes[valid_idx]

  # Rank weighting
  ranks <- rank(-valid_scores, ties.method = "min")
  raw_weights <- (length(valid_markets) + 1 - ranks) / sum(length(valid_markets) + 1 - ranks)

  # India weight constraint
  india_idx <- which(valid_codes == "India")
  if(length(india_idx) > 0 && raw_weights[india_idx] > max_allocation) {
    excess <- raw_weights[india_idx] - max_allocation
    raw_weights[india_idx] <- max_allocation
    other_idx <- setdiff(1:length(valid_markets), india_idx)
    if(length(other_idx) > 0 && sum(raw_weights[other_idx]) > 0) {
      ratio <- raw_weights[other_idx] / sum(raw_weights[other_idx])
      raw_weights[other_idx] <- raw_weights[other_idx] + excess * ratio
    }
  }

  final_weights <- raw_weights / sum(raw_weights)

  # Store weights
  weight_record <- data.frame(
    Market = valid_markets,
    Weight = final_weights,
    Smoothness = valid_scores
  )
  allocation_weights[[as.character(est_start)]] <- weight_record
  selection_dates <- c(selection_dates, est_start)

  cat("  Weight allocation:\n")
  for(i in 1:nrow(weight_record)) {
    cat(sprintf("    %s: %.1f%% (Smoothness=%.4f)\n",
                countries[[weight_record$Market[i]]]$name,
                weight_record$Weight[i] * 100,
                weight_record$Smoothness[i]))
  }

  # Calculate portfolio return
  portfolio_return <- 0
  for(i in 1:nrow(weight_record)) {
    cnt <- weight_record$Market[i]
    forecast_data <- all_price_data[[cnt]] %>%
      filter(date >= forecast_start, date < forecast_end)

    if(nrow(forecast_data) > 0) {
      monthly_return <- sum(forecast_data$return, na.rm = TRUE) / 100
      portfolio_return <- portfolio_return + weight_record$Weight[i] * monthly_return
      cat(sprintf("    %s forecast period return: %.2f%%\n",
                  countries[[cnt]]$name, monthly_return * 100))
    } else {
      cat(sprintf("    %s no data in forecast period\n", countries[[cnt]]$name))
    }
  }
  portfolio_returns <- c(portfolio_returns, portfolio_return)
  cat(sprintf("  Portfolio return: %.2f%%\n", portfolio_return * 100))
}

cat("\n\nDebug Summary:\n")
cat(sprintf("  Successfully processed windows: %d\n", successful_windows))
cat(sprintf("  Failed windows: %d\n", failed_windows))
cat(sprintf("  Stored portfolio returns: %d\n", length(portfolio_returns)))

# If first 10 windows all failed, data has issues
if(successful_windows == 0) {
  cat("\nError: All windows failed! Please check:\n")
  cat("1. Whether date ranges are correct\n")
  cat("2. Whether MC data for each market is complete\n")
  cat("3. Whether smoothness calculation window (smoothness_days) is reasonable\n")
  stop("No valid results")
}

# ============================================================
# Part 4: Strategy return and statistics calculation
# ============================================================

cat("\n\n========== Strategy Return and Statistics Calculation ==========\n")

# Ensure data exists
if(length(portfolio_returns) == 0) {
  cat("Error: No portfolio return data\n")
  stop("No data")
}

# Create return dataframe
returns_df <- data.frame(
  Date = selection_dates[1:length(portfolio_returns)],
  Return = portfolio_returns
)

# Remove NA values
returns_df <- returns_df %>%
  filter(!is.na(Return))

# ============================================================
# 1. Basic statistics
# ============================================================

cat("\n【1. Basic Statistics】\n")

# Calculate cumulative return
returns_df$Cumulative <- cumprod(1 + returns_df$Return)

# Total return
total_return <- (tail(returns_df$Cumulative, 1) - 1) * 100

# Annualized return (assuming 12 months)
n_months <- nrow(returns_df)
annual_return <- ( (1 + mean(returns_df$Return))^12 - 1 ) * 100

# Average return
avg_monthly_return <- mean(returns_df$Return) * 100
avg_daily_return <- avg_monthly_return / 22  # Assuming 22 trading days per month

# Return standard deviation
std_monthly_return <- sd(returns_df$Return) * 100

# Sharpe ratio (assuming risk-free rate = 0)
sharpe_ratio <- mean(returns_df$Return) / sd(returns_df$Return)

# Annualized Sharpe ratio
annual_sharpe <- sharpe_ratio * sqrt(12)

# Win rate
win_rate <- sum(returns_df$Return > 0) / nrow(returns_df) * 100

# Win-loss ratio
positive_returns <- returns_df$Return[returns_df$Return > 0]
negative_returns <- returns_df$Return[returns_df$Return < 0]
avg_win <- mean(positive_returns) * 100
avg_loss <- mean(negative_returns) * 100
win_loss_ratio <- abs(avg_win / avg_loss)

cat(sprintf("  Total return: %.2f%%\n", total_return))
cat(sprintf("  Annualized return: %.2f%%\n", annual_return))
cat(sprintf("  Average monthly return: %.2f%%\n", avg_monthly_return))
cat(sprintf("  Average daily return: %.3f%%\n", avg_daily_return))
cat(sprintf("  Monthly return std dev: %.2f%%\n", std_monthly_return))
cat(sprintf("  Sharpe ratio (monthly): %.3f\n", sharpe_ratio))
cat(sprintf("  Annualized Sharpe ratio: %.3f\n", annual_sharpe))
cat(sprintf("  Win rate: %.1f%%\n", win_rate))
cat(sprintf("  Average profit: %.2f%%\n", avg_win))
cat(sprintf("  Average loss: %.2f%%\n", avg_loss))
cat(sprintf("  Win-loss ratio: %.2f\n", win_loss_ratio))

# ============================================================
# 2. Risk indicators
# ============================================================

cat("\n【2. Risk Indicators】\n")

# Maximum drawdown
cumulative_wealth <- returns_df$Cumulative
running_max <- cummax(cumulative_wealth)
drawdown <- (cumulative_wealth - running_max) / running_max
max_drawdown <- min(drawdown) * 100
max_drawdown_date <- returns_df$Date[which.min(drawdown)]

# Average drawdown
avg_drawdown <- mean(drawdown[drawdown < 0]) * 100

# Drawdown duration
drawdown_periods <- rle(drawdown < 0)
drawdown_durations <- drawdown_periods$lengths[drawdown_periods$values]
if(length(drawdown_durations) > 0) {
  avg_drawdown_duration <- mean(drawdown_durations)
  max_drawdown_duration <- max(drawdown_durations)
} else {
  avg_drawdown_duration <- 0
  max_drawdown_duration <- 0
}

# VaR (95% confidence level)
VaR_95 <- quantile(returns_df$Return, 0.05) * 100

# CVaR (Conditional Value at Risk)
CVaR_95 <- mean(returns_df$Return[returns_df$Return <= quantile(returns_df$Return, 0.05)]) * 100

# Downside risk (standard deviation of negative returns only)
downside_returns <- returns_df$Return[returns_df$Return < 0]
downside_risk <- sd(downside_returns) * 100

# Sortino ratio (only considers downside volatility)
sortino_ratio <- mean(returns_df$Return) / sd(downside_returns)

cat(sprintf("  Maximum drawdown: %.2f%%\n", max_drawdown))
cat(sprintf("  Average drawdown: %.2f%%\n", avg_drawdown))

# ============================================================
# 3. Benchmark comparison (equal-weight portfolio)
# ============================================================

cat("\n【3. Benchmark Comparison】\n")

# Calculate equal-weight portfolio return
cat("\nCalculating equal-weight portfolio return...\n")

equal_weight_returns <- c()
for(w in 1:length(allocation_weights)) {
  weight_record <- allocation_weights[[w]]
  # Equal weights
  equal_weights <- rep(1/length(weight_record$Market), length(weight_record$Market))

  # Calculate forecast period return
  portfolio_return <- 0
  for(i in 1:nrow(weight_record)) {
    cnt <- weight_record$Market[i]
    date <- names(allocation_weights)[w]
    est_start <- as.Date(date)
    est_end <- est_start + months(estimation_window_months)
    forecast_start <- est_end
    forecast_end <- forecast_start + months(1)

    forecast_data <- all_price_data[[cnt]] %>%
      filter(date >= forecast_start, date < forecast_end)

    if(nrow(forecast_data) > 0) {
      monthly_return <- sum(forecast_data$return, na.rm = TRUE) / 100
      portfolio_return <- portfolio_return + equal_weights[i] * monthly_return
    }
  }
  equal_weight_returns <- c(equal_weight_returns, portfolio_return)
}

equal_weight_df <- data.frame(
  Date = selection_dates[1:length(equal_weight_returns)],
  Return = equal_weight_returns
) %>% filter(!is.na(Return))

if(nrow(equal_weight_df) > 0) {
  # Equal-weight cumulative return
  equal_weight_df$Cumulative <- cumprod(1 + equal_weight_df$Return)
  equal_total_return <- (tail(equal_weight_df$Cumulative, 1) - 1) * 100

  # Equal-weight Sharpe ratio
  equal_sharpe <- mean(equal_weight_df$Return) / sd(equal_weight_df$Return)

  # Equal-weight maximum drawdown
  equal_cum <- equal_weight_df$Cumulative
  equal_running_max <- cummax(equal_cum)
  equal_drawdown <- (equal_cum - equal_running_max) / equal_running_max
  equal_max_dd <- min(equal_drawdown) * 100

  cat(sprintf("\nEqual-weight portfolio (33.3%% each market):\n"))
  cat(sprintf("  Total return: %.2f%%\n", equal_total_return))
  cat(sprintf("  Sharpe ratio: %.3f\n", equal_sharpe))
  cat(sprintf("  Maximum drawdown: %.2f%%\n", equal_max_dd))

  cat(sprintf("\nYour strategy vs equal-weight portfolio:\n"))
  cat(sprintf("  Return advantage: %.2f%%\n", total_return - equal_total_return))
  cat(sprintf("  Sharpe ratio advantage: %.3f\n", sharpe_ratio - equal_sharpe))
  cat(sprintf("  Maximum drawdown improvement: %.2f%%\n", equal_max_dd - max_drawdown))
}
