# Read data
returns <- read.csv('C:/1/CLEANED/results/strategy_output/returns_series.csv', stringsAsFactors = FALSE)
weights <- read.csv('C:/1/CLEANED/results/strategy_output/allocation_weights.csv', stringsAsFactors = FALSE)

# Convert date format
returns$Date <- as.Date(returns$Date)
weights$Window <- as.Date(weights$Window)

# Rename columns in returns for matching
colnames(returns)[2:4] <- c("KOR", "IND", "GBR")

# Align data: use common time points
common_dates <- intersect(returns$Date, weights$Window)
returns_aligned <- returns %>% filter(Date %in% common_dates) %>% arrange(Date)
weights_aligned <- weights %>%
  filter(Window %in% common_dates) %>%
  arrange(Window) %>%
  select(Window, Market, Weight) %>%
  pivot_wider(id_cols = Window, names_from = Market, values_from = Weight)

# Verify weights sum to 1
weights_aligned$sum <- rowSums(weights_aligned[, c("KOR", "IND", "GBR")], na.rm = TRUE)
print(paste("Do weights sum to 1?", all(abs(weights_aligned$sum - 1) < 1e-6)))

# Calculate MC_Weighted portfolio returns
portfolio_returns <- numeric(nrow(returns_aligned))
for (i in 1:nrow(returns_aligned)) {
  portfolio_returns[i] <- sum(
    returns_aligned[i, c("KOR", "IND", "GBR")] *
      weights_aligned[i, c("KOR", "IND", "GBR")]
  )
}

# Calculate cumulative returns (initial value = 1)
calc_cumulative <- function(returns_vec) {
  cumprod(1 + returns_vec)
}

# Cumulative returns by country
korea_cum <- calc_cumulative(returns_aligned$KOR)
india_cum <- calc_cumulative(returns_aligned$IND)
uk_cum <- calc_cumulative(returns_aligned$GBR)
portfolio_cum <- calc_cumulative(portfolio_returns)

# Create dataframe for plotting
plot_df <- data.frame(
  Date = returns_aligned$Date,
  Korea = korea_cum,
  India = india_cum,
  UK = uk_cum,
  MC_Weighted = portfolio_cum
)

# Convert to long format for easier plotting
plot_long <- plot_df %>%
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Cumulative_Return")

# ============================================================
# Option 1: Single plot with overlay (four lines)
# ============================================================

p_single <- ggplot(plot_long, aes(x = Date, y = Cumulative_Return, color = Series)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    name = "",
    values = c("Korea" = "blue", "India" = "orange", "UK" = "green", "MC_Weighted" = "red"),
    labels = c("Korea ETF", "India ETF", "UK ETF", "MC_Weighted Portfolio")
  ) +
  labs(
    title = "Fig. 2: ETF Trajectories vs. MC_Weighted Model",
    x = "Date",
    y = "Cumulative Return (Base=1)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  annotate("rect", xmin = as.Date("2015-05-01"), xmax = as.Date("2016-02-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "gray") +
  annotate("text", x = as.Date("2015-08-01"), y = 1.5,
           label = "2015 Market Turmoil", size = 3, angle = 90)

# Save
ggsave("fig2_single_panel.png", p_single, width = 12, height = 7, dpi = 300)

# ============================================================
# Option 2: Area chart with weight changes (advanced version)
# ============================================================

# Display weight changes over time
weights_long <- weights_aligned %>%
  select(Window, KOR, IND, GBR) %>%
  pivot_longer(cols = c(KOR, IND, GBR), names_to = "Country", values_to = "Weight")

p_weights <- ggplot(weights_long, aes(x = Window, y = Weight, fill = Country)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = c("KOR" = "blue", "IND" = "orange", "GBR" = "green")) +
  labs(x = "Date", y = "Portfolio Weight", title = "Dynamic Weights of MC_Weighted Model") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format())

# Combine: cumulative returns on top, weight changes below
final_plot <- (p_single / p_weights) +
  plot_annotation(
    title = "Fig. 2: ETF Trajectories vs. MC_Weighted Model with Dynamic Weights",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )

ggsave("fig2_with_weights.png", final_plot, width = 12, height = 10, dpi = 300)

# Print plots
print(p_single)
print(final_plot)
