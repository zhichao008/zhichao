# 读取收益率数据
returns <- read.csv("C:/1/CLEANED/results/strategy_output/returns_series.csv",
                    stringsAsFactors = FALSE)
returns$Date <- as.Date(returns$Date)
colnames(returns)[2:4] <- c("KOR", "IND", "GBR")

# 读取不确定性数据的函数
read_uncertainty <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  return(df)
}

# 读取英国（GBR）的三种不确定性数据
epu <- read_uncertainty("C:/1/CLEANED/KOR_EPU_stock_cleaned.csv")
tpu <- read_uncertainty("C:/1/CLEANED/KOR_TPU_stock_cleaned.csv")
cpu <- read_uncertainty("C:/1/CLEANED/KOR_CPU_stock_cleaned.csv")

# 提取收益率序列（使用英国收益率）
returns_series <- returns$GBR
returns_series <- na.omit(returns_series)

# 提取不确定性序列（假设第二列是数值）
epu_series <- epu[, 2]
tpu_series <- tpu[, 2]
cpu_series <- cpu[, 2]

# 确保长度一致
min_len <- min(length(returns_series), length(epu_series),
               length(tpu_series), length(cpu_series))
returns_series <- returns_series[1:min_len]
epu_series <- epu_series[1:min_len]
tpu_series <- tpu_series[1:min_len]
cpu_series <- cpu_series[1:min_len]

# 3. 计算四种聚合方式
# ============================================================

# 3.1 Equal-weight average（等权平均）
equal_weight <- (epu_series + tpu_series + cpu_series) / 3

# 3.2 First Principal Component（第一主成分）
pca_data <- data.frame(EPU = epu_series, TPU = tpu_series, CPU = cpu_series)
pca_result <- prcomp(pca_data, scale. = TRUE)
pc1 <- pca_result$x[, 1]

# 3.3 Additive specification（加总）
additive <- epu_series + tpu_series + cpu_series

# 3.4 MC_Weighted（使用PCA载荷加权）
# 英国PCA载荷: EPU=0.668, TPU=0.562, CPU=0.487
loadings <- pca_result$rotation[, 1]
mc_weighted <- loadings[1] * epu_series + loadings[2] * tpu_series + loadings[3] * cpu_series

# 4. BIC 计算函数（使用线性回归模型）
# ============================================================

# 方法1：使用线性回归（将不确定性作为解释变量）
calculate_bic_lm <- function(y, x, model_name) {
  # 构建数据框
  df <- data.frame(y = y, x = x)

  # 拟合线性回归模型
  fit <- lm(y ~ x, data = df)

  # 提取 BIC
  bic <- BIC(fit)

  return(bic)
}

# 方法2：使用 AR 模型（考虑时间序列自相关）
calculate_bic_ar <- function(y, x, model_name, lag = 1) {
  # 构建滞后变量
  n <- length(y)
  y_lag <- y[1:(n-lag)]
  x_aligned <- x[(lag+1):n]
  y_aligned <- y[(lag+1):n]

  # 构建数据框
  df <- data.frame(y = y_aligned, x = x_aligned, y_lag = y_lag)

  # 拟合 AR(1) + 外生变量模型
  fit <- lm(y ~ y_lag + x, data = df)

  # 提取 BIC
  bic <- BIC(fit)

  return(bic)
}

# 方法3：使用简化 GARCH 类型的方差模型（对数收益率平方）
calculate_bic_variance <- function(y, x, model_name) {
  # 计算对数收益率平方（波动率代理）
  y_sq <- y^2

  # 构建数据框
  df <- data.frame(y_sq = y_sq, x = x)

  # 拟合方差模型
  fit <- lm(y_sq ~ x, data = df)

  # 提取 BIC
  bic <- BIC(fit)

  return(bic)
}

# 5. 选择一种方法进行计算
# ============================================================

cat("\n========== 计算 BIC 值 ==========\n\n")

# 使用 AR(1) 模型（推荐）
cat("使用 AR(1) + 外生变量模型计算 BIC...\n")

bic_eq <- calculate_bic_ar(returns_series, equal_weight, "Equal-Weight")
bic_pc1 <- calculate_bic_ar(returns_series, pc1, "PC1")
bic_add <- calculate_bic_ar(returns_series, additive, "Additive")
bic_mcw <- calculate_bic_ar(returns_series, mc_weighted, "MC_Weighted")

# 6. 汇总结果
# ============================================================

results <- data.frame(
  Model = c("Equal-Weight", "PC1", "Additive", "MC_Weighted"),
  BIC = c(bic_eq, bic_pc1, bic_add, bic_mcw)
)

# 按 BIC 排序（越小越好）
results <- results[order(results$BIC), ]
results$Rank <- 1:nrow(results)

# 7. 打印结果
# ============================================================

cat("\n========== BIC 比较结果 ==========\n\n")
print(results)

cat("\n========== 结论 ==========\n")
cat("最佳模型:", results$Model[1], "\n")
cat("最小 BIC:", round(results$BIC[1], 2), "\n")

if (nrow(results) > 1) {
  cat("\nBIC 差值:\n")
  for (i in 2:nrow(results)) {
    diff <- results$BIC[i] - results$BIC[1]
    cat("  ", results$Model[i], ": ", round(diff, 2), "\n")
  }
}

# 8. 可视化
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

# 保存图形
ggsave("bic_comparison_uk.png", p_bic, width = 8, height = 6, dpi = 300)

# 9. 导出结果
# ============================================================
write.csv(results, "bic_comparison_results_uk.csv", row.names = FALSE)
cat("\n结果已保存至 bic_comparison_results_uk.csv\n")

# 10. 三种方法结果对比（可选）
# ============================================================

cat("\n========== 三种 BIC 计算方法对比 ==========\n\n")

# 线性回归方法
bic_lm_eq <- calculate_bic_lm(returns_series, equal_weight, "Equal-Weight")
bic_lm_pc1 <- calculate_bic_lm(returns_series, pc1, "PC1")
bic_lm_add <- calculate_bic_lm(returns_series, additive, "Additive")
bic_lm_mcw <- calculate_bic_lm(returns_series, mc_weighted, "MC_Weighted")

# 方差模型方法
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

# 保存三种方法对比结果
write.csv(results_lm, "bic_comparison_all_methods.csv", row.names = FALSE)
