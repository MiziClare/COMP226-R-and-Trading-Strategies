library(quantmod)
library(TTR)
library(xts)
library(yaml)

# 加载数据并转换为 xts
load_data <- function(filepath) {
  data <- read.csv(filepath)
  data$Date <- as.Date(data$Index)
  data_xts <- xts(data[, -1], order.by = data$Date)
  return(data_xts)
}

# 参数组合
params <- expand.grid(
  short = c(5, 10),
  medium = c(50, 100),
  long = c(200, 300),
  series = list(combn(1:4, 2, simplify = FALSE), combn(1:4, 3, simplify = FALSE), combn(1:4, 4, simplify = FALSE))
)

# 计算收益的函数
calculate_profit <- function(data) {
  # 假设param是一行数据框
  short <- param$short
  medium <- param$medium
  long <- param$long
  
  # 使用 TTR 包计算移动平均线，并基于此制定交易策略
  sma_short <- SMA(closes, n = params$short)
  sma_medium <- SMA(closes, n = params$medium)
  sma_long <- SMA(closes, n = params$long)
  
  # 基于移动平均线生成交易信号
  signals <- ifelse(sma_short > sma_medium & sma_medium > sma_long, 1, 
                    ifelse(sma_short < sma_medium & sma_medium < sma_long, -1, 0))
  positions <- lag(signals, 1)  # 交易信号滞后一期执行
  positions[is.na(positions)] <- 0
  
  # 计算策略收益
  returns <- diff(log(closes))
  strategy_returns <- returns * positions[-1]  # 移除第一个 NA
  total_return <- sum(strategy_returns, na.rm = TRUE)
  return(total_return)
}

# 交叉验证函数
cross_validation <- function(dataList, params) {
  ins <- dataList[[1]]  # 内样本
  outs <- dataList[[2]]  # 外样本
  
  # 更改sapply的调用方式，确保传递的是data.frame的每一行
  in_profits <- apply(params, 1, function(p) calculate_profit(ins, p))
  out_profits <- apply(params, 1, function(p) calculate_profit(outs, p))
  
  best_in_idx <- which.max(in_profits)
  best_out_idx <- which.max(out_profits)
  
  results <- list(
    best_in_params = params[best_in_idx, ],
    best_out_params = params[best_out_idx, ],
    best_in_profit = in_profits[best_in_idx],
    best_out_profit = out_profits[best_out_idx]
  )
  return(results)
}

# 执行交叉验证
dataList <- lapply(paste0("DATA/A2/0", 1:4, ".csv"), load_data)
results <- cross_validation(dataList, params)

# 输出结果到 YAML 文件
write_yaml(results, "results.yaml")
