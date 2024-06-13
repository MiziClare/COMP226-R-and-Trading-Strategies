source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')

source('example_strategies.R')

# load data
dataList <- getData(directory="EXAMPLE")
# subset data: just use first 200 days
dataList <- lapply(dataList, function(x) x[1:200])

# choose strategy from example_strategies
strategy <- "fixed"

# check that the choice is valid
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

sMult <- 0.20 # slippage multiplier

# Do backtest
results <- backtest(dataList,getOrders,params,sMult)
plotResults(dataList,results)
cat("Profit:", results$aggProfit, '\n')
