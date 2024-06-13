example_strategies <- c("fixed", 
                        "copycat", 
                        "rsi_contrarian", 
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period") 
# end of example_strategies

is_valid_example_strategy <- function(strategy) { 
    strategy %in% example_strategies
}

example_params <- list(
                    "fixed"=list(sizes=rep(1,5)),
                    "copycat"=NULL,
                    "rsi_contrarian"=list(lookback=10,threshold=25,series=1:5),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,5)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5),posSizes=rep(1,5)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,5),holdPeriod=6))
# end of example_params

load_strategy <- function(strategy) {
    strategyFile <- file.path('strategies', paste0(strategy,'.R'))
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders
    params <<- example_params[[strategy]] # set params via global assignment
    print("Parameters:")
    print(params)
}
