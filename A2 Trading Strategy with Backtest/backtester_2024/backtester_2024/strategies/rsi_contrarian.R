####################################################################################
# STRATEGY CODE
#
# getOrders is the interface between the backtester and your strategies
####################################################################################

# It's arguments and return list should stay the same.

####################################################################################
# ARGUMENTS 
####################################################################################

# store: see explanation under returned list below

# newRowList: this is a list of single-row OHLCV xts objects, one for each series

# currentPos: this is the current position (so if one wants to close all
# positions one could set marketOrders as -currentPos)

# params: these are the strategy params

####################################################################################
# RETURNED LIST
####################################################################################

# This function must return a list with the the following named memebers

# store
# marketOrders
# limitOrders1
# limitPrices1
# limitOrders2
# limitPrices2

# store contains data required by the strategy is in the variable store
# store is returned by the function and passed back to it the next time 
# it is called by the backetester.

# marketOrders is a vector containing the number of lots to be traded at the 
# open for each series that the
# strategy will hold on the current day
#
# The i'th entry of the integer vector pos (of length nseries) represents 
# the number of contracts you want to be long (short) on the next trading day 
# if the entry is positive (negative).

# Entries of marketOrders, limitOrders1, and limitOrders2 should be integers 
################################################################################

# This strategy uses only market orders

# The strategy will go short if RSI is > 50 + threshold 
#                      long  if RSI is < 50 - threshold
# and will be flat otherwie

# This is a contrarian (a.k.a. mean-reversion) type of strategy
################################################################################

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, info, params) {

    allzero  <- rep(0,length(newRowList)) # used for initializing vectors

    if (is.null(store)) {
        checkParams(params)
        store <- initStore(newRowList,params$series)
    }
    store <- updateStore(store, newRowList, params$series)
	
	# RSI needs 2 extra periods beyond the lookback
	if (store$iter >= params$lookback + 2 ) {
		marketOrders   <- sapply(1:length(newRowList),
		                        function(x) ifelse(x %in% params$series, 
		                                lgStFt(store$cl,which(x==params$series),store$iter), 0))
	} else
        marketOrders <- allzero


    # exit positions from yesterday
    marketOrders <- marketOrders - currentPos 

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero,
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}

###############################################################################
# The following function is purely to help to prevent errrors by checking that 
# the requirement parameters are available
###############################################################################

checkParams <- function(params) { # make sure params are correct
    if (!"lookback" %in% names(params))
        stop("Parameter lookback not defined for strategy RSI")
    if (!"threshold" %in% names(params))
        stop("Parameter lookback not defined for strategy RSI")
    if (params$threshold < 0 || params$threshold > 50)
        stop("Parameter lookback is not between 0 and 50")
}

###############################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
###############################################################################

# functions for managing the store

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}

###############################################################################

# main strategy logic

lgStFt <-	function(clStore,column,iter) {
	# decide if we should go long/short/flat (returning 1/-1/0)
	startIndex <- iter - params$lookback - 1 # needs 2 extra periods
	rsi <- last(RSI(clStore[startIndex:iter,column],n=params$lookback)) 
	if (rsi > (50 + params$threshold))
        return(-1) # short
	if (rsi < (50 - params$threshold))
        return(1)  # long
    return(0)
}
