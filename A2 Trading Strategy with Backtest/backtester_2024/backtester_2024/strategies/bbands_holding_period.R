# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getNewPosList see rsi.R 

# This strategy only trades on certain markets, which is encoded in params$series.

# The strategy will be long (short) on contract whenever the close is below (above) 
# the lower (upper) Bollinger Band of the close.

maxRows <- 3100 # used to initialize a matrix to store closing prices

getOrders <- function(store, newRowList, currentPos, info, params) {

    allzero  <- rep(0,length(newRowList)) # used for initializing vectors

	if (is.null(store)) store <- initStore(newRowList,params$series)
	store <- updateStore(store, newRowList, params$series)

    marketOrders <- -currentPos; pos <- allzero
	
    if (store$iter > params$lookback) {
       startIndex <-  store$iter - params$lookback
       for (i in 1:length(params$series)) {
           cl <- newRowList[[params$series[i]]]$Close
           bbands <- last(BBands(store$cl[startIndex:store$iter,i],
                            n=params$lookback,sd=params$sdParam))
           if (cl < bbands[,"dn"]) {
               pos[params$series[i]] <- params$posSizes[params$series[i]]
           }
           else if (cl > bbands[,"up"]) {
               pos[params$series[i]] <- -params$posSizes[params$series[i]]
           }
           # check if we have been in trade too long

           # we maintain that pos[i] is an integer
           # if pos[i] == 0 we were flat last period
           # if pos[i] >  0 we have been long  for store$count[i] periods
           # if pos[i] <  0 we have been short for store$count[i] periods

           if (pos[params$series[i]] == 1) {# long signal today 
                if (store$count[i] < 0) # last time we were short
                    store$count[i] == pos[params$series[i]] # == 1
                else if (store$count[i] == params$holdPeriod) { # reached holding period
                    pos[params$series[i]] <- 0 # don't stay long
                    store$count[i] <- 0 # reset count to 0
                }
                else # 0 <= store$count[i] != (should be <) params$holdPeriod
                    store$count[i] <- store$count[i] + 1 
           }

           else if (pos[params$series[i]] == -1) {# short signal today

                if (store$count[i] > 0) # last time we were long
                    store$count[i] == pos[params$series[i]] # == -1
                else if (store$count[i] == -params$holdPeriod) { # reached holding period
                    pos[params$series[i]] <- 0 # don't stay short
                    store$count[i] <- 0 # reset count to 0
                }
                else # 0 >= store$count[i] != (should be >) -params$holdPeriod
                    store$count[i] <- store$count[i] - 1 
           }
           else
                store$count[i] <- 0 # reset count to 0
           }
    }
    marketOrders <- marketOrders + pos
	#cat(formatC(store$count,2),'\t',formatC(pos,2),'\n')
	#cat(formatC(pos,2),'\n')
    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

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
	count <- vector(mode="numeric",length=length(series)) # stores # of days in trade
	return(list(iter=0,cl=initClStore(newRowList,series),count=count))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
