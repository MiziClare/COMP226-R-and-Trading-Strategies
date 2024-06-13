# Long (short) one contract today 
# If yesterday's close > (<) yesterday's open 

getOrders <- function(store,newRowList,currentPos,info,params) {

    allzero  <- rep(0,length(newRowList)) 

    # exit positions from yesterday
    marketOrders <- -currentPos 

    marketOrders <- marketOrders + longOrShort(newRowList)

    return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=allzero,
	                        limitPrices1=allzero,
	                        limitOrders2=allzero,
	                        limitPrices2=allzero))
}
longOrShort <- function(newRowList) {
	sapply(newRowList,function(x) 
	                    ifelse(x$Close>x$Open,1,-1))
}
