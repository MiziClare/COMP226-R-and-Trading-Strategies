# In period 1, use market orders to take positions according to params$sizes
# No further orders are placed by getOrders
# The backtester automatically exits all positions when the data runs out

getOrders <- function(store, newRowList, currentPos, info, params) {
    allzero <- rep(0,length(newRowList)) 
    marketOrders <- allzero
    if (is.null(store)) { 
        marketOrders <- params$sizes
        store <- 1 # not null
    }
    return(list(store=store,marketOrders=marketOrders,
                            limitOrders1=allzero, 
                            limitPrices1=allzero,
                            limitOrders2=allzero,
                            limitPrices2=allzero))
}
