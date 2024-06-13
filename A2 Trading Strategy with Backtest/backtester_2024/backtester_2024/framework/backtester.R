# Load required libraries
library(quantmod)

####################################################################################
# Returns given row within a list of xts objects
#
# @param listOfXts: List of xts objects to return from
# @param rowIndex: Index to return
# @return: 

getRowList <- function(listOfXts, rowIndex) {
    # rowIndex will be recycled
    ret <- mapply(function(x, y) x[y], listOfXts, rowIndex, SIMPLIFY = FALSE)
    return(ret)
}

####################################################################################
# Calculates slippage as a multiple of the overnight gap
#
# @param prevClose: previous day's close
# @param curOpen: current day's open
# @param sMult: slipppage multiplier, a positive value corresponds to negative slippage 
# @return: Slippage value

slip  <-  function(prevClose, curOpen, sMult) { 
    overnightGap  <- abs(prevClose-curOpen)
    return(sMult * overnightGap) 
}

####################################################################################
# Creates list containing prices for previous, current and next row
#
# @param prevRow: Previous rows prices
# @param curRow: Current rows prices
# @param nextRow: Next rows prices (default: NULL)
# @return: list of prices and current date for use with other methods

getPrices <- function(prevRow, curRow, nextRow = NULL) {
    # these are the three prices we need
    # nextOp-curOp is the pnl for today
    # curOp-prevCl is used to compute slippage
    prevCl     <- as.numeric(prevRow["Close"])
    curOp     <- as.numeric(curRow["Open"])
    curHi     <- as.numeric(curRow["High"])
    curLo     <- as.numeric(curRow["Low"])
    nextOp     <- as.numeric(nextRow["Open"])
    prices     <- list(prevCl = prevCl, 
                    curOp = curOp, 
                    curHi = curHi, 
                    curLo = curLo, 
                    nextOp = nextOp)

    return(prices)
}

####################################################################################
# Calculates slippage for final holding
# @param prices from getPrices()
# @param pos current position
# @param sMult
# @return slippage for closing out positions 

closeOut <- function(prices, pos, sMult) {
# incur slippage for selling final holding
# run on final day, so pos is holding on penultimate day
    slippage <- slip(prices$prevCl,prices$curOp,sMult)
    pnl <- -abs(pos) * slippage           
    return(pnl)
}

###############################################################################
# Computes the net worth of a portfolio according to the current prices. Ignores
# the slippage that would be incurred by liquidating the portfolio.
#
# @param position: The position that is to be valued
# @param prices: The prices to use for the valuation
# 
getNetWorth <- function(position, prices, cash)
{

}

####################################################################################
# Finds prices and trades using old and new positions
#
# @param rows: list of prev, cur, and nxt rows of series to get prices from
# @param oldPos: lots held of each series on previous day 
# @param newPos: lots held of each series on current day 
# @param sMult: slippage multiplier 
# @return: pnl for current day inclusive of slippage incurred at current open

# Calculates the profit or loss for each series for current day
# I.e. position held today multiplied by the difference between 
# today's and tomorrow open, minus the number of lots traded today multiplied
# by slippage (derived from today's open and yesterday's close)
#
# @param prices 
# @param oldPos used to calculate lots traded at current open as abs(newPos-oldPos)
# @param newPos number of lots of each series held on current day
# @param sMult
# @return days pnl for each series


findPnL <- function(rows, oldPos, marketOrder, 
                                  limitOrder1, 
                                  limitPrice1, 
                                  limitOrder2, 
                                  limitPrice2, 
                                  balance,
                                  sMult) {

    # Fetch prices
    prices <- getPrices(rows$prev, rows$cur, rows$nxt)
    
    # collects pnl for open positions
    # processes market and limit orders and updates positions

    oldPnl <- oldPos * (prices$nextOp - prices$curOp) # pnl from oldPos
    pnl <- oldPnl

    oldValue <- oldPos * prices$nextOp # The value of the holding *before* the trades take place. Useful if the trades are cancelled due to overspending

    newPos <- oldPos

    # Total amount of money spent (positive) or received (negative)
    moneySpent <- 0

    if (marketOrder != 0) {
        # run from day 2, where oldPos would always be 0, until penultimate day
        slippage <- slip(prices$prevCl, prices$curOp, sMult)
        # +/- (nextOp - curOp) * "held on cur" - slippage * "traded on cur"
        pnl <- pnl + marketOrder * (prices$nextOp - prices$curOp) - abs(marketOrder) * slippage

        # Money spent is the execution (+ or - depending on buy or sell) price PLUS incurred slippage
        moneySpent <- moneySpent + (marketOrder * prices$curOp) + (abs(marketOrder) * slippage)

        newPos <- newPos + marketOrder
    }

    l1 <- checkLimitOrder(prices,limitOrder1,limitPrice1)
    l2 <- checkLimitOrder(prices,limitOrder2,limitPrice2) 

    pnl <- pnl + l1$pnl + l2$pnl
    moneySpent <- moneySpent + l1$moneySpent + l2$moneySpent
    newPos <- newPos + l1$newPos + l2$newPos

    currentValue <- prices$nextOp * newPos

    return(list(pnl=pnl,basePnl=oldPnl,moneySpent=moneySpent,newPos=newPos, currentValue=currentValue, oldValue=oldValue))
}

####################################################################################
# Gives pnl (until next open) and newPos arising from a *single* limit order

checkLimitOrder <- function(prices,limitOrder,limitPrice) {

    eps <- 0.0001 # used to (try) to prevent floating errors when comparing limit
                  # prices to highs and lows

    pnl     <- 0 
    newPos  <- 0 
    moneySpent <- 0

    # execute if:
    # buy  limit order (-eps) price is above low; or
    # sell limit order (+eps) price is below high
    if (limitOrder > 0 & prices$curLo < (limitPrice-eps) || limitOrder < 0 & prices$curHi > (limitPrice+eps))  { 
    
            executionPrice <- ifelse(limitOrder > 0,
                                     min(limitPrice,prices$curHi), # execute buy order at minimum of limit price and High 
                                     max(limitPrice,prices$curLo)) # execute sell order at maxmim of limit price and Low 

            pnl <- pnl + limitOrder * (prices$nextOp - executionPrice)
            moneySpent <- executionPrice * limitOrder
            newPos <- limitOrder
    }

    return(list(pnl=pnl,moneySpent=moneySpent,newPos=newPos))
}

####################################################################################
# For a single series, closes out all positions at open on final day
#
# @param rows: Rows to get prices from
# @param oldPos: old position
# @param sMult: slippage multiplier
# @return: xts containing slippage incurred at open on final day 

finalExit <- function(rows, oldPos, sMult) { 
    # Fetch prices
    prices <- getPrices(rows$cur, rows$nxt) # note these use cur and nxt (usually prev and cur)

    # Slippage incurred by closing out a position by a market order
    pnl_from_slippage <- closeOut(prices, oldPos, sMult)

    # Our final net worth is our portfolio value accounting for slippage
    # incurred to liquidate
    netWorth <- prices$curOp * oldPos + pnl_from_slippage 

    return(list(pnl_from_slippage=pnl_from_slippage, netWorth=netWorth))
}


####################################################################################
# MAIN backtesting function
####################################################################################

# Iterates over numOfDays, call getOrders with yesterday's new data, 
# resulting trades are reflected in the returned object, which should be a list 
# of lot sizes (integers) of length = number of series
#
# To provide a simple way to go flat, the entries represent the desired *total
# holding*, so to go flat, just return 0,
#
# So the number of lots traded on series s at the current i'th open will be 
# getOrders(store, getRowList(dataList,i-1), params)[s] -
# getOrders(store, getRowList(dataList,i-2), params)[s]
# i.e. the value for series s returned today minus the value returned yesterday

# @param dataList: list of OHLCV xts objects, one for each series 
# @param getOrders:  this is the strategy
# @param sMult: slippage multiplier
# @return: list of xts objects two columns, DailyPnL and CumPnL

backtest <- function(dataList, getOrders, params, sMult) {

    #cat("Starting backtester\n")

    numOfDays   <- nrow(dataList[[1]]) # assumes elements of dataList have same number of rows
    numOfSeries <- length(dataList)

    # initialise as 0-vector of length length(dataList) 
    newPosList        <- vector(mode="numeric", length = length(dataList))

    # Initialisation of getOrders with first row of data, via is.null(store)
    store             <- NULL

    # pnlList will store trading results
    # initialize lists of 0 rows; getRowList(dataList,1) used to get date for each via index()
    #pnlList <- mapply(function(x, y) xts(x, index(y)),0, getRowList(dataList,1), SIMPLIFY = FALSE)
    pnlList <- lapply(1:numOfSeries,function(x) matrix(0,nrow=numOfDays,ncol=1))
    positionValuesList <- lapply(1:numOfSeries,function(x) matrix(0,nrow=numOfDays,ncol=1))
    netWorthList <- rep(0, numOfDays)

    ##################################################
    # vector that stores a 1 for every day a position was taken in some
    # series and a 0 otherwise
    # initialized as all zero vecotr 
    posCounter <-  0

    nonxtsDataList <- lapply(dataList, function(x) as.matrix(x))

    balance <- 1000000
    newNetWorth <- balance
    netWorthList[[1]] <- balance

    bankrupt <- FALSE # Are we bankrupt?

    

    # MAIN LOOP
    for (i in 2: (numOfDays-1)) { # cannot hold on day 1; day 1 data is given to strategy on day 2
    
        #if (i %% 100 == 0)
            #cat("Processing row", i, "\n")

        oldPosList    <- newPosList

        info = list(balance=balance, netWorth=newNetWorth)

        # getOrders() is run with YESTERDAY's data to determine what we hold TODAY
        x <- getOrders(store, getRowList(dataList,i-1), oldPosList, info, params)

        store            <- x$store

        if (length(x$marketOrders) != numOfSeries) {
            cat("Error: length(marketOrders) != numOfSeries\n"); 
            quit();
        }
        if (length(x$limitOrders1) != numOfSeries) {
            cat("Error: length(limitOrders1) != numOfSeries\n"); 
            quit();
        }
        if (length(x$limitPrices1) != numOfSeries) {
            cat("Error: length(limitPrices1) != numOfSeries\n"); 
            quit();
        }
        if (length(x$limitOrders2) != numOfSeries) {
            cat("Error: length(limitOrders2) != numOfSeries\n"); 
            quit();
        }
        if (length(x$limitPrices2) != numOfSeries) {
            cat("Error: length(limitPrices2) != numOfSeries\n"); 
            quit();
        }

        # For each day of backtest, we trade at open, using:
        # yesterday's close (in prev) and today's open (in cur) to compute slippage
        # today's open (in cur) and tomorrow's open (in nxt) to compute pnl from holding today
        pricesLists    <- lapply(nonxtsDataList, function(x) list(prev = x[i-1,], cur = x[i,], nxt = x[i+1,]))

        tradeResults <- mapply(findPnL, pricesLists, 
                                        oldPosList, 
                                        x$marketOrders,
                                        x$limitOrders1, 
                                        x$limitPrices1, 
                                        x$limitOrders2, 
                                        x$limitPrices2, 
                                        balance,
                                        sMult, SIMPLIFY = FALSE)

        pnlRow      <- lapply(tradeResults,function(x) x$pnl)
        newPosList  <- sapply(tradeResults, function(x) x$newPos)
        moneySpent <-  sum(sapply(tradeResults, function(x) x$moneySpent))
        positionValues <- sapply(tradeResults, function(x) x$currentValue)
        newNetWorth <- sum(positionValues)


        # Check whether there is enough money to execute these orders, if not,
        # cancel all trades for today
        if(moneySpent > balance)
        {
            cat("Warning: attempted to spend £", moneySpent, " but only £", balance, " available. No trades executed today\n")

            pnlRow <- lapply(tradeResults, function(x) x$basePnl)
            newPosList <- oldPosList
            moneySpent <- 0
            newNetWorth <- sum(sapply(tradeResults, function(x) x$oldValue))

        }

        balance <- balance - moneySpent
        netWorth <-  newNetWorth + balance


        if(netWorth < 0)
        {
            cat("Bankrupt on day ", i, "!\n", sep="")
            bankrupt <- TRUE

            # Liquidate the position as if we are on the final day
            exit <- mapply(finalExit,pricesLists,newPosList,sMult, SIMPLIFY=FALSE)
            pnlRow <- lapply(exit, function(x) x$pnl_from_slippage)
            netWorth <- sum(sapply(exit, function(x) x$netWorth)) + balance

            # Update the records for all subsequent days
            for(j in 1:numOfSeries) pnlList[[j]][i] <- pnlRow[[j]] 
            for(d in i:numOfDays)
            {
                #for(j in 1:numOfSeries) pnlList[[j]][d] <- pnlRow[[j]] 
                netWorthList[[d]] = netWorth
            }

            break
        }


        ############################################
        # add new row to posCounter
        if (sum(abs(newPosList))>0) posCounter <- posCounter + 1

        for(j in 1:numOfSeries)
        {
            pnlList[[j]][i] <- pnlRow[[j]] 
            positionValuesList[[j]][i] <- positionValues[[j]]
        }

        netWorthList[[i]] <- netWorth

    }

    #cat("FINAL DAY: INVENTORY", newPosList, "\n")

    # Final day: apply finalExit to all series (inner mapply)
    # add this slippage as new xts row in for each element of pnllist (outer mapply)

    if(!bankrupt)
    {
        exit <- mapply(finalExit,pricesLists,newPosList,sMult, SIMPLIFY=FALSE)

        pnlRow <- lapply(exit, function(x) x$pnl_from_slippage)
        for(j in 1:numOfSeries) pnlList[[j]][numOfDays] <- pnlRow[[j]] 

        finalNetWorth <- sum(sapply(exit, function(x) x$netWorth)) + balance
        netWorthList[[numOfDays]] <- finalNetWorth
    }


    # END OF TRADING
    #############################################
    # Cumulate results

    index <- index(dataList[[1]])
    # add cumulative pnl column
    pnlList <- lapply(pnlList, function(x) xts(cbind(x, cumsum(x)),index))

    # add colnames
    for (i in 1:length(pnlList)) {
        colnames(pnlList[[i]]) <- c("DailyPnL", "CumPnL")
    }
    # Note that daily pnl corresponds to open to open
    #pnlList <- mapply(function(x, y) cbind(x$Close, y), dataList, pnlList, SIMPLIFY = FALSE)

    # k is the proportion of days that a trading position was taken in some
    # series
    k <- round(100*posCounter/(numOfDays-2))    

    pfolioPnL <- aggregatePnL(pnlList)
    aggProfit <- last(pfolioPnL$CumPnL) 

    return(list(pnlList=pnlList, 
                netWorthList=netWorthList, 
                positionValuesList=positionValuesList, 
                k=k,
                pfolioPnL=pfolioPnL,
                aggProfit=aggProfit))
}
