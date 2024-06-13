###############################################################################
# Source the functions that you would like to test, e.g., with 
# source('strategies/a2_strategy_template.R') or source('strategies/strategy.R') 
###############################################################################
source('framework/data.R'); dataList <- getData(directory="A2")
prices <- dataList[[1]]
prices_19_rows <- dataList[[1]]$Close[1:19]
prices_20_rows <- dataList[[1]]$Close[1:20]
prices_20_rows_renamed <- prices_20_rows
colnames(prices_20_rows_renamed) <- 'Closed'
bad_prices <- c(1,2,3) 
lookbacks_no_names <- list(5,10,25) # list elements not named
lookbacks_not_integer <- list(short=5,medium=as.integer(10),long=as.integer(20))
lookbacks_wrong_order <- list(short=as.integer(15),medium=as.integer(10),long=as.integer(20))
lookbacks <- list(short=as.integer(5),medium=as.integer(10),long=as.integer(20))

test_checkE01 <- function()
    checkE01(prices,lookbacks_no_names) 
test_checkE02 <- function()
    checkE02(prices,lookbacks_not_integer) 
test_checkE03 <- function()
    checkE03(prices,lookbacks_wrong_order) 
test_checkE04 <- function()
    checkE04(bad_prices,lookbacks) 
test_checkE05 <- function()
    checkE05(prices_19_rows,lookbacks) 
test_checkE06 <- function()
    checkE06(prices_20_rows_renamed,lookbacks) 
test_pass_all_checks <- function(check_func) 
    check_func(prices_20_rows,lookbacks)
test_getTMA <- function() # same inputs as test_pass_all_checks()
    getTMA(prices_20_rows,lookbacks)
