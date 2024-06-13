getOrders <- function(store, newRowList, currentPos, info, params) {
  allzero <- rep(0, length(newRowList)) # used for initializing vectors
  pos <- allzero

  if (is.null(store)) {
    store <- initStore(newRowList)
  } else {
    store <- updateStore(store, newRowList)
  }
  # Only start computing the moving averages when you have enough (close) prices for the long moving average

  # Check if the iterator is within the valid range of indices
  if (!is.null(store$iter) && store$iter > params$lookbacks$long && store$iter <= length(store$cl[[1]])) {
    # Iterate over the series specified in params$series
    for (s in params$series) {
      if (s <= length(store$cl) && !is.null(store$cl[[s]]) && nrow(store$cl[[s]]) >= store$iter) {
        # Extract the current close price safely
        current_close <- store$cl[[s]][store$iter, "Close", drop = TRUE]

        if (!is.na(current_close)) {
          # Compute TMA, position sign and size
          tma_list <- getTMA(store$cl[[s]][(store$iter - params$lookbacks$long + 1):store$iter, ], params$lookbacks)
          pos_sign <- getPosSignFromTMA(tma_list)
          pos_size <- getPosSize(current_close)

          # Calculate the position for the current series
          pos[s] <- pos_sign * pos_size
        }
      }
    }
  }

  marketOrders <- -currentPos + pos

  return(list(
    store = store, marketOrders = marketOrders,
    limitOrders1 = allzero, limitPrices1 = allzero,
    limitOrders2 = allzero, limitPrices2 = allzero
  ))
}

checkE01 <- function(prices, lookbacks) {
  # Return FALSE if lookbacks contains named elements short, medium, and long
  # otherwise return TRUE to indicate an error
  return(!all(c("short", "medium", "long") %in% names(lookbacks)))
}
checkE02 <- function(prices, lookbacks) {
  # Return FALSE if all the elements of lookbacks are integers (as in the R
  # data type) otherwise return TRUE to indicate an error
  return(!all(sapply(lookbacks, is.integer)))
}
checkE03 <- function(prices, lookbacks) {
  # Return FALSE if lookbacks$short < lookbacks$medium < lookbacks$long
  # otherwise return TRUE to indicate an error
  return(!(lookbacks$short < lookbacks$medium && lookbacks$medium < lookbacks$long))
}
checkE04 <- function(prices, lookbacks) {
  # Return FALSE if prices is an xts object, otherwise return TRUE to
  # indicate an error
  return(!("xts" %in% class(prices)))
}
checkE05 <- function(prices, lookbacks) {
  # Return FALSE if prices has enough rows to getTMA otherwise return TRUE
  # to indicate an error
  required_rows <- max(unlist(lookbacks))
  return(nrow(prices) < required_rows)
}
checkE06 <- function(prices, lookbacks) {
  # Return FALSE if prices contains a column called "Close" otherwise return
  # TRUE to indicate an error
  return(!("Close" %in% colnames(prices)))
}


atLeastOneError <- function(prices, lookbacks) {
  # return TRUE if any of the error checks return TRUE
  ret <- FALSE
  ret <- ret | checkE01(prices, lookbacks)
  ret <- ret | checkE02(prices, lookbacks)
  ret <- ret | checkE03(prices, lookbacks)
  ret <- ret | checkE04(prices, lookbacks)
  ret <- ret | checkE05(prices, lookbacks)
  ret <- ret | checkE06(prices, lookbacks)
  return(ret)
}

getTMA <- function(prices, lookbacks, with_checks = FALSE) {
  if (with_checks) {
    if (atLeastOneError(prices, lookbacks)) {
      stop("At least one of the errors E01...E06 occurred")
    }
  }

  # Assuming prices is an xts object and lookbacks is a list with named elements
  short_MA <- SMA(prices$Close, n = lookbacks$short)
  medium_MA <- SMA(prices$Close, n = lookbacks$medium)
  long_MA <- SMA(prices$Close, n = lookbacks$long)

  # Selecting the last element of each SMA series
  ret <- list(
    short = last(short_MA),
    medium = last(medium_MA),
    long = last(long_MA)
  )

  return(ret)
}

getPosSignFromTMA <- function(tma_list) {
  if (tma_list$short < tma_list$medium && tma_list$medium < tma_list$long) {
    return(-1) # short signal
  } else if (tma_list$short > tma_list$medium && tma_list$medium > tma_list$long) {
    return(1) # long signal
  } else {
    return(0) # flat/no position
  }
}

getPosSize <- function(current_close, constant = 5000) {
  # This function should return (constant divided by current_close)
  # rounded down to the nearest integer
  return(floor(constant / current_close))
}

initClStore <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x, y) rbind(x, y$Close), clStore, newRowList, SIMPLIFY = FALSE)
  return(clStore)
}
initStore <- function(newRowList, series) {
  return(list(iter = 1, cl = initClStore(newRowList)))
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl, newRowList)
  return(store)
}
