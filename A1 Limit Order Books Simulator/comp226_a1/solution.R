book.total_volumes <- function(book) {
  total_bid_volume <- sum(book$bid$size)
  total_ask_volume <- sum(book$ask$size)
  return(list(bid = total_bid_volume, ask = total_ask_volume))
}

book.best_prices <- function(book) {
  best_bid <- if (nrow(book$bid) > 0) max(book$bid$price) else NA
  best_ask <- if (nrow(book$ask) > 0) min(book$ask$price) else NA
  return(list(bid = best_bid, ask = best_ask))
}

book.midprice <- function(book) {
  best_prices <- book.best_prices(book)
  if (is.na(best_prices$bid) || is.na(best_prices$ask)) {
    return(NA)
  } else {
    midprice <- (best_prices$bid + best_prices$ask) / 2
    return(midprice)
  }
}

book.spread <- function(book) {
  best_prices <- book.best_prices(book)
  if (is.na(best_prices$bid) || is.na(best_prices$ask)) {
    return(NA)
  } else {
    spread <- best_prices$ask - best_prices$bid
    return(spread)
  }
}

book.add <- function(book, message) {
  oid <- message$oid
  side <- message$side
  price <- as.numeric(message$price)
  size <- as.numeric(message$size)
  
  if (side == "B") {
    if (nrow(book$ask) > 0 && min(book$ask$price) <= price) {
      while (size > 0 && nrow(book$ask) > 0 && min(book$ask$price) <= price) {
        best_ask <- min(book$ask$price)
        idx <- which(book$ask$price == best_ask)[1]
        if (book$ask$size[idx] > size) {
          book$ask$size[idx] <- book$ask$size[idx] - size
          size <- 0
        } else {
          size <- size - book$ask$size[idx]
          book$ask <- book$ask[-idx, ]
        }
      }
    }
    if (size > 0) {
      book$bid <- rbind(book$bid, data.frame(oid = oid, price = price, size = size))
    }
  } else if (side == "S") {
    if (nrow(book$bid) > 0 && max(book$bid$price) >= price) {
      while (size > 0 && nrow(book$bid) > 0 && max(book$bid$price) >= price) {
        best_bid <- max(book$bid$price)
        idx <- which(book$bid$price == best_bid)[1]
        if (book$bid$size[idx] > size) {
          book$bid$size[idx] <- book$bid$size[idx] - size
          size <- 0
        } else {
          size <- size - book$bid$size[idx]
          book$bid <- book$bid[-idx, ]
        }
      }
    }
    if (size > 0) {
      book$ask <- rbind(book$ask, data.frame(oid = oid, price = price, size = size))
    }
  }
  
  book$bid <- book$bid[order(-book$bid$price, book$bid$oid), ]
  book$ask <- book$ask[order(book$ask$price, book$ask$oid), ]
  return(book)
}

book.reduce <- function(book, message) {
  oid <- message$oid
  size_to_reduce <- as.numeric(message$amount)
  order_found <- FALSE
  
  for (side in c("bid", "ask")) {
    if (oid %in% book[[side]]$oid) {
      order_index <- which(book[[side]]$oid == oid)
      if (!is.na(order_index) && length(order_index) > 0) {
        order_found <- TRUE
        book[[side]]$size[order_index] <- book[[side]]$size[order_index] - size_to_reduce
        if (book[[side]]$size[order_index] <= 0) {
          book[[side]] <- book[[side]][-order_index, ]
        }
        break
      }
    }
  }
  
  if (!order_found) {
    warning(paste("Order ID", oid, "not found. No action was taken."))
  }
  
  return(book)
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
  if (nrow(book$ask) == 0) {
    return(NA)
  }
  
  possible_prices <- unique(book$ask$price)
  
  midprices <- numeric(length(possible_prices))
  
  for (i in seq_along(possible_prices)) {
    price <- possible_prices[i]
    
    book_copy <- list(ask = book$ask, bid = book$bid)
    
    book_copy <- book.add(book_copy, list(oid = "simulated", side = "B", price = price, size = size))
    
    midprices[i] <- book.midprice(book_copy)
  }
  
  average_midprice <- mean(midprices, na.rm = TRUE)
  
  return(average_midprice)
}

book.extra2 <- function(book, size) {
  if (nrow(book$ask) == 0 || size > sum(book$ask$size)) {
    return(NA)  
  }
  
  best_ask <- min(book$ask$price)
  highest_ask <- max(book$ask$price)
  prices <- seq(best_ask, highest_ask) 
  
  midprices <- numeric(length(prices))
  
  for (p in seq_along(prices)) {
    
    book_copy <- list(ask = book$ask, bid = book$bid)
    
    book_copy <- book.add(book_copy, list(oid = NA, side = "B", price = prices[p], size = size))
    
    midprice <- book.midprice(book_copy)
    midprices[p] <- midprice
  }
  
  valid_midprices <- midprices[!is.na(midprices)]
  if (length(valid_midprices) > 0) {
    return(mean(valid_midprices))
  } else {
    return(NA)
  }
}

book.extra3 <- function(book) {
  if (nrow(book$ask) == 0) {
    return(NA)  
  }
  
  total_ask_volume <- sum(book$ask$size)
  if (total_ask_volume <= 1) {
    return(NA)  
  }
  
  simulated_mid_prices <- numeric()
  
  
  for (s in 1:(total_ask_volume - 1)) {
    book_copy <- list(ask = book$ask, bid = book$bid)
    highest_ask_price <- max(book_copy$ask$price)
    market_buy_message <- list(oid = "simulate", side = "B", price = highest_ask_price, size = s)
    book_copy <- book.add(book_copy, market_buy_message)
    
    book_copy$ask <- book_copy$ask[book_copy$ask$size > 0, ]
    
    new_midprice <- if (nrow(book_copy$ask) > 0 && nrow(book_copy$bid) > 0) {
      (min(book_copy$ask$price) + max(book_copy$bid$price)) / 2
    } else {
      NA
    }
    
    simulated_mid_prices <- c(simulated_mid_prices, new_midprice)
  }
  
  valid_midprices <- simulated_mid_prices[!is.na(simulated_mid_prices)]
  if (length(valid_midprices) > 0) {
    return(mean(valid_midprices))
  } else {
    return(NA)
  }
}

book.extra4 <- function(book, k) {
  if (nrow(book$ask) == 0) return(0)
  
  original_midprice <- book.midprice(book)
  if (is.na(original_midprice)) return(0)
  
  max_volume <- sum(book$ask$size)
  k_decimal <- k / 100
  max_v <- 0
  
  for (v in 1:max_volume) {
    book_copy <- list(ask = book$ask, bid = book$bid)
    
    remaining_size <- v
    for (i in 1:nrow(book_copy$ask)) {
      if (remaining_size <= 0) break
      if (remaining_size >= book_copy$ask$size[i]) {
        remaining_size <- remaining_size - book_copy$ask$size[i]
        book_copy$ask$size[i] <- 0
      } else {
        book_copy$ask$size[i] <- book_copy$ask$size[i] - remaining_size
        remaining_size <- 0
        break
      }
    }
    book_copy$ask <- book_copy$ask[book_copy$ask$size > 0, ]
    
    new_midprice <- book.midprice(book_copy)
    
    if (is.na(new_midprice) || (new_midprice - original_midprice) / original_midprice > k_decimal) {
      return(max_v)
    } else {
      max_v <- v
    }
  }
  
  return(max_v)
}
