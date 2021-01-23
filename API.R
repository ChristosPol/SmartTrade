# Packages --------------------------------------------------------------------
# install_github("daroczig/binancer")
# https://github.com/daroczig/binancer/
suppressMessages(library(xts))
suppressMessages(library(Rbitcoin))
suppressMessages(library(httr))
suppressMessages(library(anytime))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))
suppressMessages(library(TTR))
suppressMessages(library(openssl))
suppressMessages(library(digest))
suppressMessages(library(zoo))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(bit64))
suppressMessages(library(nanotime))
suppressMessages(library(gganimate))
suppressMessages(library(gapminder))
suppressMessages(library(gifski))
suppressMessages(library(gridExtra))
suppressMessages(library(R.utils))
suppressMessages(library(plotly))
suppressMessages(library(Metrics))
suppressMessages(library(plm))
suppressMessages(library(randomForest))
suppressMessages(library(stringr))

# Options
setDTthreads(1)
options(stringsAsFactors = FALSE)

# API info
api_info <- read.table(paste("/media/chris/DATA/Documents/Bot_Trading",
                             "API_Keys.txt",
                             sep = "/"),
                       sep = ";", header = T)
API_Key <- as.character(api_info$API_Key)
API_Sign <- as.character(api_info$API_Sign)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Add standard order
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# pair = asset pair
# type = type of order (buy/sell)
# ordertype = order type:
#   market
# limit (price = limit price)
# stop-loss (price = stop loss price)
# take-profit (price = take profit price)
# stop-loss-profit (price = stop loss price, price2 = take profit price)
# stop-loss-profit-limit (price = stop loss price, price2 = take profit price)
# stop-loss-limit (price = stop loss trigger price, price2 = triggered limit price)
# take-profit-limit (price = take profit trigger price, price2 = triggered limit price)
# trailing-stop (price = trailing stop offset)
# trailing-stop-limit (price = trailing stop offset, price2 = triggered limit offset)
# stop-loss-and-limit (price = stop loss price, price2 = limit price)
# settle-position
# price = price (optional.  dependent upon ordertype)
# price2 = secondary price (optional.  dependent upon ordertype)
# volume = order volume in lots
# leverage = amount of leverage desired (optional.  default = none)
# oflags = comma delimited list of order flags (optional):
#   viqc = volume in quote currency (not available for leveraged orders)
# fcib = prefer fee in base currency
# fciq = prefer fee in quote currency
# nompp = no market price protection
# post = post only order (available when ordertype = limit)
# starttm = scheduled start time (optional):
#   0 = now (default)
# +<n> = schedule start time <n> seconds from now
# <n> = unix timestamp of start time
# expiretm = expiration time (optional):
#   0 = no expiration (default)
# +<n> = expire <n> seconds from now
# <n> = unix timestamp of expiration time
# userref = user reference id.  32-bit signed number.  (optional)
# validate = validate inputs only.  do not submit order (optional)
# optional closing order to add to system when order gets filled:
#   close[ordertype] = order type
# close[price] = price
# close[price2] = secondary price

# Values -----------------------------------------------------------------------
# descr = order description info
# order = order description
# close = conditional close order description (if conditional close set)
# txid = array of transaction ids for order (if order was added successfully)
# url      <- "https://api.kraken.com/0/private/AddOrder"
# type <- "sell"
# ordertype <- "market"
# volume <- 0.1
add_order <- function(url, key, secret, pair, type, price, ordertype, volume,
                      leverage) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce, "&pair=", pair, "&type=", type, "&ordertype=", ordertype,
                      "&volume=", volume, "&price=", price, "&leverage=", leverage)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Trade Balance
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# aclass = asset class (optional):  currency (default, always currency)
# asset = base asset used to determine balance (default = ZUSD)

# Values -----------------------------------------------------------------------
# eb = equivalent balance (combined bbalancealance of all currencies)
# tb = trade balance (combined  of all equity currencies)
# m = margin amount of open positions
# n = unrealized net profit/loss of open positions
# c = cost basis of open positions
# v = current floating valuation of open positions
# e = equity = trade balance + unrealized net profit/loss
# mf = free margin = equity - initial margin (maximum margin available to open new positions)
# ml = margin level = (equity / initial margin) * 100
# url      <- "https://api.kraken.com/0/private/Balance"

get_balance <- function (url, key, secret) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}


simple_OHLC <- function(interval, pair){
  
  what <- tryCatch(
    {
      url <- paste0('https://api.kraken.com/0/public/OHLC?pair=',pair,'&interval=', interval)
      dat <- jsonlite::fromJSON(url)
    },
    error = function(e){})
  
  df <- as.data.table(dat$result[1])
  colnames(df) <- c("time", "open", "high", "low", "close",
                    "vwap", "volume", "count")
  df[, Date_POSIXct := anytime(as.numeric(as.character(time)))]
  df$Date_POSIXct <- as.character(df$Date_POSIXct)
  
  # as numeric
  df$open <- as.numeric(df$open)
  df$high <- as.numeric(df$high)
  df$low <- as.numeric(df$low)
  df$close <- as.numeric(df$close)
  df$volume <- as.numeric(df$volume)
  return(df)
}


plot_OHLC <- function(dataset, roll, n_sort, Ns){
  
  df <- dataset
  fig <- df %>% plot_ly(x = ~Date_POSIXct , type="candlestick",
                        open = ~open, close = ~close,
                        high = ~high, low = ~low) 
  fig <- fig %>% layout(title = pair,
                        xaxis = list(rangeslider = list(visible = F)))
  
  SR <- SR_lines(df, roll, n_sort, Ns)
  

  fig <- fig %>% layout(shapes = list(hline(SR$SL), hline(SR$RL)))
  
  print(SR)
  fig
}


vline <- function(x = 0, color = "red") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

hline <- function(y = 0, color = "blue") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}


# Plot chart with SR lines and return values 
SR_lines <- function(dataset, roll, n_sort, Ns){
  
  last_close <- dataset$high[nrow(dataset)]
  
  last_prices <- tail(dataset$high[-nrow(dataset)], roll)
  last_volumes <- tail(dataset$volume[-nrow(dataset)], roll)
  mydf <- data.frame(last_prices, last_volumes)
  mydf <- arrange(mydf, mydf$last_prices)
  
  sup_df <- head(mydf, n_sort)
  sup_w_mean <- sum(sup_df$last_prices *sup_df$last_volumes)/sum(sup_df$last_volumes)
  
  rs_df <- tail(mydf, n_sort)
  rs_w_mean <- sum(rs_df$last_prices *rs_df$last_volumes)/sum(rs_df$last_volumes)
  
  return(list(SL = sup_w_mean, RL = rs_w_mean))
  
}



# Private API calls ------------------------------------------------------------
myfun <- function (url, key, secret) {
  
  # Nonce and post info
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  
  # Strip kraken url
  method_path <- gsub("^.*?kraken.com", "", url)
  
  # Secret APi key 
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data),
                                                         algo = "sha256",
                                                         serialize = FALSE, 
                                                         raw = TRUE)),
               algo = "sha512", raw = TRUE)
  # Header
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url,
                                                      binary = TRUE,
                                                      postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                   Trade Balance
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Inputs -----------------------------------------------------------------------
# aclass = asset class (optional):  currency (default, always currency)
# asset = base asset used to determine balance (default = ZUSD)

# Values -----------------------------------------------------------------------
# eb = equivalent balance (combined bbalancealance of all currencies)
# tb = trade balance (combined  of all equity currencies)
# m = margin amount of open positions
# n = unrealized net profit/loss of open positions
# c = cost basis of open positions
# v = current floating valuation of open positions
# e = equity = trade balance + unrealized net profit/loss
# mf = free margin = equity - initial margin (maximum margin available to open new positions)
# ml = margin level = (equity / initial margin) * 100
# url      <- "https://api.kraken.com/0/private/Balance"

get_balance <- function (url, key, secret) {
  
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key =  RCurl::base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, 
                                                                         post_data), algo = "sha256", serialize = FALSE, 
                                                         raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c(`API-Key` = key, `API-Sign` =  RCurl::base64Encode(sign))
  
  curl <- RCurl::getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  query_result_json <- rawToChar(RCurl::getURLContent(curl = curl, 
                                                      url = url, binary = TRUE, postfields = post_data, 
                                                      httpheader = httpheader))
  query_result <- jsonlite::fromJSON(query_result_json)
  
  return(query_result)
}
