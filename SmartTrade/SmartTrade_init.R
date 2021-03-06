# Initialization
rm(list = ls())

path_source <- "/media/chris/DATA/Documents/Bot_Trading/SmartTrade"
files.sources = grep("API.R", list.files(path_source, full.names = T), value = T)
sapply(files.sources, source)

# Params
pair <- "ETHEUR"
interval <- 1

# Get data
OHLC <- simple_OHLC(interval, pair)

# Get a first visual with support and reisstance lines
plot_OHLC(dataset = OHLC, roll = 50, n_sort = 3)

# Trade params
price <- 991
tp <- 5/100
sl <- 3/100
volume = 0.2
leverage = 2

# Give API Order to limit buy 
buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                           ordertype = "limit", volume = volume, price = price,
                           leverage = leverage)

# Set your exits once order is open
price_tp <- price + price * tp
price_sl <- price - price * sl

repeat {
 
  # retrieve all closed orders
  closed <- myfun(url = "https://api.kraken.com/0/private/ClosedOrders",
                  key = API_Key,
                  secret = API_Sign)
  
  if (length(closed$error) > 0){
    print("API rate limit exceeded")
  } else {
    print("API rate limit ok")
  }
  
  
  if(buy_it$result$txid %in% names(closed$result$closed)) {
    
    OHLC <- simple_OHLC(interval, pair)
    
    if(tail(OHLC$close, 1) > price_tp) {

      sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                          key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                          ordertype = "market", volume = volume,
                          leverage = leverage, price = 10000)
      status <- "tp_sold"
      print(status)
    } else if(tail(OHLC$close, 1) < price_sl) {
      
      sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                           ordertype = "market", volume = volume,
                           leverage = leverage, price = 10000)
      status <- "sl_sold"
      print(status)
    } else {
      
      status <- "no_action"
      print(paste(status, Sys.time()))
    }
  
  if(status %in% c("tp_sold", "sl_sold")){
    print("exiting procedure")
    break
  }
  
  } else {
    
    print(paste("Position not yet entered", Sys.time()))
  }
  Sys.sleep(10)
}

