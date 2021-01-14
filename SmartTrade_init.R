# Initialization
rm(list = ls())

path_source <- "/media/chris/DATA/Documents/Bot_Trading/SmartTrade"
files.sources = grep("API.R", list.files(path_source, full.names = T), value = T)
sapply(files.sources, source)

# Params
pair <- "ETHEUR"
interval <- 60

# Get data
OHLC <- simple_OHLC(interval, pair)

# Get a first visual with support and reisstance lines
plot_OHLC(dataset = OHLC, roll = 200, n_sort = 3)

# Trade params
price <- 700
tp <- 0.02
sl <- 0.01
volume = 0.1
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
  
  
  if(buy_it$result$txid %in% names(closed$result$closed)) {
    
    OHLC <- simple_OHLC(interval, pair)
    
    if(tail(OHLC$close, 1) > price_tp) {

      sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                          key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                          ordertype = "market", volume = volume,
                          leverage = leverage)
      status <- "tp_sold"
      print(status)
    } else if(tail(OHLC$close, 1) < price_sl) {
      
      sell_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = pair, type = "sell",
                           ordertype = "market", volume = volume,
                           leverage = leverage)
      status <- "sl_sold"
      print(status)
    } else {
      
      status <- "no_action"
      print(status)
    }
  
  if(status %in% c("tp_sold", "sl_sold")){
    print("exiting procedure")
    break
  }
  
  } else {
    
    print("Position not yet entered")
  }
  Sys.sleep(5)
}
