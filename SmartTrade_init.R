# Initialization
rm(list = ls())

path_source <- "/media/chris/DATA/Documents/Bot_Trading/SmartTrade"
files.sources = grep("API.R", list.files(path_source, full.names = T), value = T)
sapply(files.sources, source)

# Params
pair <- "BTCEUR"
interval <- 60

# Get data
OHLC <- simple_OHLC(interval, pair)

# Get a first visual with support and reisstance lines
plot_OHLC(dataset = OHLC, roll = 50, n_sort = 5)

# Trade params
limit_buy <- 28000
tp <- 0.02
sl <- 0.01



# Give API Order to buy at market
buy_it <- add_order(url = "https://api.kraken.com/0/private/AddOrder",
                           key = API_Key, secret = API_Sign, pair = pair, type = "buy",
                           ordertype = "market", volume = initial_budget / da$close[nrow(da)])

