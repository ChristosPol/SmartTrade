# Initialization
rm(list = ls())

path_source <- "/media/chris/DATA/Documents/Bot_Trading/SmartTrade"
files.sources = grep("API.R", list.files(path_source, full.names = T), value = T)
sapply(files.sources, source)

# Params
pair <- "ETHEUR"
interval <- 60

# Get data

repeat {
  
  OHLC <- simple_OHLC(interval, pair)
  values <- tail(OHLC[, close], 2)
  print((values[2] - values[1]) / values[2] * 100)
  
  # print(OHLC)
  Sys.sleep(1)
  
  
}
OHLC <- simple_OHLC(interval, pair)
