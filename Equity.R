# Initialization
rm(list = ls())

path_source <- "/media/chris/DATA/Documents/Bot_Trading/SmartTrade"
files.sources = grep("API.R", list.files(path_source, full.names = T), value = T)
sapply(files.sources, source)

# Report path
path <- "/media/chris/DATA/Documents/Bot_Trading/equity_report/"

# Get your balance in coins
equity <- get_balance(url = "https://api.kraken.com/0/private/Balance",
            key = API_Key,
            secret = API_Sign)
eq <- data.frame(balance = unlist(equity$result))
eq$coin <- rownames(eq)
rownames(eq) <- NULL

# remove currencies
currency <- c("ZEUR", "CHF", "USDT")
eq <- eq[!eq$coin %in% currency, ]
eq$pair <- NA
eq$last_price <- NA
eq$date_time <- NA

# Match coins to euro pairs to get equity
avail_pairs <- myfun("https://api.kraken.com/0/public/AssetPairs", secret = API_Sign, key = API_Key)
avail_pairs <- names(avail_pairs$result)

# Get the official ticker
for(i in 1:nrow(eq)){
  eq$pair[i] <- grep("EUR", grep(eq$coin[i], avail_pairs, value = T), value = T)[1]
}

# Get last trade price for each ticker
for (i in 1:length(eq$pair)){
  url <- paste0('https://api.kraken.com/0/public/Ticker?pair=', eq$pair[i])
  dat <- jsonlite::fromJSON(url)
  eq$last_price[i] <- unlist(dat$result)[7]
  eq$date_time <- Sys.time()
  Sys.sleep(3)
  print(i)
}

# Extract as a csv
write.table(eq, file = paste0(path, "equity_report.csv"), append = T,
          row.names = F, sep = ",")

