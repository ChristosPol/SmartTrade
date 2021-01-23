# Report path
path <- "/media/chris/DATA/Documents/Bot_Trading/equity_report/"

eq <- read.csv(list.files(path, full.names = T))
eq <- eq[eq$balance != "balance", ]
eq$balance <- as.numeric(eq$balance)
eq$last_price <- as.numeric(eq$last_price)
eq$date_time <- as.POSIXct(eq$date_time)
pairs <- unique(eq$pair)
eq$equity <- eq$balance * eq$last_price
eq <- subset(eq, eq$equity >= 1)

# eq_1 <- subset(eq, eq$pair == pairs[1])
# eq_1$equity <- eq_1$balance * eq_1$last_price


ggplot(data = eq, aes(x = date_time, y = equity))+
  geom_point()+
  geom_line(alpha = 0.5) +
  facet_wrap(~coin, scales = "free_y")

# Overall
eq <- as.data.table(eq)
eq_overall<- eq[, sum(equity), by = date_time]

ggplot(data = eq_overall, aes(x = date_time, y = V1))+
  geom_point()+
  geom_line(alpha = 0.5)
