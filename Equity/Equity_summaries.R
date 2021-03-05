# Report path
path <- "/media/chris/DATA/Documents/Bot_Trading/equity_report/"

eq <- read.csv(list.files(path, full.names = T))
eq <- eq[eq$balance != "balance", ]
eq$balance <- as.numeric(eq$balance)
eq$last_price <- as.numeric(eq$last_price)
eq$date_time <- as.POSIXct(eq$date_time)
# eq$date_time <- as.Date(eq$date_time, format ="%y-%m-%d")

pairs <- unique(eq$pair)
eq$equity <- eq$balance * eq$last_price
eq <- subset(eq, eq$equity >= 0.5)
eq$equity <- round(eq$equity, 1)
# eq_1 <- subset(eq, eq$pair == pairs[1])
# eq_1$equity <- eq_1$balance * eq_1$last_price


ggplot(data = eq, aes(x = date_time, y = equity))+
  geom_point(size = 1.5, colour = "black")+
  geom_line(alpha = 1, linetype = "dashed") +
  facet_wrap(~coin, scales = "free_y") +
  # scale_y_continuous(breaks=unique(eq$equity)) +
  theme_light()

# Overall
eq <- as.data.table(eq)
eq_overall<- eq[, sum(equity), by = date_time]
colnames(eq_overall)[2] <- "total_equity"

ggplot(data = eq_overall, aes(x = date_time, y = total_equity))+
  geom_point(size = 1.5, colour = "black")+
  geom_line(alpha = 1, linetype = "dashed")+
  # scale_y_continuous(breaks=unique(eq_overall$total_equity)) +
  ggtitle("Total equity in Euros") +
  xlab("Date") + ylab("Euros")+
  theme_light()

