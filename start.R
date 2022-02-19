library(tidyverse)

lookback_period <- 50
price_data <- read.csv("_data/NET.csv", header = TRUE)

price_data <- arrange(price_data, desc(Date))
price_close <- pull(slice(price_data, 1:lookback_period), Close)
net_change <- abs(price_close[1] - price_close[lookback_period])
sum_price_changes <- sum(abs(price_close))
eff_ratio <- round(net_change / sum_price_changes, 4)
print(paste("The Efficiency Ratio of NET for the last", lookback_period, "days is", eff_ratio))