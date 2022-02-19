library(tidyverse)

lookback_period <- 50
price_data <- read.csv("_data/NET.csv", header = TRUE)

print(paste("The Efficiency Ratio of NET for the last ", lookback_period,
            " days is ", EffRatio(price_data, lookback_period),
            ", Price Density is ", PriceDensity(price_data, lookback_period),
            ", and Fractal Dimension is ",
            FractalDimension(price_data, lookback_period), sep = ""))

EffRatio <- function(data, lookback) {
    prices <- arrange(data, desc(Date))
    prices_close <- pull(slice(prices, 1:lookback), Close)
    prices_close_1 <- pull(slice(prices, 2:(lookback+1)), Close)
    net_change <- abs(prices_close[1] - prices_close[lookback])
    sum_price_changes <- sum(abs(prices_close - prices_close_1))
    eff_ratio <- round(net_change / sum_price_changes, 4)
    return(eff_ratio)
}

PriceDensity <- function(data, lookback) {
    prices <- arrange(data, desc(Date))
    prices_high <- pull(slice(prices, 1:lookback), High)
    prices_low <- pull(slice(prices, 1:lookback), Low)
    high_low_diff <- sum(prices_high - prices_low)
    max_high <- max(prices_high)
    min_low <- min(prices_low)
    price_density <- round(high_low_diff / (max_high - min_low), 4)
    return(price_density)
}

FractalDimension <- function(data, lookback) {
    prices <- arrange(data, desc(Date))
    prices_close <- pull(slice(prices, 1:lookback), Close)
    prices_close_1 <- pull(slice(prices, 2:(lookback+1)), Close)
    prices_high <- pull(slice(prices, 1:lookback), High)
    prices_low <- pull(slice(prices, 1:lookback), Low)
    range_max_min <- max(prices_high) - min(prices_low)
    dx <- (1 / lookback) ** 2
    L <- sum(sqrt(dx + abs(prices_close -  prices_close_1) / range_max_min))
    frac_dim <- round(1 + (log(L) + log(2)) / log(2 * lookback), 4)
    return(frac_dim)
}
