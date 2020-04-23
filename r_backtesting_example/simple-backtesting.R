library(tidyverse)
library(TTR)
library(PerformanceAnalytics)

# 5 minute AAPL data
aapl_5m <- readRDS("nasdaq_aapl_5.rds")

aapl_5m <- aapl_5m %>%
  arrange(datetime) %>%
  mutate(
    pnl = replace_na(close / lag(close) - 1, 0)
  )

str <- aapl_5m %>%
  arrange(datetime) %>%
  mutate(
    fast_ma = SMA(close, 25),
    slow_ma = SMA(close, 100),
    pos     = if_else(fast_ma > slow_ma, 1, -1) %>% lag() %>% replace_na(0),
    str_pnl = pos * pnl 
  )

daily <- str %>%
  group_by(
    date = as.Date(datetime)
  ) %>%
  summarise(
    pnl = prod(1 + str_pnl) - 1
  )

daily_xts <- xts(daily$pnl, order.by = daily$date)

(prod(daily$pnl + 1) - 1) * 100

maxDrawdown(daily_xts)

table.AnnualizedReturns(daily_xts)

charts.PerformanceSummary(daily_xts)
