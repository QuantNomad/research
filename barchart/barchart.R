library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
library(gridExtra)

# Loading barchart of the day data for 2019
barchart <- read_csv("barchart_of_the_day.csv")

# Glimpse to the data
barchart %>% head

# % of technical analysis indicators are long
as.tibble(barchart$signal_perc %>% table %>% prop.table %>% round(4) * 100) 

###########################
## GETTING DATA FROM IEX ##
###########################

# Set params for IEX API 
params <- list(
  token = "your_token_here",
  chartByDay = "true"
)

# Get data for all symbols
symbol_data_list <- lapply(
  barchart$symbol, 
  function(s){
    
    # Compose API URL
    url <- str_c(
      "https://cloud.iexapis.com/stable/stock/", 
      str_to_lower(s) ,
      "/chart/ytd"
    )
    
    # Requesting Data from API 
    # Parsing it from JSON to Data Frame
    data <- as.data.frame(
      fromJSON(
        content(
          GET(url, query = params), 
          as = "text"
        )
      ), 
      stringsAsFactors = F
    )
    
    # Add symbol and leave only needed columns
    data <- data %>%
      mutate(
        symbol = s, 
        date = as.Date(date)
      ) %>%
      select(symbol, date, close)
    
    data
  }
)

# Union list as data frame 
symbol_data_df <- bind_rows(symbol_data_list)

# Get SPY data
spy_url <- "https://cloud.iexapis.com/stable/stock/SPY/chart/ytd"

spy <- as.data.frame(
  fromJSON(
    content(GET(spy_url, query = params), as = "text")
  ), 
  stringsAsFactors = F
)

spy <- spy %>%
  mutate(date = as.Date(date)) %>%
  select(date, spy = close) %>%
  filter(date >= min(barchart$entry_date))

head(spy)

# Get Last availabe price for each symbol
last_price <- symbol_data_df %>%
  group_by(symbol) %>%
  filter(row_number(desc(date)) == 1) %>%
  select(symbol, last_price = close)

# Join price at publishing date and last availabe price
# Calculating pnl
barchart_pnl_publish_today <- barchart %>%
  inner_join(
    symbol_data_df %>% rename(entry_price = close), 
    by = c("symbol", "entry_date" = "date")
  ) %>%
  inner_join(last_price, by = "symbol") %>%
  mutate(
    pnl = (last_price / entry_price -1) * 100
  )

# Looking at pnl summary
barchart_pnl_publish_today$pnl %>% summary

# Check SPY performance
cat(str_c("SPY Perf: ", round((last(spy$spy) / first(spy$spy) - 1) * 100, 2)))

options(repr.plot.width=4, repr.plot.height=3)

# PNL histogram
barchart_pnl_publish_today %>%
  ggplot(aes(x = pnl)) + 
  geom_histogram(bins = 30) + 
  geom_vline(xintercept = mean(barchart_pnl_publish_today$pnl))

# Get daily prices for all symbols from publishing date to today
barchart_daily <- barchart %>%
  inner_join(symbol_data_df, by = "symbol") %>%
  filter(date >= entry_date)

# Normilize price, starting from 100$
barchart_daily <- barchart_daily %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    price_norm = close / first(close) * 100,
    days_from_entry = date - entry_date
  ) %>%
  ungroup

# Calculating average price for all symbols by date
day_avg <- barchart_daily %>%
  group_by(date) %>%
  mutate(
    avg_price_norm = mean(price_norm)
  ) 

# Calculating average price for all symbol by number of date from investments/publishing date
day_from_entry_avg <- barchart_daily %>%
  group_by(days_from_entry) %>%
  mutate(
    avg_price_norm = mean(price_norm)
  ) 

spy <- spy %>%
  mutate(
    spy_norm = spy / first(spy) * 100
  )

# Plotting prices by date
p1 <- ggplot(barchart_daily, aes(x = date, y = price_norm, color = symbol)) + 
  geom_line() + 
  theme(legend.position = "none") + 
  ylab("Price") + 
  xlab("")

p2 <- ggplot(data = day_avg, aes(x = date, y = avg_price_norm, color = "Avg Price")) + 
  geom_line() + 
  geom_hline(yintercept = 100) + 
  geom_line(data = spy, aes(x = date, y = spy_norm, color = "SPY")) +
  ylab("Price") + 
  xlab("Date") + 
  theme(legend.position = c(0.1, 0.8), legend.title = element_blank())

grid.arrange(p1, p2, ncol = 1, nrow = 2)       

p1 <- ggplot(barchart_daily, aes(x = days_from_entry, y = price_norm, color = symbol)) + 
  geom_line() + 
  theme(legend.position = "none")  + 
  ylab("Price") + 
  xlab("")

p2 <- ggplot(data = day_from_entry_avg, aes(x = days_from_entry, y = avg_price_norm)) + 
  geom_line(color = "gray") + 
  geom_hline(yintercept = 100) + 
  ylab("Price") + 
  xlab("Days in Trade")

grid.arrange(p1, p2, ncol = 1, nrow = 2)         