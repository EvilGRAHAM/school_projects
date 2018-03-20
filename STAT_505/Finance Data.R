library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

theme_minimal2 <- theme_minimal() %>%  theme_set()
theme_minimal2 <-
  theme_update(
    panel.border = element_rect(
      linetype = "solid"
      ,colour = "grey92"
      ,fill = NA
    )
    ,strip.background = element_rect(
      linetype = "solid"
      ,colour = "grey92"
      ,fill = NA
    )
  )

start_date <- as_date("2018-03-13")

# Anything of the form: https://fred.stlouisfed.org/series/*, where * is the ticker that is looked up in the first parameter of tq_get
# https://fred.stlouisfed.org/series/DPROPANEMBTX Mount Belvieu Propane Prices
commodity_prices <- 
  tq_get(
    x = tibble(commodity = c("DPROPANEMBTX"))
    ,get = "economic.data"
    ,from = start_date - years(5)
    ,to = start_date
  ) %>%
  group_by(commodity) %>% 
  tq_transmute(
    select = price
    ,mutate_fun = periodReturn
    ,period = "daily"
    ,col_rename = "R_a"
  )
commodity_prices

# FANG stocks as coined by Jim Cramer
fang_list <- c("FB", "AMZN", "NFLX", "GOOG")
num_stocks <- length(fang_list)

fang_stocks <- 
  tq_get(
    x = tibble(symbol = fang_list)
    ,get = "stock.prices"
    ,from = start_date - years(5)
    ,to = start_date
  ) %>% 
  group_by(symbol)

fang_stocks <- 
  fang_stocks%>% 
  tq_mutate(
    select = adjusted
    ,mutate_fun = periodReturn
    ,period = "daily"
    ,col_rename = "R_a"
  ) %>% 
  tq_mutate(
    select = adjusted
    ,mutate_fun = apply.daily
    ,FUN = log
    ,col_rename = "log_adj"
  )

fang_stocks

fang_stocks %>% 
  ggplot(
    aes(
      x = date
      ,y = adjusted
      ,open = open
      ,high = high
      ,low = low
      ,close = close
    )
  ) +
  geom_barchart() +
  # geom_bbands(
  #   ma_fun = SMA
  #   ,sd = 2
  #   ,n = 20
  # ) +
  facet_wrap(
    ~ symbol
    ,scales = "free_y"
  )

weight_portfolio <- rep(1/num_stocks, times = num_stocks)
fang_portfolio <- 
  fang_stocks %>% 
  tq_portfolio(
    assets_col = symbol
    ,returns_col = R_a
    ,weights = weight_portfolio
    ,col_rename = "R_Port"
  )
fang_portfolio

fang_stocks %>% 
  select(
    date
    ,symbol
    ,R_a
  ) %>% 
  spread(
    key = symbol
    ,value = R_a
  ) %>% 
  left_join(
    fang_portfolio
    ,by = "date"
  ) %>% 
  gather(
    key = symbol
    ,value = R_a
    ,-date
  ) %>% 
  ggplot(aes(x = date, y = R_a)) +
  geom_line() +
  facet_wrap(
    ~ symbol
    ,scales = "fixed"
  )
