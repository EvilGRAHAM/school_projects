library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)

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

ggacf <- function(data, column, alpha = 0.05, ...){
  column <- enquo(column)
  
  data_acf <- 
    data %>%
    select(!! column) %>% 
    acf(plot = FALSE, ...)
  
  y_label <- 
    (data_acf$type %>%
       as_tibble() %>%
       mutate(
         title = case_when(
           value == "correlation" ~ "ACF"
           ,value == "partial" ~ "Partial ACF"
           ,value == 'covariance' ~ "ACF (Cov)"
         )
       ))$title
  
  df_acf <- data.frame(Lag = data_acf$lag, Autocorrelation = data_acf$acf)
  
  if(data_acf$type == "partial"){
    df_acf <- bind_rows(df_acf, tibble(Lag = 0, Autocorrelation = 1))
  }
  
  df_acf %>% 
    ggplot(
      aes(
        x = Lag
        ,y = Autocorrelation
      )
    ) +
    geom_ribbon(
      aes(
        ymin = qnorm(p = alpha/2, mean = 0, sd = 1, lower.tail = TRUE)/sqrt(data %>% count() %>% as.numeric())
        ,ymax = qnorm(p = alpha/2, mean = 0, sd = 1, lower.tail = FALSE)/sqrt(data %>% count() %>% as.numeric())
      )
      ,fill = "grey60"
      ,alpha = 0.4
    ) +
    geom_hline(
      aes(
        yintercept = 0
      )
      ,linetype = "dotted"
    ) +
    geom_segment(
      aes(
        xend = Lag
        ,yend = 0
      )
    ) +
    labs(y = y_label)
}


start_date <- as_date("2018-03-13")

# Anything of the form: https://fred.stlouisfed.org/series/*, where * is the ticker that is looked up in the first parameter of tq_get
# https://fred.stlouisfed.org/series/DPROPANEMBTX Mount Belvieu Propane Prices
commodity_prices <- 
  tq_get(
    x = tibble(commodity = c("DPROPANEMBTX", "DCOILWTICO", "PMAIZMTUSDM", "PWHEAMTUSDM"))
    ,get = "economic.data"
    ,from = start_date - years(5)
    ,to = start_date
  ) %>%
  filter(!is.na(price)) %>% 
  group_by(commodity) %>% 
  tq_mutate(
    select = price
    ,mutate_fun = periodReturn
    ,period = "daily"
    ,col_rename = "R_a"
  )
commodity_prices

commodity_prices %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  facet_wrap(
    ~ commodity
    ,scales = "free_y"
  )

commodity_prices %>% 
  ggplot(aes(x = date, y = R_a)) +
  geom_line() +
  geom_smooth(
    method = "lm"
    ,se = FALSE
  ) +
  geom_smooth(
    method = "gam"
    ,se = FALSE
  ) +
  facet_wrap(
    ~ commodity
    ,scales = "fixed"
  )

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
  fang_stocks %>% 
  tq_mutate(
    select = adjusted
    ,mutate_fun = periodReturn
    ,period = "daily"
    ,col_rename = "R_a"
  ) %>% 
  mutate(log_return = log(R_a + 1))

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
  geom_smooth(
    method = "lm"
    ,se = FALSE
  ) +
  geom_smooth(
    method = "gam"
    ,se = FALSE
  ) +
  facet_wrap(
    ~ symbol
    ,scales = "fixed"
  )

fang_stocks %>%  
  split(.$symbol) %>% 
  map(ungroup) %>% 
  map(
    ~ ggacf(data = ., column = log_return, na.action = na.pass)
  )

fang_stocks %>% 
  ungroup() %>% 
  filter(symbol == "GOOG") %>% 
  ggacf(
    column = adjusted
    # column = R_a
    ,na.action = na.pass
  )

fang_stocks %>% 
  ungroup() %>% 
  filter(symbol == "GOOG") %>% 
  ggacf(
    column = adjusted
    # column = R_a
    ,type = "partial"
    ,na.action = na.pass
  )
