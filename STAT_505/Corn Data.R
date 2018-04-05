library(forecast, warn.conflicts = FALSE, quietly = TRUE)
library(tseries, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant, warn.conflicts = FALSE, quietly = TRUE)
library(gridExtra, warn.conflicts = FALSE, quietly = TRUE)

source("ggacf.R", echo = FALSE)
source("ggcor.R", echo = FALSE)

num_months_name <- 
  tibble(
    Name =
      as.ordered(
        c(
          "Jan"
          ,"Feb"
          ,"Mar"
          ,"Apr"
          ,"May"
          ,"Jun"
          ,"Jul"
          ,"Aug"
          ,"Sep"
          ,"Oct"
          ,"Nov"
          ,"Dec"
        )
      )
    ,Num = 1:12
    ,Season = 
      as.ordered(
        c(
          rep("Winter", each = 2)
          ,rep(c("Spring", "Summer", "Fall"), each = 3)
          ,"Winter"
        )
      )
  )
num_months_name$Name <- factor(num_months_name$Name, levels = num_months_name$Name)
num_months_name$Season <- factor(num_months_name$Season, levels = c("Spring", "Summer", "Fall", "Winter"))

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

# Data Setup and Retieval ----------
# start_date <- as_date("2018-03-13")
end_date <- as_date("2017-01-01")

# Anything of the form: https://fred.stlouisfed.org/series/*, where * is the ticker that is looked up in the first parameter of tq_get
# https://fred.stlouisfed.org/series/DPROPANEMBTX Mount Belvieu Propane Prices
commodity_prices <- 
  tq_get(
    x = tibble(commodity = c("DPROPANEMBTX", "DCOILWTICO", "PMAIZMTUSDM", "PWHEAMTUSDM", "PBANSOPUSDM"))
    ,get = "economic.data"
    ,from = end_date - years(20)
    ,to = end_date
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

corn_price <-
  commodity_prices %>% 
  filter(commodity == "PMAIZMTUSDM") %>% 
  ungroup() %>% 
  select(-commodity) %>% 
  rename(
    Date = date
    ,Price = price
  ) %>% 
  mutate(
    Year = year(Date)
    ,Month = month(Date)
    ,Day = day(Date)
  ) %>% 
  left_join(
    num_months_name
    ,by = c("Month" = "Num")
  ) %>% 
  rename(`Month Name` = Name)

corn_price

corn_price %>% 
  tq_performance(
    Ra = R_a
    ,Rb = NULL
    , performance_fun = table.AnnualizedReturns
  )

# Original TS Plot ----------
corn_price %>% 
  ggplot(
    aes(
      x = Date
      ,y = Price
    )
  ) +
  geom_smooth(method = "loess", se = FALSE, colour = "#66c2a5") +
  geom_smooth(method = "lm", se = FALSE, colour = "#fc8d62") +
  geom_line() +
  labs(
    title = "Corn Price"
    ,y = "Price (USD/Metric Ton)"
  )


# Original Seasonal Plot ----------
corn_price %>% 
  ggplot(
    aes(
      x = Year
      ,y = Price
      ,colour = `Month Name`
    )
  ) +
  geom_line() +
  labs(
    title = "Seasonal Corn Price"
    ,y = "Price (USD/Metric Ton)"
    ,colour = "Month"
  )

corn_price %>%
  ggplot(
    aes(
      x = Season
      ,y = Price
    )
  ) +
  geom_violin(fill = NA) +
  geom_boxplot(fill = NA, width = 0.1, outlier.colour = NA) +
  geom_jitter(aes(colour = Year)) +
  facet_wrap(
    ~ Season
    ,nrow = 1
    ,scales = "free_x"
  ) +
  scale_color_distiller(
    type = "seq"
    ,palette = "RdYlBu"
  ) +
  labs(
    title = "Seasonal Corn Price"
    ,y = "Price (USD/Metric Ton)"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  )

# Lagged Scatter Plot ----------
corn_lagged <- 
  corn_price %>% 
  mutate(
    `Lag 01` = lag(Price, n = 1)
    ,`Lag 02` = lag(Price, n = 2)
    ,`Lag 03` = lag(Price, n = 3)
    ,`Lag 04` = lag(Price, n = 4)
    ,`Lag 05` = lag(Price, n = 5)
    ,`Lag 06` = lag(Price, n = 6)
    ,`Lag 07` = lag(Price, n = 7)
    ,`Lag 08` = lag(Price, n = 8)
    ,`Lag 09` = lag(Price, n = 9)
    ,`Lag 10` = lag(Price, n = 10)
    ,`Lag 11` = lag(Price, n = 11)
    ,`Lag 12` = lag(Price, n = 12)
  ) 

corn_lagged %>% 
  gather(
    key = Lag
    ,value = `Lagged Price`
    ,-Date
    ,-Year
    ,-Month
    ,-`Month Name`
    ,-Season
    ,-Day
    ,-Price
    ,-R_a
  ) %>% 
  ggplot(
    aes(
      x = Price
      ,y = `Lagged Price`
    )
  ) +
  geom_smooth(method = "loess", se = FALSE, colour = "#66c2a5") +
  geom_smooth(method = "lm", se = FALSE, colour = "#fc8d62") +
  geom_point() +
  facet_wrap(~ Lag) +
  labs(title = "Lag Scatter Plot") +
  labs(title = "Lagged Correlation Plot")

corn_lagged %>% 
  select(`Lag 00` = Price, starts_with("Lag")) %>% 
  ggcor(lb = 0.75, use = "complete.obs")

# Original ACF + PACF ----------
ggacf(corn_price, col = Price, type = "correlation") + labs(title = "Price ACF")
ggacf(corn_price, col = Price, type = "partial") + labs(title = "Price PACF")

# Transformations ----------
corn_price <- 
  corn_price %>% 
  mutate(
    `Log Price` = log(Price)
    ,`Log Return` = log(1 + R_a)
    ,`Box Cox Price` = BoxCox(Price, lambda = BoxCox.lambda(Price))
    ,`Box Cox Return` = BoxCox(Price, lambda = BoxCox.lambda(R_a))    
  )

# Transformation TS Plot ----------
corn_price %>% 
  gather(
    key = Transformation
    ,value = Value
    ,-Date
    ,-Year
    ,-Month
    ,-`Month Name`
    ,-Season
    ,-Day
  ) %>% 
  ggplot(
    aes(
      x = Date
      ,y = Value
    )
  ) +
  geom_smooth(method = "loess", se = FALSE, colour = "#66c2a5") +
  geom_smooth(method = "lm", se = FALSE, colour = "#fc8d62") +
  geom_line() +
  facet_wrap(
    ~ Transformation
    ,scales = "free_y"
    ,ncol = 1
  ) +
  labs(title = "Time Series Plot of Transformations")

corn_price %>%
  ggplot(
    aes(
      x = Season
      ,y = R_a
    )
  ) +
  geom_violin(fill = NA) +
  geom_boxplot(fill = NA, width = 0.1, outlier.colour = NA) +
  geom_jitter(aes(colour = Year)) +
  facet_wrap(
    ~ Season
    ,nrow = 1
    ,scales = "free_x"
  ) +
  scale_color_distiller(
    type = "seq"
    ,palette = "RdYlBu"
  ) +
  labs(
    title = "Seasonal Corn Monthly Return"
    ,y = "Monthly Return (%)"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  )

# Tranformation ACF ----------
corn_R_a_acf <- ggacf(corn_price, col = R_a, type = "correlation") + labs(title = "R_a ACF")
corn_R_a_pacf <- ggacf(corn_price, col = R_a, type = "partial") + labs(title = "R_a PACF")
# ggacf(corn_price, col = `Log Price`, type = "correlation")
# ggacf(corn_price, col = `Log Price`, type = "partial")
corn_log_ret_acf <- ggacf(corn_price, col = `Log Return`, type = "correlation") + labs(title = "Log Return ACF")
corn_log_ret_pacf <- ggacf(corn_price, col = `Log Return`, type = "partial") + labs(title = "Log Return PACF")
# ggacf(corn_price, col = `Box Cox Price`, type = "correlation")
# ggacf(corn_price, col = `Box Cox Price`, type = "partial")
grid.arrange(corn_R_a_acf, corn_R_a_pacf, corn_log_ret_acf, corn_log_ret_pacf)

# Decomposition of Transformed Data ----------
corn_price_decomp <- 
  corn_price$Price %>% 
  ts(frequency = 12) %>% 
  stl(s.window = "periodic")
corn_ra_decomp <- 
  corn_price$R_a %>% 
  ts(frequency = 12) %>% 
  stl(s.window = "periodic")
corn_logreturn_decomp <- 
  corn_price$`Log Return` %>% 
  ts(frequency = 12) %>% 
  stl(s.window = "periodic")
summary(corn_price_decomp)
summary(corn_ra_decomp)
summary(corn_logreturn_decomp)

corn_price %>% 
  select(
    Date
    ,Year
    ,Month
    ,`Month Name`
    ,Day
    ,Price
  ) %>% 
  bind_cols(
    corn_price_decomp$time.series %>% 
      data.frame() %>% 
      as_tibble() %>% 
      rename(
        Remainder = remainder
        ,Seasonal = seasonal
        ,Trend = trend
      )
  ) %>% 
  gather(
    key = Decomposition
    ,value = Price
    ,-Date
    ,-Year
    ,-Month
    ,-`Month Name`
    ,-Day
  ) %>% 
  ggplot(
    aes(
      x = Date
      ,y = Price
    )
  ) +
  geom_line() +
  facet_wrap(
    ~ Decomposition
    ,ncol = 1
    ,scales = "free_y"
  ) +
  labs(title = "Corn Prices Decomposition")

corn_price %>% 
  select(
    Date
    ,Year
    ,Month
    ,`Month Name`
    ,Day
    ,R_a
  ) %>% 
  bind_cols(
    corn_ra_decomp$time.series %>% 
      data.frame() %>% 
      as_tibble() %>% 
      rename(
        Remainder = remainder
        ,Seasonal = seasonal
        ,Trend = trend
      )
  ) %>% 
  gather(
    key = Decomposition
    ,value = Return
    ,-Date
    ,-Year
    ,-Month
    ,-`Month Name`
    ,-Day
  ) %>% 
  ggplot(
    aes(
      x = Date
      ,y = Return
    )
  ) +
  geom_line() +
  facet_wrap(
    ~ Decomposition
    ,ncol = 1
    ,scales = "free_y"
  ) +
  labs(
    title = "Corn Returns Decomposition"
    ,y = expression("R"[a])
  )

corn_price %>% 
  select(
    Date
    ,Year
    ,Month
    ,`Month Name`
    ,Day
    ,`Log Return`
  ) %>% 
  bind_cols(
    corn_logreturn_decomp$time.series %>% 
      data.frame() %>% 
      as_tibble() %>% 
      rename(
        Remainder = remainder
        ,Seasonal = seasonal
        ,Trend = trend
      )
  ) %>% 
  gather(
    key = Decomposition
    ,value = `Log Return`
    ,-Date
    ,-Year
    ,-Month
    ,-`Month Name`
    ,-Day
  ) %>% 
  ggplot(
    aes(
      x = Date
      ,y = `Log Return`
    )
  ) +
  geom_line() +
  facet_wrap(
    ~ Decomposition
    ,ncol = 1
    ,scales = "free_y"
  ) +
  labs(title = "Corn Log Returns Decomposition")

# ADF Test ----------
adf.test(corn_price$Price, alternative = "stationary")
adf.test(corn_price$Price, alternative = "stationary", k = 12)
adf.test(corn_price$R_a, alternative = "stationary")
adf.test(corn_price$R_a, alternative = "stationary", k = 12)
adf.test(corn_price$`Log Return`, alternative = "stationary")
adf.test(corn_price$`Log Return`, alternative = "stationary", k = 12)

# SARIMA ----------
corn_price_autoarima_01 <- auto.arima(corn_price$Price, seasonal = TRUE)
summary(corn_price_autoarima_01)

corn_ra_autoarima_01 <- auto.arima(corn_price$R_a, seasonal = TRUE)
summary(corn_ra_autoarima_01)

corn_log_ret_autoarima_01 <- auto.arima(corn_price$`Log Return`, seasonal = TRUE)
summary(corn_log_ret_autoarima_01)

# Model Selection ----------
pdqPDQ <-
  0:1 %>% 
  rep(times = 3, each = 2) %>% 
  combn(m = 6) %>% 
  t()

corn_arima_builder <- function(pdq){
  corn_arima <-
    arima(
      x = corn_price$`Log Return`
      ,order = pdq[1:3]
      ,seasonal = pdq[4:6]
    )
    corn_aic <- corn_arima$aic
    corn_bic <- AIC(corn_arima, k = as.numeric(count(corn_price)))
    list(
      p = pdq[1]
      ,d = pdq[2]
      ,q = pdq[3]
      ,P = pdq[4]
      ,D = pdq[5]
      ,Q = pdq[6]
      # ,arima = corn_arima
      ,AIC = corn_aic
      ,BIC = corn_bic
    )
}

corn_arima_stats <- 
  1:dim(pdqPDQ)[1] %>% 
  map(~ pdqPDQ[., ]) %>% 
  map_df(corn_arima_builder)

corn_arima_stats

corn_min_aic <- 
  corn_arima_stats %>% 
  filter(AIC == min(AIC)) %>% 
  unique()
corn_min_aic

corn_min_bic <- 
  corn_arima_stats %>% 
  filter(BIC == min(BIC)) %>% 
  unique()
corn_min_bic

corn_aic_arima <- 
  arima(
    x = corn_price$`Log Return`
    ,order = c(3, 0, 2)
  )
summary(corn_aic_arima)

corn_bic_arima <- 
  arima(
    x = corn_price$`Log Return`
    ,order = c(0, 1, 0)
  )
summary(corn_bic_arima)

