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

banana_price <-
  commodity_prices %>% 
  filter(commodity == "PBANSOPUSDM") %>% 
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

banana_price

banana_price %>% 
  tq_performance(
    Ra = R_a
    ,Rb = NULL
    , performance_fun = table.AnnualizedReturns
  )

# Original TS Plot ----------
banana_price %>% 
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
    title = "Banana Price"
    ,y = "Price (USD/Metric Ton)"
  )


# Original Seasonal Plot ----------
banana_price %>% 
  ggplot(
    aes(
      x = Year
      ,y = Price
      ,colour = `Month Name`
    )
  ) +
  geom_line() +
  labs(
    title = "Seasonal Banana Price"
    ,y = "Price (USD/Metric Ton)"
    ,colour = "Month"
  )

banana_price %>%
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
  scale_colour_distiller(
    type = "div"
    ,palette = "RdYlBu"
  ) +
  labs(
    title = "Seasonal Banana Price"
    ,y = "Price (USD/Metric Ton)"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  )

# Lagged Scatter Plot ----------
banana_lagged <- 
  banana_price %>% 
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

banana_lagged %>% 
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
  labs(title = "Lag Scatter Plot")

banana_lagged %>% 
  select(`Lag 00` = Price, starts_with("Lag")) %>% 
  ggcor(lb = 0.75, use = "complete.obs") +
  labs(title = "Lagged Correlation Plot")

# Original ACF + PACF ----------
ggacf(banana_price, col = Price, type = "correlation") + labs(title = "Price ACF")
ggacf(banana_price, col = Price, type = "partial") + labs(title = "Price PACF")

# Transformations ----------
banana_price <- 
  banana_price %>% 
  mutate(
    `Log Price` = log(Price)
    ,`Log Return` = log(1 + R_a)
    ,`Box Cox Price` = BoxCox(Price, lambda = BoxCox.lambda(Price))
    ,`Box Cox Return` = BoxCox(Price, lambda = BoxCox.lambda(R_a))    
  )

# Transformation TS Plot ----------
banana_price %>% 
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

banana_price %>%
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
  scale_colour_distiller(
    type = "div"
    ,palette = "RdYlBu"
  ) +
  labs(
    title = "Seasonal Banana Monthly Return"
    ,y = "Monthly Return (%)"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  )

# Tranformation ACF ----------
banana_R_a_acf <- ggacf(banana_price, col = R_a, type = "correlation") + labs(title = "R_a ACF")
banana_R_a_pacf <- ggacf(banana_price, col = R_a, type = "partial") + labs(title = "R_a PACF")
# ggacf(banana_price, col = `Log Price`, type = "correlation")
# ggacf(banana_price, col = `Log Price`, type = "partial")
banana_log_ret_acf <- ggacf(banana_price, col = `Log Return`, type = "correlation") + labs(title = "Log Return ACF")
banana_log_ret_pacf <- ggacf(banana_price, col = `Log Return`, type = "partial") + labs(title = "Log Return PACF")
# ggacf(banana_price, col = `Box Cox Price`, type = "correlation")
# ggacf(banana_price, col = `Box Cox Price`, type = "partial")
grid.arrange(banana_R_a_acf, banana_R_a_pacf, banana_log_ret_acf, banana_log_ret_pacf)

# Decomposition of Transformed Data ----------
banana_price_decomp <- 
  banana_price$Price %>% 
  ts(frequency = 12) %>% 
  stl(s.window = "periodic")
banana_ra_decomp <- 
  banana_price$R_a %>% 
  ts(frequency = 12) %>% 
  stl(s.window = "periodic")
banana_logreturn_decomp <- 
  banana_price$`Log Return` %>% 
  ts(frequency = 12) %>% 
  stl(s.window = "periodic")
summary(banana_price_decomp)
summary(banana_ra_decomp)
summary(banana_logreturn_decomp)

banana_price %>% 
  select(
    Date
    ,Year
    ,Month
    ,`Month Name`
    ,Day
    ,Price
  ) %>% 
  bind_cols(
    banana_price_decomp$time.series %>% 
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
  labs(title = "Banana Prices Decomposition")

banana_price %>% 
  select(
    Date
    ,Year
    ,Month
    ,`Month Name`
    ,Day
    ,R_a
  ) %>% 
  bind_cols(
    banana_ra_decomp$time.series %>% 
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
    title = "Banana Returns Decomposition"
    ,y = expression("R"[a])
  )

banana_price %>% 
  select(
    Date
    ,Year
    ,Month
    ,`Month Name`
    ,Day
    ,`Log Return`
  ) %>% 
  bind_cols(
    banana_logreturn_decomp$time.series %>% 
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
  labs(title = "Banana Log Returns Decomposition")

# ADF Test ----------
adf.test(banana_price$Price, alternative = "stationary")
adf.test(banana_price$Price, alternative = "stationary", k = 12)
adf.test(banana_price$R_a, alternative = "stationary")
adf.test(banana_price$R_a, alternative = "stationary", k = 12)
adf.test(banana_price$`Log Return`, alternative = "stationary")
adf.test(banana_price$`Log Return`, alternative = "stationary", k = 12)

# Auto ARIMA ----------
banana_price_autoarima_01 <- auto.arima(banana_price$Price, seasonal = TRUE)
summary(banana_price_autoarima_01)

banana_ra_autoarima_01 <- auto.arima(banana_price$R_a, seasonal = TRUE)
summary(banana_ra_autoarima_01)

banana_log_ret_autoarima_01 <- auto.arima(banana_price$`Log Return`, seasonal = TRUE)
summary(banana_log_ret_autoarima_01)

# Changes Start Here ----------
# Model Selection ----------
pdqPDQ <-
  0:1 %>% 
  rep(times = 4, each = 3) %>% 
  combn(m = 6) %>% 
  t() %>% 
  unique()

banana_arima_builder <- function(pdq){
  banana_arima <-
    Arima(
      y = banana_price$`Log Return`
      ,order = pdq[1:3]
      ,seasonal = pdq[4:6]
    )
    banana_aic <- banana_arima$aic
    banana_bic <- AIC(banana_arima, k = as.numeric(count(banana_price)))
    list(
      p = pdq[1]
      ,d = pdq[2]
      ,q = pdq[3]
      ,P = pdq[4]
      ,D = pdq[5]
      ,Q = pdq[6]
      # ,arima = banana_arima
      ,AIC = banana_aic
      ,BIC = banana_bic
    )
}

banana_arima_stats <- 
  1:dim(pdqPDQ)[1] %>% 
  map(~ pdqPDQ[., ]) %>% 
  map_df(banana_arima_builder)

banana_arima_stats

banana_min_aic <- 
  banana_arima_stats %>% 
  filter(AIC == min(AIC)) %>% 
  unique()
banana_min_aic

banana_min_bic <- 
  banana_arima_stats %>% 
  filter(BIC == min(BIC)) %>% 
  unique()
banana_min_bic

# This fits an ARIMA model for each of the entries in the min AIC/BIC tibbles. 
# I'm pretty sure they should be all identical, and this can be replaced by calling any
# of the entries in tibble, but I'm including this just to show our work and depth of analysis.
banana_aic_arima <- 
  banana_min_aic %>% 
  select(-AIC, -BIC) %>%
  mutate(`Model #` = 1:nrow(banana_min_aic)) %>%
  gather(
    key = Letter
    ,value = Num
    ,-`Model #`
  ) %>% 
  split(.$`Model #`) %>% 
  map(~ .$Num) %>% 
  map(
    ~ Arima(
      y = banana_price$`Log Return`
      ,order = .[1:3]
      ,seasonal = .[4:6]
    )
  )
# banana_aic_arima %>% 
#   map(summary)

# Just 1 entry
summary(banana_aic_arima$`1`)

banana_bic_arima <- 
  banana_min_bic %>% 
  select(-AIC, -BIC) %>%
  mutate(`Model #` = 1:nrow(banana_min_bic)) %>%
  gather(
    key = Letter
    ,value = Num
    ,-`Model #`
  ) %>% 
  split(.$`Model #`) %>% 
  map(~ .$Num) %>% 
  map(
    ~ Arima(
      y = banana_price$`Log Return`
      ,order = .[1:3]
      ,seasonal = .[4:6]
    )
  )
# banana_bic_arima %>% 
#   map(summary)
