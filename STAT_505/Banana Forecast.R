source("Banana Data.R", echo = FALSE)

# Functions ----------
log_ret_to_price <- function(historical_data, forecast_data){
  historical_data %>% 
    bind_rows(
      bind_cols(
        Date = 
          historical_data %>% 
          filter(
            Date == max(Date)
            ,Type == "Forecast"
          ) %>% 
          .$Date + months(1)
        ,(
          historical_data %>% 
            filter(
              Date == max(Date)
              ,Type == "Forecast"
            ) %>%
            select(-Date, -Type) %>% 
            mutate_all(funs(./.*Mean))
        ) * 
          (
            forecast_data %>% 
              filter(
                Date == max(historical_data$Date) + months(1)
              ) %>% 
              select(-Date, -Type) %>%
              exp()
          )
        ,Type = "Forecast"
      )
    )
}


# Auto ARIMA ----------
num_periods_ahead <- 12*3
banana_autoarima_forecast <- 
  banana_log_ret_autoarima_01 %>% 
  forecast(num_periods_ahead) 

banana_autoarima_forecast <-
  tibble(
    Date = max(banana_price$Date) + months(1:num_periods_ahead)
    ,Mean = as.numeric(banana_autoarima_forecast$mean)
  ) %>% 
  bind_cols(as_tibble(banana_autoarima_forecast$lower)) %>% 
  rename(`Lower 80%` = `80%`, `Lower 95%` = `95%`) %>% 
  bind_cols(as_tibble(banana_autoarima_forecast$upper)) %>% 
  rename(`Upper 80%` = `80%`, `Upper 95%` = `95%`) %>% 
  mutate(Type = "Forecast") %>% 
  bind_rows()
banana_autoarima_forecast

banana_forecast_logreturn <- 
  banana_price %>% 
  select(
    Date
    ,Mean = `Log Return`
  ) %>% 
  mutate(
    `Lower 80%` = Mean
    ,`Lower 95%` = Mean
    ,`Upper 80%` = Mean
    ,`Upper 95%` = Mean
    ,Type = "Actual"
  ) 

banana_autoarima_logreturn_plot <- 
  banana_forecast_logreturn %>% 
  bind_rows(banana_autoarima_forecast) %>% 
  bind_rows(
    banana_forecast_logreturn %>% 
      filter(Date == max(Date)) %>% 
      mutate(Type = "Forecast")
  ) %>% 
  mutate(Model = "Based on the Auto-ARIMA Model for Log Returns") %>% 
  ggplot(
    aes(
      x = Date
      ,y = Mean
      ,colour = Type
    )
  ) +
  geom_ribbon(
    aes(ymin = `Lower 95%`, ymax = `Upper 95%`)
    ,fill = "grey60"
    ,alpha = 0.4
  ) +
  geom_line() +
  facet_wrap(~ Model) +
  scale_colour_brewer(
    type = "seq"
    ,palette = "Set2"
  ) +
  theme(axis.title.y = element_blank())

# Convert back into price ----------
banana_forecast_price <- 
  banana_price %>% 
  select(
    Date
    ,Mean = Price
  ) %>% 
  mutate(
    `Lower 80%` = Mean
    ,`Lower 95%` = Mean
    ,`Upper 80%` = Mean
    ,`Upper 95%` = Mean
    ,Type = "Actual"
  )

banana_forecast_price <-
  banana_forecast_price %>% 
  bind_rows(banana_forecast_price) %>% 
  bind_rows(
    banana_forecast_price %>% 
      filter(Date == max(Date)) %>% 
      mutate(Type = "Forecast")
  )


for (t in 1:nrow(banana_autoarima_forecast)){
  banana_forecast_price <- 
    log_ret_to_price(
      historical_data = banana_forecast_price
      ,forecast_data = banana_autoarima_forecast
    )
}

banana_forecast_price

banana_autoarima_price_plot <- 
  banana_forecast_price %>% 
  mutate(Model = "Based on the Auto-ARIMA Model for Log Returns") %>% 
  ggplot(
    aes(
      x = Date
      ,y = Mean
      ,colour = Type
    )
  ) +
  geom_ribbon(
    aes(ymin = `Lower 95%`, ymax = `Upper 95%`)
    ,fill = "grey60"
    ,alpha = 0.4
  ) +
  geom_line() +
  facet_wrap(~ Model) +
  scale_colour_brewer(
    type = "seq"
    ,palette = "Set2"
  ) +
  theme(axis.title.y = element_blank())

# RMSE ----------
banana_log_ret_autoarima_01 %>% 
  accuracy() %>% 
  as_tibble()



# Min AIC ----------
banana_aic_forecast <- 
  banana_aic_arima$`1` %>% 
  forecast(num_periods_ahead) 

banana_aic_forecast <-
  tibble(
    Date = max(banana_price$Date) + months(1:num_periods_ahead)
    ,Mean = as.numeric(banana_aic_forecast$mean)
  ) %>% 
  bind_cols(as_tibble(banana_aic_forecast$lower)) %>% 
  rename(`Lower 80%` = `80%`, `Lower 95%` = `95%`) %>% 
  bind_cols(as_tibble(banana_aic_forecast$upper)) %>% 
  rename(`Upper 80%` = `80%`, `Upper 95%` = `95%`) %>% 
  mutate(Type = "Forecast") %>% 
  bind_rows()
banana_aic_forecast

banana_forecast_logreturn <- 
  banana_price %>% 
  select(
    Date
    ,Mean = `Log Return`
  ) %>% 
  mutate(
    `Lower 80%` = Mean
    ,`Lower 95%` = Mean
    ,`Upper 80%` = Mean
    ,`Upper 95%` = Mean
    ,Type = "Actual"
  ) 

banana_aic_logreturn_plot <- 
  banana_forecast_logreturn %>% 
  bind_rows(banana_aic_forecast) %>% 
  bind_rows(
    banana_forecast_logreturn %>% 
      filter(Date == max(Date)) %>% 
      mutate(Type = "Forecast")
  ) %>% 
  mutate(Model = "Based on the Minimum AIC Model for Log Returns") %>% 
  ggplot(
    aes(
      x = Date
      ,y = Mean
      ,colour = Type
    )
  ) +
  geom_ribbon(
    aes(ymin = `Lower 95%`, ymax = `Upper 95%`)
    ,fill = "grey60"
    ,alpha = 0.4
  ) +
  geom_line() +
  facet_wrap(~ Model) +
  scale_colour_brewer(
    type = "seq"
    ,palette = "Set2"
  ) +
  theme(axis.title.y = element_blank())

# Convert back into price ----------
banana_forecast_price <- 
  banana_price %>% 
  select(
    Date
    ,Mean = Price
  ) %>% 
  mutate(
    `Lower 80%` = Mean
    ,`Lower 95%` = Mean
    ,`Upper 80%` = Mean
    ,`Upper 95%` = Mean
    ,Type = "Actual"
  )

banana_forecast_price <-
  banana_forecast_price %>% 
  bind_rows(banana_forecast_price) %>% 
  bind_rows(
    banana_forecast_price %>% 
      filter(Date == max(Date)) %>% 
      mutate(Type = "Forecast")
  )


for (t in 1:nrow(banana_aic_forecast)){
  banana_forecast_price <- 
    log_ret_to_price(
      historical_data = banana_forecast_price
      ,forecast_data = banana_aic_forecast
    )
}

banana_forecast_price

banana_aic_price_plot <- 
  banana_forecast_price %>% 
  mutate(Model = "Based on the Minimum AIC Model for Log Returns") %>% 
  ggplot(
    aes(
      x = Date
      ,y = Mean
      ,colour = Type
    )
  ) +
  geom_ribbon(
    aes(ymin = `Lower 95%`, ymax = `Upper 95%`)
    ,fill = "grey60"
    ,alpha = 0.4
  ) +
  geom_line() +
  facet_wrap(~ Model) +
  scale_colour_brewer(
    type = "seq"
    ,palette = "Set2"
  ) +
  theme(axis.title.y = element_blank())

# RMSE ----------
banana_aic_arima$`1` %>% 
  accuracy() %>% 
  as_tibble()



# Min BIC ----------
banana_bic_forecast <- 
  banana_bic_arima$`2` %>% 
  forecast(num_periods_ahead) 

banana_bic_forecast <-
  tibble(
    Date = max(banana_price$Date) + months(1:num_periods_ahead)
    ,Mean = as.numeric(banana_bic_forecast$mean)
  ) %>% 
  bind_cols(as_tibble(banana_bic_forecast$lower)) %>% 
  rename(`Lower 80%` = `80%`, `Lower 95%` = `95%`) %>% 
  bind_cols(as_tibble(banana_bic_forecast$upper)) %>% 
  rename(`Upper 80%` = `80%`, `Upper 95%` = `95%`) %>% 
  mutate(Type = "Forecast") %>% 
  bind_rows()
banana_bic_forecast

banana_forecast_logreturn <- 
  banana_price %>% 
  select(
    Date
    ,Mean = `Log Return`
  ) %>% 
  mutate(
    `Lower 80%` = Mean
    ,`Lower 95%` = Mean
    ,`Upper 80%` = Mean
    ,`Upper 95%` = Mean
    ,Type = "Actual"
  ) 


banana_bic_logreturn_plot <-
  banana_forecast_logreturn %>% 
  bind_rows(banana_bic_forecast) %>% 
  bind_rows(
    banana_forecast_logreturn %>% 
      filter(Date == max(Date)) %>% 
      mutate(Type = "Forecast")
  ) %>% 
  mutate(Model = "Based on the Minimum BIC Model for Log Returns") %>% 
  ggplot(
    aes(
      x = Date
      ,y = Mean
      ,colour = Type
    )
  ) +
  geom_ribbon(
    aes(ymin = `Lower 95%`, ymax = `Upper 95%`)
    ,fill = "grey60"
    ,alpha = 0.4
  ) +
  geom_line() +
  facet_wrap(~ Model) +
  scale_colour_brewer(
    type = "seq"
    ,palette = "Set2"
  ) +
  theme(axis.title.y = element_blank())

# Convert back into price ----------
banana_forecast_price <- 
  banana_price %>% 
  select(
    Date
    ,Mean = Price
  ) %>% 
  mutate(
    `Lower 80%` = Mean
    ,`Lower 95%` = Mean
    ,`Upper 80%` = Mean
    ,`Upper 95%` = Mean
    ,Type = "Actual"
  )

banana_forecast_price <-
  banana_forecast_price %>% 
  bind_rows(banana_forecast_price) %>% 
  bind_rows(
    banana_forecast_price %>% 
      filter(Date == max(Date)) %>% 
      mutate(Type = "Forecast")
  )


for (t in 1:nrow(banana_bic_forecast)){
  banana_forecast_price <- 
    log_ret_to_price(
      historical_data = banana_forecast_price
      ,forecast_data = banana_bic_forecast
    )
}

banana_forecast_price

banana_bic_price_plot <-
  banana_forecast_price %>% 
  mutate(Model = "Based on the Minimum BIC Model for Log Returns") %>%  
  ggplot(
    aes(
      x = Date
      ,y = Mean
      ,colour = Type
    )
  ) +
  geom_ribbon(
    aes(ymin = `Lower 95%`, ymax = `Upper 95%`)
    ,fill = "grey60"
    ,alpha = 0.4
  ) +
  geom_line() +
  facet_wrap(~ Model) +
  scale_colour_brewer(
    type = "seq"
    ,palette = "Set2"
  ) +
  theme(axis.title.y = element_blank())

# RMSE ----------
banana_bic_arima$`1` %>% 
  accuracy() %>% 
  as_tibble()



# Combined Plots ----------
grid.arrange(
  banana_autoarima_logreturn_plot
  ,banana_aic_logreturn_plot
  ,banana_bic_logreturn_plot
  ,ncol = 1
  ,top = "Forecasted Banana Log Return"
  ,left = "Log Return"
)
grid.arrange(
  banana_autoarima_price_plot
  ,banana_aic_price_plot
  ,banana_bic_price_plot
  ,ncol = 1
  ,top = "Forecasted Banana Price"
  ,left = "Price (USD/Metric Ton)"
)

