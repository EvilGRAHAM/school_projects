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