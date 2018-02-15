# Libraries ----------
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# Data Import ----------
panss <- read_csv(file = "../Shiny/data/Panssdata_Modified.csv")

panss_rater <- 
  panss %>% 
  filter(RATER == 0)

panss_tests <- 
  panss %>% 
  filter(RATER != 0)

panss_tests %>% 
  select(-LANG) %>% 
  apply(
    2
    ,sum
  )

panss_diff <-
  as.tibble(
    abs(panss_tests[, -2] - panss_rater[rep(x = 1, times = as.numeric(count(panss_tests))), -2])
  ) %>% 
  mutate_all(as.double) %>% 
  mutate_at(
    .vars = vars(-matches("RATER"))
    ,.funs = ~ ifelse(. == 1, . - 1, .)
  ) %>%
  mutate_at(
    .vars = vars(-matches("RATER"))
    ,.funs = ~ ifelse(. != 0, 0, 1)
  ) %>%
  left_join(
    panss_tests %>% 
      select(RATER, LANG)
    ,by = "RATER"
  )

panss_results <-
  panss_diff %>% 
    transmute(
      RATER = RATER
      ,LANG = LANG
      ,P = P1 + P2 + P3 + P4 + P5 + P6 + P7
      ,N = N1 + N2 + N3 + N4 + N5 + N6 + N7
      ,G = G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16
    ) %>% 
  mutate(
    `P Pass` = if_else(P >= 5, TRUE, FALSE)
    ,`N Pass` = if_else(N >= 5, TRUE, FALSE)
    ,`G Pass` = if_else(G >= 10, TRUE, FALSE)
    ,Passes = if_else(`P Pass` & `N Pass` & `G Pass`, TRUE, FALSE)
  )

panss_results %>% 
  select(
    -c(
      P
      ,N
      ,G
    )
  ) %>% 
  filter_if()
  gather(
    key = "Test"
    ,value = "Result"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = LANG
      # ,y = Result
      ,colour = Result
      ,fill = Result
    )
  ) +
  geom_bar(position = "fill") +
  facet_wrap(
    ~ Test
    ,scales = "fixed"
  )

panss_passes_logit <- 
  panss_results %>% 
  glm(
    Passes ~ LANG
    ,data = .
    ,family = binomial
  )
summary(panss_passes_logit)

panss_P_logit <- 
  panss_results %>% 
  glm(
    `P Pass` ~ LANG
    ,data = .
    ,family = binomial
  )
summary(panss_P_logit)

panss_N_logit <- 
  panss_results %>% 
  glm(
    `N Pass` ~ LANG
    ,data = .
    ,family = binomial
  )
summary(panss_N_logit)

panss_G_logit <- 
  panss_results %>% 
  glm(
    `G Pass` ~ LANG
    ,data = .
    ,family = binomial
  )
summary(panss_G_logit)
