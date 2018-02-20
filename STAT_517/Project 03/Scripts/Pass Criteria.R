# Libraries ----------
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

# Data Import ----------
panss <- "../Results App/Panssdata_Modified.csv" %>%
  # "../Shiny/data/Panssdata_Modified.csv" %>% 
  read_csv() %>% 
  filter_all(all_vars(!is.na(.))) %>% 
  mutate(
    LANG = if_else(LANG == "E", "English", LANG)
    ,LANG = if_else(LANG == "F", "French", LANG)
    ,LANG = if_else(LANG == "I", "Italian", LANG)
  ) %>% 
  rename(
    G01 = G1
    ,G02 = G2
    ,G03 = G3
    ,G04 = G4
    ,G05 = G5
    ,G06 = G6
    ,G07 = G7
    ,G08 = G8
    ,G09 = G9
  )

# Data Cleaning ----------
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
      ,G = G01 + G02 + G03 + G04 + G05 + G06 + G07 + G08 + G09 + G10 + G11 + G12 + G13 + G14 + G15 + G16
    ) %>% 
  mutate(
    `P Pass` = if_else(P >= 5, TRUE, FALSE)
    ,`N Pass` = if_else(N >= 5, TRUE, FALSE)
    ,`G Pass` = if_else(G >= 10, TRUE, FALSE)
    ,Passes = if_else(`P Pass` & `N Pass` & `G Pass`, TRUE, FALSE)
  )

# Data Visualization ----------
panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("P")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(aes(x = Rating)) +
  geom_bar() +
  facet_grid(
    LANG ~ Question
    ,scales = "free_y"
  ) +
  scale_x_discrete(limit = 1:7) +
  labs(title = "Histogram of Positive Ratings by Language")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("N")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(aes(x = Rating)) +
  geom_bar() +
  facet_grid(
    LANG ~ Question
    ,scales = "free_y"
  ) +
  scale_x_discrete(limit = 1:7) +
  labs(title = "Histogram of Negative Ratings by Language")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("G")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(aes(x = Rating)) +
  geom_bar() +
  facet_grid(
    LANG ~ Question
    ,scales = "free_y"
  ) +
  scale_x_discrete(limit = 1:7) +
  labs(title = "Histogram of Generic Ratings by Language")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("P")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = Question
      ,y = Rating
    )
  ) +
  geom_violin() +
  geom_point(
    data = 
      panss_rater %>% 
      select(
        RATER
        ,LANG
        ,starts_with("P")
      ) %>% 
      gather(
        key = "Question"
        ,value = "Rating"
        ,-RATER
        ,-LANG
      )
  ) +
  labs(title = "Violin Plot of Positive Ratings")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("P")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = LANG
      ,y = Rating
    )
  ) +
  geom_violin() +
  # geom_line(
  #   data = 
  #     rbind(
  #       panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("P")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         )
  #       ,panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("P")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         ) %>% 
  #         mutate(LANG = "French")
  #       ,panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("P")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         ) %>% mutate(LANG = "Italian")
  #     )
  #   ,aes(group = Question)
  #   ,linetype = "dashed"
  # ) +
  # facet_wrap(~ Question) +
  facet_grid(
    LANG ~ Question
    ,scales = "free_x"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  ) +
  labs(title = "Violin Plot of Positive Ratings by Language")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("N")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = Question
      ,y = Rating
    )
  ) +
  geom_violin() +
  geom_point(
    data = 
      panss_rater %>% 
      select(
        RATER
        ,LANG
        ,starts_with("N")
      ) %>% 
      gather(
        key = "Question"
        ,value = "Rating"
        ,-RATER
        ,-LANG
      )
  ) +
  labs(title = "Violin Plot of Negative Ratings")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("N")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = LANG
      ,y = Rating
    )
  ) +
  geom_violin() +
  # geom_line(
  #   data = 
  #     rbind(
  #       panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("N")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         )
  #       ,panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("N")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         ) %>% 
  #         mutate(LANG = "French")
  #       ,panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("N")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         ) %>% mutate(LANG = "Italian")
  #     )
  #   ,aes(group = Question)
  #   ,linetype = "dashed"
  # ) +
  # facet_wrap(~ Question) +
  facet_grid(
    LANG ~ Question
    ,scales = "free_x"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  ) +
  labs(title = "Violin Plot of Negative Ratings by Language")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("G")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = Question
      ,y = Rating
    )
  ) +
  geom_violin() +
  geom_point(
    data = 
      panss_rater %>% 
      select(
        RATER
        ,LANG
        ,starts_with("G")
      ) %>% 
      gather(
        key = "Question"
        ,value = "Rating"
        ,-RATER
        ,-LANG
      )
  ) +
  labs(title = "Violin Plot of Generic Ratings")

panss_tests %>% 
  select(
    RATER
    ,LANG
    ,starts_with("G")
  ) %>% 
  gather(
    key = "Question"
    ,value = "Rating"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = LANG
      ,y = Rating
    )
  ) +
  geom_violin() +
  # geom_line(
  #   data = 
  #     rbind(
  #       panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("G")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         )
  #       ,panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("G")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         ) %>% 
  #         mutate(LANG = "French")
  #       ,panss_rater %>% 
  #         select(
  #           RATER
  #           ,LANG
  #           ,starts_with("G")
  #         ) %>% 
  #         gather(
  #           key = "Question"
  #           ,value = "Rating"
  #           ,-RATER
  #           ,-LANG
  #         ) %>% mutate(LANG = "Italian")
  #     )
  #   ,aes(group = Question)
  #   ,linetype = "dashed"
  # ) +
  # facet_wrap(~ Question) +
  facet_grid(
    LANG ~ Question
    ,scales = "free_x"
  ) +
  theme(
    axis.text.x = element_blank()
    ,axis.title.x = element_blank()
  ) +
  labs(title = "Violin Plot of Generic Ratings by Language")

panss_results %>% 
  select(
    -c(
      P
      ,N
      ,G
    )
  ) %>% 
  gather(
    key = "Test"
    ,value = "Result"
    ,-RATER
    ,-LANG
  ) %>% 
  ggplot(
    aes(
      x = LANG
      ,colour = Result
      ,fill = Result
    )
  ) +
  geom_bar(position = "fill") +
  facet_wrap(
    ~ Test
    ,scales = "fixed"
  ) +
  scale_fill_brewer(
    type = "qual"
    ,palette = "Set2"
  ) +
  scale_colour_brewer(
    type = "qual"
    ,palette = "Set2"
  ) +
  labs(title = "Proportion of Raters who Passed by Language")

# Regression Models ----------
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

