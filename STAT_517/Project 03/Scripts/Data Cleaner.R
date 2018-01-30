# Libraries ----------
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# Data Import ----------
panss <- read_delim(file = "../Shiny/data/Panssdata.txt", delim = " ")
# panss <- read_csv(file = "../Shiny/data/Panssdata_Modified.csv")


# Data Cleaning ----------
panss %>% 
  select(
    -c(
      RATER
      ,LANG
    )
  ) %>% 
  select_if(is.character) %>% 
  arrange(N7)

panss %>% 
  select(
    -c(
      RATER
      ,LANG
    )
  ) %>% 
  select_if(is.character) %>% 
  arrange(desc(G15))


panss <- 
  panss %>% 
  mutate(
    N7 = if_else(N7 == ".", as.character(NA), N7)
    ,N7 = as.integer(N7)
    ,G15 = if_else(G15 == "l", as.character(1), G15)
    ,G15 = as.integer(G15)
  )


panss %>% 
  select(
    -c(
      RATER
      ,LANG
    )
  ) %>%
  summarize_all(.funs = funs(min, max), na.rm = TRUE) %>% 
  gather(
    key = Question
    ,value = Response
  ) %>% 
  filter(!(Response %in% 1:7))

panss %>% 
  select(
    -c(
      RATER
      ,LANG
    )
  ) %>%
  filter_all(any_vars(!(. %in% 1:7)))

panss <- 
  panss %>% 
  mutate(
    N2 = if_else(N2 == 0, as.integer(1), N2)
    ,P3 = if_else(P3 == 8, as.integer(7), P3)
    ,P7 = if_else(P7 == 9, as.integer(7), P7)
  )


# Data Export ----------
write_csv(x = panss, path = "../Shiny/data/Panssdata_Modified.csv")
