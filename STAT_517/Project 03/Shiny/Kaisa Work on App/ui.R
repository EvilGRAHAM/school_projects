# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

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


#Define UI for app
ui <- fluidPage(
      
  #App Title
  titlePanel("Results of PANSS Testing"),
  
    #Inputs
      selectInput("hist","Results-Histograms", choices = c("Histogram of Positive Ratings by Language","Histogram of Negative Ratings by Language","Histogram of Generic Ratings by Language")),
      selectInput("violin","Results-Violin Plots",choices = c("Violin Plots of Positive Ratings","Violin Plot of Positive Ratings by Language","Violin Plot of Negative Ratings","Violin Plot of Negative Ratings by Language","Violin Plot of Generic Ratings","Violin Plot of Generic Ratings by Language")),
      selectInput("results","Results-Proportions Passed", choices = c("Proportions passed by Languages")),
      numericInput("inputid","Enter your Rater ID", 0, min = 0, max = 81, step = 1),
   
     #Outputs
      plotOutput("hist"),
      plotOutput("violin"),
      plotOutput("prop")
      #textOutput()
  

)

#Server
server <- function(input, output){
  output$hist <- renderPlot({ 
    if(input$hist == "Histogram of Positive Ratings by Language")
    {output$hist_PRL <- renderPlot({panss_tests %>% 
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
        labs(title = "Histogram of Positive Ratings by Language")})
    }
    
    else if(input$hist == "Histogram of Negative Ratings by Language")
    {panss_tests %>% 
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
        labs(title = "Histogram of Negative Ratings by Language")}
    
    else if(input$hist == "Histogram of Generic Rating by Language")
    {panss_tests %>% 
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
        labs(title = "Histogram of Generic Ratings by Language")}
  })
  
  output$violin <- renderPlot({ 
    if(input$violin == "Violin of Positive Ratings by Language")
    {output$hist_PRL <- renderPlot({
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
    })}
    
    else if(input$violin == "Violin of Positive Ratings by Language")
    {output$violin <-renderPlot({
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
      facet_grid(
        LANG ~ Question
        ,scales = "free_x"
      ) +
        theme(
          axis.text.x = element_blank()
          ,axis.title.x = element_blank()
        ) +
        labs(title = "Violin Plot of Positive Ratings by Language")
    })}
  
    else if(input$violin == "Violin of Negative Ratings")
    {output$violin <-renderPlot({
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
    })}
    
    else if(input$violin == "Violin of Negative Ratings by Language")
    {output$violin <-renderPlot({
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
      facet_grid(
        LANG ~ Question
        ,scales = "free_x"
      ) +
        theme(
          axis.text.x = element_blank()
          ,axis.title.x = element_blank()
        ) +
        labs(title = "Violin Plot of Negative Ratings by Language")
      
    })}
    
    else if(input$violin == "Violin of Generic Ratings")
    {output$violin <-renderPlot({
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
    })}
    
    else if(input$violin == "Violin of Generic Ratings by Language")
    {output$violin <-renderPlot({
      
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
      facet_grid(
        LANG ~ Question
        ,scales = "free_x"
      ) +
        theme(
          axis.text.x = element_blank()
          ,axis.title.x = element_blank()
        ) +
        labs(title = "Violin Plot of Generic Ratings by Language")
    })}
    
    })

  output$prop <- renderPlot({ 
    if(input$results == "Proportions Passed by Language ")
    {output$prop <- renderPlot({})}})
  
  }

#App
shinyApp(ui = ui, server = server)
