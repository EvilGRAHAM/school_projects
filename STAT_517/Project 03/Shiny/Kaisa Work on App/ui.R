# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)


# UI ----------
fluidPage(
  
  # App Title ----------
  titlePanel("Results of PANSS Testing")
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      width = 3
      ,offset = 0
      ,wellPanel(
        selectizeInput(
          inputId = "question_set"
          ,label = "Questions Displayed"
          ,choices = 
            c(
              "Positive" = "P"
              ,"Negative" = "N"
              ,"Generic" = "G"
            )
        )
        ,checkboxInput(
          inputId = "by_lang"
          ,"Facet by Language"
          ,value = FALSE
        )
        ,selectizeInput(
          inputId = "results"
          ,label = "Results-Proportions Passed"
          ,choices = 
            c(
              "Proportions passed by Languages"
            )
        )
        ,numericInput(
          inputId = "rater_num"
          ,label = "Enter your Rater ID"
          ,value = 1
          ,min = 1
          # ,max = 81
          ,step = 1
        )
      )
    )
    
    # Plots ---------
    ,column(
      width = 9
      ,offset = 0
      ,plotOutput("hist")
      ,plotOutput("violin")
      ,plotOutput("prop")
      #textOutput()
    )
  )
)
