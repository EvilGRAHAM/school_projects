# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

# UI ----------
fluidPage(
  
  # Application title ----------
  theme = shinytheme("lumen")
  ,titlePanel(title = "PANSS Instrument")
  
  ,fluidRow(
    
    # Inputs ----------
    column(
      3
      ,offset = 0
      ,wellPanel(
        selectizeInput(
          inputId = "language"
          ,label = "Language"
          ,choices = c(
            "English" = "E"
            ,"French" = "F"
            ,"Italian" = "I"
          )
        )
        ,actionButton(
          inputId = "submit"
          ,label = "Submit Response"
        )
      )
    )
    
    # P ----------
    ,column(
      3
      ,offset = 0
      ,sliderInput(
        inputId = "P1"
        ,label = "P1: Delusions"
        ,min = 1
        ,max = 7
        ,value = 4
      )
    )
    
    # N ----------
    ,column(
      3
      ,offset = 0
      ,sliderInput(
        inputId = "N1"
        ,label = "N1: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N2"
        ,label = "N2: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N3"
        ,label = "N3: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N4"
        ,label = "N4: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N5"
        ,label = "N5: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N6"
        ,label = "N6: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
      ,sliderInput(
        inputId = "N7"
        ,label = "N7: Blunted Affect"
        ,min = 1
        ,max = 7
        ,value = 4
      )
    )
    
    # G ----------
    ,column(
      3
      ,offset = 0
      ,sliderInput(
        inputId = "G1"
        ,label = "G1: Somatic Concern"
        ,min = 1
        ,max = 7
        ,value = 4
      )
    )
  )
  
)

