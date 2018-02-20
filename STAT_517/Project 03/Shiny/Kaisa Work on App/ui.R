# Libraries ----------
library(shiny, warn.conflicts = FALSE, quietly = TRUE)
library(shinythemes, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

#Import and Clean Data



#UI
ui <- fluidPage(
  #*Input() functions,
    
    #Numeric Input (Individual id)
      numericInput("IdInput","Enter your Rater ID", 0, min = 0, max = 81, step = 1),
    #From the number entered, I want to read from the csv file the info and create the graphs to determine if they passed or not
      clean_panss <- read_csv(file = "../Shiny/data/Panssdata_Modified.csv"),
      clean_panss
  #*Output() function
    #Individual Results are Created
      #Plots
        
      #Texts of PASS/FAIL
        
)

#Server
server <- function(input, output){}


#App
shinyApp(ui = ui, server = server)
