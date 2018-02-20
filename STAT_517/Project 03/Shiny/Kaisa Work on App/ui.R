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
    #Importing File for Evaluation
      fileInput("panssfile","Select the Cleaned PANSS File from your desktop", buttonLabel = "Choose File"),
      #clean_panss <- read_csv(file = "../Shiny/data/Panssdata_Modified.csv"),
  #*Output() function
    #Individual Results are Created
      #Plots
      
      #Texts of PASS/FAIL
        
)

#Server
server <- function(input, output){}



#App
shinyApp(ui = ui, server = server)
