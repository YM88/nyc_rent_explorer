library(shiny)
library(tidyverse)

ui <- fluidPage(
    # App title
    titlePanel("Housing Data of New York City"),
    tabsetPanel(
        
        # tab 1 - map and time series
        tabPanel("Map and Time Series Analysis",
                 sidebarLayout(
                     sidebarPanel(
                         # user input
                     ),
                     mainPanel(
                         # map
                     )
                 )),
        
        # tab 2 - various plots of data
        tabPanel("Data Distributions and Plots",
                 sidebarLayout(
                     sidebarPanel(
                         # user input
                     ),
                     mainPanel(
                         # chart output
                     )
                 )),
        
        # tab 3 - data spreadsheet
        tabPanel("Data Spreadsheet",
                 mainPanel(dataTableOutput("spreadsheet")))
    )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
