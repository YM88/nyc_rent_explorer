
# Authors: Yashar Mansouri, Joshua O'Steen, Christopher Hoffman

library(shiny)
library(ggplot2)
library(tidyverse)
library(broom)

df <- read_rds("./data/all_data.rds")


ui <- fluidPage(
  # App title
  titlePanel("Financial Housing Data of New York City"),
  tabsetPanel(
    
    # tab 1 - map and time series # three inputs: price of bedroom type, neighborhood, area
    # perhaps we can structure this section so that there are three 
    # blocks, upper left, upper right and bottom which hold user input
    # map and time series respectively
    tabPanel("Map and Time Series Analysis",
             sidebarLayout(
               sidebarPanel(
                 # user input
               ),
               mainPanel(
                 # map
               )
             )),
    
    # tab 2 - various plots of data # histogram similar to this: http://bradleyboehmke.github.io/tutorials/histograms
            # - time series comparison for the neighborhood for different type of rooms
    tabPanel("Data Distributions and Plots",
             sidebarLayout(
               sidebarPanel(
                 # user input
               ),
               mainPanel(
                 # chart output
               )
             )),
    
    # tab 3 - Regression and ANOVA
    tabPanel("Statistical Analysis",
             sidebarLayout(
               sidebarPanel(
                 # user input
               ),
               mainPanel(
                 # chart output
               )
             )),
    
    # tab 4 - data spreadsheet
    tabPanel("Data Spreadsheet",
             mainPanel(dataTableOutput("df")))
  )
)

server <- function(input, output, session) {
  
  
  ############################################ Spreadsheet
  
  output$df <- renderDataTable({df})
  
  
}

shinyApp(ui, server)
