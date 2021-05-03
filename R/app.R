
# Authors: Yashar Mansouri, Joshua O'Steen, Christopher Hoffman

library(shiny)
library(tidyverse)
library(rgdal)
library(leaflet)
library(plotly)
library(shinythemes)
library(prophet)
library(dygraphs)
library(shinycssloaders)

# getting data
df <- read_rds("../data/all_data.rds")
df_longer <- read_rds("../data/all_data_longer.rds")
nyc_neighborhoods <- readOGR("../data/ny.geojson", layer = "ny")
#reused values
apartment_types <- c("Studio", 
                     "One Bedroom" = "One_Bedroom",
                     "Two Bedrooms" = "Two_Bedrooms",
                     "Three+ Bedrooms" = "Three_Bedrooms")

boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx")



ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(
                  tags$style(HTML("
                  @import url('https://fonts.googleapis.com/css2?family=Bitter:ital,wght@1,500&family=Nunito&display=swap');
                  * {
                      font-family: 'Nunito', sans-serif;
                    }
                    .shiny-output-error-black {
                      color: black;
                      font-size: 200%;
                      display: flex;
                      align-items: center;
                    }
                    h1 {
                      font-family: 'Bitter', serif;
                    }
                    .modebar-container {
                      display: none !important;
                    }
                  "), HTML(
                    "#sidebar {
                      background-color: #ffffff;}"
                  ))
                ),
                # App title
                navbarPage("NEW YORK CITY RENT EXPLORER", collapsible = TRUE,
                           
                           ###TAB 1#############################################
                           tabPanel("Map and Time Series Analysis",
                           ###ROW 1#############################################
                           sidebarLayout(
                                sidebarPanel(#3,
                                  id="sidebar",
                                  selectInput("boro", "Select a borough:", 
                                              choices = boroughs, 
                                              selected = "Manhattan"),
                                  
                                  uiOutput("areaSelect"), 
                                  
                                  selectInput("type", "Select an apartment type:", 
                                              choices = apartment_types, 
                                              selected = "Studio"),
                                  
                                  h1("Welcome to the NYC Rent Data Explorer!"), 
                                  p("This web application is designed for the benefit of renters, landlords, and multifamily market researchers in New York City with up to date and useful analysis of rental market data and trends."),
                                  p("The rent data comes from", a(href = "https://streeteasy.com/blog/download-data/", "StreetEasy.com,"), "an online real estate and apartment marketplace compnay exclusively for New York City homes and apartments."), 
                                  p("This web application is proudly made by", a(href = "https://www.linkedin.com/in/jvosteen/", "Josh Vera O'Steen", .noWS=c("after")),",", a(href = "https://www.linkedin.com/in/yasharmansouri/", "Yashar Mansouri", .noWS=c("after")), ", and", a(href = "https://www.linkedin.com/in/christopher-hoffmann/", "Christopher Hoffman", .noWS=c("after")),". For contact information and further details on this project, please refer to the README and Vignette files found in our",
                                    a(href = "https://github.com/STAT-413-613-21S/fp_final-project-cjy", "GitHub repository", .noWS=c("after")), "."),
                                  p("To start using this application, select the tab of interest and the inputs. Enjoy exploring!"),
                                  align = "center"
                                  ),
                                mainPanel(#9,
                                  # MAP OUTPUT
                                  leafletOutput("mapPlot"),
                                  plotlyOutput("ts_single")
                                  ),
                                  
                                ),
                              ),
                           
                           ###TAB 2#############################################
                           tabPanel("Compare Neighborhoods & Types",
                                    ###ROW 1####################################
                           fluidRow(
                                column(3,
                                       # user input
                                       selectInput("boro_compare", 
                                                   "Select a borough:", 
                                                   choices = boroughs, 
                                                   selected = "Manhattan"),
                                       
                                       uiOutput("areaSelect_compare"), 
                                       
                                       selectInput("type_compare", 
                                                   "Select an apartment type:", 
                                                   choices = apartment_types, 
                                                   selected = "Studio")
                                       ),
                                column(9,
                                       # chart output
                                       plotlyOutput("ts_multiple")
                                      )
                                ),
                                    ###ROW 2####################################
                           fluidRow(
                                column(3,
                                       selectInput("boro_compare2",
                                                   "Select another borough to compare:", 
                                                   choices = boroughs, 
                                                   selected = "Manhattan"),
                                             
                                       uiOutput("areaSelect_compare2"), 
                                       
                                       selectInput("type_compare2", 
                                                   "Select another apartment type to compare:", 
                                                   choices = apartment_types, 
                                                   selected = "Studio"),
                                       ),
                                column(9,
                                       plotlyOutput("histogram"))
                                    )
                           ),
                           
                           
                           ###TAB 3#############################################
                           tabPanel("Time Series Forecast",
                                    ###ROW 1####################################
                              fluidRow(
                                column(3,
                                       selectInput("boro_forecast", 
                                                   "Select a borough:", 
                                                   choices = boroughs, 
                                                   selected = "Manhattan"),
                                       
                                       uiOutput("areaSelect_forecast"), 
                                       
                                       selectInput("type_forecast", 
                                                   "Select an apartment type:", 
                                                   choices = apartment_types, 
                                                   selected = "Studio")
                                ),
                                column(9,
                                       plotOutput("plot_decompose")
                                       )
                              ),
                              
                              fluidRow(
                                column(3,
                                       hr(),
                                       
                                       sliderInput(
                                         "years_to_forecast",
                                          label = "Years to forecast",
                                          min = 1, 
                                          max = 10, 
                                          value = 5),
                                       
                                       actionButton("run_forecast", 
                                                    label = "Forecast")
                                       ),
                                column(9,
                                       withSpinner(dygraphOutput("plot_forecast"))
                                       )
                              )
                           ),
                           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                            tags$div("Loading...")),
                           
                           ###TAB 4#############################################
                           tabPanel("Data Spreadsheet",
                                    dataTableOutput("df")
                           )
                )
                
)
################################################################################
#################################SERVER#########################################
################################################################################

server <- function(input, output, session) {
  
  #--- HELPER FUNCTIONS ---#
  
  # grabs coordinates for map
  getCoordinates <- function(area) {
    areaList() %>% 
      filter(Neighborhood == area) %>% 
      select(Longitude, Latitude)
  }
  
  #--- RENDER UI ---#
  
  # render the neighborhood selector input
  output$areaSelect = renderUI({
    selectInput("neighborhood", "Select a neighborhood:",
                choices = c("None", areaList() %>% filter(Borough == input$boro) %>% select(Neighborhood) %>% pull()),
                selected = "None")
  })
  
  output$areaSelect_compare = renderUI({
    selectInput("neighborhood_compare", "Select a neighborhood:",
                choices = c("None", areaList() %>% filter(Borough == input$boro_compare) %>% select(Neighborhood) %>% pull()), selected = "None")
  })
  
  output$areaSelect_compare2 = renderUI({
    selectInput("neighborhood_compare2", "Select another neighborhood to compare:",
                choices = c("None", areaList() %>% filter(Borough == input$boro_compare2) %>% select(Neighborhood) %>% pull()), selected = "None")
  })
  
  ###TAB1#######################################################################
  #--- REACTIVE FUNCTIONS ---#
  
  # locations available by Borough, Neighborhood, longitude and latitude
  areaList = reactive({
    df %>%
      select(Borough, Neighborhood, Longitude, Latitude) %>%
      distinct() %>%
      inner_join(nyc_neighborhoods@data$neighborhood %>% 
                   as.data.frame() %>% 
                   distinct() %>% 
                   rename("Neighborhood" = "."), by = "Neighborhood") %>% 
      filter(!is.na(Latitude), !is.na(Longitude))
  })
  
  # filters data frame for time series plot with single apt type (ts_single)
  ts_single <- reactive({
    df_longer %>% 
      filter(Borough == input$boro, Neighborhood == input$neighborhood)
  })
  
  ts_singleBoro <- reactive({
    df_longer %>% 
      filter(Borough == input$boro) %>% 
      group_by(Date) %>% 
      summarize(Med_RentBoro = mean(Median_Rent))
  })
  
  #--- PLOTS ---#
  
  # render the leaflet plot for showing the location of the selected neighborhood
  output$mapPlot = renderLeaflet({
    if (input$neighborhood == "None") {
      leaflet(height = "380px") %>%
        addTiles() %>%
        setView(-73.87, 40.73, zoom = 10)
    } else {
      nyc_neighborhoods[nyc_neighborhoods@data$neighborhood == input$neighborhood,] %>%
        leaflet(height = "380px") %>%
        addTiles() %>%
        setView(getCoordinates(input$neighborhood)$Longitude, 
                getCoordinates(input$neighborhood)$Latitude, 
                zoom = 13) %>%
        addPolygons(popup = ~neighborhood,
                    weight = 1,
                    fillColor = "Blue", fillOpacity = 0.35)
    }
  })
  
  output$ts_single <- renderPlotly({
    if(input$neighborhood == "None"){
      ts_singleBoro() %>% 
        plot_ly(., 
                x = ~Date, 
                y = ~Med_RentBoro, 
                type = "scatter", 
                mode = "lines+markers") %>% 
        layout(yaxis = list(title = paste("Median Rent for", input$boro)))
    } else if(ts_single() %>% filter(Type == input$type) %>% nrow() < 10){
      validate("We apologize, but there is not enough data to produce an informative plot. Please select a different combination of inputs.", 
               errorClass = "black")
    }
    else{
      ts_single() %>% 
        filter(Type == input$type) %>% 
        plot_ly(., 
                x = ~Date, 
                y = ~Median_Rent, 
                type = "scatter", 
                mode = "lines+markers")
    }
  })
  
  ###TAB2#######################################################################
  # filters data frame for time series plot with multiple apt types (ts_multiple)
  ts_multiple <- reactive({
    df %>% 
      filter(Borough == input$boro_compare, 
             Neighborhood == input$neighborhood_compare,
             !is.na(Studio),
             !is.na(One_Bedroom),
             !is.na(Two_Bedrooms),
             !is.na(Three_Bedrooms))
  })
  
  ts_multipleBoro <- reactive({
    df %>% 
      filter(Borough == input$boro_compare, 
             !is.na(Studio),
             !is.na(One_Bedroom),
             !is.na(Two_Bedrooms),
             !is.na(Three_Bedrooms)) %>% 
      group_by(Date) %>% 
      summarize(Med_Studio = mean(Studio),
                Med_One = mean(One_Bedroom),
                Med_Two = mean(Two_Bedrooms),
                Med_Three = mean(Three_Bedrooms))
  })
  
  # filters data frame for the histogram plot 
  # to compare across neighborhoods and types
  
  hist_filter <- reactive({
    df %>% 
      filter(Borough %in% c(input$boro_compare,
                            input$boro_compare2),
             Neighborhood %in% c(input$neighborhood_compare,
                                 input$neighborhood_compare2))
  })
  
  hist_filterBoro <- reactive({
    df %>% 
      filter(Borough %in% c(input$boro_compare, 
                            input$boro_compare2)) %>% 
      group_by(Borough, Date) %>% 
      summarize(Studio = mean(Studio, na.rm = T),
                One_Bedroom = mean(One_Bedroom, na.rm = T),
                Two_Bedrooms = mean(Two_Bedrooms, na.rm = T),
                Three_Bedrooms = mean(Three_Bedrooms, na.rm = T))
  })
  
  #--- PLOTS ---#
  output$ts_multiple <- renderPlotly({
    if(input$neighborhood_compare == "None"){
      ts_multipleBoro() %>% 
        plot_ly(., x = ~Date,
                y = ~Med_Studio,
                type = "scatter",
                mode = "lines",
                name = "Studio") %>% 
        add_trace(., x = ~Date,
                  y = ~Med_One,
                  type = "scatter",
                  mode = "lines",
                  name = "One Bedroom") %>% 
        add_trace(., x = ~Date,
                  y = ~Med_Two,
                  type = "scatter",
                  mode = "lines",
                  name = "Two Bedrooms") %>% 
        add_trace(., x = ~Date,
                  y = ~Med_Three,
                  type = "scatter",
                  mode = "lines",
                  name = "Three+ Bedrooms") %>% 
        layout(title = paste("Median Rent in", 
                             input$boro_compare, 
                             "Across Apartment Types"), 
               hovermode = "x", 
               yaxis = list(title="($)"))
    } else if(ts_multiple() %>% nrow() < 10){
      validate("We apologize, but there is not enough data to produce an informative plot. Please select a different combination of inputs.", errorClass = "black")
    }
    else{
      ts_multiple() %>% 
        plot_ly(., 
                x = ~Date,
                y = ~Studio,
                type = "scatter",
                mode = "lines+markers",
                name = "Studio") %>% 
        add_trace(.,
                  x = ~Date,
                  y = ~One_Bedroom,
                  type = "scatter",
                  mode = "lines+markers",
                  name = "One Bedroom") %>% 
        add_trace(.,
                  x = ~Date,
                  y = ~Two_Bedrooms,
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Two Bedrooms") %>% 
        add_trace(.,
                  x = ~Date,
                  y = ~Three_Bedrooms,
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Three+ Bedrooms") %>% 
        layout(title = paste("Median Rent in", 
                             input$neighborhood_compare, 
                             "Across Apartment Types"), 
               hovermode = "x", 
               yaxis = list(title="($)"))
    }
  })
  
  # Here is the histogram plot. 
  # It allows for user to compare two boroughs, 
  # compare a borough with a neighborhood, compare two neighborhoods. 
  # It also prevents output where there are less than ten data points for either histogram. 
  # Any ideas on how to shorten this code are welcome.
  
  output$histogram <- renderPlotly({
    if(input$neighborhood_compare == "None" & input$neighborhood_compare2 == "None"){
      plot_ly(alpha = 0.6) %>% 
        add_histogram(x = ~(hist_filterBoro() %>%
                              filter(Borough == input$boro_compare) %>%
                              pull(input$type_compare) %>%
                              na.omit()), name = paste(input$type_compare, 
                                                       "in",
                                                       input$boro_compare)) %>% 
        add_histogram(x = ~(hist_filterBoro() %>%
                              filter(Borough == input$boro_compare2) %>%
                              pull(input$type_compare2) %>%
                              na.omit()), name = paste(input$type_compare2,
                                                       "in", 
                                                       input$boro_compare2)) %>% 
        layout(title = "Distribution of Rent for Selected Apartment Types in Selected Boroughs",
               xaxis = list(title="($)"),
               yaxis = list(title="Count"),
               barmode = "overlay")
    } else if((input$neighborhood_compare != "None" & 
               hist_filter() %>% filter(Neighborhood == input$neighborhood_compare) %>% 
               select(input$type_compare) %>% 
               na.omit() %>% 
               nrow() < 10) | (input$neighborhood_compare2 != "None" &
                               hist_filter() %>% 
                               filter(Neighborhood == input$neighborhood_compare2) %>%
                               select(input$type_compare2) %>%
                               na.omit() %>% 
                               nrow() < 10)){
      validate("We apologize, but there is not enough data to produce an informative plot. Please select a different combination of inputs.", 
               errorClass = "black")
    } else if(input$neighborhood_compare != "None" & 
              input$neighborhood_compare2 == "None"){
      plot_ly(alpha = 0.6) %>% 
        add_histogram(x = ~(hist_filter() %>%
                              filter(Neighborhood == input$neighborhood_compare) %>%
                              pull(input$type_compare) %>%
                              na.omit()), 
                      name = paste(input$type_compare, "in", 
                                   input$neighborhood_compare)) %>% 
        add_histogram(x = ~(hist_filterBoro() %>%
                              filter(Borough == input$boro_compare2) %>%
                              pull(input$type_compare2) %>%
                              na.omit()), 
                      name = paste(input$type_compare2, "in", 
                                   input$boro_compare2)) %>% 
        layout(title = "Distribution of Rent for Selected Apartment Types in Selected Areas", 
               xaxis = list(title="($)"), 
               yaxis = list(title="Count"), 
               barmode = "overlay")
    } else if(input$neighborhood_compare == "None" & 
              input$neighborhood_compare2 != "None"){
      plot_ly(alpha = 0.6) %>% 
        add_histogram(x = ~(hist_filterBoro() %>%
                              filter(Borough == input$boro_compare) %>%
                              pull(input$type_compare)%>% na.omit()), 
                      name = paste(input$type_compare, "in", 
                                   input$boro_compare)) %>% 
        add_histogram(x = ~(hist_filter() %>%
                              filter(Neighborhood == input$neighborhood_compare2) %>%
                              pull(input$type_compare2) %>% na.omit()), 
                      name = paste(input$type_compare2, "in", 
                                   input$neighborhood_compare2)) %>% 
        layout(title = "Distribution of Rent for Selected Apartment Types in Selected Areas", 
               xaxis = list(title="($)"), 
               yaxis = list(title="Count"), 
               barmode = "overlay")
    } else{
      plot_ly(data = ., alpha = 0.6) %>%
        add_histogram(x = ~(hist_filter() %>%
                              filter(Neighborhood == input$neighborhood_compare) %>%
                              pull(input$type_compare) %>%
                              na.omit()), 
                      name = paste(input$type_compare, "in",
                                   input$neighborhood_compare)) %>% 
        add_histogram(x = ~(hist_filter() %>%
                              filter(Neighborhood == input$neighborhood_compare2) %>%
                              pull(input$type_compare2) %>%
                              na.omit()),
                      name = paste(input$type_compare2, "in",
                                   input$neighborhood_compare2)) %>% 
        layout(title = "Distribution of Rent for Selected Apartment Types in Selected Neighborhoods", 
               xaxis = list(title="($)"), 
               yaxis = list(title="Count"), 
               barmode = "overlay")
    }
  })
  ###TAB3#######################################################################
  #--- TIME SERIES FORECAST ---#
  # Choosing variables per user input
  output$areaSelect_forecast <- renderUI({
    selectInput("neighborhood_forecast", "Select a neighborhood:",
                choices = c(
                  df %>%
                    filter(Borough == input$boro_forecast) %>%
                    select(Neighborhood) %>% 
                    distinct() %>% 
                    pull()))
  })
  
  # preparing prophet ready time series dataframe
  df_ts <- reactive({
    req(input$boro_forecast,
        input$neighborhood_forecast,
        input$type_forecast
    )
    
    df %>%
      filter(Borough == input$boro_forecast &
             Neighborhood == input$neighborhood_forecast) %>%  
      select(ds='Date', y=input$type_forecast)
    
  })
  
  output$plot_decompose <- renderPlot({
    validate(
      need(!any(is.na(df_ts()[,2])), 
           paste('Selected options has missing values and time series decomposition is not possible.',
                 '',
                 "Please choose a different selection if you'd like to see the time series decomposition.",
                 '',
                 'Forecasting can still run but may lead to less accurate results.', 
                 sep = '\n'))
      )
    
      plot(decompose(ts(df_ts()[,2], frequency = 12)))
  })
  
  
  # running prophet after action button
  m <- eventReactive(input$run_forecast, {
    req(df_ts())
    prophet(df_ts(),
            changepoint.prior.scale = 0.01, 
            seasonality.mode = 'additive', 
            mcmc.samples = 50)
  })
  
  # creating future to forecast dataframe 
  future <- reactive({
    req(m(), input$years_to_forecast)
    make_future_dataframe(m(),
                          periods=input$years_to_forecast*12, 
                          freq = 'month')
  })
  
  # forecast
  forecast <- reactive({
    req(m(), future())
    forecast <- predict(m(), future())
  })
  
  
  output$plot_forecast <- renderDygraph({
    req(input$run_forecast, m(), forecast())
    dyplot.prophet(m(), forecast())
  })
  ###TAB4#######################################################################
  #--- DATA SPREADSHEET ---#
  
  output$df <- renderDataTable({ df }, options = list(pageLength = 10))
  
}

shinyApp(ui, server)
