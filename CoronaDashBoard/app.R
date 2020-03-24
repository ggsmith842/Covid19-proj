library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(httr)
library(plotly)
library(lubridate)
library(rromeo)
library(DT)
library(leaflet)

# Define UI
ui <- ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Tracker"),
    dashboardSidebar(),
    dashboardBody(
        
        fluidRow(
            infoBoxOutput("ConfirmedCases"),
            infoBoxOutput("Mortalities"),
            box("Barchart",
                plotlyOutput("plot1",height = 500)),
            
                 )
    )
)

# Define server logic 
server <- function(input, output) {
    
    corona_api <- GET(
        url = "https://covid-19-coronavirus-statistics.p.rapidapi.com/v1/stats",
        add_headers("X-RapidApi-Key" = paste(Sys.getenv("Rapid_KEY"))),
        query = list(
            country = "US"
        )
    )
    stop_for_status(corona_api)
    json <- content(corona_api, as = "text", encoding = "UTF-8")
    
    api_data <- fromJSON(json)
    
    api_data <- api_data$data$covid19Stats
    api_data <- api_data %>% mutate(total = confirmed - deaths - recovered) 
    
    
    api_data_byProvince <- api_data %>%
        group_by(province) %>%
        summarise(total = sum(total))
    
    
    output$ConfirmedCases <-renderInfoBox({
        infoBox("ConfirmedCases",sum(api_data$confirmed),
                 color="yellow",fill=TRUE)
    })
    
    output$Mortalities <-renderInfoBox({
        infoBox("Mortalities",sum(api_data$deaths),
                color="red",fill=TRUE)
    })
    
    output$plot1 <-renderPlotly({
        plot_ly(x=api_data_byProvince$total,
                y=~api_data_byProvince$province,
                color=~api_data_byProvince$province)
        
    })
    
    }



# Run the application 
shinyApp(ui = ui, server = server)
