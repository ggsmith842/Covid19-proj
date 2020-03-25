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

# Historical data----------------------------------------------
conf_dat <- read_csv("time_series-ncov-Confirmed.csv")
death_dat <- read_csv("time_series-ncov-Deaths.csv")
recvd_dat <- read_csv("time_series-ncov-Recovered.csv")


Global_Confirmed <- conf_dat %>%
    select(`Country/Region`, Date, Value, Lat, Long, `Province/State`) %>%
    slice(-1) %>%
    arrange(Date) %>%
    mutate(Date = ymd(Date))
Global_Deaths <- death_dat %>%
    select(`Country/Region`, Date, Value) %>%
    slice(-1) %>%
    arrange(Date) %>%
    mutate(Date = ymd(Date))
Global_Rcvd <- recvd_dat %>%
    select(`Country/Region`, Date, Value) %>%
    slice(-1) %>%
    arrange(Date) %>%
    mutate(Date = ymd(Date))

Global_dat <- bind_cols(Global_Confirmed, Global_Rcvd[, 3], Global_Deaths[, 3])
Global_byDay <- Global_dat %>%
    mutate(Confirmed = as.numeric(Value), Recovered = as.numeric(Value1), Deaths = as.numeric(Value2)) %>%
    select(-3, -7, -8) %>%
    mutate(Total = Confirmed - Recovered - Deaths)

Global_count <- Global_byDay %>%
    group_by(Date) %>%
    summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths), Total = sum(Total))

US_data <- Global_byDay %>%
    filter(`Country/Region` == "US") %>%
    group_by(Date) %>%
    summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths), Total = sum(Total))
US_stats <- US_data %>% mutate(`% change` = 100 * (lead(Confirmed) - Confirmed) / Confirmed)

# Define UI-----------------------------------------------------
ui <- ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Tracker"),
    dashboardSidebar(sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About", icon = icon("th"), tabName = "about",
                 badgeLabel = "new", badgeColor = "green")
    )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(
                    infoBoxOutput("ConfirmedCases"),
                    infoBoxOutput("Mortalities"),
                    infoBoxOutput("deathRatio"),
                    box("Cases by Province",solidHeader = TRUE,
                        background = "black",
                        selectInput("Country","Choose a country",
                                    choices = list("US","Italy")),
                        plotlyOutput("plot1",height = 500)),
                    
                )
            ),
            tabItem(
                tabName = "about",
                h2("Historic Data"),
                fluidRow(box("US Percent Change",solidHeader = TRUE,
                             background = "black",
                             plotlyOutput("plot2",height = 500))
                    
                )
            )
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
        infoBox(title="Confirmed Cases",sum(api_data$confirmed),
                 color="yellow",fill=TRUE)
    })
    
    output$Mortalities <-renderInfoBox({
        infoBox("Mortalities",sum(api_data$deaths),
                color="red",fill=TRUE)
    })
    
    output$deathRatio <-renderInfoBox({
        infoBox("Mortality Rate",
                paste(round(sum(api_data$deaths)/sum(api_data$confirmed),4)*100,"%"),
                color="black",fill=TRUE)
    })
    
    output$plot1 <-renderPlotly({
        plot_ly(x=api_data_byProvince$total,
                y=~api_data_byProvince$province,
                color=~api_data_byProvince$province)
        
    })
    
    output$plot2 <- renderPlotly({
        plot_ly(x = ~US_stats$Date, y = ~US_stats$`% change`,
                type = "scatter",
                mode = "markers", 
                fill = "tonexty") 
            
    })
    
    }



# Run the application 
shinyApp(ui = ui, server = server)
