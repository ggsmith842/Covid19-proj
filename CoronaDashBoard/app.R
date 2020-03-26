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
country_names <-read_csv("api_names.csv")

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
ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Tracker"),
    dashboardSidebar(sidebarMenu(
        menuItem("Live Data", tabName = "dashboard", icon = icon("dashboard"),
                 menuSubItem(selectInput("country","Select a Country",choices=country_names),tabName = "dashboard")),
        menuItem("Trends", icon = icon("th"), tabName = "about",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Global Map",tabName="maps",
                 badgeLabel = 'geo',badgeColor = "green")
    )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(
                    infoBoxOutput("ConfirmedCases",width = 3),
                    infoBoxOutput("Recoveries",width=3),
                    infoBoxOutput("Mortalities",width=3),
                    infoBoxOutput("deathRatio",width=2),
                    box("Cases by Province",solidHeader = TRUE,
                        helpText("Due to reporting restrictions
                                 not all countries have meaningful provincial data."),
                        helpText("You can also remove 'trace 0' to see a better represenation."),
                        background = "black", width=7,
                        plotlyOutput("plot1",height = 465,width=700),),
                    box(solidHeader = TRUE,strong("Please Note: Not all locations have City data"),
                        dataTableOutput("dataTable"),width=5)
                    
                )
            ),
            tabItem(
                tabName = "about",
                h2("Historic Data"),
                h4("Last Updated 03/21/2020"),
                fluidRow(box("US Percent Change",solidHeader = TRUE,
                             background = "blue",
                             plotlyOutput("plot2",height = 300,width=300),height=3,width=3),
                         box("Global Count",solidHeader = TRUE,
                             background = "blue",
                             plotlyOutput("plot3",height = 300,width=300),height=3,width=3),
                         box("Global Recovery vs Death",solidHeader = TRUE,
                             background = "blue",
                             plotlyOutput("plot4",height = 300,width=400),height=3,width=4)
            
                    
                )
            ),
            tabItem(
                tabName ="maps",
                h2("The Map")
            )
        )
        
    )
)

# Define server logic 
server <- function(input, output) {
    
 api<-reactive({corona_api <- GET(
     url = "https://covid-19-coronavirus-statistics.p.rapidapi.com/v1/stats",
     add_headers("X-RapidApi-Key" = paste(Sys.getenv("Rapid_KEY"))),
     query = list(
         country = input$country
     )
 )
 stop_for_status(corona_api)
 json <- content(corona_api, as = "text", encoding = "UTF-8")
 
 api_data <- fromJSON(json)
 
 api_data <- api_data$data$covid19Stats
 api_data <- api_data %>% mutate(total = confirmed - deaths - recovered) 
 
 })
 
 
 api_by_province <- reactive({
     
     api_data_byProvince <- api() %>%
         group_by(province) %>%
         summarise(total = sum(total))
 }) 
 
 
     
    output$ConfirmedCases <-renderInfoBox({
        infoBox(title="Confirmed Cases",sum(api()$confirmed),
                 color="yellow",fill=TRUE,width=2)
    })
    
    output$Mortalities <-renderInfoBox({
        infoBox("Mortalities",sum(api()$deaths),
                color="red",fill=TRUE,width=2)
    })
    
    output$Recoveries <-renderInfoBox({
        infoBox("Recoveries",sum(api()$recovered),
                color="green",fill=TRUE,width=2)
    })
    
    output$deathRatio <-renderInfoBox({
        infoBox("Mortality Rate",
                paste(round(sum(api()$deaths)/sum(api()$confirmed),4)*100,"%"),
                color="black",fill=TRUE,width=2)
    })
    
    output$plot1 <-renderPlotly({
        plot_ly(api_by_province(),x=api_by_province()$total,
                y=~api_by_province()$province,
                color=~api_by_province()$province) %>% 
            layout(xaxis=list(title="Total Active Cases"),
                   yaxis=list(title='State/Province'))
        
    })
    
    output$dataTable <- {renderDataTable(api()[-3:-5],options=
                                             list(lengthMenu=c(10,15),
                                                  scrollX=TRUE))}
    
    output$plot2 <- renderPlotly({
        plot_ly(x = ~US_stats$Date, y = ~US_stats$`% change`,
                type = "scatter",
                mode = "markers", 
                fill = "tonexty") 
            
    })
    
    output$plot3 <- renderPlotly({
        plot_ly(x = ~Global_count$Date, y = ~Global_count$Total,
                mode = "lines", 
                line = list(color = "green"), 
                fill = "tonexty", 
                fillcolor = "lightgreen")
        
    })
    
    output$plot4 <-renderPlotly({
        
        plot_ly(x = ~Global_count$Date, y = ~Global_count$Total) %>% 
              add_trace(x=~Global_count$Date,y=~Global_count$Deaths,
                        name="Deaths",
                        mode="lines",fill="tonexty")  %>% 
             add_trace(x = ~Global_count$Date, 
                       y = ~Global_count$Recovered, 
                       mode = "lines",
                       name='Recovered',
                       fill="tonexty")
    })
    
    }



# Run the application 
shinyApp(ui = ui, server = server)
