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
library(leaflet.extras)

# Historical data----------------------------------------------
conf_dat <- read_csv("time_series-ncov-Confirmed.csv")
death_dat <- read_csv("time_series-ncov-Deaths.csv")
recvd_dat <- read_csv("time_series-ncov-Recovered.csv")
all_data <- read_csv("covid-19-all.csv")
country_names <-read_csv("api_names.csv")

#NEW DATA
#----------------------------------------------------------------

all_data<-all_data %>% replace_na(list(Confirmed=0,Recovered=0,Deaths=0)) %>%  mutate(Total = Confirmed - Recovered - Deaths)

all_count <- all_data %>%  group_by(Date) %>%
    summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths), Total = sum(Total))
all_count<-all_count %>% mutate(`% change` = 100 * (lead(Confirmed) - Confirmed) / Confirmed)



US_data <- all_data %>%
    filter(`Country/Region` == "US") %>%
    group_by(Date) %>%
    summarise(Confirmed = sum(Confirmed), 
              Recovered = sum(Recovered), 
              Deaths = sum(Deaths), 
              Total = sum(Total))

US_stats <- US_data %>% mutate(`% change` = 100 * (lead(Confirmed) - Confirmed) / Confirmed)
#------------------------------------------------------------


# Define UI-----------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Tracker"),
    dashboardSidebar(sidebarMenu(
        menuItem("Live Data", tabName = "dashboard", icon = icon("dashboard"),
                 menuSubItem(selectInput("country","Select a Country",choices=country_names),tabName = "dashboard")),
        menuItem("Trends", icon = icon("chart-area"), tabName = "about",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Global Map",icon = icon("map"),tabName="maps",
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
                            plotlyOutput("plot1",height = 465,width=700)),
                        box(solidHeader = TRUE,strong("Please Note: Not all locations have City data"),
                            dataTableOutput("dataTable"),width=5)
                        
                    )
            ),
            tabItem(
                tabName = "about",
                h2("Historic Data"),
                h4("Last Updated 03/25/2020"),
                fluidRow(box("Global Percent Change",style = 'color:black',solidHeader = TRUE,
                             background = "light-blue",
                             plotlyOutput("plot2",height = 300,width=375),width=4),
                         box("Global Count",style = 'color:black',solidHeader = TRUE,
                             background = "light-blue",
                             plotlyOutput("plot3",height = 300,width=375),width=4),
                         box("Global Recovery vs Death",style = 'color:black',solidHeader = TRUE,
                             background = "light-blue",
                             plotlyOutput("plot4",height = 300,width=385),width=4)
                         
                         
                )
            ),
            tabItem(
                tabName ="maps",
                h2("Insert Title about Heat Map here"),
                box("Heat Map",style = 'color:black',solidHeader = TRUE,
                    leafletOutput("heat",width='100%'))
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
        plot_ly(x = ~all_count$Date, y = ~all_count$`% change`,
                type = "scatter",
                mode = "lines",
                line = list(color="green"),
                fill = "tonexty",
                fillcolor = "lightgreen") %>% 
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Percentage Change"))
        
    })
    
    
    output$plot3 <- renderPlotly({
        plot_ly(x = ~all_count$Date, y = ~all_count$Total,
                mode = "lines", 
                line = list(color = "green"), 
                fill = "tonexty", 
                fillcolor = "lightgreen")%>% 
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Global Count Total"))
        
    })
    
    output$plot4 <-renderPlotly({
        
        plot_ly(x = ~all_count$Date, y = ~all_count$Total) %>% 
            add_trace(x=~all_count$Date,y=~all_count$Deaths,
                      name="Deaths",
                      mode="lines",fill="tonexty")  %>% 
            add_trace(x = ~all_count$Date, 
                      y = ~all_count$Recovered, 
                      mode = "lines",
                      name='Recovered',
                      fill="tonexty") %>% 
            layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Global Count Total & Deaths"))
    })
    
    output$heat <- renderLeaflet({
        
        # GET WORLD DATA
        
        world_corona_api <- GET(
            url = "https://covid-19-coronavirus-statistics.p.rapidapi.com/v1/stats",
            add_headers("X-RapidApi-Key" = paste(Sys.getenv("Rapid_KEY")))
        )
        stop_for_status(world_corona_api)
        json <- content(world_corona_api, as = "text", encoding = "UTF-8")
        
        world_api_data <- fromJSON(json)
        
        
        
        # find gps coordinates for countries and merge with api data
        
        geo = read_csv("geo_countries.csv")
        
        geo$name[geo$name=="Congo [Republic]"] = "Congo (Brazzaville)" 
        geo$name[geo$name=="Congo [DRC]"] = "Congo (Kinshasa)" 
        geo$name[geo$name=="Taiwan"] = "Taiwan*"
        geo$name[geo$name=="Côte d'Ivoire"] = "Cote d'Ivoire"
        geo$name[geo$name=="South Korea"] = "Korea, South"
        
        countries = unique(world_api_data$data$covid19Stats$country) %>% tibble(name = .)
        countries$name[countries$name=="US"] = "United States"
        
        # subset geo_countries.csv with countries existing in API
        country_coord = merge(geo,countries, by ="name") 
        country_coord = country_coord %>% rename(Place = name)
        
        
        # aggregated statistics grouped by countries
        
        world_count = world_api_data$data$covid19Stats %>% 
            select(country,confirmed,deaths,recovered) %>% 
            group_by(country) %>% 
            summarise(total_confirmed = sum(confirmed),
                      total_deaths = sum(deaths),
                      total_recovered = sum(recovered)) %>% 
            mutate(active_cases = total_confirmed - total_deaths - total_recovered)
        
        world_count$country[world_count$country=="US"] = "United States"
        world_count = world_count %>% rename(Place = country)
        
        
        # merge with gps data
        
        world_rona = merge(world_count,country_coord,by="Place") %>% 
            select(Place,latitude,longitude,active_cases)
        
        leaflet(world_rona) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            addHeatmap(
                lng = ~longitude, lat = ~latitude, intensity = ~active_cases,
                blur = 20, max = 0.05, radius = 15
            )
        
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)