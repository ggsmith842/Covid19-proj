library(shiny)
library(shinydashboard)
#library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(jsonlite)
library(httr)
library(plotly)
library(lubridate)
library(rromeo)
library(DT)
library(leaflet)
library(leaflet.extras)
library(reactable)
library(shinythemes)
library(repmis) 
# Historical data----------------------------------------------

#read data from link, will get working later
#all_data <- paste("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/",format(Sys.Date(),"%m-%d-%Y"),".csv") %>% gsub("\\s", "",.) 

all_data <- read_csv("covid-19-all.csv") 

country_names <-read_csv("api_names.csv")

#NEW DATA from Kaggle
#----------------------------------------------------------------

all_data<-all_data %>% replace_na(list(Confirmed=0,Recovered=0,Deaths=0)) %>% 
  mutate(Total = Confirmed - Recovered - Deaths)

all_data = all_data %>% rename(Country = `Country/Region`)

#list of unique countries for dropdown
trend_country = all_data %>% select(Country) %>% unique()

#last updated info
last = all_data$Date %>% unique() %>% tail(1)

#global
all_count <- all_data %>%  group_by(Date) %>%
  summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths), Total = sum(Total))
all_count<-all_count %>% mutate(`% change` = 100 * (lead(Confirmed) - Confirmed) / Confirmed) %>% 
  head(59) #some bug that returns odd days, but its it correct up to the 59th row





US_data <- all_data %>%
  filter(Country == "US") %>%
  group_by(Date) %>%
  summarise(Confirmed = sum(Confirmed), 
            Recovered = sum(Recovered), 
            Deaths = sum(Deaths), 
            Total = sum(Total))

US_stats <- US_data %>% mutate(`% change` = 100 * (lead(Confirmed) - Confirmed) / Confirmed)
#------------------------------------------------------------


# Define UI-----------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Tracker",titleWidth = 180),
  
  dashboardSidebar(width = 180,tags$head(tags$style(HTML('.content-wrapper { height: 1000px !important;}'))),
                   sidebarMenu(
                     menuItem("Live Data", tabName = "live", icon = icon("dashboard"),
                              menuSubItem(selectInput("country","Select a Country",choices=country_names),tabName = "live")),
                     menuItem("Trends", icon = icon("chart-area"), tabName = "trend",
                              menuSubItem(selectInput("country2","Select a Country",choices=trend_country),tabName = "trend")),
                     menuItem("Global Map",icon = icon("map"),tabName="maps",
                              menuSubItem(selectInput("country3","Select a Country",choices=trend_country),tabName = "maps")),
                     menuItem("About",tabName = "about")
                     
                     
                     
                     
                   )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "live",
              fluidRow(
                infoBoxOutput("ConfirmedCases",width = 3),
                infoBoxOutput("Recoveries",width=3),
                infoBoxOutput("Mortalities",width=3),
                infoBoxOutput("deathRatio",width=3),
                box("Cases by Province",align="center",solidHeader = TRUE,
                    helpText("Due to reporting restrictions
                                 not all countries have meaningful provincial data."),
                    helpText("You can also remove 'trace 0' to see a better represenation."),
                    width=7,
                    plotlyOutput("plot1",height = 500,width=600)),
                box(solidHeader = TRUE,strong("Please Note: Not all locations have City data"),
                    reactableOutput("dataTable"),width=5)
                
              )
      ),
      tabItem(
        tabName = "trend",
        #h2("Historic Data"),
        tags$p(style = "font-size: 30px; 
                        font-family: 'Palatino Linotype', 'Book Antiqua', 'Palatino', 'serif';
                        text-align:center;", 
               "Historic Data Trends"),
        h5("Curated Data as of ", paste(last),"reported by ",tags$a("John Hopkins CSSE", href="https://www.kaggle.com/gpreda/coronavirus-2019ncov/data")),
        #global trends plots
        fluidRow(box(strong("Global Percentage Change"),align="center",style = 'color:black',solidHeader = TRUE,
                     #background = "light-blue",
                     plotlyOutput("plot2",height = 300,width=375),width=4),
                 box(strong("Global Active Cases"),align="center",style = 'color:black',solidHeader = TRUE,
                     #background = "light-blue",
                     plotlyOutput("plot3",height = 300,width=375),width=4),
                 box(strong("Global Recovery vs Death"),align="center",style = 'color:black',solidHeader = TRUE,
                     #background = "light-blue",
                     plotlyOutput("plot4",height = 300,width=385),width=4)
        ),
        #global averages info boxes
        fluidRow(infoBoxOutput("avgConf",width=4),
                 infoBoxOutput("avgRecovered",width = 4),
                 infoBoxOutput("avgDeaths",width = 4)),
        
        #by country plots
        fluidRow(box(strong("Percentage Change"),align="center",style = 'color:black',solidHeader = TRUE,
                     #background = "light-blue",
                     plotlyOutput("country_plot2",height = 300,width=375),width=4),
                 box(strong("Active Cases"),align="center",style = 'color:black',solidHeader = TRUE,
                     #background = "light-blue",
                     plotlyOutput("country_plot3",height = 300,width=375),width=4),
                 box(strong("Recovery vs Death"),align="center",style = 'color:black',solidHeader = TRUE,
                     #background = "light-blue",
                     plotlyOutput("country_plot4",height = 300,width=385),width=4)
                 
                 
                 
                 
        )
      ),
      tabItem(
        tabName ="maps",
        #h2("Heat Map",align="center"), 
        tags$p(style = "font-size: 30px; 
                        font-family: 'Palatino Linotype', 'Book Antiqua', 'Palatino', 'serif';
                        text-align:center;", 
               "Geographic Information"),
        h5("Curated Data as of ", paste(last),"reported by ",tags$a("John Hopkins CSSE", href="https://www.kaggle.com/gpreda/coronavirus-2019ncov/data")),
        box("Global Stats",style = 'color:black',solidHeader = TRUE,
            width = 3),
        box("Global Heat Map",style = 'color:black',solidHeader = TRUE,
            width = 9,
            leafletOutput("global_heat",width='100%')),
        box("Country Heat Map ",style = 'color:black',solidHeader = TRUE,
            width = 12,
            leafletOutput("country_heat",width='100%'))
      ),
      tabItem(tabName = "about",
              wellPanel(
                h3(strong("About COVID-19 Dashboard")),
                tags$ul(
                  p("The dashboard aims to provide a representative visualization of live, up-to-date data, sourced from an API, and historic trends, 
            contributed by John Hopkin's CSSE"),
                  tags$li(
                    "Live Data:
              On the landing page, you will find country-specefic statistics on confirmed,recovered,deaths, and active cases.
              To better understanding the magnitude of these numbers on a provincial basis, there is a bar plot and data table showing exactly where these numbers
              are seen growing."),
                  br(),
                  tags$li(
                    "Trends:
              We show how the trends have been growing over time for percentage changes, active cases, and a comparison for recoveries versus
              death. The first row presents the plots for global data and below are the plots for the specefic countries."
                  ),
                  br(),
                  tags$li(
                    "Heat Map:
              Based on geo-coordinates for each reported location, we plot its location on the map and provide cluster objects to show the concentration
              of the spread."
                  )
                ),
                hr()),
              wellPanel(
                h4(strong("Contributors: Grant Smith, Jaymie Tam, Christopher Ton"))),
              wellPanel(
                h4(strong("References")),
                p("RapidAPI", tags$a(href = "https://rapidapi.com/KishCom/api/covid-19-coronavirus-statistics?endpoint=apiendpoint_53587227-476d-4279-8f1d-4884e60d1db7", "COVID-19 Coronavirus Statistics"),"(last updated: 14 days ago)"),
                p("Kaggle", tags$a(href = "https://www.kaggle.com/gpreda/coronavirus-2019ncov/data", "Coronavirus 2019-nCoV"),"(Updated almost daily)"),
                h4("Please visit our ",tags$a(href = "#", "github"), "link to see our project. Thanks for visiting!!"),
                hr()
              ),
              wellPanel(
                h4("For more information, please visit the resources below for guidelines and recommendations!"),
                p(tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html", "Center for Disease Control and Prevention")),
                p( tags$a(href = "https://www.who.int/", "World Health Organization"))
              )
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
  
  
  #by country
  data_by_country <- reactive({
    
    data_byCountry <- all_data %>%  filter(Country == paste(input$country2)) %>% group_by(Date) %>%
      summarise(Confirmed = sum(Confirmed), 
                Recovered = sum(Recovered), 
                Deaths = sum(Deaths), 
                Total = sum(Total))  
    
    data_byCountry<-data_byCountry %>% mutate(`% change` = 100 * (lead(Confirmed) - Confirmed) / Confirmed)
  })
  
  # LIVE DATA PAGE
  #--------------------------------------------------------------------------------------------------------
  
  output$ConfirmedCases <-renderInfoBox({
    infoBox(tags$p(style = "font-size: 15px; font-family: 'Palatino Linotype', 'Book Antiqua', 'Palatino', 'serif';", "Confirmed Cases"),
            sum(api()$confirmed),
            color="yellow",fill=TRUE,icon=icon("notes-medical"),width=2)
  })
  
  output$Mortalities <-renderInfoBox({
    infoBox(tags$p(style = "font-size: 15px;font-family: 'Palatino Linotype', 'Book Antiqua', 'Palatino', 'serif';", "Mortalities"),
            sum(api()$deaths),
            color="red",fill=TRUE,icon=icon("diagnoses"),width=2)
  })
  
  output$Recoveries <-renderInfoBox({
    infoBox(tags$p(style = "font-size: 15px;font-family: 'Palatino Linotype', 'Book Antiqua', 'Palatino', 'serif';", "Recoveries"),
            sum(api()$recovered),
            color="green",fill=TRUE,icon=icon("star-of-life"),width=2)
  })
  
  output$deathRatio <-renderInfoBox({
    infoBox(tags$p(style = "font-size: 15px;font-family: 'Palatino Linotype', 'Book Antiqua', 'Palatino', 'serif';", "Mortality Rate"),
            paste(round(sum(api()$deaths)/sum(api()$confirmed),4)*100,"%"),
            color="black",fill=TRUE,icon=icon('percentage'),width=2)
  })
  
  
  output$plot1 <-renderPlotly({
    plot_ly(api_by_province(),x=api_by_province()$total,
            y=~api_by_province()$province,
            color=~api_by_province()$province) %>% 
      layout(xaxis=list(title="Total Active Cases"),
             yaxis=list(title='State/Province'))
    
  })
  
  # output$dataTable <- {renderDataTable(api()[-3:-5],options=
  #                                          list(lengthMenu=c(10,15),
  #                                               scrollX=TRUE))}
  
  output$dataTable <- renderReactable({
    reactable(api()[-3:-5] %>% arrange(desc(confirmed)),
              filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
              showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(15,30,45,60,75), 
              columns = list(
                confirmed = colDef(filterable = FALSE,defaultSortOrder = "desc"),
                deaths = colDef(filterable = FALSE, defaultSortOrder = "desc"),
                recovered = colDef(filterable =  FALSE, defaultSortOrder = "desc")
              ))
  })
  
  # TRENDS PAGE        
  #-----------------------------------------------------------------------------------------------    
  
  #Global historic count plots
  output$plot2 <- renderPlotly({
    plot_ly(x = ~all_count$Date, y = ~all_count$`% change`,
            type = "scatter",
            mode = "lines",
            line = list(color="darkblue"),
            fill = "tonexty",
            fillcolor = "lightblue") %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Percentage Change")) %>% config(displayModeBar = F)
    
  })
  
  
  output$plot3 <- renderPlotly({
    plot_ly(x = ~all_count$Date, y = ~all_count$Total,
            mode = "lines", 
            line = list(color = "orange"), 
            fill = "tonexty", 
            fillcolor = "lightorange")%>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Global Count Total")) %>% config(displayModeBar = F)
    
  })
  
  output$plot4 <-renderPlotly({
    
    plot_ly(x = ~all_count$Date, y = ~all_count$Total) %>% 
      add_trace(x=~all_count$Date,y=~all_count$Deaths,
                name="Deaths",
                mode="lines",
                line = list(color='red'),
                fill="tonexty",
                fillcolor='lightred')  %>% 
      add_trace(x = ~all_count$Date, 
                y = ~all_count$Recovered, 
                mode = "lines",
                line = list(color='darkgreen'),
                name='Recovered',
                fill="tonexty",
                fillcolor='lightgreen') %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Global Count Total & Deaths")) %>% config(displayModeBar = F)
  })
  
  #averages infoBoxes------------
  output$avgConf <- renderInfoBox({
    infoBox("Avg Global Change in Confirmed Cases Per Day",
            color="yellow",fill=TRUE,icon=icon("notes-medical"),
            all_count %>% 
              transmute(diff=lead(Confirmed)-Confirmed) %>% 
              summarise(avg=round(mean(diff,na.rm=TRUE),0)))
  })
  
  output$avgRecovered <- renderInfoBox({
    infoBox("Avg Global Change in Recoveries Per Day",
            color="green",fill=TRUE,icon=icon("star-of-life"),
            all_count %>% 
              transmute(diff=lead(Recovered)-Recovered) %>% 
              summarise(avg=round(mean(diff,na.rm=TRUE),0)))
  })
  
  output$avgDeaths <- renderInfoBox({
    infoBox("Avg Global Change in Mortalities Per Day",
            color="red",fill=TRUE,icon=icon("diagnoses"),
            all_count %>% 
              transmute(diff=lead(Deaths)-Deaths) %>% 
              summarise(avg=round(mean(diff,na.rm=TRUE),0)))
  })
  
  
  #by country plots----------------
  output$country_plot2 <- renderPlotly({
    
    req(data_by_country())
    
    plot_ly(x = ~data_by_country()$Date, y = ~data_by_country()$`% change`,
            type = "scatter",
            mode = "lines",
            line = list(color="darkblue"),
            fill = "tonexty",
            fillcolor = "lightblue") %>% 
      layout(title = paste(input$country2),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Percentage Change")) %>% config(displayModeBar = F)
    
  })
  
  
  output$country_plot3 <- renderPlotly({
    
    req(data_by_country())
    
    plot_ly(x = ~data_by_country()$Date, y = ~data_by_country()$Total,
            mode = "lines", 
            line = list(color = "orange"), 
            fill = "tonexty", 
            fillcolor = "lightorange")%>% 
      layout(title = paste(input$country2),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Global Count Total")) %>% config(displayModeBar = F)
    
  })
  
  output$country_plot4 <-renderPlotly({
    
    req(data_by_country())
    
    plot_ly(x = ~data_by_country()$Date, y = ~data_by_country()$Total) %>% 
      add_trace(x=~data_by_country()$Date,y=~data_by_country()$Deaths,
                name="Deaths",
                mode="lines",
                line = list(color='red'),
                fill="tonexty",
                fillcolor='lightred')  %>% 
      add_trace(x = ~data_by_country()$Date, 
                y = ~data_by_country()$Recovered, 
                mode = "lines",
                line = list(color='darkgreen'),
                name='Recovered',
                fill="tonexty",
                fillcolor='lightgreen') %>% 
      layout(title = paste(input$country2),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Global Count Total & Deaths")) %>% config(displayModeBar = F)
  })
  
  # GLOBAL MAPS PAGE        
  #---------------------------------------------------------------------------------------------------    
  output$global_heat <- renderLeaflet({
    
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
    
    color_scheme <- viridis::cividis(n_distinct(world_rona$active_cases))
    pal = colorFactor(color_scheme, world_rona$active_cases)
    
    #https://www.r-bloggers.com/exploring-london-crime-with-r-heat-maps/
    world_rona %>% 
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~as.numeric(longitude),
                       ~as.numeric(latitude),
                       fillColor = ~pal(active_cases),
                       stroke = FALSE, fillOpacity = 0.8,
                       clusterOptions = markerClusterOptions(), # adds summary circles
                       popup = ~paste(world_rona$Place,":",world_rona$active_cases)
      ) %>%
      addHeatmap(lng=~as.numeric(longitude),
                 lat=~as.numeric(latitude),
                 radius = 8) %>% 
      addResetMapButton()
  })
  
  
  output$country_heat <-  renderLeaflet({
    
    specefic_c = all_data %>% 
      filter(Country == paste(input$country3))%>% 
      select(`Province/State`,Latitude,Longitude,Total)
    
    color_scheme <- viridis::cividis(n_distinct(specefic_c$Total))
    pal = colorFactor(color_scheme, specefic_c$Total)
    
    specefic_c  %>% 
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~as.numeric(Longitude),
                       ~as.numeric(Latitude),
                       fillColor = ~pal(Total),
                       stroke = FALSE, fillOpacity = 0.8,
                       clusterOptions = markerClusterOptions(), # adds summary circles
                       popup = ~paste(specefic_c$`Province/State`,":",specefic_c$Total)
      ) %>%
      addHeatmap(lng=~as.numeric(Longitude),
                 lat=~as.numeric(Latitude),
                 radius = 8) %>% 
      addResetMapButton()
    
    
  })
  
  
  
}

#-------------------------------------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)