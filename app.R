library(shiny)
library(leaflet)
library(RColorBrewer)
library(nominatim)
library(tmap)
library(tidygeocoder)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(zoo)
library(tseries)
library(forecast)

### Importing Data ###
country_df <- read.csv('data/country_data/countries of the world.csv')
coordinate_df <- read.csv('data/country_data/countries.csv')
covid_df <- read.csv('data/covid_data/covid_19_clean_complete.csv')
# Prediction import
covid_set <- read.csv('data/covid_data/covid_19_clean_complete.csv')
covid_set <- covid_set %>% mutate(date_str =  as.Date(as.POSIXct(Date, format="%m/%d/%y"))) %>% arrange(date_str)
covid_set
# Time series import
covid_data = read.csv(file.path(getwd(), "data", "covid_data", "covid_19_clean_complete.csv"), header = TRUE, stringsAsFactors = FALSE)
covid_data$Date <- as.Date(covid_data$Date, format="%m/%d/%y")
covid_data <- aggregate(Confirmed ~ Country.Region + Date, FUN = sum, data=covid_data)

### Cleaning and formatting Data ###

# removes trailing whitespaces
country_df[,c(1)]=trimws(country_df[,c(1)])

# renaming columns to conform to the leaflet lat / long naming convention
names(coordinate_df)[names(coordinate_df) == "name"] <- "Country"
names(coordinate_df)[names(coordinate_df) == "latitude"] <- "lat"
names(coordinate_df)[names(coordinate_df) == "longitude"] <- "long"
# joining lat long dictionary to our high level country data
mergedData <<- merge(x=country_df, y=coordinate_df, by="Country", all.x=TRUE)
# drops rows with any NA in them
cleanData <<- mergedData[complete.cases(mergedData), ]
# drops rows with any empty in them
cleanData[!apply(cleanData == "", 1, all),]
# get a vector of names of cols for dimension input
display_vec <<- unique(names(cleanData))
display_vec <<- display_vec[display_vec != c('Country', 'Region')]
# For some reson only a staggred removal is rendering in the ui ...
display_vec <<- display_vec[display_vec != c('country', 'lat', 'long')]

# test to join covid and geo data
names(covid_df)[names(covid_df) == "Country.Region"] <- "Country"
names(covid_df)[names(covid_df) == "Lat"] <- "lat"
names(covid_df)[names(covid_df) == "Long"] <- "long"
main_df <<- merge(x=covid_df, y=country_df, by="Country", all.x=TRUE)
main_df$Date <- as.Date(main_df$Date, format="%m/%d/%y")

### Helper functions ###

# Transparent theme enforced here (apparentlty you have to double enfore it in renderOutput as well)
# TODO - see how to make semi transparent ...
transparent_theme <-  theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent",colour = NA),
  plot.background = element_rect(fill = "transparent",colour = NA)
)

white_theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)
# plotting deaths function
# USAGE: plot_deaths('Canada')
plot_deaths <- function(country) {
  country_set <- covid_set %>% filter(Country.Region == country)
  country_set.agg <- aggregate(select(country_set, Confirmed, Deaths, Recovered), by=country_set['date_str'], sum)
  deaths <- ggplot(country_set.agg, mapping = aes(date_str, Deaths)) + geom_line() + labs(title=paste('Deaths', country), x='Date', y='Deaths') + transparent_theme
  return(deaths)
}
# plotting confirmed function
# USAGE: plot_confirmed('Canada')
plot_confirmed <- function(country) {
  country_set <- covid_set %>% filter(Country.Region == country)
  country_set.agg <- aggregate(select(country_set, Confirmed, Deaths, Recovered), by=country_set['date_str'], sum)
  confirmed <- ggplot(country_set.agg, mapping = aes(date_str, Confirmed)) + geom_line() + labs(title=paste('Confirmed Cases ', country), x='Date', y='Cases') + transparent_theme
  return(confirmed)
}
# plotting recovered function
# USAGE: plot_confirmed('Canada')
plot_recovered <- function(country) {
  country_set <- covid_set %>% filter(Country.Region == country)
  country_set.agg <- aggregate(select(country_set, Confirmed, Deaths, Recovered), by=country_set['date_str'], sum)
  recovered <- ggplot(country_set.agg, mapping = aes(date_str, Recovered)) + geom_line() + labs(title=paste('Recovered ', country), x='Date', y='Recovered') + transparent_theme
  return(recovered)
}
# plotting change in deaths
# USAGE plot_change_deaths('Canada')
plot_change_deaths <- function(country) {
  country_set <- covid_set %>% filter(Country.Region == country)
  country_set.agg <- aggregate(select(country_set, Confirmed, Deaths, Recovered), by=country_set['date_str'], sum)
  country_set.new_by_week <- country_set.agg %>% mutate(new_deaths_by_week=Deaths - lag(Deaths, 7))
  plot <- ggplot(country_set.new_by_week, mapping = aes(Deaths, new_deaths_by_week)) + geom_line() + labs(title=paste('Trajectory of Deaths', country), y='New Deaths (past week)', x='Total Deaths') + transparent_theme
  return(plot)
}
# plotting change in cases
# USAGE plot_change_confirmed('Canada')
plot_change_confirmed <- function(country) {
  country_set <- covid_set %>% filter(Country.Region == country)
  country_set.agg <- aggregate(select(country_set, Confirmed, Deaths, Recovered), by=country_set['date_str'], sum)
  country_set.new_by_week <- country_set.agg %>% mutate(new_confirmed_by_week=Confirmed - lag(Confirmed, 7))
  plot <- ggplot(country_set.new_by_week, mapping = aes(Confirmed, new_confirmed_by_week)) + geom_line() + labs(title=paste('Trajectory of Confirmed Cases', country), y='New Confirmed Cases (past week)', x='Total Confirmed Cases') + transparent_theme
  return(plot)
}

forecast_confirmed <- function(country) {
  country_ts <- covid_data %>% filter(Country.Region == country) %>% select(c(Confirmed))
  country_ts <- ts(country_ts, start = as.Date("2020-1-22"), frequency = 365)
  fit <- auto.arima(country_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  fore <- forecast(fit, h = 30, level = 90)
  print(fore)
  
  # Create a dataframe from 'fore'
  result_df <- as.data.frame(fore)
  result_df <- tibble::rownames_to_column(result_df, "point")
  rownames(result_df) <- NULL
  colnames(result_df) <- c("point","forecast","lo", "hi")
  final_plot <- plot(fore, ylab = "Number of People", main = paste("Forecast for Number of Cases in", country), xlab = "Time", xaxt = "n")
  ashar_plot <- autoplot(fore, ylab = "Number of People", main = paste("Forecast for Number of Cases in", country), xlab = "Time", xaxt = "n") +
    theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  return(ashar_plot)
}

forecast_confirmed("Canada")

ui <- bootstrapPage(theme = shinytheme('cosmo'),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 20,
                useShinyjs(),
                sliderInput("date_slider", "Date (cumulative)", min = min(main_df$Date),
                            max = max(main_df$Date),value=as.Date("2020-4-01"),timeFormat="%b %d - %Y"),
                selectInput("dimension", "Country (projections)",
                            main_df$Country, selected = "Canada", multiple = FALSE
                ),
                actionButton(inputId = "button", label = "show/hide graphs",
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                plotOutput("plot_confirmed", height="125px"),
                plotOutput("plot_change_confirmed", height="125px"),
                plotOutput("plot_deaths", height="125px"),
                plotOutput("time_series", height="125px"),#,
                #checkboxInput("legend", "Show legend", TRUE)as.Date(as.yearmon(dataset$minDate))
  )
)


server <- function(input, output, session) {
  observeEvent(input$button, {
    shinyjs::toggle("plot_deaths")
    shinyjs::toggle("plot_confirmed")
    shinyjs::toggle("plot_change_deaths")
    shinyjs::toggle("plot_change_confirmed")
    shinyjs::toggle("plot_recovered")
  })
  output$plot_deaths <- renderPlot({
    country <- input$dimension
    plot(plot_deaths(country))
  }) # or }, bg="transparent")
  
  output$plot_confirmed <- renderPlot({
    country <- input$dimension
    plot(plot_confirmed(country))
  })
  
  output$plot_recovered <- renderPlot({
    country <- input$dimension
    plot(plot_recovered(country))
  })
  
  output$plot_change_deaths <- renderPlot({
    country <- input$dimension
    plot(plot_change_deaths(country))
  })
  
  output$plot_change_confirmed <- renderPlot({
    country <- input$dimension
    plot(plot_change_confirmed(country))
  })
  
  output$time_series <- renderPlot({
    country <- input$dimension
    options(scipen=10000)
    par(mar=rep(2,4))
    plot(forecast_confirmed(country))
    #plot(forecast_confirmed(country), ylab = "Number of People", main = paste("Forecast for Number of Cases in", country), xlab = "Time", xaxt = "n")
  })
  
  filteredData2 <- reactive({
    mergedData[as.numeric(cleanData$Deathrate) >= input$range[1] & as.numeric(cleanData$Deathrate) <= input$range[2],]
  })
  
  filteredData3 <- reactive({
    main_df[as.Date(main_df$Date,format = "%m/%d/%y") == input$date_slider[1],]
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  current_selection <- reactive({
    input$overlay_picker  
  })
  # Create a continuous palette function
  pal2 <- reactive({colorNumeric(
    palette = input$colors,
    domain = as.numeric(main_df$Deaths))
  })
  
  # Palette for deaths
  pal3 <- reactive({colorNumeric(
    palette = "Reds",
    domain = as.numeric(main_df$Deaths))
  })
  
  # Palette for recoveries
  pal4 <- reactive({colorNumeric(
    palette = "YlGn",
    domain = as.numeric(main_df$Recovered))
  })
  
  # Palette for confirmed
  pal5 <- reactive({colorNumeric(
    palette = "Oranges",
    domain = as.numeric(main_df$Confirmed))
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(cleanData) %>% addTiles() %>%
      #fitBounds(~min(as.numeric(long)), ~min(as.numeric(lat)), ~max(as.numeric(long)), ~max(as.numeric(lat)))
      fitBounds(-135, 16, 8, 45)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal_3 <- pal3()
    pal_4 <- pal4()
    pal_5 <- pal5()
    
    leafletProxy("map", data = filteredData3()) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~as.numeric(Deaths)/400, weight = 1, color = "#f26363",
                 fillColor = ~pal_3(as.numeric(Deaths)), fillOpacity = 0.7, group = "Deaths", popup = ~paste(as.numeric(Deaths), "dead:", Country)
      ) %>%
      addCircleMarkers(radius = ~as.numeric(Recovered)/500, weight = 1, color = "#f26363",
                       fillColor = ~pal_4(as.numeric(Recovered)), fillOpacity = 0.7, group = "Recovered", popup = ~paste(as.numeric(Recovered), "recovered:", Country)
      ) %>%
      addCircleMarkers(radius = ~as.numeric(Confirmed)/2000, weight = 1, color = "#f26363",
                       fillColor = ~pal_5(as.numeric(Confirmed)), fillOpacity = 0.7, group = "Confirmed", popup = ~paste(as.numeric(Confirmed), "confirmed:", Country)
      ) %>%
      addLayersControl(
        baseGroups = c("Deaths", "Confirmed", "Recovered"),
        options = layersControlOptions(collapsed = FALSE, position='bottomleft')
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = main_df)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    # If ever we want to play around with legends
    # if (input$legend) {
    #   pal <- pal2()
    #   proxy %>% addLegend(position = "bottomright",
    #                       pal = pal, values = ~(as.numeric(Deaths))
    #   )
    # }
  })
}

shinyApp(ui, server)