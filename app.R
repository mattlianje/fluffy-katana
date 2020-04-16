library(shiny)
library(leaflet)
library(RColorBrewer)
library(nominatim)
library(tmap)
library(tidygeocoder)
library(dplyr)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Deathrate", min(as.numeric(cleanData$Deathrate)), max(as.numeric(cleanData$Deathrate)),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  # Country high level data
  country_df <- read.csv('data/country_data/countries of the world.csv')
  coordinate_df <- read.csv('data/country_data/countries.csv')
  
  country_df[,c(1)]=trimws(country_df[,c(1)])
  names(coordinate_df)[names(coordinate_df) == "name"] <- "Country"
  names(coordinate_df)[names(coordinate_df) == "latitude"] <- "lat"
  names(coordinate_df)[names(coordinate_df) == "longitude"] <- "long"
  mergedData <<- merge(x=country_df, y=coordinate_df, by="Country", all.x=TRUE)
  cleanData <<- mergedData[complete.cases(mergedData), ]
  print(cleanData)
  
  filteredData2 <- reactive({
    mergedData[as.numeric(cleanData$Deathrate) >= input$range[1] & as.numeric(cleanData$Deathrate) <= input$range[2],]
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
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(cleanData) %>% addTiles() %>%
      fitBounds(~min(as.numeric(long)), ~min(as.numeric(lat)), ~max(as.numeric(long)), ~max(as.numeric(lat)))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData2()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^(as.numeric(Deathrate)/37), weight = 1, color = "#f26363",
                 fillColor = ~pal(as.numeric(Deathrate)), fillOpacity = 0.7, popup = ~paste(as.numeric(Deathrate), Country)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = cleanData)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~as.numeric(Deathrate)
      )
    }
  })
}

shinyApp(ui, server)