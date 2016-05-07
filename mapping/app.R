library(rsconnect)
library(leaflet)
library(maptools)
library(tm)
library(plyr)
library(rgdal)
library(devtools)
library(rgeos)
library(tigris)
library(sp)
library(viridis)
library(shiny)
library(dplyr)

# Shiny code based heavily on:
# https://github.com/gary159/Edav-P3-ggteam/blob/master/ce2330/app.R
#################################################################################################
load("nyc.Rda")

pal <- function(x) {colorNumeric(
  palette = rev(viridis(max(x)*100,option="magma")),
  domain = x
  )
}

namesMap = list("Heat/Hot Water" = "heat_hot_water", "Street Light Condition"= "street_light_condition",
                "Plumbing" = "plumbing", "Water System" = "water_system", "Unsanitary Condition" = "unsanitary_condition",
                "Door/Window" = "door_window", "Paint/Plaster" = "paint_plaster", "Street Condition" = "street_condition",
                "Traffic Signal Condition" = "traffic_signal_condition", "Blocked Driveway" = "blocked_driveway")

ui <- bootstrapPage(
  #tags$head(
  #  includeCSS("styles.css")
  #),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = "controls", class = "panel panel-default",
                fixed = TRUE, draggable = TRUE,height = "100px", width =  "250px",
                selectInput("complaint", "Complaint Type",
                            choices = names(namesMap))
  ),
  absolutePanel(top = 10, left = 35,headerPanel("311 Complaints: January 8, 2015"))
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  selectedFeature <- reactive({
    selected = input$complaint
    return(list(complaint = namesMap[[selected]], name = selected))
  })
  
  output$map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet(nyc) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat = 40.7, lng = -74.0, zoom = 11) #%>%
    
  })
  observe({
    complaint = unlist(nyc@data[selectedFeature()[["complaint"]]])
    featureName = selectedFeature()[["name"]]
    
    leafletProxy("map") %>%
      clearControls() %>%
      addPolygons(data = nyc, color = pal(complaint)(complaint),stroke = TRUE, weight = 1,
                  popup = paste(featureName,complaint, sep=": "), fillOpacity = 0.7)%>%
      addLegend(pal = pal(complaint),
                values = complaint,
                position = "bottomright",
                title = featureName)
  })
  
}

shinyApp(ui, server)

