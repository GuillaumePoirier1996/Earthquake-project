data = read.csv("data_clean.csv")
str(data)

#data = subset(data,data$state=="Alaska")
data$year <- as.numeric(format(as.Date(data$date), "%Y"))

data  = subset(data,data$year==2023)

# Load libraries
library(leaflet)
library(leaflet.extras)

# Load libraries
library(leaflet)
library(shiny)
library(dplyr)

# Supposons que 'data' soit votre dataframe
# Assurez-vous que 'latitude', 'longitude', 'mag' et 'date' sont des colonnes dans votre dataframe

# Création de l'application Shiny
# Convertir la colonne 'date' en format de date
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Création de l'application Shiny
shinyApp(
  ui = fluidPage(
    leafletOutput("map"),
    sliderInput("date_range", "Sélectionnez la plage de dates", 
                min = min(data$date), max = max(data$date), 
                value = c(min(data$date), max(data$date)), 
                timeFormat = "%Y-%m-%d"),
    br(),
    tags$style("#map {height: calc(100vh - 150px) !important;}")
  ),
  
  server = function(input, output) {
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(lng = 0, lat = 0, zoom = 1) %>%
        addTiles() %>%
        addHeatmap(lng = data$longitude, lat = data$latitude, 
                   intensity = data$mag, blur = 5, radius = 3) %>%
        addLegend(position = "bottomright", 
                  pal = colorNumeric(palette = viridisLite::viridis(5), domain = data$mag),
                  values = data$mag,
                  opacity = 1,
                  title = "Magnitude",
                  layerId = "unique_legend_id")
    })
    
    observe({
      filtered_data <- data %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
      
      # Supprimer la légende existante
      leafletProxy("map") %>%
        removeControl("unique_legend_id")
      
      # Ajouter la nouvelle légende
      leafletProxy("map") %>%
        addLegend(position = "bottomright", 
                  pal = colorNumeric(palette = viridisLite::viridis(5), domain = filtered_data$mag),
                  values = filtered_data$mag,
                  opacity = 1,
                  title = "Magnitude",
                  layerId = "unique_legend_id")
      
      # Mettre à jour la couche de heatmap
      leafletProxy("map") %>%
        clearHeatmap() %>%
        addHeatmap(lng = filtered_data$longitude, lat = filtered_data$latitude, 
                   intensity = filtered_data$mag, blur = 5, radius = 3)
    })
  }
)
