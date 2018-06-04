if (!require(shiny)){install.packages("shiny")}
if (!require(geosphere)){install.packages("geosphere")}
if (!require(htmltools)){install.packages("htmltools")}
if (!require(leaflet)){install.packages("leaflet")}
if (!require(data.table)){install.packages("data.table")}
if (!require(plotGoogleMaps)){install.packages("plotGoogleMaps")}
if (!require(reshape)){install.packages("plotGoogleMaps")}

library(shiny)
library(geosphere)
library(htmltools)
library(leaflet)
library(data.table)
library(plotGoogleMaps)
library(reshape)
library(proxy)


shinyServer(function(input, output,session){
  
  # Initializing the data
  loc_data <- data.table(readRDS('data/All_data_app.Rds'))
  e_dt <- read.csv('data/Entities_mapping.csv', stringsAsFactors = F)
  
  api="https://maps.googleapis.com/maps/api/js?key=AIzaSyBk1iNGBRPz1DQuKwtKzSwRFHckK996YPw"
  
  
  output$plotLocations = renderLeaflet({
    
    loc_sub = loc_data[loc_data$e_key %in% input$entity & loc_data$city_id == input$city,1:3]
    loc_sub = merge(loc_sub,e_dt, by = 'e_key')
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = loc_sub$col
    )
    
    leaflet(loc_sub) %>% addTiles() %>% addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(entity))
    
    
  })
  
  output$plotClusters = renderLeaflet({
    
    loc_sub = loc_data[loc_data$e_key %in% input$entity & loc_data$city_id == input$city,1:3]
    loc_sub = merge(loc_sub,e_dt, by = 'e_key')
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = loc_sub$col
    )
    
    leaflet() %>% addTiles() %>% 
      addMarkers(data=loc_sub,
                 clusterOptions = markerClusterOptions(),
                 clusterId = "quakesCluster",
                 popup = ~htmlEscape(entity)
      ) %>% 
      addEasyButton(easyButton(
        states = list(
          easyButtonState(
            stateName="unfrozen-markers",
            icon="ion-toggle",
            title="Freeze Clusters",
            onClick = JS("
                     function(btn, map) {
                     var clusterManager =
                     map.layerManager.getLayer('cluster', 'quakesCluster');
                     clusterManager.freezeAtZoom();
                     btn.state('frozen-markers');
                     }")
          ),
          easyButtonState(
            stateName="frozen-markers",
            icon="ion-toggle-filled",
            title="UnFreeze Clusters",
            onClick = JS("
                     function(btn, map) {
                     var clusterManager =
                     map.layerManager.getLayer('cluster', 'quakesCluster');
                     clusterManager.unfreeze();
                     btn.state('unfrozen-markers');
                     }")
          )
        )
      ))
  })
  
  output$plotClusteredLocations = renderLeaflet({ 
    
    loc_sub = loc_data[loc_data$e_key %in% input$entity & loc_data$city_id == input$city,1:3]
    loc_sub = merge(loc_sub,e_dt, by = 'e_key')
    loc_sub = loc_sub[,2:3]
    
    # loc_sub$id = 1:nrow(loc_sub)
    loc_dt <- expand.grid.df(loc_sub,loc_sub)
    names(loc_dt)[3:4] <- c("latitude_dest","longitude_dest")
    # 
    setDT(loc_dt)[ , dist_km := distGeo(matrix(c(longitude, latitude), ncol = 2),
                                        matrix(c(longitude_dest, latitude_dest), ncol = 2))/1000]
    distm = matrix(loc_dt$dist_km,sqrt(nrow(loc_dt)),sqrt(nrow(loc_dt)))
    
    # Create clusters based in distances
    fit <- hclust(as.dist(distm), method="ward.D2")
    plot(fit) # display dendogram
    #
    groups <- cutree(fit, k=input$k) # cut tree into 18 clusters
    # # draw dendogram with red borders around the 18 clusters
    rect.hclust(fit, k=input$k, border="red")
    loc_sub$group = groups # Assign cluster groups
    # # Plot stores with clustor as label
    
    leaflet(loc_sub) %>% addTiles() %>% addAwesomeMarkers(~longitude, ~latitude, label =~ as.character(group))
    
  })
  
})
