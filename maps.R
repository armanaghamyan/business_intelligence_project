library("leaflet")

# map of Armenia
armenia_map<-leaflet(options = leafletOptions(minZoom = 0,maxZoom = 18))
armenia_map<-addTiles(armenia_map) 
armenia_map<-setView(armenia_map,lng =45.0382,lat=40.0691,zoom = 5.5)
armenia_map<-addMarkers(armenia_map,lng=44.5152,lat=40.1872,popup="Capital Yerevan",clusterOptions = markerClusterOptions())
armenia_borders<-readLines("Armenia_full.geojson") %>% paste(collapse = "\n")
armenia_map<-addGeoJSON(armenia_map,geojson = armenia_borders,weight = 1,color = "#fc6f03",fill = TRUE)

# map of Georgia
georgia_map<-leaflet(options = leafletOptions(minZoom = 0,maxZoom = 18))
georgia_map<-addTiles(georgia_map) 
georgia_map<-setView(georgia_map,lng =43.3569,lat=42.3154,zoom = 5.5)
georgia_map<-addMarkers(georgia_map,lng=44.8271,lat=41.7151,popup="Capital Tbilisi",clusterOptions = markerClusterOptions())
georgia_borders<-readLines("Georgia_full.geojson") %>% paste(collapse = "\n")
georgia_map<-addGeoJSON(georgia_map,geojson = georgia_borders,weight = 1,color = "#fc6f03",fill = TRUE)

# map of Azerbaijan
azer_map<-leaflet(options = leafletOptions(minZoom = 0,maxZoom = 18))
azer_map<-addTiles(azer_map) 
azer_map<-setView(azer_map,lng =47.5769,lat=40.1431,zoom = 5.5)
azer_map<-addMarkers(azer_map,lng=49.8671,lat=40.4093,popup="Capital Baku",clusterOptions = markerClusterOptions())
azer_borders<-readLines("Azerbaijan_full.geojson") %>% paste(collapse = "\n")
azer_map<-addGeoJSON(azer_map,geojson = azer_borders,weight = 1,color = "#fc6f03",fill = TRUE)

