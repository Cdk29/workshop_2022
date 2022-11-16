library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(sf)
library(DT)


got_eolienne_data <- function(path) {
  stuff <- st_read(path)
  
  st_crs(stuff)
  s.sf.gcs <- st_transform(stuff, crs = 4326)
  y = as(s.sf.gcs, "Spatial")
  df_eolienne <- as.data.frame(SpatialPoints(y))
  colnames(df_eolienne)<-c("lon", "lat")
  df_eolienne$nom <- s.sf.gcs$nom_parc
  
  df_eolienne$type <- "Eolienne"
  
  return(df_eolienne)
  
}

got_solar_data <- function() {
  
  BDPV_opendata_installations <- read_delim("BDPV-opendata-installations.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
  df <- BDPV_opendata_installations[c("lon", "lat",  "locality")]
  df$type <- "Panneau solaire"
  df <- as.data.frame(df)
  return(df)
  
}


shinyServer(function(input, output) {
  # Import Data and clean it
  
  bb_data <- read.csv("blood-banks.csv", stringsAsFactors = FALSE )
  bb_data <- data.frame(bb_data)
  bb_data$Latitude <-  as.numeric(bb_data$Latitude)
  bb_data$Longitude <-  as.numeric(bb_data$Longitude)
  bb_data=filter(bb_data, Latitude != "NA") # removing NA values
  
  # new column for the popup label
  
  bb_data <- mutate(bb_data, cntnt=paste0('<strong>Name: </strong>',Blood.Bank.Name,
                                          '<br><strong>State:</strong> ', State,
                                          '<br><strong>Time:</strong> ', Service.Time,
                                          '<br><strong>Mobile:</strong> ',Mobile,
                                          '<br><strong>HelpLine:</strong> ',Helpline,
                                          '<br><strong>Contact1:</strong> ',Contact.No.1,
                                          '<br><strong>Contact2:</strong> ',Contact.No.2,
                                          '<br><strong>Contact3:</strong> ',Contact.No.3,
                                          '<br><strong>Contact4:</strong> ',Contact.No.4,
                                          '<br><strong>Contact5:</strong> ',Contact.No.5,
                                          '<br><strong>Contact6:</strong> ',Contact.No.6,
                                          '<br><strong>Contact7:</strong> ',Contact.No.7,
                                          '<br><strong>Email:</strong> ',Email,
                                          '<br><strong>Website:</strong> ',Website))

  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("Eolienne", "Panneau solaire"))
  
  
  # # create the leaflet map  
  # output$bbmap <- renderLeaflet({
  #     leaflet(bb_data) %>%
  #     addCircles(lng = ~Longitude, lat = ~Latitude) %>%
  #     addTiles() %>%
  #     addCircleMarkers(data = bb_data, lat =  ~Latitude, lng =~Longitude,
  #                      radius = 3, popup = ~as.character(cntnt),
  #                      color = ~pal(Category),
  #                      stroke = FALSE, fillOpacity = 0.8)%>%
  #     addLegend(pal=pal, values=bb_data$Category,opacity=1, na.label = "Not Available")%>%
  #     addEasyButton(easyButton(
  #       icon="fa-crosshairs", title="ME",
  #       onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  #       })
  
  #eol_data <- got_eolienne_data('Bureau/workshop_2022/eolien-filtre/eolien_filtre.shp')
  
  eol_data <- got_eolienne_data('eolien-filtre/eolien_filtre.shp')
  solar_data <- got_solar_data()
  colnames(solar_data) <- colnames(eol_data)
  
  #solar_data <- head(solar_data, 200)
  
  data <- rbind(eol_data, solar_data) 
  
  output$bbmap <- renderLeaflet({
      leaflet(data) %>%
      addCircles(lng = ~lon, lat = ~lat) %>%
      addTiles() %>%
        addCircleMarkers(data = data, lat =  ~lat, lng =~lon,
                         radius = 3, # popup = ~as.character(cntnt),
                         color = ~pal(type),
                         stroke = FALSE, fillOpacity = 0.8) %>%
         addLegend(pal=pal, values=data$type,opacity=1, na.label = "Not Available")
    })
  
})
