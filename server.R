library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(sf)
library(DT)
library(rgdal)
library(colorblindr)
library(lubridate)

# ggthemr('light')


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

#
time_frame <- as_datetime(c("2022-11-18 00:00:00", "2022-11-18 23:59:59"))
electricity <- data.frame(
  heure = seq(time_frame[1], time_frame[2], by = "hour"),
  value = (runif(24) + sqrt(runif(24))) * 100
)

production <- data.frame(
  heure = seq(time_frame[1], time_frame[2], by = "hour"),
  value = (runif(24)) * 10
)

energie_pilotable <- data.frame(
  heure = seq(time_frame[1], time_frame[2], by = "hour"),
  value = electricity$value -  production$value
)


p <- electricity %>%
  ggplot( aes(x=heure, y=value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Consommation en MW") +
  theme_minimal()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p

g <- production %>%
  ggplot( aes(x=heure, y=value)) +
  geom_area(fill="#a269b3", alpha=0.5) +
  geom_line(color="#a269b3") +
  ylab("Production en MW") +
  theme_minimal()

# Turn it interactive with ggplotly
g <- ggplotly(g)
g

pilot <- energie_pilotable %>%
  ggplot( aes(x=heure, y=value)) +
  geom_area(fill="#b3a269", alpha=0.5) +
  geom_line(color="#b3a269") +
  ylab("Energie pilotable \n à fournir, en MW") +
  theme_minimal()

# Turn it interactive with ggplotly
pilot <- ggplotly(pilot)
pilot


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
  pal <- palette_OkabeIto[c(5,1)] 
  
  # pal <- c("#6778A5", "#ECBF93")
  pal <- colorFactor(pal = pal, domain = c("Eolienne", "Panneau solaire"))

  output$reactive_case_count <- renderText({
    paste0("Production éolienne estimé : ", max(production$value), " MW")
  })
  
  output$reactive_death_count <- renderText({
    paste0("Consommation estimé : ", max(electricity$value), " MW")
  })
  
  #eol_data <- got_eolienne_data('Bureau/workshop_2022/eolien-filtre/eolien_filtre.shp')
  
  eol_data <- got_eolienne_data('eolien-filtre/eolien_filtre.shp')
  solar_data <- got_solar_data()
  colnames(solar_data) <- colnames(eol_data)
  
  solar_data <- head(solar_data, 200)
  
  data <- rbind(eol_data, solar_data)
  
  output$bbmap <- renderLeaflet({
      leaflet(data) %>%
      addCircles(lng = ~lon, lat = ~lat) %>%
      addTiles() %>%
        addCircleMarkers(data = data, lat =  ~lat, lng =~lon,
                         radius = 3, popup = ~nom,
                         color = ~pal(type),
                         stroke = FALSE, fillOpacity = 0.8) %>%
         addLegend(pal=pal, values=data$type, opacity=1, na.label = "Not Available")
    })
  output$prediction_plot <- renderPlotly({p})
  output$production_plot <- renderPlotly({g})
  output$pilot_plot <- renderPlotly({pilot})
  
})
