library(rgdal)
library(sf)

stuff <- st_read('Bureau/workshop_2022/eolien-filtre/eolien_filtre.shp')

st_crs(stuff)
s.sf.gcs <- st_transform(stuff, crs = 4326)
#plot(st_geometry(s.sf.gcs))
y = as(s.sf.gcs, "Spatial")
#plot(y)
df_eolienne <- as.data.frame(SpatialPoints(y))


df_eolienne$nom_parc <- s.sf.gcs$nom_parc

got_eolienne_data <- function(path) {
  stuff <- st_read(path)
  
  st_crs(stuff)
  s.sf.gcs <- st_transform(stuff, crs = 4326)
  #plot(st_geometry(s.sf.gcs))
  y = as(s.sf.gcs, "Spatial")
  #plot(y)
  df_eolienne <- as.data.frame(SpatialPoints(y))
  
  
  df_eolienne$nom_parc <- s.sf.gcs$nom_parc
  return(df_eolienne)
  
}
