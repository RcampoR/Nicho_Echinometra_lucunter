
library(tmap)
library(terra)
library(geodata)
library(readr)


# revisar nuevas ocurrencias

ocurrencias_nuevas <- read_delim(here("ocurrencia_final.csv"))

# cargar area de estudio

mar_caribe <- vect(here("..", "..", "Marine_Regions", "capa_limpia.shp")) %>% 
              aggregate(dissolve = TRUE)

crs(mar_caribe) <- "EPSG:4326"
              


# explorar 

ocurrencias_nuevas %>% 
  group_by(countryCode) %>% 
  summarise(n())



#explorar ocurrencias en un mapa

tmap_mode("view")



tm_shape(mar_caribe) +
  tm_polygons() +
ocurrencias_nuevas %>% 
  vect(geom = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  tm_shape() +
  tm_dots(fill = "red", size = 0.5, fill.alpha = 1) +
  tm_layout(legend.outside = FALSE) 


# filtrado de ocurrencias repetidas 
  
ocurrencias_filtradas <- ocurrencias_nuevas %>% 
  mutate(
    longitud = decimalLongitude,
    latitud = decimalLatitude
  ) %>% 
  distinct(
    longitud, latitud, .keep_all = TRUE
  ) 




# filtrar ocurrencias dentro del area de estudio

ocurrencias_final <- ocurrencias_filtradas %>% 
  vect(geom = c("longitud", "latitud"), crs = 4326) %>% 
  crop(mar_caribe)




# revisar ocurrencias filtradas en un mapa


tm_shape(mar_caribe) +
  tm_polygons() +
  tm_shape(ocurrencias_final) +
  tm_dots(fill = "red", size = 0.5, fill_alpha = 1) +
  tm_layout(legend.outside = FALSE) 



# guardar ocurrencias nuevas filtradas

ocurrencias_final %>% 
  as.data.frame() %>% 
write_csv(here("ocurrencias_filtradas.csv"))





