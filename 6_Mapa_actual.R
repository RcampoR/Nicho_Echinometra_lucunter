library(terra)
library(tmap)
library(tidyverse)
library(geodata)


#limpiar entorno
rm(list = ls())
gc()

tmap_mode("plot")

#vectores
Colombia <- vect(here("..", "..", "COL_shp", "gadm36_COL_0.shp"))
Panama <- vect(here("..", "..", "PAN_shp", "gadm41_PAN_0.shp"))
Costa_rica <- vect(here("..", "..", "CRI_shp", "gadm41_CRI_0.shp"))
Nicaragua <- vect(here("..", "..", "NIC_shp", "gadm41_NIC_0.shp"))

ocurrencias_E_lucunter <- readr::read_delim(here("BD_E_lucunter_submuestreado_Caribe.csv")) %>% 
  transmute(lon = decimalLongitude,
            lat = decimalLatitude) %>% 
  vect()

crs(ocurrencias_E_lucunter) <- "EPSG:4326"

#MAR CARIBE

Caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp")) %>% 
  aggregate(dissolve = TRUE) 

#FILTRADO DE PUNTOS POR FUERA DEL Caribe (el modelo nunca los tuvo en cuenta)

ocurrencias_E_lucunter_filtradas <- crop(ocurrencias_E_lucunter, Caribe) 

#raster
Raster_idoneidad <- rast(here("..", "..", "MAPAS", "Raster_idoneidad_caribe.tif"))


# MAPA CONTEXTO PUNTOS OCURRENCIA



mundo <- world(path=".")
crs(mundo) <- "EPSG:4326"


mapa_contexto <- tm_shape(mundo, xlim = c(-100, -40), ylim = c(-60, 40)) + 
  tm_fill(fill = "white") + 
  tm_borders(col = "gray23", lwd = 0.5) + 
  tm_shape(Colombia) + 
  tm_polygons(fill = "gray48") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray48") + 
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray48") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray48") +
  tm_borders(col = "black", lwd = 1.2) +  
  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") 
 




# MAPA DE OCURRENCIAS E. lucunter

mapa_ocurrencias <- tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
  tm_shape(ocurrencias_E_lucunter_filtradas) +
  tm_dots(fill = "red",
          size = 0.7) +
  tm_shape(Colombia) +
  tm_polygons(fill = "gray48") +
  tm_shape(Panama) +
  tm_polygons(fill = "gray48") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray48") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray48") +
  tm_scalebar(position = c("bottom", "left"), text.size = 0.5) +
  tm_compass(position = c("top", "left"), size = 3, type = "arrow") +
  tm_graticules(lines = FALSE,
                labels.col = "gray10") + 
  tm_add_legend(title = "LEYENDA",
                type = "polygons",
                labels = c("Mar Caribe", "Paises area de estudio", 
                           expression("Puntos de Ocurrencia " * italic("Echinometra lucunter"))),
                fill = c("lightblue", "gray48", "red"),
                fontfamily = "sans",
                position = c("top", "right")) +
  tm_layout(frame = TRUE,
            frame.lwd = 3,
            frame.color = "gray20") 


  

# imprimir mapa_ocurrencias
print(mapa_ocurrencias)
print(mapa_contexto, vp = grid::viewport(0.92, 0.25, width = 0.2, height = 0.25))


# guardar mapa 
dev.copy(png, 
         filename = here("..", "..", "MAPAS", "mapa_distribución_caribe.png"), 
         width = 11,        
         height = 7,       
         units = "in",     # Unidades en pulgadas
         res = 1000)   

dev.off()


# MAPA CONTEXTO IDONEIDAD

mapa_contexto_2 <- tm_shape(mundo, xlim = c(-100, -40), ylim = c(-60, 40)) + # Define la extensión del mapa
  tm_fill(fill = "white") + # Rellena todos los países del mundo
  tm_borders(col = "gray23", lwd = 0.5) + # Bordes para todos los países
  tm_shape(Colombia) + # Añade una nueva capa solo para Colombia
  tm_polygons(fill = "gray89") + # Rellena Colombia con un color específico (ej. verde oscuro)
  tm_borders(col = "black", lwd = 1.2) + # Bordes más gruesos para Colombia 
  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue")

# Mapa de idoneidad de habitat 

Mapa_idoneidad <-  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
tm_shape(Colombia) +
  tm_fill(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_shape(Raster_idoneidad) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd"),
            col.legend = tm_legend(title = "Probabilidad de presencia",
                                   position = c("top", "right"))) +
 tm_scalebar(position = c("bottom", "left"), text.size = 0.5) +
   tm_compass(position = c("top", "left"), size = 3, type = "arrow") +
   tm_graticules(lines = FALSE,
                 labels.col = "gray10") +
   tm_add_legend(title = "LEYENDA",
                 type = "polygons",
                 labels = c("Mar Caribe", "Paises area de estudio"),
                 fill = c("lightblue", "gray89"),
                 fontfamily = "sans",
                 position = c("top", "right")) +
   tm_layout(frame = TRUE,
             frame.lwd = 3,
             frame.color = "gray20")
 

# imprimir mapa_idoneidad
print(Mapa_idoneidad)
print(mapa_contexto_2, vp = grid::viewport(0.92, 0.25, width = 0.2, height = 0.25))

# guardar mapa 
dev.copy(png, 
         filename = here("..", "..", "MAPAS", "mapa_idoneidad_caribe.png"), 
         width = 11,       
         height = 7,       
         units = "in",     # Unidades en pulgadas
         res = 1000)   

dev.off()



