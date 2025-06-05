library(terra)
library(tmap)
library(tidyverse)
library(geodata)

#vectores
Colombia <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\COL_shp\\gadm36_COL_0.shp")

ocurrencias_E_lucunter <- readr::read_delim("BD_E_lucunter_submuestreado_COL.csv") %>% 
  transmute(lon = decimalLongitude,
            lat = decimalLatitude) %>% 
  vect()

crs(ocurrencias_E_lucunter) <- "EPSG:4326"

#raster
mapa_idoneidad <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MAPAS\\capa_idoneidad_E_lucunter.tif")

#MAR CARIBE
Mar_caribe_INVEMAR <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\COL_shp\\mar_caribe.json") 


# MAPA CONTEXTO PUNTOS OCURRENCIA

mundo <- world


mapa_contexto <- tm_shape(mundo, xlim = c(-100, -40), ylim = c(-60, 40)) + # Define la extensión del mapa
  tm_fill(fill = "white") + # Rellena todos los países del mundo
  tm_borders(col = "gray23", lwd = 0.5) + # Bordes para todos los países
  tm_shape(Colombia) + # Añade una nueva capa solo para Colombia
  tm_polygons(fill = "gray48") + # Rellena Colombia con un color específico (ej. verde oscuro)
  tm_borders(col = "black", lwd = 1.2) + # Bordes más gruesos para Colombia 
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_shape(Mar_caribe_INVEMAR) +
  tm_polygons(fill = "lightblue")




# MAPA DE OCURRENCIAS E. lucunter

mapa_ocurrencias <- tm_shape(Mar_caribe_INVEMAR) +
  tm_polygons(fill = "lightblue") +
tm_shape(ocurrencias_E_lucunter) +
  tm_dots(fill = "red",
          size = 0.5) +
  tm_shape(Colombia) +
  tm_polygons(fill = "gray48",
              fill.legend = tm_legend(title = "LEYENDA", 
                                      position = c("left", "bottom"))) +
  tm_scalebar(position = c("bottom", "right"), size = 5) +
  tm_compass(position = c("top", "right"), size = 3, type = "arrow") +
  tm_grid() +
  tm_add_legend(title = "LEYENDA",
                type = "polygons",
                labels = c("Mar Caribe colombiano", "Republica de Colombia", 
                           expression("Puntos de Ocurrencia " * italic("Echinometra lucunter"))),
                fill = c("lightblue", "gray48", "red"),
                fontfamily = "sans",
                position = c(-0.4, 0.25))









tm_shape(Colombia) +
  tm_fill() + 
  tm_shape(mapa_idoneidad) +
  tm_raster()

