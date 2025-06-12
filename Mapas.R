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
Raster_idoneidad <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MAPAS\\Raster_idoneidad.tif")

#MAR CARIBE
Mar_caribe_INVEMAR <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\COL_shp\\mar_caribe.json") 


# MAPA CONTEXTO PUNTOS OCURRENCIA



mundo <- world(path=".")
crs(mundo) <- "EPSG:4326"


mapa_contexto <- tm_shape(mundo, xlim = c(-100, -40), ylim = c(-60, 40)) + # Define la extensión del mapa
  tm_fill(fill = "white") + # Rellena todos los países del mundo
  tm_borders(col = "gray23", lwd = 0.5) + # Bordes para todos los países
  tm_shape(Colombia) + # Añade una nueva capa solo para Colombia
  tm_polygons(fill = "gray48") + # Rellena Colombia con un color específico (ej. verde oscuro)
  tm_borders(col = "black", lwd = 1.2) + # Bordes más gruesos para Colombia 
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
  tm_graticules(lines = FALSE,
                labels.col = "gray10") + 
  tm_add_legend(title = "LEYENDA",
                type = "polygons",
                labels = c("Mar Caribe colombiano", "Republica de Colombia", 
                           expression("Puntos de Ocurrencia " * italic("Echinometra lucunter"))),
                fill = c("lightblue", "gray48", "red"),
                fontfamily = "sans",
                position = c(-0.08, 0.13)) +
  tm_layout(frame = TRUE,
            frame.lwd = 3,
            frame.color = "gray20")

# imprimir mapa_ocurrencias
print(mapa_ocurrencias)
print(mapa_contexto, vp = grid::viewport(0.92, 0.3, width = 0.15, height = 0.2))


# guardar mapa 
dev.copy(png, 
         filename = "mapa_distribución.png", 
         width = 10,        # 8 pulgadas de ancho
         height = 6,       # 6 pulgadas de alto
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
  tm_shape(Mar_caribe_INVEMAR) +
  tm_polygons(fill = "lightblue")

# Mapa de idoneidad de habitat 

Mapa_idoneidad <-  tm_shape(Mar_caribe_INVEMAR) +
  tm_polygons(fill = "lightblue") +
tm_shape(Colombia) +
  tm_fill(fill = "gray89") + 
  tm_shape(Raster_idoneidad) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd"),
            col.legend = tm_legend(title = expression("Probabilidad de presencia"),
                                   position = c(-0.098, 0.348))) +
 tm_scalebar(position = c("bottom", "right"), text.size = 0.5) +
   tm_compass(position = c("top", "right"), size = 3, type = "arrow") +
   tm_graticules(lines = FALSE,
                 labels.col = "gray10") +
   tm_add_legend(title = "LEYENDA",
                 type = "polygons",
                 labels = c("Mar Caribe colombiano", "Republica de Colombia"),
                 fill = c("lightblue", "gray89"),
                 fontfamily = "sans",
                 position = c(-0.098, 0.348)) +
   tm_layout(frame = TRUE,
             frame.lwd = 3,
             frame.color = "gray20")
 

# imprimir mapa_idoneidad
print(Mapa_idoneidad)
print(mapa_contexto_2, vp = grid::viewport(0.92, 0.3, width = 0.15, height = 0.2))

# guardar mapa 
dev.copy(png, 
         filename = "mapa_idoneidad.png", 
         width = 10,        # 8 pulgadas de ancho
         height = 6,       # 6 pulgadas de alto
         units = "in",     # Unidades en pulgadas
         res = 1000)   

dev.off()


 # visualizar
tmap_mode("view")

# estatico

tmap_mode("plot")









