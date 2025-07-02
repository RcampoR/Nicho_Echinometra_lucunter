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


#MAR CARIBE

Caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp")) %>% 
  aggregate(dissolve = TRUE) 


# raster optimista

raster_optimista <- rast(here("..", "..", "MAPAS", "Raster_optimista_caribe.tif"))




# MAPA CONTEXTO 



mundo <- world(path=".")
crs(mundo) <- "EPSG:4326"


mapa_contexto <- tm_shape(mundo, xlim = c(-100, -40), ylim = c(-60, 40)) + 
  tm_fill(fill = "white") + 
  tm_borders(col = "gray23", lwd = 0.5) + 
  tm_shape(Colombia) + 
  tm_polygons(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") + 
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_borders(col = "black", lwd = 1.2) +  
  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") 






mapa_optimista <-  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
  tm_shape(Colombia) +
  tm_fill(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_shape(raster_optimista) +
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


# imprimir mapa_optimista
print(mapa_optimista)
print(mapa_contexto, vp = grid::viewport(0.92, 0.25, width = 0.2, height = 0.25))

# guardar mapa 
dev.copy(png, 
         filename = here("..", "..", "MAPAS", "mapa_optimista_caribe.png"), 
         width = 11,       
         height = 7,       
         units = "in",     # Unidades en pulgadas
         res = 1000)   

dev.off()



