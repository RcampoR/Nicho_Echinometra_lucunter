

library(terra)
library(here)
library(tidyverse)
library(sdm)
library(tmap)



rm(list = ls())




#RASTER DE IDONEIDAD DEL ENSAMBLE

raster_actual <- rast(here("..", "..", "MAPAS", "raster_actual_ponderado.tif"))

raster_ssp1 <- rast(here("..", "..", "MAPAS", "raster_optimista_ponderado.tif"))

raster_ssp5 <- rast(here("..", "..", "MAPAS", "raster_pesimista_ponderado.tif"))


# Binarizar los mapas raster

# Definir el umbral
umbral <- 0.564

# Crear un raster binario: 1 si es idóneo, 0 si no
raster_binario_ACTUAL <- classify(raster_actual, matrix(c(-Inf, umbral, 0, umbral, Inf, 1), ncol = 3, byrow = TRUE))

raster_binario_SSP1 <- classify(raster_ssp1, matrix(c(-Inf, umbral, 0, umbral, Inf, 1), ncol = 3, byrow = TRUE))

raster_binario_SSP5 <- classify(raster_ssp5, matrix(c(-Inf, umbral, 0, umbral, Inf, 1), ncol = 3, byrow = TRUE))


# Crear un mapa de cambio neto para los escenarios "Actual" vs. "SSP1"
mapa_cambio_ssp1 <- (raster_binario_ACTUAL * 2) + raster_binario_SSP1

plot(mapa_cambio_ssp1)

# Reclasificar los valores del mapa para una mejor visualización

# Ausente (no idóneo en ninguno de los escenarios) -> 0
#ganancia (actual no idoneo, futuro si es idoneo) -> 1
# Pérdida (idóneo en Actual, no en SSP1) -> 2
# Estable (idóneo en ambos escenarios) -> 3


# Crear un mapa de cambio neto para los escenarios "Actual" vs. "SSP5"
mapa_cambio_ssp5 <- (raster_binario_ACTUAL * 2) + raster_binario_SSP5



#vectores
Colombia <- vect(here("..", "..", "COL_shp", "gadm36_COL_0.shp"))
Panama <- vect(here("..", "..", "PAN_shp", "gadm41_PAN_0.shp"))
Costa_rica <- vect(here("..", "..", "CRI_shp", "gadm41_CRI_0.shp"))
Nicaragua <- vect(here("..", "..", "NIC_shp", "gadm41_NIC_0.shp"))



#MAR CARIBE

Caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp")) %>% 
  aggregate(dissolve = TRUE) 


# ACTUAL VS SSP1 
Mapa_act_ssp1 <- tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
  tm_shape(Colombia) +
  tm_fill(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_shape(mapa_cambio_ssp1) +
  tm_raster(col.scale = tm_scale_categorical(values = c("lightblue", "#E34A33", "green4"),
                                             labels = c("No Idóneo", "Pérdida", "Estable")),
            col.legend = tm_legend(title = "Clasificación de Cambios",
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


# ACTUAL VS SSP5

mapa_act_ssp5 <- tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
  tm_shape(Colombia) +
  tm_fill(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_shape(mapa_cambio_ssp5) +
  tm_raster(col.scale = tm_scale_categorical(values = c("lightblue", "#E34A33", "green4"),
                                             labels = c("No Idóneo", "Pérdida", "Estable")),
            col.legend = tm_legend(title = "Clasificación de Cambios",
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



tmap_arrange(Mapa_act_ssp1, mapa_act_ssp5)


# Paso 4: Guardar los mapas combinados en un archivo
tmap_save(tmap_arrange(Mapa_act_ssp1, mapa_act_ssp5), 
          filename = here("..", "..", "MAPAS", "cambio_idoneidad_mapas.png"),
          width = 14,
          height = 11,
          dpi = 1000)









# SOLAPAMIENTO DE NICHO G-ESPACIAL

# actualidad

r_actual_filtrado <- classify(raster_actual, cbind(-Inf, 0.564, 0))

r_actual_norm <- r_actual_filtrado / global(r_actual_filtrado, "sum", na.rm=TRUE)[1,1]


# SSP1

r_OPTIMISTA_filtrado <- classify(raster_ssp1, cbind(-Inf, 0.564, 0))

r_OPTIMISTA_norm <- r_OPTIMISTA_filtrado / global(r_OPTIMISTA_filtrado, "sum", na.rm=TRUE)[1,1]

# SSP5

r_PESIMISTA_filtrado <- classify(raster_ssp5, cbind(-Inf, 0.564, 0))

r_PESIMISTA_norm <- r_PESIMISTA_filtrado / global(r_PESIMISTA_filtrado, "sum", na.rm=TRUE)[1,1]


nicheSimilarity(r_actual_norm, r_OPTIMISTA_norm)
nicheSimilarity(r_actual_norm, r_PESIMISTA_norm)
nicheSimilarity(r_OPTIMISTA_norm, r_PESIMISTA_norm)




