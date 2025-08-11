

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
change_map_ssp1 <- (raster_binario_ACTUAL * 2) + raster_binario_SSP1

plot(change_map_ssp1)

# Reclasificar los valores del mapa para una mejor visualización

# Ausente (no idóneo en ninguno de los escenarios) -> 0
#ganancia (actual no idoneo, futuro si es idoneo) -> 1
# Pérdida (idóneo en Actual, no en SSP1) -> 2
# Estable (idóneo en ambos escenarios) -> 3


# Crear un mapa de cambio neto para los escenarios "Actual" vs. "SSP5"
change_map_ssp5 <- (raster_binario_ACTUAL * 2) + raster_binario_SSP5

plot(change_map_ssp5)





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




