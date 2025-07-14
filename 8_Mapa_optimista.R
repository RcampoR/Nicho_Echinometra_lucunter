library(terra)
library(tmap)
library(tidyverse)
library(geodata)
library(here)


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
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.2426687, 0.4, 0.6, 0.8, 1),
                                 labels = c("< 0.243 (No presencia)", 
                                            "0.243 a 0.4",
                                            "0.4 a 0.6",
                                            "0.6 a 0.8",
                                            "0.8 a 1")),
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


# Definir el umbral
umbral <- 0.2426687

# Crear un raster binario: 1 si es idóneo, 0 si no
raster_binario <- classify(raster_optimista, matrix(c(-Inf, umbral, 0, umbral, Inf, 1), ncol = 3, byrow = TRUE))

# Si el raster tiene un CRS proyectado (e.g., UTM), `cellSize` devolverá el área en las unidades del CRS (e.g., m^2).
area_celda <- cellSize(raster_optimista, unit = "m")

# Multiplicar el raster binario por el área de la celda para obtener el área idónea de cada celda
area_idonea_por_celda <- raster_binario * area_celda

# Sumar todas las áreas de las celdas idóneas para obtener el área total
area_total_idonea_m2 <- global(area_idonea_por_celda, "sum", na.rm = TRUE)

# Convertir a kilómetros cuadrados para mayor legibilidad
area_total_idonea_km2 <- area_total_idonea_m2 / 1e6

#MAR CARIBE

Caribe_por_pais <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp"))

# Lista para almacenar los resultados
resultados_area_por_pais <- list()

# Obtener los nombres únicos de los países de tu SpatVector Caribe_por_pais
# **Ajusta "NAME" por el nombre real de la columna en tu SHP que identifica el país.**
paises_en_caribe <- unique(Caribe_por_pais$SOVEREIGN1)

# Bucle para iterar sobre cada país y calcular el área
for (pais in paises_en_caribe) {
  cat("Calculando para:", pais, "...\n")
  
  # Seleccionar el polígono correspondiente al país actual
  limite_pais <- Caribe_por_pais[Caribe_por_pais$SOVEREIGN1 == pais, ]
  
  # Recortar el raster de áreas idóneas a la zona marítima del país
  raster_idonea_pais <- crop(area_idonea_por_celda, limite_pais)
  raster_idonea_pais <- mask(raster_idonea_pais, limite_pais) # Asegura que solo se consideren las celdas dentro del límite
  
  # Sumar las áreas de las celdas idóneas dentro del límite del país
  area_m2_pais <- global(raster_idonea_pais, "sum", na.rm = TRUE)
  
  # Convertir a kilómetros cuadrados
  area_km2_pais <- area_m2_pais / 1e6
  
  # Almacenar el resultado
  resultados_area_por_pais[[pais]] <- round(area_km2_pais$sum, 2)
}

# Imprimir los resultados para cada país
cat("\n--- Área Idónea por País ---\n")
for (pais in names(resultados_area_por_pais)) {
  cat("El área idónea para", pais, "es de:", resultados_area_por_pais[[pais]], "km².\n")
}


