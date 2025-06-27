library(tidyverse)
library(terra)
library(geodata)

#limpiar entorno
rm(list = ls())
gc()

# cargar Base de datos 
Base_Caribe <- read_delim("DB_E_lucunter_Caribe_limpia.csv")

## usaremos un vector de la libreria terra
vector_1 <- vect(Base_Caribe, geom = c("longitud", "latitud"), crs = 4326)

class(vector_1)


# consulta espacial de coincidencia

consulta_spac <- extract(vector_1, mundo)

# guardar la variable NAME-0

nombre_0 <- consulta_spac$NAME_0


#CON TIDYVERSE
# Crear un dataframe de comparación
comparacion <- tibble(
  nombre_poligono = vector_1$nombre_0,
  nombre_punto = vector_1$country
)

# Filtrar para encontrar discrepancias
discrepancias <- comparacion %>%
  filter(nombre_poligono != nombre_punto)

# Ver resultados
if(nrow(discrepancias) > 0) {
  discrepancias %>%
    rename(polygons = nombre_poligono, acaule = nombre_punto)
} else {
  print("No hay discrepancias entre los nombres")
}


# TRATANDO EL SESGO DE MUESTREO 

# se crea un raster en base al vector 
raster_1 <- rast(vector_1)

# se establece la resolución (se recomienda en base al home range)

res(raster_1) <- 0.009 # 1 km

# se expanden las celdas 

raster_1 <- extend(raster_1, ext(raster_1)+0.01)

set.seed(456)

vector_submuestreo <- spatSample(vector_1, size= 1, "random", strata=raster_1)

# MAPA SUBMUESTREO (solo correr si es necesario, muy exigente computacionalmente)

# Mostrar mapa submuestreo

mapa_sub <- as.polygons(raster_1)
plot(mapa_sub, border='gray')
points(vector_1)

# PUNTOS DE SUBMUESTREO

points(raster_submuestreo, cex=1, col='red', pch='x')


#GUARDAR COMO CSV LOS DATOS LIMPIOS

as.data.frame(vector_submuestreo) %>% 
write_csv(file = "BD_E_lucunter_submuestreado_Caribe.csv")
