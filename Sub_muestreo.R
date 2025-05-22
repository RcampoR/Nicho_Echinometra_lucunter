

rm(list = ls())

# cargar Base de datos 
Base_Colombia <- read_delim("DB_E_lucunter_CO_limpia.csv")

## usaremos un vector de la libreria terra
vector_1 <- vect(Base_Colombia, geom = c("longitud", "latitud"), crs = 4326)

class(vector_1)


# consulta espacial de coincidencia

consulta_spac <- extract(vector_1, mundo)

# guardar la variable NAME-0

nombre_0 <- consulta_spac$NAME_0


#bBUSCANDO DISCREPANCIAS 

i <- which(is.na(nombre_0))
i
## integer(0)
j <- which(nombre_0 != vector_1$country)
# for the mismatches, bind the country names of the polygons and points
m <- cbind(nombre_0[j], vector_1$country[j])
colnames(m) <- c("polygons", "acaule")
m
##      polygons acaule


#CON TIDYVERSE
# Crear un dataframe de comparación
comparacion <- tibble(
  nombre_poligono = nombre_0,
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

res(raster_1) <- 0.018

# se expanden las celdas 

raster_1 <- extend(raster_1, ext(raster_1)+0.1)

set.seed(456)

raster_submuestreo <- spatSample(vector_1, size= 1, "random", strata=raster_1)



