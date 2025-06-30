library(tidyverse)
library(terra)

#limpiar entorno
rm(list = ls())
gc()

# cargar Base de datos 
Base_Caribe <- read_delim("DB_E_lucunter_Caribe_limpia.csv")

## usaremos un vector 
vector_1 <- vect(Base_Caribe, geom = c("longitud", "latitud"), crs = 4326)

class(vector_1)



# TRATANDO EL SESGO DE MUESTREO 

# se crea un raster con base al vector 
raster_1 <- rast(vector_1)

# se establece la resolución (se recomienda en base al home range)

res(raster_1) <- 0.009 # 1 km

# se expanden las celdas 

raster_1 <- extend(raster_1, ext(raster_1)+0.01)

set.seed(456)

vector_submuestreo <- spatSample(vector_1, size= 1, "random", strata=raster_1)


#GUARDAR COMO CSV LOS DATOS LIMPIOS

as.data.frame(vector_submuestreo) %>% 
write_csv(file = "BD_E_lucunter_submuestreado_Caribe.csv")
