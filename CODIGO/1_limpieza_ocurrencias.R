library(tidyverse)
library(here) 
library(terra) 


#SE CARGAN LOS DATOS

Base_Original <- read.delim(here("E_lucunter_mundo.csv"))

# SE EXPLORAN LOS DATOS

### reviso si hay una unica especie

Base_Original %>% 
  group_by(species) %>% 
  summarise(n())

### revisar si hay registros de presencia y ausencia en el caribe

Base_Original %>% 
  filter(countryCode %in% c("CO", "PA", "CR", "NI")) %>% 
  group_by(occurrenceStatus) %>% 
  summarise(n())


### Eliminar coordenadas identicas, eliminar NA, eliminar irregularidades, fechas a partir del 2000...


Base_Caribe <- Base_Original %>% 
  filter(
    countryCode %in% c("CO", "PA", "CR", "NI") 
    & 
      locality != "Bahía Málaga, Isla Palma"
  ) %>% 
  mutate(
    longitud = decimalLongitude,
    latitud = decimalLatitude
  ) %>% 
  filter(
    !is.na(longitud) & !is.na(latitud)
  ) %>% 
  distinct(
    longitud, latitud, .keep_all = TRUE
  ) %>% 
  filter(
    year >= "2000" & !is.na(year)
  ) %>% 
  filter(!longitud == "-74.8112" & !latitud == "9.3829")


# revisar paises

Base_Caribe %>% 
  group_by(countryCode) %>% 
  summarise(n())


### guardar tabla

write.csv(Base_Caribe, here("DB_E_lucunter_Caribe_limpia.csv")) 



#limpiar entorno
rm(list = ls())
gc()

# cargar Base de datos 
Base_Caribe <- read_delim(here("DB_E_lucunter_Caribe_limpia.csv"))

## usaremos un vector 
vector_1 <- vect(Base_Caribe, geom = c("longitud", "latitud"), crs = 4326)

class(vector_1)



# TRATANDO EL SESGO DE MUESTREO 

# se crea un raster con base al vector 
raster_1 <- rast(vector_1)

# se establece la resolución (se recomienda con base al home range)

res(raster_1) <- 0.009 # 1 km en el ecuador,  0 minutos, 32.4 segundos de arco

# se expanden las celdas 

raster_1 <- extend(raster_1, ext(raster_1)+0.01)

set.seed(456)

# Remuestreo de ocurrencias a solo 1 por cada 1km

vector_submuestreo <- spatSample(vector_1, size= 1, "random", strata=raster_1)


#GUARDAR COMO CSV LOS DATOS LIMPIOS

as.data.frame(vector_submuestreo) %>% 
write_csv(file = here("BD_E_lucunter_submuestreado_Caribe.csv"))
