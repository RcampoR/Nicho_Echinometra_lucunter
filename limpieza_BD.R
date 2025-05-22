library(tidyverse)
library(terra)
library(geodata)

rm(list = ls())
#SE CARGAN LOS DATOS

Base_Original <- read.delim("E_lucunter_mundo.csv")

# SE EXPLORAN LOS DATOS

### reviso si hay una unica especie

Base_Original %>% 
  group_by(species) %>% 
  summarise(n())

### revisar si hay registros de presencia y ausencia en colombia

Base_Original %>% 
  filter(countryCode == "CO") %>% 
  group_by(occurrenceStatus) %>% 
  summarise(n())


#ENCONTRAR DATO ERRONEO
Base_Colombia %>% 
  filter(decimalLongitude <= -74 & decimalLongitude >= -75.3) %>% 
  filter(decimalLatitude >= 9 & decimalLatitude <= 10) %>% 
  select(decimalLongitude, decimalLatitude)

### Eliminar coordenadas identicas, eliminar NA, eliminar irregularidades, fechas a partir del 2000...


Base_Colombia <- Base_Original %>% 
  filter(countryCode == "CO" & locality != "Bahía Málaga, Isla Palma") %>% 
  mutate(longitud = decimalLongitude,
         latitud = decimalLatitude) %>% 
  filter(!is.na(longitud) & !is.na(latitud)) %>% 
  distinct(longitud, latitud, .keep_all = TRUE) %>% 
  filter(year >= "2000" & !is.na(year)) %>% 
  filter(!longitud == "-74.8112" & !latitud == "9.3829")



### revisar fechas

Base_Colombia %>% 
  group_by(eventDate) %>% 
  summarise(n())

### guardar tabla

write.csv(Base_Colombia, "DB_E_lucunter_CO_limpia.csv")
