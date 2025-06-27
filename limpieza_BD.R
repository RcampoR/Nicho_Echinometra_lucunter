library(tidyverse)
library(terra)
library(geodata)


#SE CARGAN LOS DATOS

Base_Original <- read.delim("E_lucunter_mundo.csv")

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


#ENCONTRAR DATO ERRONEO
Base_Original %>% 
  filter(decimalLongitude <= -74 & decimalLongitude >= -75.3) %>% 
  filter(decimalLatitude >= 9 & decimalLatitude <= 10) %>% 
  select(decimalLongitude, decimalLatitude)

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


### revisar fechas

Base_Caribe %>% 
  group_by(eventDate) %>% 
  summarise(n())

# revisar paises

Base_Caribe %>% 
  group_by(countryCode) %>% 
  summarise(n())


# Mapa Mundo
mundo <- world(path=".")
plot(mundo, xlim=c(-110,60), ylim=c(-80,40), col="light yellow", border="light gray")

# PUNTOS ECHINOMETRA LUCUNTER
points(Base_Caribe$longitud, Base_Caribe$latitud, col='red', pch=20)


### guardar tabla

write.csv(Base_Caribe, "DB_E_lucunter_Caribe_limpia.csv")






