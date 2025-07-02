library(tidyverse)
library(here)



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








