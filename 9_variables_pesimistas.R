library(terra) # raster y vectores
library(tidyverse) # # manipular datos y graficar
library(here) # control de direcciones
library(dismo) # evaluar modelo final
library(raster) # raster compatible con dismo
library(rJava) # java en R
library(tmap) # mapas tematicos 

#limpiar entorno
rm(list = ls())
gc()


# PREPARANDO ARCHIVOS

## Variables de BIO-ORACLE

# Directorio base para las variables de BIO-ORACLE
ruta_bio_oracle <- here("..", "..", "Variables_BIOORACLE", "bio_oracle_2040_pesimista")

# Clorofila
clorofila_media <- rast(here(ruta_bio_oracle, "clorofila_media_2040_pesimista.nc"))

# Salinidad
salinidad_rango <- rast(here(ruta_bio_oracle, "salinidad_rango_2040_pesimista.nc"))

# Temperatura
temperatura_rango <- rast(here(ruta_bio_oracle, "temperatura_rango_2040_pesimista.nc"))

# Corriente
velocidad_corriente_media <- rast(here(ruta_bio_oracle, "velocidad_corriente_media_2040_pesimista.nc"))


# pH
ph_rango <- rast(here(ruta_bio_oracle, "pH_rango_2040_pesimista.nc"))



## Variables de MARSPEC

# Directorio base para las variables de MARSPEC
# De nuevo, subimos dos niveles para llegar a Nicho_E_lucunter y luego bajamos a MARSPEC
ruta_marspec <- here("..", "..", "MARSPEC")

# Batimetría no cambia mucho en la proxima decada, así que se usara la actual de MARSPEC
marspec_batimetria <- rast(here(ruta_marspec, "bathymetry_30s", "bathymetry_30s", "bathy_30s", "hdr.adf"))

#CORREGIR NA en Bio-Oracle
# Valor NA en bio-oracle

NO_DATA_VALUE_BIOORACLE <- -9999.9

# Primero verificar el rango de valores

marspec_batimetria


#CAMBIAR VALORES -999.9

# Clorofila
clorofila_media[clorofila_media == NO_DATA_VALUE_BIOORACLE] <- NA

# Salinidad
salinidad_rango[salinidad_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Temperatura
temperatura_rango[temperatura_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Corriente
velocidad_corriente_media[velocidad_corriente_media == NO_DATA_VALUE_BIOORACLE] <- NA


# pH
ph_rango[ph_rango == NO_DATA_VALUE_BIOORACLE] <- NA 


#ASIGNAR CRS

#ASIGNAR CRS A BIO-ORACLE

Var_Bio_oracle <- c(
  clorofila_media,
  salinidad_rango,
  temperatura_rango,
  velocidad_corriente_media,
  ph_rango
)


#cambiar crs de bio-oracle a EPSG:4326

if(crs(Var_Bio_oracle) == "") {
  crs(Var_Bio_oracle) <- "EPSG:4326"
}


# revisar extencion de las capas
ext(Var_Bio_oracle)
ext(marspec_batimetria)

# solucionar con remuestreo bilinear para variables continuas

if(ext(Var_Bio_oracle) != ext(marspec_batimetria)) {
  Var_Bio_oracle_remues <- resample(Var_Bio_oracle, marspec_batimetria, method = "bilinear")
  print("Remuestreo completado para todas las capas BIO-ORACLE")
}


variables_raster <- c(
  Var_Bio_oracle_remues$chl_mean,
  Var_Bio_oracle_remues$so_range,
  Var_Bio_oracle_remues$thetao_range,
  Var_Bio_oracle_remues$sws_mean,
  Var_Bio_oracle_remues$ph_range,
  marspec_batimetria
)


# cambiar nombres
nombres_capas <- c( "clorofila_media",
                    "salinidad_rango",
                    "temperatura_rango",
                    "velocidad_corriente_media",
                    "ph_rango",
                    "batimetria") 

names(variables_raster) <- nombres_capas


#RECORTAR CAPAS A TAMAÑO DEL CARIBE cerca a colombia

vector_caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp"))

# Unir todos los polígonos en uno solo (disolverlos)
area_estudio_caribe <- aggregate(vector_caribe, dissolve = TRUE)

# cortar rasters con base al vector del caribe 

variables_caribe <- crop(variables_raster, ext(area_estudio_caribe)+0.01)

plot(variables_caribe)


# enmascarar con el vector del caribe

variables_enmascaradas_caribe <- mask(variables_caribe, area_estudio_caribe)
plot(variables_enmascaradas_caribe)

# mascara de 50 metros batimetria 


# mascara batimetria

batimetria <- classify(variables_enmascaradas_caribe$batimetria, cbind(-Inf, -50, NA), right = FALSE)

plot(batimetria)

# enmascarar
variables_a_enmascarar <- c(
  variables_enmascaradas_caribe$clorofila_media,
  variables_enmascaradas_caribe$salinidad_rango,
  variables_enmascaradas_caribe$temperatura_rango,
  variables_enmascaradas_caribe$velocidad_corriente_media,
  variables_enmascaradas_caribe$ph_rango
)

variables_enmascaradas_50m <- mask(variables_a_enmascarar, batimetria)

plot(variables_enmascaradas_50m)

#concatenar variables ya procesadas

variables_limpias <- c(
  variables_enmascaradas_50m$clorofila_media,      
  variables_enmascaradas_50m$salinidad_rango,
  variables_enmascaradas_50m$temperatura_rango,
  variables_enmascaradas_50m$velocidad_corriente_media,
  variables_enmascaradas_50m$ph_rango
)

#verificar nuevamente
plot(variables_limpias)

#volver una lista, para iterar
variables_caribe <- as.list(variables_limpias)
nombres_capas <- as.list(nombres_capas)

#GUARDAR CAPAS YA PROCESADAS PARA COLOMBIA

# Crear carpeta donde guardar los archivos
dir.create(here("..", "..", "BIO_MARS_limpias_caribe_50m", "variables_2040_pesimistas"), showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_caribe)) {
  writeRaster(
    variables_caribe[[i]],
    filename = here("..", "..", "BIO_MARS_limpias_caribe_50m", "variables_2040_pesimistas", paste0(nombres_capas[[i]], "_2040_pesimista.tif"))
  )
}


#LIMPIAR ENTORNO

rm(list = ls())
gc()


# CARGAR LAS VARIABLES OPTIMISTAS YA PROCESADAS


# CARGAR VARIABLES

pack_variables_marspec <- here("..", "..", "BIO_MARS_limpias_caribe_50m")

# Cargando las 8 variables con sus nombres largo

batimetria <- rast(here(pack_variables_marspec, "batimetria.tif"))
distancia_costa <- rast(here(pack_variables_marspec, "distancia_costa.tif"))
concavidad <- rast(here(pack_variables_marspec, "concavidad.tif"))


pack_variables_bio_oracle <- here("..", "..", "BIO_MARS_limpias_caribe_50m", "variables_2040_pesimistas")

clorofila_media <- rast(here(pack_variables_bio_oracle, "clorofila_media_2040_pesimista.tif"))
velocidad_corriente_media <- rast(here(pack_variables_bio_oracle, "velocidad_corriente_media_2040_pesimista.tif"))
ph_rango <- rast(here(pack_variables_bio_oracle, "ph_rango_2040_pesimista.tif")) 
salinidad_rango <- rast(here(pack_variables_bio_oracle, "salinidad_rango_2040_pesimista.tif"))
temperatura_rango <- rast(here(pack_variables_bio_oracle, "temperatura_rango_2040_pesimista.tif"))



# concatenar variables
variables_raster <- c(
  clorofila_media,
  velocidad_corriente_media,
  ph_rango,
  batimetria,
  distancia_costa,
  concavidad,
  salinidad_rango,
  temperatura_rango)

# pasar variables a raster

variables_raster <- brick(variables_raster)

#llamar modelo

Modelo_LQ_rm_1 <- readRDS(here("..", "..", "Modelos_Entrenados", "ESPECIFICOS", "3_Modelo_LQ_rm_1.rds"))


# PREDICCIONES

Raster_pesimista <- predict(variables_raster, Modelo_LQ_rm_1, type = "logistic")

tmap_mode("view")

tm_shape(Raster_pesimista) +
  tm_raster()


# guardar raster_idoneidad

writeRaster(Raster_pesimista, filename = here("..", "..", "MAPAS", "Raster_pesimista_caribe.tif"))


