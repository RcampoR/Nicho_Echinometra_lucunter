library(terra) # raster y vectores
library(tidyverse) # manipular datos y graficar
library(here) # control de direcciones


#limpiar entorno
rm(list = ls())



# PREPARANDO ARCHIVOS

## Variables de BIO-ORACLE

# Directorio base para las variables de BIO-ORACLE
ruta_bio_oracle <- here("..", "..", "Variables_BIOORACLE")

# Clorofila
clorofila_media <- rast(here(ruta_bio_oracle, "clorofila.nc"))

# Salinidad
salinidad_media <- rast(here(ruta_bio_oracle, "salinidad_media.nc"))
salinidad_rango <- rast(here(ruta_bio_oracle, "salinidad_rango.nc"))

# Temperatura
temperatura_media <- rast(here(ruta_bio_oracle, "temperatura_media.nc"))
temperatura_rango <- rast(here(ruta_bio_oracle, "temperatura_rango.nc"))

# Corriente
velocidad_corriente_media <- rast(here(ruta_bio_oracle, "velocidad_corriente_media.nc"))
direccion_corriente_media <- rast(here(ruta_bio_oracle, "direccion_corriente_media.nc"))

# Oxígeno disuelto
oxigeno_disuelto_medio <- rast(here(ruta_bio_oracle, "oxigeno_disuelto_medio.nc"))

# pH
ph_medio <- rast(here(ruta_bio_oracle, "pH_medio.nc"))
ph_rango <- rast(here(ruta_bio_oracle, "pH_rango.nc"))

# Productividad primaria
productividad_primaria_media <- rast(here(ruta_bio_oracle, "productividad_primaria_media.nc"))

  
  ## Variables de MARSPEC
  
  # Directorio base para las variables de MARSPEC
  # De nuevo, subimos dos niveles para llegar a Nicho_E_lucunter y luego bajamos a MARSPEC
  ruta_marspec <- here("..", "..", "MARSPEC")

## Variables de MARSPEC

# Batimetría
marspec_batimetria <- here(ruta_marspec, "bathymetry_30s", "bathymetry_30s", "bathy_30s", "hdr.adf") %>% rast()

# Biogeo 1: Aspecto Este/Oeste (sin(aspecto en radianes))
marspec_aspecto_este_oeste <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo01_30s", "hdr.adf") %>% rast()

# Biogeo 2: Aspecto Norte/Sur (cos(aspecto en radianes))
marspec_aspecto_norte_sur <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo02_30s", "hdr.adf") %>% rast()

# Biogeo 3: Curvatura del plan
marspec_curvatura_plan <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo03_30s", "hdr.adf") %>% rast()

# Biogeo 4: Perfil de curvatura
marspec_perfil_curvatura <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo04_30s", "hdr.adf") %>% rast()

# Biogeo 5: Distancia a la costa
marspec_distancia_costa <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo05_30s", "hdr.adf") %>% rast()

# Biogeo 6: Pendiente batimétrica (Slope)
marspec_pendiente_batimetrica <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo06_30s", "hdr.adf") %>% rast()

# Biogeo 7: Concavidad
marspec_concavidad <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo07_30s", "hdr.adf") %>% rast()

#CORREGIR NA en Bio-Oracle
# Valor NA en bio-oracle

NO_DATA_VALUE_BIOORACLE <- -9999.9

# Primero verificar el rango de valores

marspec_batimetria


#CAMBIAR VALORES -999.9

# Clorofila
clorofila_media[clorofila_media == NO_DATA_VALUE_BIOORACLE] <- NA

# Salinidad
salinidad_media[salinidad_media == NO_DATA_VALUE_BIOORACLE] <- NA
salinidad_rango[salinidad_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Temperatura
temperatura_media[temperatura_media == NO_DATA_VALUE_BIOORACLE] <- NA
temperatura_rango[temperatura_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Corriente
velocidad_corriente_media[velocidad_corriente_media == NO_DATA_VALUE_BIOORACLE] <- NA
direccion_corriente_media[direccion_corriente_media == NO_DATA_VALUE_BIOORACLE] <- NA

# Oxígeno disuelto
oxigeno_disuelto_medio[oxigeno_disuelto_medio == NO_DATA_VALUE_BIOORACLE] <- NA

# pH
ph_medio[ph_medio == NO_DATA_VALUE_BIOORACLE] <- NA
ph_rango[ph_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Productividad primaria
productividad_primaria_media[productividad_primaria_media == NO_DATA_VALUE_BIOORACLE] <- NA




#ASIGNAR CRS

#ASIGNAR CRS A BIO-ORACLE

Var_Bio_oracle <- c(
  clorofila_media,
  salinidad_media,
  salinidad_rango,
  temperatura_media,
  temperatura_rango,
  velocidad_corriente_media,
  direccion_corriente_media,
  oxigeno_disuelto_medio,
  ph_medio,
  ph_rango,
  productividad_primaria_media
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
  Var_Bio_oracle_remues$so_mean,
  Var_Bio_oracle_remues$so_range,
  Var_Bio_oracle_remues$thetao_mean,
  Var_Bio_oracle_remues$thetao_range,
  Var_Bio_oracle_remues$sws_mean,
  Var_Bio_oracle_remues$swd_mean,
  Var_Bio_oracle_remues$o2_mean,
  Var_Bio_oracle_remues$ph_mean,
  Var_Bio_oracle_remues$ph_range,
  Var_Bio_oracle_remues$phyc_mean,
  marspec_batimetria,
  marspec_aspecto_este_oeste,
  marspec_aspecto_norte_sur,
  marspec_curvatura_plan,
  marspec_perfil_curvatura,
  marspec_distancia_costa,
  marspec_pendiente_batimetrica,
  marspec_concavidad
  )


# cambiar nombres
nombres_capas <- c( "clorofila_media",
                    "salinidad_media",
                    "salinidad_rango",
                    "temperatura_media",
                    "temperatura_rango",
                    "velocidad_corriente_media",
                    "direccion_corriente_media",
                    "oxigeno_disuelto_medio",
                    "ph_medio",
                    "ph_rango",
                    "productividad_primaria_media",
                    "batimetria",
                    "aspecto_este_oeste",
                    "aspecto_norte_sur",
                    "curvatura_plan",
                    "perfil_curvatura",
                    "distancia_costa",
                    "pendiente_batimetrica",
                    "concavidad") 

names(variables_raster) <- nombres_capas


#RECORTAR CAPAS A TAMAÑO DEL CARIBE cerca a colombia

vector_caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp"))

# Unir todos los polígonos en uno solo (disolverlos)
area_estudio_caribe <- aggregate(vector_caribe, dissolve = TRUE)

 # cortar rasters con base al vector del caribe 

variables_caribe <- crop(variables_raster, ext(area_estudio_caribe)+0.01)

plot(variables_caribe$temperatura_media)


# enmascarar con el vector del caribe

variables_enmascaradas_caribe <- mask(variables_caribe, area_estudio_caribe)
plot(variables_enmascaradas_caribe)

#volver una lista, para iterar
variables_enmascaradas_caribe <- as.list(variables_enmascaradas_caribe)
nombres_capas <- as.list(nombres_capas)

#guardar variables enmascaradas caribe
# Crear carpeta donde guardar los archivos
dir.create(here("..", "..", "BIO_MARS_caribe_completas"), showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_enmascaradas_caribe)) {
  writeRaster(
    variables_enmascaradas_caribe[[i]],
    filename = here("..", "..", "BIO_MARS_caribe_completas", paste0(nombres_capas[[i]], ".tif")), overwrite = TRUE)
  
  
}




# mascara de 50 metros batimetria 


# mascara batimetria

batimetria <- classify(variables_enmascaradas_caribe$batimetria, cbind(-Inf, -50, NA), right = FALSE)

plot(batimetria)

# enmascarar
variables_a_enmascarar <- c(
  variables_enmascaradas_caribe$clorofila_media,
  variables_enmascaradas_caribe$salinidad_media,
  variables_enmascaradas_caribe$salinidad_rango,
  variables_enmascaradas_caribe$temperatura_media,
  variables_enmascaradas_caribe$temperatura_rango,
  variables_enmascaradas_caribe$velocidad_corriente_media,
  variables_enmascaradas_caribe$direccion_corriente_media,
  variables_enmascaradas_caribe$oxigeno_disuelto_medio,
  variables_enmascaradas_caribe$ph_medio,
  variables_enmascaradas_caribe$ph_rango,
  variables_enmascaradas_caribe$productividad_primaria_media,
  variables_enmascaradas_caribe$aspecto_este_oeste,
  variables_enmascaradas_caribe$aspecto_norte_sur,
  variables_enmascaradas_caribe$curvatura_plan,
  variables_enmascaradas_caribe$perfil_curvatura,
  variables_enmascaradas_caribe$distancia_costa,
  variables_enmascaradas_caribe$pendiente_batimetrica,
  variables_enmascaradas_caribe$concavidad
)

variables_enmascaradas_50m <- mask(variables_a_enmascarar, batimetria)

plot(variables_enmascaradas_50m)

#concatenar variables ya procesadas

variables_limpias <- c(
  variables_enmascaradas_50m$clorofila_media,      
  variables_enmascaradas_50m$salinidad_media,
  variables_enmascaradas_50m$salinidad_rango,
  variables_enmascaradas_50m$temperatura_media,
  variables_enmascaradas_50m$temperatura_rango,
  variables_enmascaradas_50m$velocidad_corriente_media,
  variables_enmascaradas_50m$direccion_corriente_media,
  variables_enmascaradas_50m$oxigeno_disuelto_medio,
  variables_enmascaradas_50m$ph_medio,
  variables_enmascaradas_50m$ph_rango,
  variables_enmascaradas_50m$productividad_primaria_media,
  batimetria, 
  variables_enmascaradas_50m$aspecto_este_oeste,
  variables_enmascaradas_50m$aspecto_norte_sur,
  variables_enmascaradas_50m$curvatura_plan,
  variables_enmascaradas_50m$perfil_curvatura,
  variables_enmascaradas_50m$distancia_costa,
  variables_enmascaradas_50m$pendiente_batimetrica,
  variables_enmascaradas_50m$concavidad
)

#verificar nuevamente
plot(variables_limpias)

#volver una lista, para iterar
variables_caribe <- as.list(variables_limpias)
nombres_capas <- as.list(nombres_capas)

#GUARDAR CAPAS YA PROCESADAS PARA COLOMBIA

# Crear carpeta donde guardar los archivos
dir.create(here("..", "..", "BIO_MARS_limpias_caribe_50m"), showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_caribe)) {
  writeRaster(
    variables_caribe[[i]],
    filename = here("..", "..", "BIO_MARS_limpias_caribe_50m", paste0(nombres_capas[[i]], ".tif")), overwrite = TRUE)
   
  
}



