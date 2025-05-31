library(terra)
library(tidyverse)
library(corrplot)
library(car)

rm(list = ls())


# PREPARANDO ARCHIVOS

# VARIABLES BIO-ORACLE (usan .nc y _FillValue=-9999.9)
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")
salinidad_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\salinidad_media.nc")
temp_media <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_media.nc")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\velocidad_corriente_media.nc")
pH_medio <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\pH_medio.nc")

# VARIABLES MARSPEC (usan .adf y tienen NaN como no data)
batimetria <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\bathymetry_30s\\bathymetry_30s\\bathy_30s\\hdr.adf")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo05_30s\\hdr.adf")

# VECTORES COLOMBIA

Mar_caribe_INVEMAR <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\COL_shp\\mar_caribe.json") 


# Valor NA en bio-oracle

NO_DATA_VALUE_BIOORACLE <- -9999.9

# Primero verificar el rango de valores

batimetria


#CAMBIAR VALORES -999.9

clorofila[clorofila == NO_DATA_VALUE_BIOORACLE] <- NA
salinidad_media[salinidad_media == NO_DATA_VALUE_BIOORACLE] <- NA
temp_media[temp_media == NO_DATA_VALUE_BIOORACLE] <- NA
velocidad_corriente[velocidad_corriente == NO_DATA_VALUE_BIOORACLE] <- NA
pH_medio[pH_medio == NO_DATA_VALUE_BIOORACLE] <- NA


#ASIGNAR CRS

#ASIGNAR CRS A BIO-ORACLE

Var_Bio_oracle <- c(clorofila,
                    salinidad_media,                
                    temp_media,                     
                    velocidad_corriente,
                    pH_medio)


#cambiar crs de bio-oracle a EPSG:4326

if(crs(Var_Bio_oracle) == "") {
  crs(Var_Bio_oracle) <- "EPSG:4326"
}

# CAMBIAR NOMBRES A ALGO MÁS SIMPLE
# Opción 1: Nombres simples
nombres_simples <- c("clorofila", 
                     "salinidad", 
                     "temperatura",
                     "velocidad_corriente",
                     "pH")

names(Var_Bio_oracle) <- nombres_simples

Var_Bio_oracle$clorofila


# revisar extencion de las capas
ext(Var_Bio_oracle)
ext(batimetria)

# solucionar con remuestreo bilinear para variables continuas

if(ext(Var_Bio_oracle) != ext(batimetria)) {
  Var_Bio_oracle_remues <- resample(Var_Bio_oracle, batimetria, method = "bilinear")
  print("Remuestreo completado para todas las capas BIO-ORACLE")
}


variables_raster <- c(Var_Bio_oracle_remues$clorofila,
                      Var_Bio_oracle_remues$salinidad,                
                      Var_Bio_oracle_remues$temperatura,                     
                      Var_Bio_oracle_remues$velocidad_corriente,
                      Var_Bio_oracle_remues$pH,
                      batimetria,
                      distancia_costa)


# cambiar nombres
nombres_capas <- c("clorofila",
                   "salinidad",
                   "temperatura",
                   "velocidad_corriente",
                   "pH",
                   "batimetria",
                   "distancia_costa") 

names(variables_raster) <- nombres_capas

#REPROYECTAR MAR CARIBE EN BASE A LAS VARIABLES

Mar_caribe_INVEMAR <- project(Mar_caribe_INVEMAR, crs(variables_raster))

#RECORTAR CAPAS A TAMAÑO DEL CARIBE COLOMBIANO

 # cortar rasters con base al vector del caribe colombiano

variables_caribe_COL <- crop(variables_raster, ext(Mar_caribe_INVEMAR))

plot(variables_caribe_COL$temperatura)


# enmascarar con la capa del mar caribe

variables_enmascaradas_INVEMAR <- mask(variables_caribe_COL, Mar_caribe_INVEMAR)

# mascara de 70 metros batimetria 


# mascara batimetria

batimetria <- classify(variables_enmascaradas_INVEMAR$batimetria, cbind(-Inf, -50, NA), right = FALSE)

plot(batimetria)

# enmascarar 

variables_a_enmascarar <- c(variables_enmascaradas_INVEMAR$clorofila, 
                            variables_enmascaradas_INVEMAR$salinidad, 
                            variables_enmascaradas_INVEMAR$temperatura, 
                            variables_enmascaradas_INVEMAR$velocidad_corriente, 
                            variables_enmascaradas_INVEMAR$pH, 
                            variables_enmascaradas_INVEMAR$distancia_costa)

variables_enmascaradas_50m <- mask(variables_a_enmascarar, batimetria)

plot(variables_enmascaradas_50m)

#concatenar variables ya procesadas

variables_limpias <- c(variables_enmascaradas_50m$clorofila, 
                       variables_enmascaradas_50m$salinidad, 
                       variables_enmascaradas_50m$temperatura, 
                       variables_enmascaradas_50m$velocidad_corriente, 
                       variables_enmascaradas_50m$pH,
                       batimetria,
                       variables_enmascaradas_50m$distancia_costa)

#verificar nuevamente
plot(variables_limpias)

#volver una lista, para iterar
variables_caribe_COL_50m <- as.list(variables_limpias)
nombres_capas <- as.list(nombres_capas)

#GUARDAR CAPAS YA PROCESADAS PARA COLOMBIA

# Crear carpeta donde guardar los archivos
dir.create("BIO_MARS_limpias_caribe_COL_50m", showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_caribe_COL_50m)) {
  writeRaster(
    variables_caribe_COL_50m[[i]],
    filename = file.path("BIO_MARS_limpias_caribe_COL_50m", paste0(nombres_capas[[i]], ".tif")),
    overwrite = TRUE
  )
}

