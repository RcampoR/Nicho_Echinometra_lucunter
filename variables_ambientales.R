library(terra)
rm(list = ls())


file.choose()


Batimetria <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\batimetria.nc")

Clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")

Salinidad <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\salinidad_media.nc")

Temp_max <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_max.nc")

Temp_med <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_media.nc")

Temp_min <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_minima.nc")

Velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\velocidad_corriente.nc")



# Ruta base con todas las capas en subcarpetas

marspec_batimetria <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\bathymetry_30s\\bathymetry_30s\\bathy_30s\\hdr.adf")

#Aspecto Este/Oeste (sin(aspecto en radianes))
marspec_Biogeo_1 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo01_30s\\hdr.adf")

#Aspecto Norte/Sur (cos (cos(aspecto en radianes)))
marspec_Biogeo_2 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo02_30s\\hdr.adf")

#Curvatura del plan 
marspec_Biogeo_3 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo03_30s\\hdr.adf")

#Perfil Curvatura
marspec_Biogeo_4 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo04_30s\\hdr.adf")

#Distancia a la costa
marspec_Biogeo_5 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo05_30s\\hdr.adf")

#Slope bathymétrica
marspec_Biogeo_6 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo06_30s\\hdr.adf")

#concavidad
marspec_Biogeo_7 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo07_30s\\hdr.adf")

#SSS medio anual
marspec_Biogeo_8 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo08_30s\\hdr.adf")

#SSS mínimo mensual
marspec_Biogeo_9 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo09_30s\\hdr.adf")

#SSS Máximo Mensual
marspec_Biogeo_10 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo10_30s\\hdr.adf")

#Alcance anual en SSS
marspec_Biogeo_11 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo11_30s\\hdr.adf")

#Variancia anual en SSS
marspec_Biogeo_12 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo12_30s\\hdr.adf")

#SST anual medio

marspec_Biogeo_13 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo13_30s\\hdr.adf")

#SST del mes más frío sin hielo
marspec_Biogeo_14 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo14_30s\\hdr.adf")

# SST del mes más cálido sin hielo
marspec_Biogeo_15 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo15_30s\\hdr.adf")

#Coravisión anual en SST
marspec_Biogeo_16 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo16_30s\\hdr.adf")

#Variancia anual en SST
marspec_Biogeo_17 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo17_30s\\hdr.adf")


plot(Batimetria)
plot(marspec_batimetria)

res(marspec_batimetria)


¡Perfecto! Vamos a continuar el código incorporando todas las variables de MARSPEC hasta biogeo17 que mencionaste, además de Clorofila y Velocidad_corriente.

Primero, necesitamos asumir que ya tienes cargadas todas las variables individuales de MARSPEC. Basado en la tabla que mostraste en la imagen, las variables de biogeo01 a biogeo17 (incluyendo batymetry que sería marspec_batimetria), SSS y SST.

Asunciones (cargue estos archivos si aún no lo ha hecho):
  R

library(terra)
library(dplyr)
library(caret) # Para findCorrelation
library(corrplot) # Para visualizar la matriz de correlación

# Supongamos que ya tienes cargadas tus variables de BIO-ORACLE:
# Clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")
# Velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\velocidad_corriente.nc")

# Y que tienes cargadas tus variables de MARSPEC (basado en la imagen de la tabla):
# marspec_batimetria <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_batimetria.tif") # La de 0.008333333 grados
# marspec_Biogeo_1 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo01.tif")
# marspec_Biogeo_2 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo02.tif")
# marspec_Biogeo_3 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo03.tif")
# marspec_Biogeo_4 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo04.tif")
# marspec_Biogeo_5 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo05.tif")
# marspec_Biogeo_6 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo06.tif")
# marspec_Biogeo_7 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo07.tif")
# marspec_Biogeo_8 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo08.tif") # SSS Mean Annual
# marspec_Biogeo_9 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo09.tif") # SSS Min Monthly
# marspec_Biogeo_10 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo10.tif") # SSS Max Monthly
# marspec_Biogeo_11 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo11.tif") # SSS Annual Range
# marspec_Biogeo_12 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo12.tif") # SSS Annual Variance
# marspec_Biogeo_13 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo13.tif") # SST Mean Annual
# marspec_Biogeo_14 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo14.tif") # SST Coldest month
# marspec_Biogeo_15 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo15.tif") # SST Warmest month
# marspec_Biogeo_16 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo16.tif") # SST Annual Range
# marspec_Biogeo_17 <- rast("C:\\ruta\\a\\tus\\archivos\\marspec_biogeo17.tif") # SST Annual Variance

Código Continuado:
  R

# 1. Apilar todas las variables seleccionadas inicialmente
variables_raster <- c(marspec_batimetria,
                      Clorofila,
                      Velocidad_corriente,
                      marspec_Biogeo_1,
                      marspec_Biogeo_2,
                      marspec_Biogeo_3,
                      marspec_Biogeo_4,
                      marspec_Biogeo_5,  
                      marspec_Biogeo_6,
                      marspec_Biogeo_7,
                      marspec_Biogeo_8,  
                      marspec_Biogeo_9, 
                      marspec_Biogeo_10, 
                      marspec_Biogeo_11, 
                      marspec_Biogeo_12, 
                      marspec_Biogeo_13, 
                      marspec_Biogeo_14, 
                      marspec_Biogeo_15, 
                      marspec_Biogeo_16, 
                      marspec_Biogeo_17) 

crs(Clorofila) <- "EPSG:4326"
crs(Velocidad_corriente) <- "EPSG:4326"
