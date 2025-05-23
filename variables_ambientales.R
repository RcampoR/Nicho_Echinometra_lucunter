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


M_batimetria <- rast()

# Ruta base con todas las capas en subcarpetas

marspec_batimetria <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\bathymetry_30s\\bathymetry_30s\\bathy_30s\\hdr.adf")

marspec_Biogeo_1 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo01_30s\\hdr.adf")

marspec_Biogeo_2 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo02_30s\\hdr.adf")

marspec_Biogeo_3 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo03_30s\\hdr.adf")

marspec_Biogeo_4 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo04_30s\\hdr.adf")

marspec_Biogeo_5 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo05_30s\\hdr.adf")

marspec_Biogeo_6 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo06_30s\\hdr.adf")

marspec_Biogeo_7 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo07_30s\\hdr.adf")

marspec_Biogeo_8 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo08_30s\\hdr.adf")

marspec_Biogeo_9 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo09_30s\\hdr.adf")

marspec_Biogeo_10 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo10_30s\\hdr.adf")

marspec_Biogeo_11 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo11_30s\\hdr.adf")

marspec_Biogeo_12 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo12_30s\\hdr.adf")

marspec_Biogeo_13 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo13_30s\\hdr.adf")

marspec_Biogeo_14 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo14_30s\\hdr.adf")

marspec_Biogeo_15 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo15_30s\\hdr.adf")

marspec_Biogeo_16 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo16_30s\\hdr.adf")

marspec_Biogeo_17 <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo08_17_30s\\biogeo08_17_30s\\biogeo17_30s\\hdr.adf")