library(terra)
rm(list = ls())


file.choose()

#BIO-ORACLE
Clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")

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

crs(Clorofila) <- "EPSG:4326"
crs(Velocidad_corriente) <- "EPSG:4326"



# Definir el raster de referencia
referencia_raster <- marspec_batimetria

# 2. Crear una lista de todos los rasters que deseas incluir
lista_de_rasters_originales <- list(
  marspec_batimetria = marspec_batimetria, # Damos nombres para el debugging
  Clorofila = Clorofila,
  Velocidad_corriente = Velocidad_corriente,
  marspec_Biogeo_1 = marspec_Biogeo_1,
  marspec_Biogeo_2 = marspec_Biogeo_2,
  marspec_Biogeo_3 = marspec_Biogeo_3,
  marspec_Biogeo_4 = marspec_Biogeo_4,
  marspec_Biogeo_5 = marspec_Biogeo_5,
  marspec_Biogeo_6 = marspec_Biogeo_6,
  marspec_Biogeo_7 = marspec_Biogeo_7,
  marspec_Biogeo_8 = marspec_Biogeo_8,
  marspec_Biogeo_9 = marspec_Biogeo_9,
  marspec_Biogeo_10 = marspec_Biogeo_10,
  marspec_Biogeo_11 = marspec_Biogeo_11,
  marspec_Biogeo_12 = marspec_Biogeo_12,
  marspec_Biogeo_13 = marspec_Biogeo_13,
  marspec_Biogeo_14 = marspec_Biogeo_14,
  marspec_Biogeo_15 = marspec_Biogeo_15,
  marspec_Biogeo_16 = marspec_Biogeo_16,
  marspec_Biogeo_17 = marspec_Biogeo_17
)

# 3. Nombres para las capas finales (para el stack final)
nombres_capas <- c("batimetria_marspec", "clorofila", "velocidad_corriente",
                   "biogeo01_EW_aspect", "biogeo02_NS_aspect", "biogeo03_plan_curvature", "biogeo04_profile_curvature",
                   "biogeo05_dist_to_shore", "biogeo06_bath_slope", "biogeo07_concavity",
                   "biogeo08_sss_mean", "biogeo09_sss_min", "biogeo10_sss_max",
                   "biogeo11_sss_range", "biogeo12_sss_variance",
                   "biogeo13_sst_mean", "biogeo14_sst_coldest", "biogeo15_sst_warmest",
                   "biogeo16_sst_range", "biogeo17_sst_variance")

# 4. Lista para almacenar los rasters ya alineados
aligned_rasters_list <- list()

# 5. Iterar sobre la lista de rasters originales, alineándolos individualmente
for (name in names(lista_de_rasters_originales)) {
  current_layer <- lista_de_rasters_originales[[name]]
  
  message(paste("Procesando capa:", name))
  
  # Paso A: Asegurarse de que el CRS sea el mismo que el de referencia
  if (crs(current_layer) != crs(referencia_raster)) {
    message(paste("  Reproyectando", name, "de", crs(current_layer), "a", crs(referencia_raster)))
    current_layer <- project(current_layer, crs(referencia_raster))
  }
  
  # Paso B: Re-muestrear para que coincida con la resolución y extensión de la referencia
  # Este es el paso que corrige "number of rows and/or columns do not match" al final
  message(paste("  Re-muestreando", name, "a la rejilla de referencia..."))
  aligned_layer <- resample(current_layer, referencia_raster, method = "bilinear")
  
  # Asignar el nombre original para mantener la trazabilidad
  names(aligned_layer) <- name # Esto es temporal, se renombrará al final
  
  # Añadir la capa alineada a la lista
  aligned_rasters_list[[name]] <- aligned_layer
}

# 6. Apilar todos los rasters YA ALINEADOS en un solo SpatRaster
# Esto debería funcionar sin el error "number of rows and/or columns do not match"
# porque todas las capas en aligned_rasters_list ahora tienen la misma rejilla.
variables_raster_aligned <- rast(aligned_rasters_list)

# 7. Asignar los nombres finales y limpios al stack
names(variables_raster_aligned) <- nombres_capas

cat("\nStack de variables creado exitosamente. Propiedades del stack final:\n")
print(variables_raster_aligned)

plot(Clorofila)
plot(variables_raster_aligned$clorofila)

plot(variables_raster_aligned$velocidad_corriente)
plot(Velocidad_corriente)

plot(marspec_Biogeo_15)
plot(variables_raster_aligned$biogeo15_sst_warmest)

plot(marspec_Biogeo_7)
plot(variables_raster_aligned$biogeo07_concavity)

plot(marspec_Biogeo_3)
plot(variables_raster_aligned$biogeo03_plan_curvature)

ext(marspec_Biogeo_12)
ext(marspec_Biogeo_14)
