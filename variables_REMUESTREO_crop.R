library(terra)
rm(list = ls())


file.choose()


#VARIABLES BIO-ORACLE
Clorofila_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")

#salinidad
Salinidad_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\salinidad_media.nc")

Salinidad_range <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\salinidad_rango.nc")

#temperatura
Temp_media <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_media.nc")

Temp_rango <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_rango.nc")

#corriente
Velocidad_corriente_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\velocidad_corriente_media.nc")

Direccion_corriente_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\direccion_corriente_media.nc")

#oxigeno disuelto
Oxigeno_disuelto_medio <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\oxigeno_disuelto_medio.nc")

#pH
pH_medio <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\pH_medio.nc")

pH_rango <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\pH_rango.nc")

#Productividad primaria
Productividad_primaria_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\productividad_primaria_media.nc")

#VARIABLES MARSPEC
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



# VERIFICAR DATOS DE ALGUNAS VARIABLES
print(marspec_batimetria)

print(Temp_media)

#ASIGNAR CRS A BIO-ORACLE

Var_Bio_oracle <- c(Clorofila_media,
                    Salinidad_media,                
                    Salinidad_range,                 
                    Temp_media,                     
                    Temp_rango,                      
                    Velocidad_corriente_media,
                    Direccion_corriente_media,
                    pH_medio,                        
                    pH_rango,                      
                    Productividad_primaria_media,
                    Oxigeno_disuelto_medio)


#cambiar crs de bio-oracle a EPSG:4326

if(crs(Var_Bio_oracle) == "") {
  crs(Var_Bio_oracle) <- "EPSG:4326"
}

Var_Bio_oracle$thetao_mean
Var_Bio_oracle

crs(Var_Bio_oracle)

# CAMBIAR NOMBRES A ALGO MÁS SIMPLE
# Opción 1: Nombres simples
nombres_simples <- c("clorofila", "salinidad_media", "salinidad_rango", 
                     "temp_media", "temp_rango",
                     "velocidad_corriente", "direccion_corriente",
                     "pH_medio", "pH_rango", 
                     "productividad_primaria", "oxigeno_disuelto")

names(Var_Bio_oracle) <- nombres_simples

Var_Bio_oracle$clorofila

################ Método robusto para tratar valores de punto flotante como NA ##############
# Usando rangos específicos para capturar imprecisiones de punto flotante

# Clorofila
Var_Bio_oracle$clorofila[Var_Bio_oracle$clorofila < -9999.8 & 
                           Var_Bio_oracle$clorofila > -10000.2] <- NA

# Salinidad
Var_Bio_oracle$salinidad_media[Var_Bio_oracle$salinidad_media < -9999.8 & 
                                 Var_Bio_oracle$salinidad_media > -10000.2] <- NA

Var_Bio_oracle$salinidad_rango[Var_Bio_oracle$salinidad_rango < -9999.8 & 
                                 Var_Bio_oracle$salinidad_rango > -10000.2] <- NA

# Temperatura  
Var_Bio_oracle$temp_media[Var_Bio_oracle$temp_media < -9999.8 & 
                            Var_Bio_oracle$temp_media > -10000.2] <- NA

Var_Bio_oracle$temp_rango[Var_Bio_oracle$temp_rango < -9999.8 & 
                            Var_Bio_oracle$temp_rango > -10000.2] <- NA

# Corrientes
Var_Bio_oracle$velocidad_corriente[Var_Bio_oracle$velocidad_corriente < -9999.8 & 
                                     Var_Bio_oracle$velocidad_corriente > -10000.2] <- NA

Var_Bio_oracle$direccion_corriente[Var_Bio_oracle$direccion_corriente < -9999.8 & 
                                     Var_Bio_oracle$direccion_corriente > -10000.2] <- NA

# pH
Var_Bio_oracle$pH_medio[Var_Bio_oracle$pH_medio < -9999.8 & 
                          Var_Bio_oracle$pH_medio > -10000.2] <- NA

Var_Bio_oracle$pH_rango[Var_Bio_oracle$pH_rango < -9999.8 & 
                          Var_Bio_oracle$pH_rango > -10000.2] <- NA

# Productividad y oxígeno
Var_Bio_oracle$productividad_primaria[Var_Bio_oracle$productividad_primaria < -9999.8 & 
                                        Var_Bio_oracle$productividad_primaria > -10000.2] <- NA

Var_Bio_oracle$oxigeno_disuelto[Var_Bio_oracle$oxigeno_disuelto < -9999.8 & 
                                  Var_Bio_oracle$oxigeno_disuelto > -10000.2] <- NA

#######################################################################################
#Remuestreo de capas BIO-ORACLE

# revisar extencion de las capas
ext(Var_Bio_oracle)
ext(marspec_batimetria)

# solucionar con remuestreo bilinear para variables continuas

if(ext(Var_Bio_oracle) != ext(marspec_batimetria)) {
  Var_Bio_oracle_remues <- resample(Var_Bio_oracle, marspec_batimetria, method = "bilinear")
  print("Remuestreo completado para todas las capas BIO-ORACLE")
}


Var_Bio_oracle_remues
Var_Bio_oracle_remues$direccion_corriente


# verificando que todo este bien

plot(Temp_media)
plot(Var_Bio_oracle$temp_media)
plot(Var_Bio_oracle_remues$temp_media)

ext(Temp_media)
ext(Var_Bio_oracle$temp_media)
ext(Var_Bio_oracle_remues$temp_media)



variables_raster <- c(
  # Variables de Bio-ORACLE
  Var_Bio_oracle_remues$clorofila,                 
  Var_Bio_oracle_remues$salinidad_media,                
  Var_Bio_oracle_remues$salinidad_rango,                 
  Var_Bio_oracle_remues$temp_media,                     
  Var_Bio_oracle_remues$temp_rango,                      
  Var_Bio_oracle_remues$velocidad_corriente,
  Var_Bio_oracle_remues$direccion_corriente,
  Var_Bio_oracle_remues$pH_medio,                        
  Var_Bio_oracle_remues$pH_rango,                      
  Var_Bio_oracle_remues$productividad_primaria,
  Var_Bio_oracle_remues$oxigeno_disuelto,
  
  # Variables de MARSPEC (geofísicas/estructurales)
  marspec_batimetria,
  marspec_Biogeo_1, # Aspecto Este/Oeste
  marspec_Biogeo_2, # Aspecto Norte/Sur
  marspec_Biogeo_3, # Curvatura del plan
  marspec_Biogeo_4, # Perfil Curvatura
  marspec_Biogeo_5, # Distancia a la costa
  marspec_Biogeo_6, # Slope batimétrica
  marspec_Biogeo_7  # Concavidad

)


# cambiar nombres
nombres_capas <- c(
  "clorofila", "salinidad_media", "salinidad_rango", "temp_media", "temp_rango",
  "velocidad_corriente", "direccion_corriente", "pH_medio", "pH_rango",
  "productividad_primaria", "oxigeno_disuelto", "batimetria", "aspecto_EO",
  "aspecto_NS", "curvatura_plana", "curvatura_perfil", "distancia_costa",
  "pendiente_batimetrica", "concavidad"
)

names(variables_raster) <- nombres_capas



#RECORTAR CAPAS A TAMAÑO DE COLOMBIA

#cargar .shp de colombia

colombia_vector <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\COL_shp\\gadm36_COL_1.shp")
plot(colombia_vector)


# cortar rasters con base al vector de colombia

variables_COL <- crop(variables_raster, ext(colombia_vector))

plot(variables_COL$temp_media)


#volver una lista, para iterar
variables_COL <- as.list(variables_COL)
nombres_capas <- as.list(nombres_capas)

#GUARDAR CAPAS YA PROCESADAS PARA COLOMBIA

# Crear carpeta donde guardar los archivos
dir.create("BIO_MARS_remuestreados_COL", showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_COL)) {
  writeRaster(
    variables_COL[[i]],
    filename = file.path("BIO_MARS_remuestreados_COL", paste0(nombres_capas[[i]], ".tif")),
    overwrite = TRUE
  )
}

