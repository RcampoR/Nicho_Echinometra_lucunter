library(terra)
rm(list = ls())

#VAMOS AL SIGUIENTE PASO


# PREPARANDO ARCHIVOS

# ruta de la carpeta
ruta_variables <- "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_remuestreados_COL"

# lista de direcciones de los .tif
archivos_tif <- list.files(ruta_variables, pattern = "\\.tif$", full.names = TRUE)

# nombres para las variables
nombres <- tools::file_path_sans_ext(basename(archivos_tif))

# cargar archivos al entorno
for(i in 1:length(archivos_tif)) {
  assign(nombres[i], rast(archivos_tif[i]))
}

# Primero verificar el rango de valores

batimetria


# ENMASCARAR VARIABLES EN BASE A -50 METROS DE BATIMETRIA

# mascara batimetria

batimetria <- classify(batimetria, cbind(-Inf, -50, NA), right = FALSE)

plot(batimetria)

# Si hay alguna variable que NO deba ser enmascarada, deberás excluirla de esta lista.
variables_a_enmascarar <- setdiff(nombres, c("batimetria")) # Excluye la batimetría misma de la lista a enmascarar

# Iterar sobre las variables y aplicar la máscara
for(var_name in variables_a_enmascarar) {
  current_raster <- get(var_name) # Obtener el objeto raster por su nombre
  masked_raster <- mask(current_raster, batimetria) # Aplicar la máscara
  assign(var_name, masked_raster)  # Asignar el raster enmascarado a las variables ya existentes
  
  # Opcional: Si quieres crear una nueva variable con _masked
  # assign(paste0(var_name, "_masked"), masked_raster)
}

# verificar mascaras

plot(pH_medio)
plot(salinidad_rango)
plot(distancia_costa)
plot(temp_rango)
plot(temp_media)


variables_raster_mascara <- c(aspecto_EO, aspecto_NS, batimetria, clorofila, 
                              concavidad, curvatura_perfil, curvatura_plana, 
                              direccion_corriente, distancia_costa, oxigeno_disuelto, 
                              pendiente_batimetrica, pH_medio, pH_rango, 
                              productividad_primaria, salinidad_media, salinidad_rango, 
                              temp_media, temp_rango, velocidad_corriente)


#MUESTREO DE VALORES

# número de puntos a muestrear
n_puntos_muestrear <- 100000

set.seed(789) # Para reproducibilidad

#tomar muestra
valores_muestreados <- spatSample(variables_raster_mascara, n_puntos_muestrear, 
                                  "random", na.rm = TRUE, as.data.frame = TRUE)

# spatSample puede añadir una columna 'ID' o 'geometry' al inicio.
# Asegurémonos de que solo tenemos las columnas de las variables.
# Asume que la primera columna es de geometría/ID si hay más columnas que capas en el stack
if (ncol(valores_muestreados) > nlyr(variables_raster_mascara)) {
  valores_muestreados <- valores_muestreados[, -1] 
}

#CORRELACION CON DATA FRAME Y NO CON layercor() de terra para más velocidad 
correlation_matrix <- cor(valores_muestreados, use = "pairwise.complete.obs", method = "pearson")



