library(terra) # raster y vectores
library(tidyverse) # manipular datos y graficar
library(corrplot) # graficas de correlación
library(car) # algunas estadisticas
library(here) # control de direcciones



#limpiar entorno
rm(list = ls())
gc()

# Define la ruta base a la carpeta
pack_variables_base <- here("..", "..", "BIO_MARS_limpias_caribe_50m")

# La lista completa de los nombres "limpios" que deberían ser los nombres de tus archivos .tif
nombres_capas_completos <- c(
  "clorofila_media",
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
  "concavidad"
)

# Cargar todas las variables usando un bucle y assign()
# Cada SpatRaster se creará en tu entorno global con el nombre correspondiente
for (nombre_variable in nombres_capas_completos) {
  ruta_archivo <- here("..", "..", "BIO_MARS_limpias_caribe_50m", paste0(nombre_variable, ".tif"))
  
  if (file.exists(ruta_archivo)) {
    assign(nombre_variable, rast(ruta_archivo), envir = .GlobalEnv)
    message(paste("Cargada:", nombre_variable))
  } else {
    warning(paste("Advertencia: El archivo", ruta_archivo, "no se encontró y no se cargó."))
  }
}


#BASE ECHINOMETRA SUBMUESTREADA
ocurrencias_E_lucunter <- readr::read_delim(here("BD_E_lucunter_submuestreado_Caribe.csv")) %>% 
  select(lon, lat) %>% 
  vect()



#CORRELACIÓN DE CAPAS
variables_raster <- c(
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
  productividad_primaria_media,
  batimetria,
  aspecto_este_oeste,
  aspecto_norte_sur,
  curvatura_plan,
  perfil_curvatura,
  distancia_costa,
  pendiente_batimetrica,
  concavidad
)
#correlacion de pearson con terra
correlacion_capas <- layerCor(variables_raster,"pearson", na.rm = TRUE)


#matriz de correlación
matriz_cor <- correlacion_capas$correlation

correlación_df <- as.data.frame(matriz_cor, row.names = row.names(matriz_cor))

# ilustrando matriz de corr
corrplot(matriz_cor,
         method = "circle", # "circle", "square", "ellipse", "number", "shade", "color", "pie"
         type = "upper",    # "upper", "lower", "full" - muestra solo la parte superior (sin duplicados)
         tl.col = "black",  # Color de las etiquetas de texto
         tl.srt = 45,       # Ángulo de las etiquetas (para que no se superpongan)
         diag = FALSE,      # No mostrar los valores de correlación de una variable consigo misma (siempre 1)
         col = COL2("RdBu", 200)) # Paleta de colores: RdBu para rojo-azul (negativo-positivo)

colnames(correlación_df)

#extraer valores altamente correlacionados
correlación_df %>% 
  rownames_to_column(var = "VARIABLES1") %>% 
  pivot_longer(cols = -VARIABLES1,
               values_to = "CORRELACION",
               names_to = "VARIABLES2") %>% 
  filter(CORRELACION >= 0.7 | CORRELACION <= -0.7) %>%
  filter(VARIABLES1 != VARIABLES2) %>% 
  view()


# eliminar variables altamente correlacionadas
# productividad primaria, salinidad media, oxigeno disuelto
# curvatura plano, pendiente batimetrica, perfil de curvatura

#VARIABLES RESTANTES

variables_restantes <- list(
  clorofila_media,
  salinidad_rango,
  temperatura_rango,
  temperatura_media,
  velocidad_corriente_media,
  direccion_corriente_media,
  ph_medio,
  ph_rango,
  batimetria,
  aspecto_este_oeste,
  aspecto_norte_sur,
  distancia_costa,
  concavidad
)


#calcular VIF  a las variables restantes

#haciendo el data.frame
variables_raster_para_vif <- c(
  clorofila_media,
  salinidad_rango,
  temperatura_rango,
  temperatura_media,
  velocidad_corriente_media,
  direccion_corriente_media,
  ph_medio,
  ph_rango,
  batimetria,
  aspecto_este_oeste,
  aspecto_norte_sur,
  distancia_costa,
  concavidad
)


variables_vif_df <- as.data.frame(variables_raster_para_vif)


#modelo lm arbitrario
modelo_vif_sencillo <- lm(batimetria ~ ., data = variables_vif_df)

summary(modelo_vif_sencillo)

# 5. Calcular y mostrar los VIFs
resultados_vif <- vif(modelo_vif_sencillo)

resultados_vif


#ELIMINANDO VARIABLES PROBLEMATICAS VIF >= 5 y mantener las ecologicamente relevantes

#calcular VIF  a las variables restantes

#haciendo el data.frame
variables_vif_validas <-  c(clorofila_media,
                            salinidad_rango,
                            temperatura_rango,
                            velocidad_corriente_media,
                            ph_rango,
                            batimetria,
                            distancia_costa,
                            concavidad)

variables_validas_df <- as.data.frame(variables_vif_validas)


#modelo lm arbitrario
modelo_vif_valido <- lm(batimetria ~ ., data = variables_validas_df)

summary(modelo_vif_valido)

# 5. Calcular y mostrar los VIFs
resultados_vif_valido <- vif(modelo_vif_valido)

resultados_vif_valido




#LIMPIAR ENTORNO 

rm(list = ls())
gc()

# cargar nuevamente las variables del caribe 50m

variables_limpias_caribe <- c(clorofila_media,
                              salinidad_rango,
                              temperatura_rango,
                              velocidad_corriente_media,
                              ph_rango,
                              batimetria,
                              distancia_costa,
                              concavidad)


#CREANDO PUNTOS DE FONDO
set.seed(456)
puntos_fondo_crudos <- spatSample(variables_limpias_caribe, 770,
                                  "random", na.rm = TRUE, as.points = TRUE)

# TRATANDO EL SESGO DE MUESTREO DEL FONDO

# se crea un raster con base en el vector 
raster_pf <- rast(puntos_fondo_crudos)

# se establece la resolución (se recomienda utilizar el home range de la especie)

res(raster_pf) <- 0.009 # 1 km

# se expanden las celdas 

raster_pf <- extend(raster_pf, ext(raster_pf)+0.01)

set.seed(456)


pf_submuestreados <- spatSample(puntos_fondo_crudos, size= 1, "random", strata=raster_pf)

# guardar capa vectorial PUNTOS FONDO
writeVector(pf_submuestreados, here("..", "..", "puntos_fondo", "pf_caribe_submuestreados.shp"), 
            overwrite = TRUE)


# ANALISIS DE COMPONENTES PRINCIPALES

# extraer valores de las variables con ocurrencias


valores_variables <- terra::extract(variables_limpias_caribe, ocurrencias_E_lucunter)

# borrar filas con NA

valores_variables %>% 
  filter(!is.na(clorofila_media) & !is.na(salinidad_rango) & 
           !is.na(temperatura_rango) & !is.na(velocidad_corriente_media) &
           !is.na(ph_rango) & !is.na(batimetria) & 
           !is.na(distancia_costa) & !is.na(concavidad)) -> valores_variables_limpios
 
  
