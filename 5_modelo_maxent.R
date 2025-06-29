library(terra)
library(dplyr)
library(ENMeval)
library(rJava)
library(dismo)
library(raster)
library(tmap)

# limpiar entorno 
rm(list = ls())
gc()


# CARGAR VARIABLES

pack_variables_base <- "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_50m"

# Cargando las 9 variables con sus nombres largo

clorofila_media <- rast(file.path(pack_variables_base, "clorofila_media.tif"))
velocidad_corriente_media <- rast(file.path(pack_variables_base, "velocidad_corriente_media.tif"))
ph_rango <- rast(file.path(pack_variables_base, "ph_rango.tif"))
batimetria <- rast(file.path(pack_variables_base, "batimetria.tif"))
distancia_costa <- rast(file.path(pack_variables_base, "distancia_costa.tif"))
concavidad <- rast(file.path(pack_variables_base, "concavidad.tif"))
salinidad_rango <- rast(file.path(pack_variables_base, "salinidad_rango.tif"))
temperatura_rango <- rast(file.path(pack_variables_base, "temperatura_rango.tif"))

variables_raster <- c(
  clorofila_media,
  velocidad_corriente_media,
  ph_rango,
  batimetria,
  distancia_costa,
  concavidad,
  salinidad_rango,
  temperatura_rango)


# CARGAR OCURRENCIA

ocurrencias_E_lucunter <- readr::read_delim("BD_E_lucunter_submuestreado_Caribe.csv") %>% 
                          transmute(lon = decimalLongitude,
                                    lat = decimalLatitude) %>% 
  as.data.frame()


                          
class(ocurrencias_E_lucunter)


# CARGAR PUNTOS DE FONDO

puntos_fondo_submuestreados <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\puntos_fondo\\pf_crudos_caribe_submuestreados.shp")

puntos_fondo_df <- as.data.frame(geom(puntos_fondo_submuestreados)) %>%
  dplyr::select(x, y) %>%
  dplyr::rename(lon = x, lat = y) %>% 
  as.data.frame()

#RUTA ENMEVAL 

options(ENMeval.maxent.jar = "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\maxent_software\\maxent.jar")

# ---  Realizar la evaluación y optimización con ENMeval ---

# Definir las combinaciones de Feature Classes (FC) y Regularization Multipliers (RM) a probar
# Es buena práctica empezar con un rango razonable.
# La elección de FCs puede depender de la cantidad de tus puntos de presencia.
# Si tienes pocos datos (< 50-100), podrías mantener las FCs más simples (L, LQ, H).
# Si tienes muchos (>200), puedes explorar más complejas (LQHPT).

ENMeval_FCs <- c("L", "LQ", "H", "LQH") # Considera tu número de puntos de presencia 

ENMeval_RMs <- seq(1.0, 5.0, by = 0.5) # Puedes ajustar este rango


# Ejecutar ENMeval con validación cruzada espacial
message("\nIniciando la evaluación de hiperparámetros con ENMeval (versión 1.x.x). Esto puede tomar tiempo...")

eval_results <- ENMeval::ENMevaluate(
  occs = ocurrencias_E_lucunter[, c("lon", "lat")],
  envs = variables_raster,
  bg = puntos_fondo_df,
  # Argumentos para la optimización de hiperparámetros
  tune.args = list(fc = ENMeval_FCs, rm = ENMeval_RMs),
  # Método de partición
  partitions = "randomkfold",
  partition.settings = list(kfolds = 5), # kfolds para randomkfold
  algorithm = "maxent.jar",
  categoricals = NULL,
  # Argumentos para MaxEnt.jar, dentro de other.settings
  other.settings = list(
    "outputformat=logistic",
    "betamultiplier=1",
    doClamp = TRUE, # doClamp también puede ir aquí, si no es un argumento directo de ENMevaluate
    other.args = c("jackknife=TRUE", "responsecurves=TRUE")
  ),
  parallel = TRUE,
  numCores = parallel::detectCores() - 1,
  # updateProgress (o progbar) no existe en esta versión, lo quitamos
  # quiet = FALSE # Puedes poner TRUE si quieres silenciar mensajes de la función
)

message("Evaluación de hiperparámetros completada.")
Sys.sleep(2) # Pausa para asegurar que el mensaje sea visible



# --- 6. ANALIZAR LOS RESULTADOS DE LA EVALUACIÓN ---

# Convertir los resultados a un data.frame para un análisis más fácil
eval_df <- eval_results@results

eval_df[eval_df$tune.args == "fc.LQ_rm.1", ]

# GUARDAR TODOS LOS MODELOS ENTRENADOS EN ENMEVALS (HIPERPARAMETROS)

saveRDS(eval_results, "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Modelos_Entrenados\\GENERALES\\ENMeval_TODOS_actuales_caribe.rds")


# Definir los parámetros óptimos seleccionados explícitamente

best_fc <- "LQ"
best_rm <- 1

#DISMO ACEPTA SOLO RASTER DE Raster

variables_raster <- brick(variables_raster)


# ENTRENAR MODELO FINAL fc.LQ_rm.1

Modelo_LQ_rm_1 <- maxent(x = variables_raster, 
                         p = ocurrencias_E_lucunter[, c("lon", "lat")],
                         a = puntos_fondo_df, 
                         args = c(paste0("betamultiplier=", best_rm),
                                  "outputformat=logistic",
                                  "responsecurves=TRUE",
                                  "jackknife=TRUE",
                                  "doclamp=TRUE",
                                  "linear=true",    
                                  "quadratic=true", # FALSE O TRUE???
                                  "hinge=false",     
                                  "product=false",   
                                  "threshold=false")) 
                                                                      

# PREDICCIONES

Raster_idoneidad <- predict(variables_raster, Modelo_LQ_rm_1, type = "logistic")

tmap_mode("view")

tm_shape(Raster_idoneidad) +
  tm_raster()


# guardar raster_idoneidad

writeRaster(Raster_idoneidad, filename = "Raster_idoneidad_caribe.tif")

# guardar modelo final

saveRDS(Modelo_LQ_rm_1, "Modelo_LQ_rm_1.rds")

