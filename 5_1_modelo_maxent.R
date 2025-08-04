library(terra) # raster y vectores
library(dplyr) # manipulkación de datos
library(ENMeval) # evaluación de hiperparametros para MAXENT
library(rJava) # Java en R, para maxent
library(dismo) # Modelo MAXENT final
library(raster) # Raster compatible con dismo 
library(tmap) # Mapas tematicos
library(here) # control de direcciones
library(readr) # leer csv

# limpiar entorno 
rm(list = ls())
gc()


# CARGAR VARIABLES

pack_variables_base <- here("..", "..", "BIO_MARS_limpias_caribe_50m")

# Cargando las 9 variables con sus nombres largo

clorofila_media <- rast(here(pack_variables_base, "clorofila_media.tif"))
velocidad_corriente_media <- rast(here(pack_variables_base, "velocidad_corriente_media.tif"))
ph_rango <- rast(here(pack_variables_base, "ph_rango.tif"))
batimetria <- rast(here(pack_variables_base, "batimetria.tif"))
concavidad <- rast(here(pack_variables_base, "concavidad.tif"))
distancia_costa <- rast(here(pack_variables_base, "distancia_costa.tif"))
salinidad_rango <- rast(here(pack_variables_base, "salinidad_rango.tif"))
temperatura_rango <- rast(here(pack_variables_base, "temperatura_rango.tif"))

variables_raster <- c(
  clorofila_media,
  velocidad_corriente_media,
  ph_rango,
  distancia_costa,
  batimetria,
  concavidad,
  salinidad_rango,
  temperatura_rango)




# CARGAR OCURRENCIA

ocurrencias_E_lucunter <- readr::read_delim(here("BD_E_lucunter_submuestreado_Caribe.csv")) %>% 
  transmute(lon = decimalLongitude,
            lat = decimalLatitude) %>% 
  as.data.frame()



class(ocurrencias_E_lucunter)


# CARGAR PUNTOS DE FONDO

puntos_fondo_submuestreados <- read_delim(here("puntos_fondo_submuestreados.csv"))



#RUTA ENMEVAL 

options(ENMeval.maxent.jar = here("..", "..", "Proyecmaxent_software", "maxent.jar"))

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
  bg = puntos_fondo_submuestreados,
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

eval_df[eval_df$tune.args == "fc.H_rm.3.5", ]

# GUARDAR TODOS LOS MODELOS ENTRENADOS EN ENMEVALS (HIPERPARAMETROS)

saveRDS(eval_results, here("Modelos", "ENMeval.rds"))


rm(list = ls())
# Cargar los resultados guardados de ENMeval


eval_results <- readRDS(here("Modelos", "ENMeval.rds"))

# --- 7. SELECCIONAR EL MEJOR MODELO ---
Modelo_fc.H_rm.3.5 <- eval_results@models[["fc.H_rm.3.5"]]



# predecir idoneidad con el mejor modelo

Raster_idoneidad <- terra::predict(variables_raster, Modelo_fc.H_rm.3.5, type = "logistic")

# EVALUAR CON dismo

evaluacion_H_3.5 <- evaluate(p = ocurrencias_E_lucunter,
                           a = puntos_fondo_submuestreados,
                           Modelo_fc.H_rm.3.5,
                           x = variables_raster)

plot(evaluacion_H_3.5, "ROC")
plot(evaluacion_H_3.5, "TPR")

# 1. Obtener el TSS
tss_valores_generales <- evaluacion_H_3.5@TPR + evaluacion_H_3.5@TNR - 1

# 2. Encontrar el TSS máximo
# Busca el valor máximo dentro del vector 'tss_valores_calculados'.
tss_maximo <- max(tss_valores_generales)

# Opción 1: Usar threshold() con un criterio que a menudo maximiza TSS
# El criterio 'spec_sens' busca el umbral donde la suma de sensibilidad y especificidad es máxima,
# lo que es equivalente a maximizar el TSS.
umbral_optimo_dismo_funcion <- dismo::threshold(evaluacion_H_3.5, 'spec_sens')



# visualizar
tmap_mode("view")

tm_shape(Raster_idoneidad) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.2830035, 0.4, 0.6, 0.8, 1),
                                 labels = c("< 0.283 (No presencia)", 
                                            "0.283 a 0.4",
                                            "0.4 a 0.6",
                                            "0.6 a 8",
                                            "0.8 a 1")))

# contribución de variables
plot(Modelo_fc.H_rm.3.5) 




# guardar el modelo final
saveRDS(Modelo_fc.H_rm.3.5, here("Modelos", "SDM_maxent.rds"))

