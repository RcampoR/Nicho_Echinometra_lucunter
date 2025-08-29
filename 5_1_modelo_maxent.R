library(terra) # raster y vectores
library(dplyr) # manipulkación de datos
library(ENMeval) # evaluación de hiperparametros para MAXENT
library(rJava) # Java en R, para maxent
library(dismo) # Modelo MAXENT final
library(raster) # Raster compatible con dismo 
library(tmap) # Mapas tematicos
library(here) # control de direcciones
library(readr) # leer csv
library(predicts)


# limpiar entorno 
rm(list = ls())



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
# La elección de FCs puede depender de la cantidad de los puntos de presencia.
# pocos datos (< 50-100), se podría mantener las FCs más simples (L, LQ, H).
# Si hay muchos (>200), se pueden explorar más complejas (LQHPT).

ENMeval_FCs <- c("L", "LQ", "H", "LQH") # Considerar el número de puntos de presencia 

ENMeval_RMs <- seq(1.0, 5.0, by = 0.5) # se puedeajustar este rango


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
)

message("Evaluación de hiperparámetros completada.")
Sys.sleep(2) 



# --- 6. ANALIZAR LOS RESULTADOS DE LA EVALUACIÓN ---

# Convertir los resultados a un data.frame para un análisis más fácil
eval_df <- eval_results@results

eval_df[eval_df$tune.args == "fc.H_rm.3.5", ]

# GUARDAR TODOS LOS MODELOS ENTRENADOS EN ENMEVALS (HIPERPARAMETROS)

saveRDS(eval_results, here("Modelos", "ENMeval.rds"))






#----------------------------------------------------------------------------------------

rm(list = ls())
# Cargar los resultados guardados de ENMeval


eval_results <- readRDS(here("Modelos", "ENMeval.rds"))

# ver k-folds
eval_results@results.partitions[eval_results@results.partitions == "fc.H_rm.3.5", ]

# --- 7. SELECCIONAR EL MEJOR MODELO ---
Modelo_fc.H_rm.3.5 <- eval_results@models[["fc.H_rm.3.5"]]


# predecir idoneidad con el mejor modelo

Raster_idoneidad <- terra::predict(variables_raster, Modelo_fc.H_rm.3.5, type = "logistic")



#-----------------------------------------------------------------------------------

# --- 3. OBTENER EL UMBRAL ÓPTIMO MAXENT ---

# Cargar los datos de presencia/ausencia
sdmdata <- read_csv(here("datos_modelos.csv")) %>%
  rename(pb = presencia_ausencia)

# Extraer los valores del mapa de ensamble en las ubicaciones de tus puntos
valores_predichos_maxent <- terra::extract(Raster_idoneidad,
                                             sdmdata[, c("x", "y")]) %>% 
  pull(2)

# Separar las predicciones para presencias y ausencias
pres_vals_maxent <- valores_predichos_maxent[sdmdata$pb == 1]
abs_vals_maxent <- valores_predichos_maxent[sdmdata$pb == 0]

eval_maxent <- pa_evaluate(p = pres_vals_maxent, a = abs_vals_maxent)

umbral_optimo_maxent <- eval_maxent@thresholds$max_spec_sens

cat("\n--- Umbral óptimo para el ensamble (max_TSS/Kappa) ---\n")
cat("Umbral:", round(umbral_optimo_maxent, 3), "\n")

cat("\n--- Evaluación del Modelo Final ---\n")
print(eval_maxent@stats)
print(eval_maxent@thresholds)

cat("\n*** Métricas Específicas ***\n")
cat("AUC:", round(eval_maxent@stats$auc, 3), "\n")
cat("Kappa (máximo):", round(eval_maxent@tr_stats$kappa[which.max(eval_maxent@tr_stats$kappa)], 3), "\n")

tss_values <- eval_maxent@tr_stats$TPR + eval_maxent@tr_stats$TNR - 1
tss_max <- max(tss_values)
cat("TSS (máximo):", round(tss_max, 3), "\n")

tss_threshold <- eval_maxent@tr_stats$treshold[which.max(tss_values)]
cat("Umbral óptimo (max_TSS):", round(tss_threshold, 3), "\n")




# ------------------------------------------------------------------------------------------


# visualizar
tmap_mode("view")

tm_shape(Raster_idoneidad) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.283, 0.4, 0.6, 0.8, 1),
                                 labels = c("< 0.283 (No presencia)", 
                                            "0.283 a 0.4",
                                            "0.4 a 0.6",
                                            "0.6 a 8",
                                            "0.8 a 1")))

# contribución de variables
plot(Modelo_fc.H_rm.3.5) 




# guardar el modelo final
saveRDS(Modelo_fc.H_rm.3.5, here("Modelos", "SDM_maxent.rds"))


# CREAR MATRIZ DE CONFUSIÓN DEL MODELO FINAL

# Predicciones binarias con el umbral óptimo
pred_binarias <- ifelse(valores_predichos_maxent >= umbral_optimo_maxent, 1, 0)

# Crear la matriz de confusión
conf_matrix <- table(
  Observado = sdmdata$pb,
  Predicho  = pred_binarias
)

print(conf_matrix)


