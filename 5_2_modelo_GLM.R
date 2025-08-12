
library(tidyverse)
library(terra)
library(dismo)  # Para la función kfold
library(pROC)   # Para calcular e
library(tidyverse)
library(terra)
library(dismo)  # Para la función kfold
library(pROC)   # Para calcular el AUC
library(here)   # Para manejar rutas de archivos
library(tmap)  # Para visualizar mapas

rm(list = ls())  # Limpiar el entorno




# Define la ruta base a la carpeta
pack_variables_base <- here("..", "..", "BIO_MARS_limpias_caribe_50m")

# La lista completa de los nombres "limpios" que deberían ser los nombres de tus archivos .tif
nombres_capas_completos <- c(
  "clorofila_media",
  "salinidad_rango",
  "temperatura_rango",
  "velocidad_corriente_media",
  "ph_rango",
  "batimetria",
  "concavidad",
  "distancia_costa"
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

variables_completas <- c(clorofila_media,
                         salinidad_rango,
                         temperatura_rango,
                         velocidad_corriente_media,
                         ph_rango,
                         batimetria,
                         distancia_costa,
                         concavidad)

datos_modelos <- read_csv(here("datos_modelos.csv"))[ , -c(10, 11)]
           


# ==============================================================================
#                      1. PREPARACIÓN DE LOS DATOS
# ==============================================================================

# Convertir la columna de respuesta a factor para algunos modelos (buena práctica)
datos_modelos$presencia_ausencia <- as.factor(datos_modelos$presencia_ausencia)

# ==============================================================================
#                      2. VALIDACIÓN CRUZADA DE K-FOLDS
# ==============================================================================

# Definir el número de 'folds'

k_folds <- 5

# Crear los grupos de validación en la tabla 'datos_modelos'
# kfold asigna aleatoriamente un grupo (1 a 5) a cada registro
datos_modelos$k_fold_group <- kfold(datos_modelos, k_folds)

# Lista para almacenar los resultados del AUC de cada fold
auc_scores <- list()
auc_df <- data.frame()

# Bucle para iterar a través de cada fold
for (i in 1:k_folds) {
  
  message(paste("Procesando Fold", i, "..."))
  
  # Dividir los datos en entrenamiento y prueba
  datos_entrenamiento <- datos_modelos[datos_modelos$k_fold_group != i, ]
  datos_prueba <- datos_modelos[datos_modelos$k_fold_group == i, ]
  
  # Eliminar la columna del grupo antes de modelar
  datos_entrenamiento <- datos_entrenamiento %>% dplyr::select(-k_fold_group)
  datos_prueba <- datos_prueba %>% dplyr::select(-k_fold_group)
  
  # Ajustar el modelo GLM con la familia binomial
  m_glm_fold <- glm(presencia_ausencia ~ ., data = datos_entrenamiento, family = binomial)
  
  # Predecir las probabilidades para los datos de prueba
  predicciones <- predict(m_glm_fold, datos_prueba, type = "response")
  
  # Calcular el AUC para este fold
  roc_obj <- roc(datos_prueba$presencia_ausencia, predicciones)
  auc_valor <- auc(roc_obj)
  
  # Almacenar el resultado en un data frame para mejor visualización
  # Solución al error: convertimos el objeto auc a un número con as.numeric()
  auc_df <- bind_rows(auc_df, data.frame(fold = i, auc = as.numeric(auc_valor)))
}

# Calcular el promedio de AUC de todos los folds
auc_promedio <- mean(auc_df$auc)
auc_desviacion <- sd(auc_df$auc)

# Imprimir los resultados de la validación
print(auc_df)
cat("\n")
cat("AUC Promedio de los 5 Folds:", round(auc_promedio, 3), "\n")
cat("Desviación Estándar del AUC:", round(auc_desviacion, 3), "\n")

# ==============================================================================
#                      3. AJUSTAR EL MODELO FINAL Y PREDECIR UN MAPA
# ==============================================================================

# Asume que 'variables_completas' ya está en el entorno.

# Entrenar el modelo con el 100% de los datos para la predicción final
m_glm_final <- glm(presencia_ausencia ~ ., data = datos_modelos %>% dplyr::select(-k_fold_group), family = binomial)
summary(m_glm_final)


# guardar el modelo final
write_rds(m_glm_final, here("Modelos", "SDM_glm.rds"))



SDM_glm <- readRDS(here("Modelos", "SDM_glm.rds"))


# Predecir un mapa de idoneidad para todo el Caribe
raster_idoneidad_glm <- terra::predict(variables_completas, SDM_glm, type = "response")



tmap_mode("view")


tm_shape(raster_idoneidad_glm) +
  tm_raster()


# EVALUAR MODELO FINAL

predicciones_finales <- predict(SDM_glm, datos_modelos, type = "response")
roc_obj_final <- roc(datos_modelos$presencia_ausencia, predicciones_finales)
auc_final <- auc(roc_obj_final)


# Obtener los datos de presencia y ausencia de tu tabla de modelado
# 'presencia_ausencia' debe ser 1 para presencia y 0 para ausencia
presencias <- datos_modelos$presencia_ausencia == 1
ausencias <- datos_modelos$presencia_ausencia == 0

# Obtener las predicciones para el modelo final en todos tus datos
predicciones_finales <- predict(SDM_glm, datos_modelos, type = "response")

# ==============================================================================


# --- EVALUACIÓN DETALLADA DEL MODELO FINAL ---


rm(list = ls())

sdmdata <- read_csv(here("datos_modelos.csv"))[ , -c(10, 11)] %>% 
  rename(pb = presencia_ausencia)

SDM_glm <- readRDS(here("Modelos", "SDM_glm.rds"))

# Hacer predicciones sobre los mismos datos usados para entrenar
pres_vals <- predict(SDM_glm, newdata = sdmdata[sdmdata$pb == 1, ], type = "response")
abs_vals <- predict(SDM_glm, newdata = sdmdata[sdmdata$pb == 0, ], type = "response")

# Usar pa_evaluate para una evaluación completa
eval_final <- pa_evaluate(p = pres_vals, a = abs_vals)

# Imprimir las métricas principales
cat("\n--- Evaluación del Modelo Final ---\n")
print(eval_final@stats)
print(eval_final@thresholds)

# Métricas específicas: AUC, Kappa, TSS y umbral óptimo
cat("\n*** Métricas Específicas ***\n")
cat("AUC:", round(eval_final@stats$auc, 3), "\n")
cat("Kappa (máximo):", round(eval_final@tr_stats$kappa[which.max(eval_final@tr_stats$kappa)], 3), "\n")
tss_values <- eval_final@tr_stats$TPR + eval_final@tr_stats$TNR - 1
tss_max <- max(tss_values)
cat("TSS (máximo):", round(tss_max, 3), "\n")
cat("Umbral óptimo (max_TSS):", round(eval_final@thresholds$max_spec_sens, 3), "\n")



# Predecir un mapa de idoneidad para todo el Caribe
raster_idoneidad_glm <- terra::predict(variables_completas, SDM_glm, type = "response")



tmap_mode("view")


tm_shape(raster_idoneidad_glm) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.641, 0.7, 0.8, 0.9, 1),
                                 labels = c("< 0.641 (No presencia)", 
                                            "0.641 a 0.7",
                                            "0.7 a 0.8",
                                            "0.8 a 9",
                                            "0.9 a 1")))

# guardar raster final

writeRaster(raster_idoneidad_glm, 
            filename = here("..", "..", "Mapas", "r_actual_glm.tif"), 
            overwrite = TRUE)
