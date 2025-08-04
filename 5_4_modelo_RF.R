library(here)
library(terra)
library(predicts)
library(tidyverse)
library(randomForest) # El paquete para Random Forest
library(tmap)

# Set seed para reproducibilidad
set.seed(456)
rm(list = ls())



# --- CARGA DE DATOS AMBIENTALES Y DE ESPECIES ---


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



# Cargar los datos de presencia/ausencia
sdmdata <- read_csv(here("datos_modelos.csv")) %>%
  rename(pb = presencia_ausencia)

# El Random Forest trabaja mejor con la variable de respuesta como factor
# (Aunque puede manejarla como numérica, para clasificación es mejor factor)
sdmdata$pb <- as.factor(sdmdata$pb)


# --- VALIDACIÓN CRUZADA Y AJUSTE DE MODELO RF (k-fold) ---

# Usar la función folds() de predicts para crear 5 grupos
k <- 5
sdmdata$group <- folds(sdmdata, k = k, by = sdmdata$pb)

# Lista para guardar los resultados de la evaluación de cada fold
evaluations <- list()

# Bucle para cada fold
for (i in 1:k) {
  message(paste("Procesando Fold", i, "..."))
  
  # Dividir los datos en entrenamiento y prueba
  train_data <- sdmdata[sdmdata$group != i, ]
  test_data <- sdmdata[sdmdata$group == i, ]
  
  # Preparar los datos de presencia y ausencia para la evaluación
  pres_test <- test_data[test_data$pb == 1, ]
  back_test <- test_data[test_data$pb == 0, ]
  
  # Construir la fórmula de forma dinámica
  var_names <- names(sdmdata)[-c(1, 10)]
  formula_rf <- as.formula(paste("pb ~", paste(var_names, collapse = " + ")))
  
  # --- Ajustar el modelo Random Forest para este fold ---
  # El argumento 'proximity = TRUE' es opcional pero útil para análisis posterior
  m_rf <- randomForest(formula_rf,
                       data = train_data,
                       ntree = 500, # Número de árboles en el bosque
                       mtry = 2,   # Número de variables muestreadas en cada división (ajustable)
                       importance = TRUE,
                       maxnodes = 10,
                       nodesize = 20,
                       keep.forest = TRUE) # Mantener el bosque para predicciones posteriores)
  
  # Predecir los valores de probabilidad (0-1) para los datos de prueba
  # Para Random Forest, el 'type="prob"' da las probabilidades de cada clase.
  p <- predict(m_rf, newdata = pres_test, type = "prob")[, 2] # Probabilidad de la clase "1" (presencia)
  a <- predict(m_rf, newdata = back_test, type = "prob")[, 2] # Probabilidad de la clase "1" (presencia)
  
  # Evaluar las predicciones con pa_evaluate
  evaluations[[i]] <- pa_evaluate(p = p, a = a)
}

# --- 4. RESUMEN DE LOS RESULTADOS DE LA VALIDACIÓN CRUZADA ---

stats_folds <- lapply(evaluations, function(x) x@stats)
stats_summary <- do.call(rbind, stats_folds)

cat("--- Resumen de la Validación Cruzada (promedio de 5 folds) ---\n")
print(colMeans(stats_summary))



# --- 5. AJUSTAR EL MODELO RF FINAL CON TODOS LOS DATOS ---

# Construir la fórmula de forma dinámica
var_names <- names(sdmdata)[-c(1, 10)]
formula_rf_final <- as.formula(paste("pb ~", paste(var_names, collapse = " + ")))

# Ajustar el modelo Random Forest con el 100% de los datos
final_rf_model <- randomForest(formula_rf_final,
                               data = sdmdata,
                               ntree = 500,
                               mtry = 2,
                               importance = TRUE,
                               maxnodes = 10,
                               nodesize = 20,
                               keep.forest = TRUE)

summary(final_rf_model)

plot(final_rf_model)
varImpPlot(final_rf_model)
# --- 6. EVALUACIÓN DETALLADA DEL MODELO FINAL ---

# Hacer predicciones sobre los mismos datos usados para entrenar
pres_vals <- predict(final_rf_model, newdata = sdmdata[sdmdata$pb == 1, ], type = "prob")[, 2]
abs_vals <- predict(final_rf_model, newdata = sdmdata[sdmdata$pb == 0, ], type = "prob")[, 2]

# Usar pa_evaluate para una evaluación completa
eval_final <- pa_evaluate(p = pres_vals, a = abs_vals)

cat("\n--- Evaluación del Modelo Final ---\n")
print(eval_final@stats)
print(eval_final@thresholds)

cat("\n*** Métricas Específicas ***\n")
cat("AUC:", round(eval_final@stats$auc, 3), "\n")
cat("Kappa (máximo):", round(eval_final@tr_stats$kappa[which.max(eval_final@tr_stats$kappa)], 3), "\n")

tss_values <- eval_final@tr_stats$TPR + eval_final@tr_stats$TNR - 1
tss_max <- max(tss_values)
cat("TSS (máximo):", round(tss_max, 3), "\n")

tss_threshold <- eval_final@tr_stats$treshold[which.max(tss_values)]
cat("Umbral óptimo (max_TSS):", round(tss_threshold, 3), "\n")


# --- 7. VISUALIZACIONES (Importancia de variables y Curva ROC) ---

# Ver la importancia de las variables para el modelo
varImpPlot(final_rf_model)

# Curva ROC con predicts
plot(eval_final, "ROC")
title("Curva ROC del Modelo Random Forest Final")

# --- 8. PREDECIR Y VISUALIZAR EL MAPA FINAL ---

# Predecir el mapa de idoneidad con el modelo Random Forest
# La predicción debe ser del tipo "prob" para obtener las probabilidades
raster_idoneidad <- terra::predict(variables_completas, final_rf_model, type = "prob")

tmap_mode("view")

# Visualizar el mapa de idoneidad
tm_shape(raster_idoneidad$X1) + 
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.664, 0.7, 0.8, 0.9, 1),
                                 labels = c("< 0.664 (No presencia)", 
                                            "0.664 a 0.7",
                                            "0.7 a 0.8",
                                            "0.8 a 0.9",
                                            "0.9 a 1")))



# Guardar el modelo final
saveRDS(final_rf_model, here("Modelos", "SDM_RF.rds"))
