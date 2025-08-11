
library(here)      # Para manejar rutas de archivos
library(terra)     # Para trabajar con datos raster
library(predicts)  # Para la evaluación y partición de datos
library(tidyverse) # Para manipulación de datos
library(mgcv)      # El paquete principal para GAM
library(tmap)

# Limpiar el entorno de trabajo
rm(list = ls())

# --- 2. CARGA DE DATOS AMBIENTALES Y DE ESPECIES ---


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
sdmdata <- read_csv(here("datos_modelos.csv")) [ , -c(10, 11)] %>%
  rename(pb = presencia_ausencia) 

# Asegurarse de que 'pb' sea un factor
sdmdata$pb <- as.factor(sdmdata$pb)


# --- 3. VALIDACIÓN CRUZADA Y AJUSTE DE MODELO GAM (k-fold) ---

# Usar la función folds() de predicts para crear 5 grupos
k <- 5

# Corregido: 'by' debe ser el vector de la columna 'pb'
sdmdata$group <- folds(sdmdata, k = k, by = sdmdata$pb)

# Verificar que los grupos se crearon correctamente
print(table(sdmdata$group))

# Lista para guardar los resultados de la evaluación de cada fold
evaluations <- list()

# Bucle para cada fold
for (i in 1:k) {   set.seed(456)
  
  message(paste("Procesando Fold", i, "..."))
  
  # Dividir los datos en entrenamiento y prueba
  train_data <- sdmdata[sdmdata$group != i, ]
  test_data <- sdmdata[sdmdata$group == i, ]
  
  # Preparar los datos de presencia y ausencia para la evaluación
  pres_test <- test_data[test_data$pb == 1, ]
  back_test <- test_data[test_data$pb == 0, ]
  
  
  # --- Definir la fórmula del modelo GAM ---
  formula_gam <- pb ~ s(batimetria) + s(clorofila_media, k = 3) + s(concavidad, k = 3) +
    s(distancia_costa, k = 3) + s(ph_rango, k = 3) + s(salinidad_rango, k = 3) +
    s(temperatura_rango, k = 3) + s(velocidad_corriente_media, k = 3)
  
  
  # --- Ajustar el modelo GAM para este fold ---
  m_gam <- mgcv::gam(formula_gam,
                     data = train_data[, 1:9],
                     family = "binomial",
                     method = "REML",
                     gamma = 1.4,
                     keep.data = TRUE) # Mantener los datos para predicciones posteriores)
  
  # --- Predecir los valores para los datos de prueba ---
  # El tipo de predicción 'response' da las probabilidades (0-1)
  p <- predict(m_gam, newdata = pres_test, type = "response")
  a <- predict(m_gam, newdata = back_test, type = "response")
  
  # --- Evaluar las predicciones con pa_evaluate ---
  evaluations[[i]] <- pa_evaluate(p = p, a = a)
  
  
}



# --- 4. RESUMEN DE LOS RESULTADOS DE LA VALIDACIÓN CRUZADA ---

# Extraer las métricas de todos los folds
stats_folds <- lapply(evaluations, function(x) x@stats)
stats_summary <- do.call(rbind, stats_folds)

# Calcular el promedio de las métricas
cat("--- Resumen de la Validación Cruzada (promedio de 5 folds) ---\n")
print(colMeans(stats_summary))


# --- 5. AJUSTAR EL MODELO GAM FINAL CON TODOS LOS DATOS ---

# Construir la fórmula del GAM final
formula_gam_final <- pb ~ s(batimetria) + s(clorofila_media, k = 3) + s(concavidad, k = 3) +
  s(distancia_costa, k = 3) + s(ph_rango, k = 3) + s(salinidad_rango, k = 3) +
  s(temperatura_rango, k = 3) + s(velocidad_corriente_media, k = 3)

# Ajustar el modelo GAM con el 100% de los datos
final_gam_model <- mgcv::gam(formula_gam_final,
                             data = sdmdata[, 1:9],
                             family = "binomial",
                             method = "REML",
                             gamma = 1.4,
                             keep.data = TRUE)

summary(final_gam_model)

# Guardar el modelo final 
saveRDS(final_gam_model, here("Modelos", "SDM_GAM.rds"))


# leer el modelo final guardado (opcional)

final_gam_model <- readRDS(here("Modelos", "SDM_GAM.rds"))



# --- 6. EVALUACIÓN DETALLADA DEL MODELO FINAL ---

# Hacer predicciones sobre los mismos datos usados para entrenar
pres_vals <- predict(final_gam_model, newdata = sdmdata[sdmdata$pb == 1, ], type = "response")
abs_vals <- predict(final_gam_model, newdata = sdmdata[sdmdata$pb == 0, ], type = "response")

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

  
  ### **Paso 4: Visualizaciones y Predicción del Mapa Final**
  
 
# --- 7. VISUALIZACIONES (Curvas de respuesta y Curva ROC) ---

# Curvas de respuesta parciales de mgcv
par(mfrow=c(3, 3), mar=c(4, 4, 2, 1))
plot(final_gam_model, pages = 1)
par(mfrow=c(1, 1))

# Curva ROC con predicts
plot(eval_final, "ROC")
title("Curva ROC del Modelo GAM Final")

# --- 8. PREDECIR Y VISUALIZAR EL MAPA FINAL ---

# Predecir el mapa de idoneidad con el modelo GAM
raster_idoneidad <- terra::predict(variables_completas, final_gam_model, type = "response")

# Visualizar el mapa de idoneidad con tmap
tmap_mode("view")

tm_shape(raster_idoneidad) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.349, 0.5, 0.7, 0.9, 1),
                                 labels = c("< 0.349 (No presencia)", 
                                            "0.349 a 0.5",
                                            "0.5 a 0.7",
                                            "0.7 a 0.9",
                                            "0.9 a 1")))
# Visualizar el mapa binario usando el umbral óptimo (max_kappa)
umbral <- eval_final@thresholds$max_kappa
mapa_presencia <- ifel(raster_idoneidad >= umbral, 1, 0)
names(mapa_presencia) <- "Presencia_Predicha"

tm_shape(mapa_presencia) +
  tm_raster(palette = c("gray", "red"), title = "Presencia Predicha",
            labels = c("Ausencia", "Presencia")) +
  tm_layout(main.title = paste("Mapa Binario (Umbral =", round(umbral, 3), ")"))


# guardar raster final

writeRaster(raster_idoneidad, here("..", "..", "MAPAS", "r_actual_GAM.tif"))
