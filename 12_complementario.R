
# CARGAR TODOS LOS MODELOS

SDM_maxent <- readRDS(here("Modelos", "SDM_maxent.rds"))

SDM_rf <- readRDS(here("Modelos", "SDM_RF.rds"))

SDM_gam <- readRDS(here("Modelos", "SDM_GAM.rds"))

SDM_glm <- readRDS(here("Modelos", "SDM_glm.rds"))



evaluación <- tibble(Modelo = c("MAXENT", "GLM", "GAM", "RF"),
                     AUC = c(0.957, 0.9503, 0.969, 0.997),
                     TSS = c(0.796, 0.7592593, 0.852, 0.963),
                     Kappa = c(0.796, 0.7592593, 0.852, 0.963),
                     Umbral_TSS = c(0.2830035, 0.641, 0.349, 0.664))



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
# Cada SpatRaster se creará en el entorno global con el nombre correspondiente
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

# NORMALIZAR PESOS POR TSS

pesos_normalizados <- evaluación$TSS / sum(evaluación$TSS)

# PREDICCION DE LOS MODELOS

raster_maxent <- terra::predict(variables_completas, SDM_maxent, type = "logistic")
raster_glm <- terra::predict(variables_completas, SDM_glm, type = "response")
raster_gam <- terra::predict(variables_completas, SDM_gam, type = "response")
raster_rf <- terra::predict(variables_completas, SDM_rf, type = "prob")

# Apilar todos los mapas de idoneidad en un solo objeto `SpatRaster`
mapas_para_ensamble <- c(raster_maxent, raster_glm, raster_gam, raster_rf$X1)


# CREAR RASTER POR PESOS

raster_actual_ponderado <- terra::weighted.mean(mapas_para_ensamble, w = pesos_normalizados)

