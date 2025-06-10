library(terra)
library(dplyr)
library(ENMeval)
library(rJava)


#CARGAR VARIABLES
pack_variables_base <- "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m"

batimetria <- rast(file.path(pack_variables_base, "batimetria.tif"))
clorofila <- rast(file.path(pack_variables_base, "clorofila.tif"))
distancia_costa <- rast(file.path(pack_variables_base, "distancia_costa.tif"))
pH <- rast(file.path(pack_variables_base, "pH.tif"))
temperatura <- rast(file.path(pack_variables_base, "temperatura.tif"))
velocidad_corriente <- rast(file.path(pack_variables_base, "velocidad_corriente.tif"))


# CARGAR OCURRENCIA

ocurrencias_E_lucunter <- readr::read_delim("BD_E_lucunter_submuestreado_COL.csv") %>% 
                          transmute(lon = decimalLongitude,
                                    lat = decimalLatitude)
                          

#CARGAR VARIABLES DEL CARIBE COLOMBIANO A 50M DE BATIMETRIA

variables_raster <- c(batimetria, 
                      clorofila,
                      distancia_costa,
                      pH,
                      temperatura,
                      velocidad_corriente)

# CARGAR PUNTOS DE FONDO

puntos_fondo_submuestreados <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\puntos_fondo\\puntos_fondo_submuestreados.shp")

puntos_fondo_df <- as.data.frame(geom(puntos_fondo_submuestreados)) %>%
  dplyr::select(x, y) %>%
  dplyr::rename(lon = x, lat = y)

#RUTA ENMEVAL 

options(ENMeval.maxent.jar = "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\maxent_software\\maxent.jar")

# Verificar si MaxEnt está configurado correctamente
if (requireNamespace("rJava", quietly = TRUE)) {
  if (is.character(getOption("ENMeval.maxent.jar"))) {
    message("MaxEnt.jar path is set: ", getOption("ENMeval.maxent.jar"))
  } else {
    warning("MaxEnt.jar path is not set correctly. Please check options(ENMeval.maxent.jar = ...)")
  }
} else {
  warning("rJava package is not installed or loaded. MaxEnt functionality may be limited.")
}

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
    doClamp = TRUE # doClamp también puede ir aquí, si no es un argumento directo de ENMevaluate
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
message("\nResultados de la evaluación de hiperparámetros (primeras filas):")
print(head(eval_df))
message("\nColumnas disponibles para análisis:")
print(colnames(eval_df))


# GUARDAR TODOS LOS MODELOS ENTRENADOS

saveRDS(eval_results, "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Modelos_Entrenados\\GENERALES\\ENMeval_TODOS_actuales.rds")



# MODELO LQ_rm_2.0

LQ_rm_2.0 <- eval_results@models[["fc.LQ_rm.2"]]

# guardar modelo 

saveRDS(LQ_rm_2.0, "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Modelos_Entrenados\\ESPECIFICOS\\ENMeval_LQ_rm_2.0.rds")


mapa_idoneidad <- terra::predict(variables_raster, LQ_rm_2.0, type = "logistic", clamp = TRUE)

# guardar mapa 

writeRaster(mapa_idoneidad, "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MAPAS\\capa_idoneidad_E_lucunter.tif")
