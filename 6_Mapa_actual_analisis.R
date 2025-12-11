library(terra) # raster y vectores
library(tmap) # mapas tematicos
library(tidyverse) # maniulacion de datos y graficas
library(geodata) # datos espaciales en linea
library(here)# control de direcciones
library(randomForest)
library(mgcv) # GAM
library(predicts)


#limpiar entorno
rm(list = ls())



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


# Cargar los datos de presencia/ausencia
sdmdata <- read_csv(here("datos_modelos.csv")) %>%
  rename(pb = presencia_ausencia)

# El Random Forest trabaja mejor con la variable de respuesta como factor
# (Aunque puede manejarla como numérica, para clasificación es mejor factor)
sdmdata$pb <- as.factor(sdmdata$pb)




# --- 1. EXTRAER LAS PREDICCIONES DEL ENSAMBLE EN LOS PUNTOS ---


# Extraer los valores del mapa de ensamble en las ubicaciones de tus puntos
valores_predichos_ensamble <- terra::extract(raster_actual_ponderado,
                                             sdmdata[, c("x", "y")]) %>% 
  pull(2)

# Separar las predicciones para presencias y ausencias
pres_vals_ensamble <- valores_predichos_ensamble[sdmdata$pb == 1]
abs_vals_ensamble <- valores_predichos_ensamble[sdmdata$pb == 0]

# Usar pa_evaluate (o dismo::evaluate) para una evaluación completa
# Es consistente usar predicts ya que lo usaste para la mayoría de los modelos
eval_ensamble <- pa_evaluate(p = pres_vals_ensamble, a = abs_vals_ensamble)

# --- 3. OBTENER EL UMBRAL ÓPTIMO PARA EL ENSAMBLE ---

# Elige la métrica que quieres optimizar (por ejemplo, max_spec_sens para TSS)
# Ya vimos que para tus modelos, este umbral también maximiza Kappa.
umbral_optimo_ensamble <- eval_ensamble@thresholds$max_spec_sens

cat("\n--- Umbral óptimo para el ensamble (max_TSS/Kappa) ---\n")
cat("Umbral:", round(umbral_optimo_ensamble, 3), "\n")






cat("\n--- Evaluación del Modelo Final ---\n")
print(eval_ensamble@stats)
print(eval_ensamble@thresholds)

cat("\n*** Métricas Específicas ***\n")
cat("AUC:", round(eval_ensamble@stats$auc, 3), "\n")
cat("Kappa (máximo):", round(eval_ensamble@tr_stats$kappa[which.max(eval_ensamble@tr_stats$kappa)], 3), "\n")

tss_values <- eval_ensamble@tr_stats$TPR + eval_ensamble@tr_stats$TNR - 1
tss_max <- max(tss_values)
cat("TSS (máximo):", round(tss_max, 3), "\n")

tss_threshold <- eval_ensamble@tr_stats$treshold[which.max(tss_values)]
cat("Umbral óptimo (max_TSS):", round(tss_threshold, 3), "\n")



# Predicciones binarias del ensamble según el umbral
pred_bin_ensamble <- ifelse(valores_predichos_ensamble >= umbral_optimo_ensamble, 1, 0)

# Crear matriz de confusión
conf_matrix_ensamble <- table(
  Observado = sdmdata$pb,
  Predicho  = pred_bin_ensamble
)

print(conf_matrix_ensamble)







tmap_mode("view")

tm_shape(raster_actual_ponderado) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.564, 0.7, 0.8, 0.9, 1),
                                 labels = c("< 0.564 (No presencia)", 
                                            "0.564 a 0.7",
                                            "0.7 a 0.8",
                                            "0.8 a 9",
                                            "0.9 a 1")))

# guardar raster ponderado

writeRaster(raster_actual_ponderado, 
            here("..", "..", "MAPAS", "raster_actual_ponderado.tif"))



# HACER MAPAS DE OCURRENCIA Y IDONEIDAD

tmap_mode("plot")

#vectores
Colombia <- vect(here("..", "..", "COL_shp", "gadm36_COL_0.shp"))
Panama <- vect(here("..", "..", "PAN_shp", "gadm41_PAN_0.shp"))
Costa_rica <- vect(here("..", "..", "CRI_shp", "gadm41_CRI_0.shp"))
Nicaragua <- vect(here("..", "..", "NIC_shp", "gadm41_NIC_0.shp"))

ocurrencias_E_lucunter <- read_delim(here("datos_modelos.csv")) %>% 
                                     filter(presencia_ausencia == 1) %>%
                                     select(x, y) %>% 
                                     vect(geom = c("x", "y"), crs = "EPSG:4326")


#MAR CARIBE

Caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp")) %>% 
  aggregate(dissolve = TRUE) 

#FILTRADO DE PUNTOS POR FUERA DEL Caribe (el modelo nunca los tuvo en cuenta)

ocurrencias_E_lucunter_filtradas <- crop(ocurrencias_E_lucunter, Caribe) 

#raster
Raster_idoneidad <- rast(here("..", "..", "MAPAS", "raster_actual_ponderado.tif"))


# MAPA CONTEXTO PUNTOS OCURRENCIA



mundo <- world(path=".")
crs(mundo) <- "EPSG:4326"


tmap_mode("plot")


mapa_contexto <- tm_shape(mundo, xlim = c(-90, -65), ylim = c(-8, 20)) + 
  tm_fill(fill = "white") + 
  tm_borders(col = "gray23", lwd = 0.5) + 
  tm_shape(Colombia) + 
  tm_polygons(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") + 
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_borders(col = "black", lwd = 1.2) +  
  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") 





# MAPA DE OCURRENCIAS E. lucunter

mapa_ocurrencias <- tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
  tm_shape(ocurrencias_E_lucunter_filtradas) +
  tm_dots(fill = "red",
          size = 0.7) +
  tm_shape(Colombia) +
  tm_polygons(fill = "gray89") +
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_scalebar(position = c("bottom", "left"), text.size = 0.5) +
  tm_compass(position = c("top", "left"), size = 3, type = "arrow") +
  tm_graticules(lines = FALSE,
                labels.col = "gray10") + 
  tm_add_legend(title = "LEGEND",
                type = "polygons",
                labels = c("Caribbean sea", "Countries in the study area", 
                           expression("Occurrence points " * italic("Echinometra lucunter"))),
                fill = c("lightblue", "gray89", "red"),
                fontfamily = "sans",
                position = c("top", "right")) +
  tm_layout(frame = TRUE,
            frame.lwd = 3,
            frame.color = "gray20") +
  tm_inset(mapa_contexto, position = c("bottom", "right"))


# guardar mapa 
tmap_save(tm = mapa_ocurrencias, 
         filename = here("..", "..", "MAPAS", "mapa_distribución_caribe.png"), 
         width = 11,        
         height = 7,       
         units = "in",     # Unidades en pulgadas
         dpi = 1000)   






# Mapa de idoneidad de habitat 

Mapa_idoneidad <-  tm_shape(Caribe) +
  tm_polygons(fill = "lightblue") +
  tm_shape(Colombia) +
  tm_fill(fill = "gray89") + 
  tm_shape(Panama) +
  tm_polygons(fill = "gray89") +
  tm_shape(Costa_rica) +
  tm_polygons(fill = "gray89") +
  tm_shape(Nicaragua) +
  tm_polygons(fill = "gray89") +
  tm_shape(Raster_idoneidad) +
  tm_raster(col.scale = tm_scale(values = "brewer.yl_or_rd",
                                 breaks = c(0, 0.564, 0.7, 0.8, 0.9, 1),
                                 labels = c("< 0.564 (Absence)", 
                                            "0.564 a 0.7",
                                            "0.7 a 0.8",
                                            "0.8 a 9",
                                            "0.9 a 1")),
            col.legend = tm_legend(title = "Probability of presence",
                                   position = c("bottom", "right"))) +
  tm_scalebar(position = c("bottom", "left"), text.size = 0.5) +
  tm_compass(position = c("top", "left"), size = 3, type = "arrow") +
  tm_graticules(lines = FALSE,
                labels.col = "gray10") +
  tm_add_legend(title = "LEGEND",
                type = "polygons",
                labels = c("Caribbean sea", "Countries in the study area"),
                fill = c("lightblue", "gray89"),
                fontfamily = "sans",
                position = c("bottom", "right")) +
  tm_layout(frame = TRUE,
            frame.lwd = 3,
            frame.color = "gray20") 


# guardar mapa 
tmap_save(Mapa_idoneidad, 
         filename = here("..", "..", "MAPAS", "mapa_idoneidad_caribe.png"), 
         width = 11,       
         height = 7,       
         units = "in",     # Unidades en pulgadas
         dpi = 1000)   

dev.off()


# Definir el umbral
umbral <- 0.564

# Crear un raster binario: 1 si es idóneo, 0 si no
raster_binario <- classify(Raster_idoneidad, matrix(c(-Inf, umbral, 0, umbral, Inf, 1), ncol = 3, byrow = TRUE))

# Si el raster tiene un CRS proyectado (e.g., UTM), `cellSize` devolverá el área en las unidades del CRS (e.g., m^2).
area_celda <- cellSize(Raster_idoneidad, unit = "m")

# Multiplicar el raster binario por el área de la celda para obtener el área idónea de cada celda
area_idonea_por_celda <- raster_binario * area_celda

# Sumar todas las áreas de las celdas idóneas para obtener el área total
area_total_idonea_m2 <- global(area_idonea_por_celda, "sum", na.rm = TRUE)

# Convertir a kilómetros cuadrados para mayor legibilidad
area_total_idonea_km2 <- area_total_idonea_m2 / 1e6

#MAR CARIBE

Caribe_por_pais <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp"))

# Lista para almacenar los resultados
resultados_area_por_pais <- list()

# Obtener los nombres únicos de los países de tu SpatVector Caribe_por_pais
# **Ajusta "NAME" por el nombre real de la columna en tu SHP que identifica el país.**
paises_en_caribe <- unique(Caribe_por_pais$SOVEREIGN1)

# Bucle para iterar sobre cada país y calcular el área
for (pais in paises_en_caribe) {
  cat("Calculando para:", pais, "...\n")
  
  # Seleccionar el polígono correspondiente al país actual
  limite_pais <- Caribe_por_pais[Caribe_por_pais$SOVEREIGN1 == pais, ]
  
  # Recortar el raster de áreas idóneas a la zona marítima del país
  raster_idonea_pais <- crop(area_idonea_por_celda, limite_pais)
  raster_idonea_pais <- mask(raster_idonea_pais, limite_pais) # Asegura que solo se consideren las celdas dentro del límite
  
  # Sumar las áreas de las celdas idóneas dentro del límite del país
  area_m2_pais <- global(raster_idonea_pais, "sum", na.rm = TRUE)
  
  # Convertir a kilómetros cuadrados
  area_km2_pais <- area_m2_pais / 1e6
  
  # Almacenar el resultado
  resultados_area_por_pais[[pais]] <- round(area_km2_pais$sum, 2)
}

# Imprimir los resultados para cada país
cat("\n--- Área Idónea por País ---\n")
for (pais in names(resultados_area_por_pais)) {
  cat("El área idónea para", pais, "es de:", resultados_area_por_pais[[pais]], "km².\n")
}


