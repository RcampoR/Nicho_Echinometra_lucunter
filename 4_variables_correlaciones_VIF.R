library(terra) # raster y vectores
library(tidyverse) # manipular datos y graficar
library(corrplot) # graficas de correlación
library(car) # algunas estadisticas
library(here) # control de direcciones
library(factoextra) # PCA
library(cluster)


#limpiar entorno
rm(list = ls())
gc()

# ruta base a la carpeta
pack_variables_base <- here("..", "..", "BIO_MARS_limpias_caribe_50m")

# La lista completa de los nombres "limpios" 
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
#oxigeno, productividad, temperatura_media, distancia costa, curvatura plan, perfil curv



#VARIABLES RESTANTES

variables_restantes <- list(
  clorofila_media,
  salinidad_media,
  salinidad_rango,
  temperatura_rango,
  velocidad_corriente_media,
  direccion_corriente_media,
  ph_medio,
  ph_rango,
  batimetria,
  aspecto_este_oeste,
  aspecto_norte_sur,
  concavidad,
  pendiente_batimetrica,
  distancia_costa
)


#calcular VIF  a las variables restantes

#haciendo el data.frame
variables_raster_para_vif <- c(
  clorofila_media,
  salinidad_media,
  salinidad_rango,
  temperatura_rango,
  velocidad_corriente_media,
  direccion_corriente_media,
  ph_medio,
  ph_rango,
  batimetria,
  aspecto_este_oeste,
  aspecto_norte_sur,
  concavidad,
  pendiente_batimetrica,
  distancia_costa
)


variables_vif_df <- as.data.frame(variables_raster_para_vif)


#modelo lm arbitrario
modelo_vif_sencillo <- lm(batimetria ~ ., data = variables_vif_df)


# 5. Calcular y mostrar los VIFs
resultados_vif <- vif(modelo_vif_sencillo)

resultados_vif


#ELIMINANDO VARIABLES PROBLEMATICAS VIF >= 5 y escoger mas importantes en la ecologia

#calcular VIF  a las variables restantes

#haciendo el data.frame
variables_vif_validas <-  c(clorofila_media,
                            salinidad_rango,
                            temperatura_rango,
                            velocidad_corriente_media,
                            ph_rango,
                            batimetria,
                            concavidad,
                            distancia_costa)

variables_validas_df <- as.data.frame(variables_vif_validas)


#modelo lm arbitrario
modelo_vif_valido <- lm(batimetria ~ ., data = variables_validas_df)

# 5. Calcular y mostrar los VIFs
resultados_vif_valido <- vif(modelo_vif_valido)

resultados_vif_valido








# GENERAR PUNTOS DE FONDO


#LIMPIAR ENTORNO 

rm(list = ls())
gc()




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


plot(clorofila_media)


#BASE ECHINOMETRA SUBMUESTREADA
ocurrencias_E_lucunter <- readr::read_delim(here("BD_E_lucunter_submuestreado_Caribe.csv")) %>% 
  transmute(lon = decimalLongitude,
            lat = decimalLatitude) %>% 
  vect()

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
puntos_fondo_crudos <- spatSample(variables_limpias_caribe, 540,
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

# guardar puntosl fondo submuestreados


as.data.frame(geom(pf_submuestreados)) %>%
  dplyr::select(x, y) %>%
  dplyr::rename(lon = x, lat = y) %>% 
  as.data.frame() %>% 
  write_csv(here("puntos_fondo_submuestreados.csv"))




# EXTRAER VALORES, PARA LOS GLM, GAM Y RF


#limpiar entorno
rm(list = ls())
gc()

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

# cargar nuevamente las ocurrencias de E. lucunter

ocurrencias_E_lucunter <- readr::read_delim(here("BD_E_lucunter_submuestreado_Caribe.csv")) %>% 
  transmute(lon = decimalLongitude,
            lat = decimalLatitude) %>% 
  vect()

# extraer valores de variables para las ocurrencias

valores_ocurrencias <- terra::extract(variables_completas, ocurrencias_E_lucunter, xy = TRUE )

# quitar ID

valores_ocurrencias <- valores_ocurrencias[, -1]


# conservar solo valores validos (sin NA)
valores_ocurrencias_limpios <- valores_ocurrencias %>% 
  drop_na()




# guardar valores de ocurrencias

write_csv(valores_ocurrencias_limpios, here("valores_extraidos_E_lucunter_caribe.csv"))

# ANALISIS DE COMPONENTES PRINCIPALES

# Realizar el PCA solo con los valores de ocurrencia
# Es crucial usar scale = TRUE y center = TRUE para estandarizar las variables
pca_ocurrencias <- prcomp(valores_ocurrencias_limpios[, -c(9, 10)], scale = TRUE, center = TRUE)

# Resumen de los resultados para ver la varianza explicada
summary(pca_ocurrencias)

# Visualizar la varianza explicada por cada componente (gráfico de codo)
fviz_eig(pca_ocurrencias, addlabels = TRUE, ylim = c(0, 50))

# Código para generar el biplot con mejoras estéticas
fviz_pca_biplot(pca_ocurrencias,
                
                # Puntos de los individuos
                geom.ind = "point",
                pointshape = 21,
                pointsize = 3.5,
                fill.ind = "#768B99",  # Un gris azulado elegante
                col.ind = "black",
                alpha.ind = 0.8,       # Un poco de transparencia
                
                # Etiquetas de las variables
                repel = TRUE,
                labelsize = 5,
                
                # Flechas de las variables
                col.var = "black",
                geom.var = c("arrow", "text"),
                arrowsize = 1,
                # Etiqueta de los ejes con la varianza explicada
                xlab = paste0("PC1 (", round(summary(pca_ocurrencias)$importance[2,1]*100, 2), "%)"),
                ylab = paste0("PC2 (", round(summary(pca_ocurrencias)$importance[2,2]*100, 2), "%)")) +
  
  # Mejoras adicionales con ggplot2
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") # Generalmente no se necesita leyenda si todos los puntos son iguales


# 1. Calcular la matriz de distancias entre los puntos de ocurrencia
# Se usa la distancia euclidiana, que es el método por defecto y más común
distancias <- dist(valores_ocurrencias_limpios, method = "euclidean")

# 2. Realizar el análisis de clústeres jerárquicos
# Usamos el método de "ward.D2" para minimizar la varianza dentro de cada clúster,
# lo que tiende a producir clústeres más compactos y es muy recomendado
cluster_jerarquico <- hclust(distancias, method = "ward.D2")

# 3. Visualizar el dendrograma para ver la estructura de los clústeres
fviz_dend(cluster_jerarquico,
          cex = 0.6,       # Tamaño de las etiquetas
          k = 3,           # Cortar el dendrograma en 3 clústeres (basado en el PCA)
          k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), # Colores para los clústeres
          rect = TRUE,     # Dibujar rectángulos alrededor de los clústeres
          main = "Dendrograma de Clústeres de Ocurrencias",
          xlab = "Ocurrencias",
          ylab = "Distancia")


# 1. Cortar el árbol de clústeres en 3 grupos
# La función cutree() asigna un ID de clúster a cada ocurrencia
grupos <- cutree(cluster_jerarquico, k = 3)

# 2. Convertir los IDs de clúster en un factor para la visualización
grupos_factor <- as.factor(grupos)

grupos_renombrados <- factor(grupos, 
                             levels = c("1", "2", "3"),
                             labels = c("I", "II", "III"))


# 3. Re-crear el biplot del PCA, pero esta vez coloreando los puntos por el grupo
fviz_pca_biplot(pca_ocurrencias,
                title = NULL,
                # Puntos de los individuos
                geom.ind = "point",
                pointshape = 21,
                pointsize = 3,
                fill.ind = grupos_renombrados, # ¡Aquí usamos el factor de los clústeres para colorear!
                col.ind = "black",
                palette = "jco",
                
                # Etiquetas de las variables
                repel = TRUE,
                labelsize = 5,
              
                # Flechas de las variables
                col.var = "black",
                geom.var = c("arrow", "text"),
                arrowsize = 1,
                xlab = paste0("PC1 (", round(summary(pca_ocurrencias)$importance[2,1]*100, 2), "%)"),
                ylab = paste0("PC2 (", round(summary(pca_ocurrencias)$importance[2,2]*100, 2), "%)")) +
  
  # --- TEMA Y ESTILO PROFESIONAL ---
  theme_minimal() +
  theme(
    # Configuración de texto
    text = element_text(family = "sans"),
    plot.title = NULL,
    plot.subtitle = NULL,
    
    # Ejes y etiquetas
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.line = element_line(color = "gray50", size = 0.5),
    
    # Leyenda
    legend.position = "right",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "cm"),
    
    # Panel y grilla
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Márgenes
    plot.margin = unit(c(20, 20, 20, 20), "pt")
  ) +
  
  # --- ETIQUETAS FINALES ---
  labs(
    fill = "Subgrupos",
    caption = paste0("Varianza total explicada: ", 
                     round(sum(summary(pca_ocurrencias)$importance[2,1:2]) * 100, 1), 
                     "%")
  )

# GUARDAR PCA

ggsave(
  filename = here("..", "..", "GRAFICAS", "PCA_ocurrencias.png"),
  width = 10, height = 6, dpi = 1000
)

dev.off()

# GENERAR PUNTOS DE AUSENCIA PARA GLM, GAM Y RF

# --- PASO 1: Generar Pseudo-ausencias ---
# Generaremos más puntos de los que necesitamos para poder filtrarlos
num_pseudoausencias_deseadas <- 54
distancia_minima_km <- 2

# Usaremos un bucle para generar puntos que cumplan el criterio de distancia
# Esto es más seguro que solo generar puntos aleatorios
puntos_pseudoausencia <- NULL
num_puntos_actual <- 0
set.seed(456) # Semilla para reproducibilidad

while(num_puntos_actual < num_pseudoausencias_deseadas) {
  
  # Generar un lote de puntos aleatorios en todo el área de estudio
  puntos_aleatorios_lote <- spatSample(variables_completas, size = 100, method = "random", as.points = TRUE, na.rm = TRUE)
  
  crs(puntos_aleatorios_lote) <- crs(variables_completas)# Asegurar que los CRS coincidan
  crs(ocurrencias_E_lucunter) <- crs(variables_completas)  # Asegurar que los CRS coincidan
  
  # Calcular la distancia de estos nuevos puntos a las ocurrencias
  distancias <- distance(puntos_aleatorios_lote, ocurrencias_E_lucunter, pairwise = FALSE)
  
  # Filtrar aquellos que están a una distancia mayor que la mínima
  puntos_filtrados <- puntos_aleatorios_lote[distancias >= (distancia_minima_km * 1000)]
  
  # Agregar los puntos filtrados a nuestro conjunto final
  puntos_pseudoausencia <- if(is.null(puntos_pseudoausencia)) {
    puntos_filtrados
  } else {
    c(puntos_pseudoausencia, puntos_filtrados)
  }
  
  # Actualizar el conteo de puntos
  if (!is.null(puntos_pseudoausencia)) {
    num_puntos_actual <- nrow(puntos_pseudoausencia)
  }
}

# Submuestrear para obtener exactamente el número deseado
puntos_pseudoausencia_final <- puntos_pseudoausencia[1:num_pseudoausencias_deseadas]


# --- PASO 2: Extraer valores para las Pseudo-ausencias y limpiar ---
valores_pseudoausencias <- terra::extract(variables_completas, puntos_pseudoausencia_final, xy = TRUE) %>%
  dplyr::select(-ID) %>%
  drop_na()


# --- PASO 3: Crear el data.frame final de modelado (sdmdata) ---
# Crear el vector de presencia/fondo (1 para presencia, 0 para pseudo-ausencia)
presencia_ausencia <- c(rep(1, nrow(valores_ocurrencias_limpios)), rep(0, nrow(valores_pseudoausencias)))

# Combinar los valores y el vector 'pb'
datos_modelos <- data.frame(presencia_ausencia = presencia_ausencia, rbind(valores_ocurrencias_limpios, valores_pseudoausencias))

# --- PASO 4: Inspeccionar los resultados ---
head(datos_modelos)
tail(datos_modelos)
summary(datos_modelos)

# Conservar los valores extraídos para los modelos, si es necesario


write_csv(datos_modelos, here("datos_modelos.csv"))




