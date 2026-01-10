library(terra) 
library(tidyverse)
library(corrplot) 
library(car) 
library(here)
library(factoextra) 

#limpiar entorno
rm(list = ls())



# PREPARANDO ARCHIVOS

## Variables de BIO-ORACLE

# Directorio base para las variables de BIO-ORACLE
ruta_bio_oracle <- here("..", "..", "Variables_BIOORACLE")

# Clorofila
clorofila_media <- rast(here(ruta_bio_oracle, "clorofila.nc"))

# Salinidad
salinidad_media <- rast(here(ruta_bio_oracle, "salinidad_media.nc"))
salinidad_rango <- rast(here(ruta_bio_oracle, "salinidad_rango.nc"))

# Temperatura
temperatura_media <- rast(here(ruta_bio_oracle, "temperatura_media.nc"))
temperatura_rango <- rast(here(ruta_bio_oracle, "temperatura_rango.nc"))

# Corriente
velocidad_corriente_media <- rast(here(ruta_bio_oracle, "velocidad_corriente_media.nc"))
direccion_corriente_media <- rast(here(ruta_bio_oracle, "direccion_corriente_media.nc"))

# Oxígeno disuelto
oxigeno_disuelto_medio <- rast(here(ruta_bio_oracle, "oxigeno_disuelto_medio.nc"))

# pH
ph_medio <- rast(here(ruta_bio_oracle, "pH_medio.nc"))
ph_rango <- rast(here(ruta_bio_oracle, "pH_rango.nc"))

# Productividad primaria
productividad_primaria_media <- rast(here(ruta_bio_oracle, "productividad_primaria_media.nc"))

  
  ## Variables de MARSPEC
  
  # Directorio base para las variables de MARSPEC
  ruta_marspec <- here("..", "..", "MARSPEC")

## Variables de MARSPEC

# Batimetría
marspec_batimetria <- here(ruta_marspec, "bathymetry_30s", "bathymetry_30s", "bathy_30s", "hdr.adf") %>% rast()

# Biogeo 1: Aspecto Este/Oeste (sin(aspecto en radianes))
marspec_aspecto_este_oeste <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo01_30s", "hdr.adf") %>% rast()

# Biogeo 2: Aspecto Norte/Sur (cos(aspecto en radianes))
marspec_aspecto_norte_sur <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo02_30s", "hdr.adf") %>% rast()

# Biogeo 3: Curvatura del plan
marspec_curvatura_plan <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo03_30s", "hdr.adf") %>% rast()

# Biogeo 4: Perfil de curvatura
marspec_perfil_curvatura <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo04_30s", "hdr.adf") %>% rast()

# Biogeo 5: Distancia a la costa
marspec_distancia_costa <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo05_30s", "hdr.adf") %>% rast()

# Biogeo 6: Pendiente batimétrica (Slope)
marspec_pendiente_batimetrica <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo06_30s", "hdr.adf") %>% rast()

# Biogeo 7: Concavidad
marspec_concavidad <- here(ruta_marspec, "biogeo01_07_30s", "biogeo01_07_30s", "biogeo07_30s", "hdr.adf") %>% rast()

#CORREGIR NA en Bio-Oracle
# Valor NA en bio-oracle

NO_DATA_VALUE_BIOORACLE <- -9999.9

# Primero verificar el rango de valores

marspec_batimetria


#CAMBIAR VALORES -999.9

# Clorofila
clorofila_media[clorofila_media == NO_DATA_VALUE_BIOORACLE] <- NA

# Salinidad
salinidad_media[salinidad_media == NO_DATA_VALUE_BIOORACLE] <- NA
salinidad_rango[salinidad_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Temperatura
temperatura_media[temperatura_media == NO_DATA_VALUE_BIOORACLE] <- NA
temperatura_rango[temperatura_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Corriente
velocidad_corriente_media[velocidad_corriente_media == NO_DATA_VALUE_BIOORACLE] <- NA
direccion_corriente_media[direccion_corriente_media == NO_DATA_VALUE_BIOORACLE] <- NA

# Oxígeno disuelto
oxigeno_disuelto_medio[oxigeno_disuelto_medio == NO_DATA_VALUE_BIOORACLE] <- NA

# pH
ph_medio[ph_medio == NO_DATA_VALUE_BIOORACLE] <- NA
ph_rango[ph_rango == NO_DATA_VALUE_BIOORACLE] <- NA 

# Productividad primaria
productividad_primaria_media[productividad_primaria_media == NO_DATA_VALUE_BIOORACLE] <- NA




#ASIGNAR CRS

#ASIGNAR CRS A BIO-ORACLE

Var_Bio_oracle <- c(
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
  productividad_primaria_media
)


#cambiar crs de bio-oracle a EPSG:4326

if(crs(Var_Bio_oracle) == "") {
  crs(Var_Bio_oracle) <- "EPSG:4326"
}


# revisar extencion de las capas
ext(Var_Bio_oracle)
ext(marspec_batimetria)

# solucionar con remuestreo bilinear para variables continuas

if(ext(Var_Bio_oracle) != ext(marspec_batimetria)) {
  Var_Bio_oracle_remues <- resample(Var_Bio_oracle, marspec_batimetria, method = "bilinear")
  print("Remuestreo completado para todas las capas BIO-ORACLE")
}


variables_raster <- c(
  Var_Bio_oracle_remues$chl_mean,
  Var_Bio_oracle_remues$so_mean,
  Var_Bio_oracle_remues$so_range,
  Var_Bio_oracle_remues$thetao_mean,
  Var_Bio_oracle_remues$thetao_range,
  Var_Bio_oracle_remues$sws_mean,
  Var_Bio_oracle_remues$swd_mean,
  Var_Bio_oracle_remues$o2_mean,
  Var_Bio_oracle_remues$ph_mean,
  Var_Bio_oracle_remues$ph_range,
  Var_Bio_oracle_remues$phyc_mean,
  marspec_batimetria,
  marspec_aspecto_este_oeste,
  marspec_aspecto_norte_sur,
  marspec_curvatura_plan,
  marspec_perfil_curvatura,
  marspec_distancia_costa,
  marspec_pendiente_batimetrica,
  marspec_concavidad
  )


# cambiar nombres
nombres_capas <- c( "clorofila_media",
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
                    "concavidad") 

names(variables_raster) <- nombres_capas


#RECORTAR CAPAS A TAMAÑO DEL CARIBE cerca a colombia

vector_caribe <- vect(here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp"))

# Unir todos los polígonos en uno solo (disolverlos)
area_estudio_caribe <- aggregate(vector_caribe, dissolve = TRUE)

 # cortar rasters con base al vector del caribe 

variables_caribe <- crop(variables_raster, ext(area_estudio_caribe)+0.01)

plot(variables_caribe$temperatura_media)


# enmascarar con el vector del caribe

variables_enmascaradas_caribe <- mask(variables_caribe, area_estudio_caribe)
plot(variables_enmascaradas_caribe)

#volver una lista, para iterar
variables_enmascaradas_caribe <- as.list(variables_enmascaradas_caribe)
nombres_capas <- as.list(nombres_capas)

#guardar variables enmascaradas caribe
# Crear carpeta donde guardar los archivos
dir.create(here("..", "..", "BIO_MARS_caribe_completas"), showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_enmascaradas_caribe)) {
  writeRaster(
    variables_enmascaradas_caribe[[i]],
    filename = here("..", "..", "BIO_MARS_caribe_completas", paste0(nombres_capas[[i]], ".tif")), overwrite = TRUE)
  
  
}




# mascara de 50 metros batimetria 


# mascara batimetria

batimetria <- classify(variables_enmascaradas_caribe$batimetria, cbind(-Inf, -50, NA), right = FALSE)

plot(batimetria)

# enmascarar
variables_a_enmascarar <- c(
  variables_enmascaradas_caribe$clorofila_media,
  variables_enmascaradas_caribe$salinidad_media,
  variables_enmascaradas_caribe$salinidad_rango,
  variables_enmascaradas_caribe$temperatura_media,
  variables_enmascaradas_caribe$temperatura_rango,
  variables_enmascaradas_caribe$velocidad_corriente_media,
  variables_enmascaradas_caribe$direccion_corriente_media,
  variables_enmascaradas_caribe$oxigeno_disuelto_medio,
  variables_enmascaradas_caribe$ph_medio,
  variables_enmascaradas_caribe$ph_rango,
  variables_enmascaradas_caribe$productividad_primaria_media,
  variables_enmascaradas_caribe$aspecto_este_oeste,
  variables_enmascaradas_caribe$aspecto_norte_sur,
  variables_enmascaradas_caribe$curvatura_plan,
  variables_enmascaradas_caribe$perfil_curvatura,
  variables_enmascaradas_caribe$distancia_costa,
  variables_enmascaradas_caribe$pendiente_batimetrica,
  variables_enmascaradas_caribe$concavidad
)

variables_enmascaradas_50m <- mask(variables_a_enmascarar, batimetria)

plot(variables_enmascaradas_50m)

#concatenar variables ya procesadas

variables_limpias <- c(
  variables_enmascaradas_50m$clorofila_media,      
  variables_enmascaradas_50m$salinidad_media,
  variables_enmascaradas_50m$salinidad_rango,
  variables_enmascaradas_50m$temperatura_media,
  variables_enmascaradas_50m$temperatura_rango,
  variables_enmascaradas_50m$velocidad_corriente_media,
  variables_enmascaradas_50m$direccion_corriente_media,
  variables_enmascaradas_50m$oxigeno_disuelto_medio,
  variables_enmascaradas_50m$ph_medio,
  variables_enmascaradas_50m$ph_rango,
  variables_enmascaradas_50m$productividad_primaria_media,
  batimetria, 
  variables_enmascaradas_50m$aspecto_este_oeste,
  variables_enmascaradas_50m$aspecto_norte_sur,
  variables_enmascaradas_50m$curvatura_plan,
  variables_enmascaradas_50m$perfil_curvatura,
  variables_enmascaradas_50m$distancia_costa,
  variables_enmascaradas_50m$pendiente_batimetrica,
  variables_enmascaradas_50m$concavidad
)

#verificar nuevamente
plot(variables_limpias)

#volver una lista, para iterar
variables_caribe <- as.list(variables_limpias)
nombres_capas <- as.list(nombres_capas)

#GUARDAR CAPAS YA PROCESADAS PARA COLOMBIA

# Crear carpeta donde guardar los archivos
dir.create(here("..", "..", "BIO_MARS_limpias_caribe_50m"), showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_caribe)) {
  writeRaster(
    variables_caribe[[i]],
    filename = here("..", "..", "BIO_MARS_limpias_caribe_50m", paste0(nombres_capas[[i]], ".tif")), overwrite = TRUE)
   
  
}




#limpiar entorno
rm(list = ls())

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
         method = "circle",
         type = "upper",    
         tl.col = "black",  
         tl.srt = 45,       
         diag = FALSE,      
         col = COL2("RdBu", 200)) 

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

# ANALISIS DE COMPONENTES PRINCIPALES

# medias y desviaciones del presente para normalizar las futuras
medias <- global(variables_completas, mean, na.rm = TRUE)
sds    <- global(variables_completas, sd, na.rm = TRUE)

medias
sds


# guardar medias y desviaciones estandar
write.csv(medias, here("medias_presente.csv"), row.names = TRUE)
write.csv(sds, here("sds_presente.csv"), row.names = TRUE)


# PCA de las variables seleccionadas en area de estudio

pca_area_estudio <- prcomp(variables_completas, center = TRUE, scale. = TRUE)

summary(pca_area_estudio)

# 1. Extraer las cargas de las primeras 5 componentes
loadings <- as.data.frame(pca_area_estudio$rotation[, 1:5])

# 2. Agregar columna con nombres de las variables
loadings$Variable <- rownames(loadings)

# 3. Reorganizar el dataframe en formato largo
loadings_long <- loadings %>%
  pivot_longer(cols = starts_with("PC"),
               names_to = "Componente",
               values_to = "Peso")

# 4. Graficar
ggplot(loadings_long, aes(x = Variable, y = Peso, fill = Componente)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Componente ~ ., scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Peso de la variable",
       x = "Variables ambientales",
       title = "Cargas de las variables en las primeras 5 componentes")

# 2. Proyectar fondo y ocurrencias al espacio PCA 

fondo_proj <- as.data.frame(predict(pca_area_estudio, newdata = as.data.frame(variables_completas))) 
fondo_proj$grupo <- "Fondo" 
occs_proj <- as.data.frame(predict(pca_area_estudio, newdata = valores_ocurrencias_limpios[ , -c(9, 10)])) 
occs_proj$grupo <- "Presencias"


# Visualizar la varianza explicada por cada componente (gráfico de codo)
fviz_eig(pca_area_estudio, addlabels = TRUE, ylim = c(0, 50))

var_exp <- round(sum(summary(pca_area_estudio)$importance[2, 1:2]) * 100, 1)

# --- 1. Biplot base ---
p_biplot <- fviz_pca_biplot(
  pca_area_estudio,
  
  # Puntos del fondo
  geom.ind = "point",
  pointshape = 21,
  pointsize = 1,
  fill.ind = "gray86",
  col.ind = "cornflowerblue",
  alpha.ind = 0.3,
  
  # Flechas y etiquetas de variables
  repel = TRUE,
  labelsize = 5,
  col.var = "black",
  geom.var = c("arrow", "text"),
  arrowsize = 1,
  
  # Ejes con % de varianza
  xlab = paste0("PC1 (", round(summary(pca_area_estudio)$importance[2,1]*100, 1), "%)"),
  ylab = paste0("PC2 (", round(summary(pca_area_estudio)$importance[2,2]*100, 1), "%)")
) +
  theme_classic() +
  theme(
    # Texto de ejes
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, color = "gray30"),
    
    # Sin título ni subtítulo
    plot.title    = element_blank(),
    plot.subtitle = element_blank(),
    
    # Estética de grillas
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    
    # Leyenda clara
    legend.position = "right",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 12)
  ) + 
  labs(caption = paste0("Varianza total explicada (PC1 + PC2): ", var_exp, "%"))

# --- 2. Añadir ocurrencias ---
p_biplot_final <- p_biplot +
  geom_point(data = occs_proj,
             aes(x = PC1, y = PC2, color = grupo),
             size = 2, alpha = 0.8,
             show.legend = FALSE) +
  scale_color_manual(values = c("Presencias" = "red"))

# Mostrar
p_biplot_final




# GUARDAR PCA

ggsave(
  filename = here("..", "..", "GRAFICAS", "PCA_ocurrencias.png"),
  width = 10, height = 6, dpi = 1000
)

dev.off()


# guardar valores de ocurrencias

write_csv(valores_ocurrencias_limpios, here("valores_extraidos_E_lucunter_caribe.csv"))


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







