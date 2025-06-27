library(terra)
library(tidyverse)
library(corrplot)
library(car)
library(tmap)
#limpiar entorno
rm(list = ls())
gc()

pack_variables_base <- "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_convexo_50m"


batimetria <- rast(file.path(pack_variables_base, "batimetria.tif"))
clorofila <- rast(file.path(pack_variables_base, "clorofila.tif"))
distancia_costa <- rast(file.path(pack_variables_base, "distancia_costa.tif"))
pH <- rast(file.path(pack_variables_base, "pH.tif"))
temperatura <- rast(file.path(pack_variables_base, "temperatura.tif"))
salinidad <- rast(file.path(pack_variables_base, "salinidad.tif"))

# En otros modelos la velocidad de la corriente no aporta nada, mejor dejar salinidad
#####velocidad_corriente <- rast(file.path(pack_variables_base, "velocidad_corriente.tif"))


#BASE ECHINOMETRA SUBMUESTREADA
ocurrencias_E_lucunter <- readr::read_delim("BD_E_lucunter_submuestreado_Caribe.csv") %>% 
  transmute(lon = decimalLongitude,
            lat = decimalLatitude) %>% 
  vect()



#CORRELACIÓN DE CAPAS
variables_raster  <- c(batimetria, clorofila, distancia_costa, pH, 
                       salinidad, temperatura)

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


# Ahora, asegurémonos de que todas sean numéricas.
# Aunque SpatRaster ya son numéricos en esencia, esto no está de más
# para asegurar que R los trate como tales en el dataframe.
batimetria <- as.numeric(batimetria)
clorofila <- as.numeric(clorofila)
distancia_costa <- as.numeric(distancia_costa) # ¡Crucial!
pH <- as.numeric(pH)
salinidad <- as.numeric(salinidad)
temperatura <- as.numeric(temperatura)
velocidad_corriente <- as.numeric(velocidad_corriente)


#calcular VIF  a las variables restantes

#haciendo el data.frame
variables_raster_para_vif <-  c(batimetria, clorofila, distancia_costa, pH, 
                                salinidad, temperatura)

variables_vif_df <- as.data.frame(variables_raster_para_vif)


#modelo lm arbitrario
modelo_vif_sencillo <- lm(batimetria ~ ., data = variables_vif_df)

summary(modelo_vif_sencillo)

# 5. Calcular y mostrar los VIFs
resultados_vif <- vif(modelo_vif_sencillo)

resultados_vif


#LIMPIAR ENTORNO 

rm(list = ls())
gc()

# cargar nuevamente las variables del caribe 50m

variables_limpias_caribe <- c(batimetria,
                              clorofila,
                              distancia_costa,
                              pH,
                              temperatura,
                              salinidad)


#CREANDO PUNTOS DE FONDO
set.seed(456)
puntos_fondo_crudos <- spatSample(variables_limpias_caribe, 2160,
                                  "random", na.rm = TRUE, as.points = TRUE)

# TRATANDO EL SESGO DE MUESTREO DEL FONDO

# se crea un raster en base al vector 
raster_pf <- rast(puntos_fondo_crudos)

# se establece la resolución (se recomienda en base al home range)

res(raster_pf) <- 0.009 # 1 km

# se expanden las celdas 

raster_pf <- extend(raster_pf, ext(raster_pf)+0.1)

set.seed(456)


pf_submuestreados <- spatSample(puntos_fondo_crudos, size= 1, "random", strata=raster_pf)

# guardar capa vectorial
writeVector(puntos_fondo_crudos, "pf_crudos_convexo_submuestreados.shp")

