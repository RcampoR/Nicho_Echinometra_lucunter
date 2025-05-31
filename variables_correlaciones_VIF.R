library(terra)
library(tidyverse)
library(corrplot)
library(car)


batimetria <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\batimetria.tif")
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\clorofila.tif")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\distancia_costa.tif")
pH <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\pH.tif")
salinidad <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\salinidad.tif")
temperatura <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\temperatura.tif")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\velocidad_corriente.tif")

#CORRELACIÓN DE CAPAS
variables_raster  <- c(batimetria, clorofila, distancia_costa, pH, 
                       salinidad, temperatura, velocidad_corriente)

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
                                salinidad, temperatura, velocidad_corriente)

variables_vif_df <- as.data.frame(variables_raster_para_vif)


#modelo lm arbitrario
modelo_vif_sencillo <- lm(batimetria ~ ., data = variables_vif_df)

summary(modelo_vif_sencillo)

# 5. Calcular y mostrar los VIFs
resultados_vif <- vif(modelo_vif_sencillo)

resultados_vif


# Eliminar Salinidad
variables_vif_df_sin_salinidad <- variables_vif_df %>%
  select(-salinidad)

modelo_lm_sin_salinidad <- lm(batimetria ~ ., data = variables_vif_df_sin_salinidad)

# Calcular y mostrar los VIFs del nuevo modelo
resultado_vif_sin_salinidad <- vif(modelo_lm_sin_salinidad)
print("--- Resultados VIF sin Salinidad ---")
print(resultado_vif_sin_salinidad)


resultado_vif_sin_salinidad

