library(terra)
library(tidyverse)
library(corrplot)
library(car)

rm(list = ls())

#VAMOS AL SIGUIENTE PASO


# PREPARANDO ARCHIVOS

# ruta de la carpeta
ruta_variables <- "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_remuestreados_COL"

# lista de direcciones de los .tif
archivos_tif <- list.files(ruta_variables, pattern = "\\.tif$", full.names = TRUE)

# nombres para las variables
nombres <- tools::file_path_sans_ext(basename(archivos_tif))

# cargar archivos al entorno
for(i in 1:length(archivos_tif)) {
  assign(nombres[i], rast(archivos_tif[i]))
}

# Primero verificar el rango de valores

batimetria


######## ENMASCARAR VARIABLES EN BASE A -50 METROS DE BATIMETRIA #######

# mascara batimetria

batimetria <- classify(batimetria, cbind(-Inf, -50, NA), right = FALSE)

plot(batimetria)

# Si hay alguna variable que NO deba ser enmascarada, deberás excluirla de esta lista.
variables_a_enmascarar <- setdiff(nombres, c("batimetria")) # Excluye la batimetría misma de la lista a enmascarar

# Iterar sobre las variables y aplicar la máscara
for(var_name in variables_a_enmascarar) {
  current_raster <- get(var_name) # Obtener el objeto raster por su nombre
  masked_raster <- mask(current_raster, batimetria) # Aplicar la máscara
  assign(var_name, masked_raster)  # Asignar el raster enmascarado a las variables ya existentes
  
  # Opcional: Si quieres crear una nueva variable con _masked
  # assign(paste0(var_name, "_masked"), masked_raster)
}

# verificar mascaras

plot(pH_medio)
plot(salinidad_rango)
plot(distancia_costa)
plot(temp_rango)
plot(temp_media)



##################################################################

variables_raster_mascara <- c(aspecto_EO, aspecto_NS, batimetria, clorofila, 
                              concavidad, curvatura_perfil, curvatura_plana, 
                              direccion_corriente, distancia_costa, oxigeno_disuelto, 
                              pendiente_batimetrica, pH_medio, pH_rango, 
                              productividad_primaria, salinidad_media, salinidad_rango, 
                              temp_media, temp_rango, velocidad_corriente)




#CORRELACIÓN DE CAPAS

#correlacion de pearson con terra
correlacion_capas <- layerCor(variables_raster_mascara,"pearson", na.rm = TRUE)


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

#extraer valores naltamente correlacionados
correlación_df %>% 
  rownames_to_column(var = "VARIABLES1") %>% 
pivot_longer(cols = -VARIABLES1,
             values_to = "CORRELACION",
             names_to = "VARIABLES2") %>% 
  filter(CORRELACION >= 0.7 | CORRELACION <= -0.7) %>%
  filter(VARIABLES1 != VARIABLES2) %>% 
  view()


# VARIABLES A ELIMINAR

rm(productividad_primaria, curvatura_perfil, curvatura_plana, pendiente_batimetrica, salinidad_rango)

rm(aspecto_EO, aspecto_NS, direccion_corriente, pH_rango, temp_rango)

rm(concavidad)

ls()

print(distancia_costa) # Mira su clase y nombre
print(unique(values(distancia_costa))) 



# tratar valores noNA problematicos en las capas

distancia_costa[is.nan(distancia_costa)] <- NA

# Ahora, asegurémonos de que todas sean numéricas.
# Aunque SpatRaster ya son numéricos en esencia, esto no está de más
# para asegurar que R los trate como tales en el dataframe.
batimetria <- as.numeric(batimetria)
clorofila <- as.numeric(clorofila)
distancia_costa <- as.numeric(distancia_costa) # ¡Crucial!
oxigeno_disuelto <- as.numeric(oxigeno_disuelto)
pH_medio <- as.numeric(pH_medio)
salinidad_media <- as.numeric(salinidad_media)
temp_media <- as.numeric(temp_media)
velocidad_corriente <- as.numeric(velocidad_corriente)


#calcular VIF  a las variables restantes

#haciendo el data.frame
variables_raster_para_vif <- c(batimetria, clorofila, distancia_costa,
                               oxigeno_disuelto, pH_medio, salinidad_media,
                               temp_media, velocidad_corriente)

variables_vif_df <- as.data.frame(variables_raster_para_vif)


#modelo lm arbitrario
modelo_vif_sencillo <- lm(batimetria ~ ., data = variables_vif_df)

summary(modelo_vif_sencillo)

# 5. Calcular y mostrar los VIFs
resultados_vif <- vif(modelo_vif_sencillo)

resultados_vif

# eliminar oxigeno_disuelto por un VIF de 8.612113

variables_vif_df_sin_oxigeno <- variables_vif_df %>%
  select(-oxigeno_disuelto)

modelo_lm_sin_oxigeno <- lm(batimetria ~ ., data = variables_vif_df_sin_oxigeno)

resultado_vif_sin_oxigeno <- vif(modelo_lm_sin_oxigeno)


