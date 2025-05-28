library(terra)
library(tidyverse)
library(corrplot)
library(car)

rm(list = ls())


# PREPARANDO ARCHIVOS

# VARIABLES BIO-ORACLE (usan .nc y _FillValue=-9999.9)
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")
salinidad_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\salinidad_media.nc")
temp_media <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_media.nc")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\velocidad_corriente_media.nc")
pH_medio <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\pH_medio.nc")

# VARIABLES MARSPEC (usan .adf y tienen NaN como no data)
batimetria <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\bathymetry_30s\\bathymetry_30s\\bathy_30s\\hdr.adf")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo05_30s\\hdr.adf")


NO_DATA_VALUE_BIOORACLE <- -9999.9

# Primero verificar el rango de valores

batimetria


#CAMBIAR VALORES -999.9

clorofila[clorofila == NO_DATA_VALUE_BIOORACLE] <- NA
salinidad_media[salinidad_media == NO_DATA_VALUE_BIOORACLE] <- NA
temp_media[temp_media == NO_DATA_VALUE_BIOORACLE] <- NA
velocidad_corriente[velocidad_corriente == NO_DATA_VALUE_BIOORACLE] <- NA
pH_medio[pH_medio == NO_DATA_VALUE_BIOORACLE] <- NA


#ASIGNAR CRS

#ASIGNAR CRS A BIO-ORACLE

Var_Bio_oracle <- c(clorofila,
                    salinidad_media,                
                    temp_media,                     
                    velocidad_corriente,
                    pH_medio)


#cambiar crs de bio-oracle a EPSG:4326

if(crs(Var_Bio_oracle) == "") {
  crs(Var_Bio_oracle) <- "EPSG:4326"
}

# CAMBIAR NOMBRES A ALGO MÁS SIMPLE
# Opción 1: Nombres simples
nombres_simples <- c("clorofila", 
                     "salinidad", 
                     "temperatura",
                     "velocidad_corriente",
                     "pH")

names(Var_Bio_oracle) <- nombres_simples

Var_Bio_oracle$clorofila


# revisar extencion de las capas
ext(Var_Bio_oracle)
ext(batimetria)

# solucionar con remuestreo bilinear para variables continuas

if(ext(Var_Bio_oracle) != ext(batimetria)) {
  Var_Bio_oracle_remues <- resample(Var_Bio_oracle, batimetria, method = "bilinear")
  print("Remuestreo completado para todas las capas BIO-ORACLE")
}


variables_raster <- c(Var_Bio_oracle_remues$clorofila,
                      Var_Bio_oracle_remues$salinidad,                
                      Var_Bio_oracle_remues$temperatura,                     
                      Var_Bio_oracle_remues$velocidad_corriente,
                      Var_Bio_oracle_remues$pH,
                      batimetria,
                      distancia_costa)


# cambiar nombres
nombres_capas <- c("clorofila",
                   "salinidad",
                   "temperatura",
                   "velocidad_corriente",
                   "pH",
                   "batimetria",
                   "distancia_costa") 

names(variables_raster) <- nombres_capas


#verificar valores
print("\n--- Verificación del stack final después de alineación ---")
print(stack_ambientales)

stack_completo_mask <- app(variables_raster, fun = function(x) all(!is.na(x)))
num_celdas_completas <- as.numeric(global(stack_completo_mask, fun = sum))

print(paste("Celdas con datos en TODAS las variables (para spatSample):", num_celdas_completas))


#RECORTAR CAPAS A TAMAÑO DE COLOMBIA

#cargar .shp de colombia

colombia_vector <- vect("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\COL_shp\\gadm36_COL_1.shp")
plot(colombia_vector)

e_colombia <- ext(-85, -65, -5, 15)

# cortar rasters con base al vector de colombia

variables_COL <- crop(variables_raster, ext(e_colombia))

plot(variables_COL$temperatura)


#volver una lista, para iterar
variables_COL <- as.list(variables_COL)
nombres_capas <- as.list(nombres_capas)

#GUARDAR CAPAS YA PROCESADAS PARA COLOMBIA

# Crear carpeta donde guardar los archivos
dir.create("BIO_MARS_limpias_COL", showWarnings = FALSE)

# Guardar cada raster
for (i in seq_along(variables_COL)) {
  writeRaster(
    variables_COL[[i]],
    filename = file.path("BIO_MARS_limpias_COL", paste0(nombres_capas[[i]], ".tif")),
    overwrite = TRUE
  )
}

#limpiar todo menos lo necesario


file.choose()

batimetria <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\batimetria.tif")
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\clorofila.tif")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\distancia_costa.tif")
pH <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\pH.tif")
salinidad <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\salinidad.tif")
temperatura <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\temperatura.tif")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\velocidad_corriente.tif")

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


# VARIABLES A ELIMINAR

rm(productividad_primaria, curvatura_perfil, curvatura_plana, pendiente_batimetrica, salinidad_rango)

rm(aspecto_EO, aspecto_NS, direccion_corriente, pH_rango, temp_rango)

rm(concavidad)




# tratar valores noNA problematicos en las capas

distancia_costa[is.nan(distancia_costa)] <- NA

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

# eliminar oxigeno_disuelto por un VIF de 8.612113

variables_vif_df_sin_oxigeno <- variables_vif_df %>%
  select(-oxigeno_disuelto)

modelo_lm_sin_oxigeno <- lm(batimetria ~ ., data = variables_vif_df_sin_oxigeno)

resultado_vif_sin_oxigeno <- vif(modelo_lm_sin_oxigeno)


