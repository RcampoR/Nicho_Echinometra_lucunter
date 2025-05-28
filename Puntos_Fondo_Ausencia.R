library(terra)
library(dplyr)

names(marspec_batimetria) <- batimetria



#crear stack
variables_ambientales_para_maxent <- c(batimetria, clorofila, distancia_costa,
                                       pH_medio, salinidad_media, temp_media,
                                       velocidad_corriente)

stack_ambientales <- rast(variables_ambientales_para_maxent)


print("=== VERIFICACIÓN DE VARIABLES RECARGADAS ===")
variables_lista <- list(
  batimetria = batimetria,
  clorofila = clorofila,
  distancia_costa = distancia_costa,
  pH_medio = pH_medio,
  salinidad_media = salinidad_media,
  temp_media = temp_media,
  velocidad_corriente = velocidad_corriente
)




num_puntos_fondo <- 10000 # O el número que consideres adecuado
set.seed(42) # Para reproducibilidad

puntos_fondo <- spatSample(stack_ambientales, num_puntos_fondo,
                           "random", na.rm = TRUE, as.points = TRUE)


# Verificar el número de celdas válidas (CORREGIDO)
celdas_validas <- global(stack_ambientales, fun = function(x) sum(!is.na(x)))
print(celdas_validas)


