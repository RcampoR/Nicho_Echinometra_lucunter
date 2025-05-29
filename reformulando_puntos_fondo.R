library(terra)
library(dplyr)


#CARGAR VARIABLES
batimetria <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\batimetria.tif")
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\clorofila.tif")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\distancia_costa.tif")
pH <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\pH.tif")
salinidad <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\salinidad.tif")
temperatura <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\temperatura.tif")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_COL\\velocidad_corriente.tif")


# Lista de objetos raster que vamos a procesar
layers_to_process <- list(
  clorofila = clorofila,
  salinidad = salinidad,
  temperatura = temperatura,
  velocidad_corriente = velocidad_corriente,
  pH  = pH,
  batimetria = batimetria,
  distancia_costa = distancia_costa
)


# Crear una lista de rasters alineados
aligned_rasters <- list()

# Iterar sobre los nombres de las variables en rasters_list para asegurar que usamos los objetos actualizados
for (nombre in names(layers_to_process)) {
  r <- get(nombre) # Obtiene el raster del entorno global
  
  
  # Remuestrear a la resolución y extensión de la batimetría
  # 'bilinear' para datos continuos. 'near' para datos categóricos (no aplica aquí).
  cat(paste0("\nAlineando y remuestreando ", nombre, " a la referencia (batimetria)...\n"))
  r_aligned <- resample(r, batimetria, method = "bilinear")
  layers_to_process[[nombre]] <- r_aligned
}

# Re-crear el stack con las capas alineadas
stack_ambientales <- rast(layers_to_process)

print("\n--- Verificación del stack final después de alineación ---")
print(stack_ambientales)


#crear puntos de fondo
  set.seed(42)
  puntos_fondo_crudos <- spatSample(stack_ambientales, 830,
                                    "random", na.rm = TRUE, as.points = TRUE)
  
  
