library(terra)
library(dplyr)


#CARGAR VARIABLES
batimetria <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\batimetria.tif")
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\clorofila.tif")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\distancia_costa.tif")
pH <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\pH.tif")
temperatura <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\temperatura.tif")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\BIO_MARS_limpias_caribe_COL_50m\\velocidad_corriente.tif")



variables_limpias_caribe <- c(batimetria,
                              clorofila,
                              distancia_costa,
                              pH,
                              temperatura,
                              velocidad_corriente)
  
  
#crear puntos de fondo
  set.seed(42)
  puntos_fondo_crudos <- spatSample(variables_limpias_caribe, 740,
                                    "random", na.rm = TRUE, as.points = TRUE)
  
plot(puntos_fondo_crudos)
