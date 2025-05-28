library(terra)
library(dplyr)

# --- 1. Define el valor de no data para las variables Bio-Oracle ---
# Es CRUCIAL que esta línea se ejecute.
NO_DATA_VALUE_BIOORACLE <- -9999.9

# --- 2. Cargar las variables ---

# VARIABLES BIO-ORACLE (usan .nc y _FillValue=-9999.9)
clorofila <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\clorofila.nc")
salinidad_media <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\salinidad_media.nc")
temp_media <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\temperatura_media.nc")
velocidad_corriente <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\velocidad_corriente_media.nc")
pH_medio <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\Variables_BIOORACLE\\pH_medio.nc")

# VARIABLES MARSPEC (usan .adf y tienen NaN como no data)
batimetria <- rast( "C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\bathymetry_30s\\bathymetry_30s\\bathy_30s\\hdr.adf")
distancia_costa <- rast("C:\\Proyecto_biologicos\\Proyectos Actuales\\Nicho_E_lucunter\\MARSPEC\\biogeo01_07_30s\\biogeo01_07_30s\\biogeo05_30s\\hdr.adf")

# --- 3. Asegurar que todas las capas tienen el mismo CRS (EPSG:4326) y simplificar nombres ---
# Esto es esencial para que `c()` y `stack()` funcionen bien, y para la alineación.

# Lista de objetos raster que vamos a procesar
layers_to_process <- list(
  clorofila = clorofila,
  salinidad_media = salinidad_media,
  temp_media = temp_media,
  velocidad_corriente = velocidad_corriente,
  pH_medio = pH_medio,
  batimetria = batimetria,
  distancia_costa = distancia_costa
)

for (name_obj in names(layers_to_process)) {
  r <- layers_to_process[[name_obj]]
  # Asignar CRS si está vacío
  if (crs(r) == "") {
    crs(r) <- "EPSG:4326"
    assign(name_obj, r, envir = .GlobalEnv) # Actualizar el objeto global
  }
  # Renombrar capas a nombres simples si tienen varname complejo
  if (nlyr(r) == 1 && names(r) != name_obj) {
    names(r) <- name_obj
    assign(name_obj, r, envir = .GlobalEnv) # Actualizar el objeto global
  }
}

# Volver a cargar los objetos en la lista de rasters_list para el diagnóstico y el stack final
# Asegúrate de usar los nombres de objeto corregidos si los modificaste (e.g., pH_medio vs pH)
rasters_list <- list(
  batimetria = batimetria,
  clorofila = clorofila,
  distancia_costa = distancia_costa,
  pH_medio = pH_medio, # Asegúrate que este sea el nombre correcto del objeto
  salinidad_media = salinidad_media, # Asegúrate que este sea el nombre correcto del objeto
  temp_media = temp_media, # Asegúrate que este sea el nombre correcto del objeto
  velocidad_corriente = velocidad_corriente
)

# --- 4. DIAGNÓSTICO Y LIMPIEZA DE VALORES NO DATA ---
print("--- Diagnóstico de valores y NAflag en cada raster ---")

for (nombre in names(rasters_list)) {
  r <- rasters_list[[nombre]]
  cat(paste0("\nVariable: ", nombre, "\n"))
  cat(paste0("  Tiene valores: ", hasValues(r), "\n"))
  
  if (hasValues(r)) {
    na_flag_val <- NAflag(r)
    cat(paste0("  NAflag reconocido por terra: ", na_flag_val, "\n"))
    
    # Verifica si el NAflag es el de Bio-Oracle y si hay que convertir
    if (!is.na(na_flag_val) && na_flag_val == NO_DATA_VALUE_BIOORACLE) {
      cat("  Terra ya reconoce el _FillValue de Bio-Oracle como NA. No es necesaria una conversión explícita.\n")
    } else if (is.na(na_flag_val) && as.logical(global(r, fun = function(x) any(x == NO_DATA_VALUE_BIOORACLE, na.rm = TRUE)))) {
      # Esta rama se ejecuta si na_flag_val es NA (NaN) y hay valores de Bio-Oracle NO_DATA_VALUE
      count_fill_value <- global(r, fun = function(x) sum(x == NO_DATA_VALUE_BIOORACLE, na.rm = TRUE))
      if (as.numeric(count_fill_value) > 0) {
        cat(paste0("  Celdas con _FillValue (", NO_DATA_VALUE_BIOORACLE, "): ", as.numeric(count_fill_value), "\n"))
        r[r == NO_DATA_VALUE_BIOORACLE] <- NA
        cat("  _FillValue convertido a NA explícitamente.\n")
        assign(nombre, r, envir = .GlobalEnv) # Actualiza el objeto en el entorno global
      } else {
        cat("  NAflag es NaN (ok para Marspec) y no se encontraron _FillValue de Bio-Oracle.\n")
      }
    } else {
      # Esto es para los casos donde NAflag no es -9999.9 y no es NaN, o si es NaN y no hay -9999.9
      # Esto es lo que esperas para MARSPEC y si Bio-Oracle ya se manejó.
      cat("  No se encontró _FillValue de Bio-Oracle o NAflag ya es NaN (ok para Marspec/ya manejado).\n")
    }
    
    # Finalmente, verifica el número de celdas válidas (no-NA) en el raster actual
    celdas_validas_final <- global(r, fun = function(x) sum(!is.na(x)))
    cat(paste0("  Celdas válidas (no-NA) al final del diagnóstico: ", as.numeric(celdas_validas_final), "\n"))
    
  } else {
    cat("  ¡Advertencia! Este raster no tiene valores válidos al cargarlo.\n")
    cat("  Esto indica que el archivo fuente ya está vacío o corrupto.\n")
    cat("  Necesitas verificar el archivo .nc o .adf original.\n")
  }
}

# --- 5. Crear el stack ambiental ---
# Determinar la extensión de referencia (ej. de batimetria)
# Las resoluciones de tus datos son: Bio-Oracle (0.05), MARSPEC (0.008333333). Son diferentes.
# Remuestreamos Bio-Oracle a la resolución y extensión de MARSPEC.
# Asegúrate de que batimetria (MARSPEC) sea la capa de referencia.
ext_ref <- ext(batimetria)
res_ref <- res(batimetria)
crs_ref <- crs(batimetria) # Asegurarse de usar el CRS de referencia

# Crear una lista de rasters alineados
aligned_rasters <- list()

# Iterar sobre los nombres de las variables en rasters_list para asegurar que usamos los objetos actualizados
for (nombre in names(rasters_list)) {
  r <- get(nombre) # Obtiene el raster del entorno global
  
  # Asegurarse de que el CRS es el mismo antes de remuestrear
  if (crs(r) != crs_ref) {
    cat(paste0("  Proyectando ", nombre, " a CRS de la batimetria (", crs_ref, ").\n"))
    r <- project(r, crs_ref)
  }
  
  # Remuestrear a la resolución y extensión de la batimetría
  # 'bilinear' para datos continuos. 'near' para datos categóricos (no aplica aquí).
  cat(paste0("\nAlineando y remuestreando ", nombre, " a la referencia (batimetria)...\n"))
  r_aligned <- resample(r, batimetria, method = "bilinear")
  aligned_rasters[[nombre]] <- r_aligned
}

# Re-crear el stack con las capas alineadas
stack_ambientales <- rast(aligned_rasters)

print("\n--- Verificación del stack final después de alineación ---")
print(stack_ambientales)

# Contar celdas con datos completos en el stack final
stack_completo_mask <- app(stack_ambientales, fun = function(x) all(!is.na(x)))
num_celdas_completas <- as.numeric(global(stack_completo_mask, fun = sum))

print(paste("Celdas con datos en TODAS las variables (para spatSample):", num_celdas_completas))

# --- 6. Generar puntos de fondo (si es posible) ---
if (num_celdas_completas > 0) {
  num_puntos_fondo <- min(10000, floor(num_celdas_completas * 0.95)) # Usa 95% para ser seguro
  cat(paste("\nGenerando", num_puntos_fondo, "puntos de fondo...\n"))
  
  set.seed(42)
  puntos_fondo_crudos <- spatSample(stack_ambientales, num_puntos_fondo,
                                    "random", na.rm = TRUE, as.points = TRUE)
  
  print(paste("✓ Puntos de fondo generados exitosamente:", nrow(puntos_fondo_crudos)))
  
  # Extraer valores para los puntos de fondo y filtrar por batimetría
  valores_fondo_crudos_df <- as.data.frame(extract(stack_ambientales, puntos_fondo_crudos)) %>%
    dplyr::bind_cols(terra::geom(puntos_fondo_crudos)[, c("x", "y")])
  
  # Asegúrate de que la columna 'ID' no esté si no la necesitas
  if ("ID" %in% names(valores_fondo_crudos_df)) {
    valores_fondo_crudos_df <- valores_fondo_crudos_df %>% dplyr::select(-ID)
  }
  
  # Filtra los puntos de fondo basándose en la batimetría (-50 metros o más somero)
  puntos_fondo_final <- valores_fondo_crudos_df %>%
    dplyr::filter(batimetria >= -50) # Asumiendo valores negativos para profundidad
  
  cat(paste("Número de puntos de fondo finales después de filtrar por batimetría:", nrow(puntos_fondo_final), "\n"))
  print("Primeras 5 filas de puntos de fondo finales:")
  print(head(puntos_fondo_final, 5))
  
  # Opcional: Visualizar los puntos filtrados
  plot(stack_ambientales[[1]], main = "Puntos de Fondo Filtrados por Batimetría (Profundidad <= 50m)")
  points(puntos_fondo_final$x, puntos_fondo_final$y, col = "blue", cex = 0.5)
  
} else {
  print("\n✗ ¡Crítico! No hay celdas con datos completos en el stack después de la alineación.")
  print("  Esto indica que, incluso después de ajustar, la superposición de datos es nula.")
  print("  Necesitas revisar la extensión espacial de tus capas y si realmente se superponen.")
  print("  Considera visualizar las capas individualmente para ver dónde están los datos válidos.")
}