library(terra) # raster y vectores
library(here) # control de direcciones
library(betareg) # GLM de distribución beta
library(tidyverse) # manipular datos y graficar
library(lmtest) # analisis de LRT para dispersión homogenea
library(emmeans) # pruebas POST-HOC 


# LIMPIAR ENTORNO
rm(list = ls())
gc()


# CARGAR RASTER 

# idoneidad

Raster_idoneidad <- here("..", "..", "MAPAS", "Raster_idoneidad_caribe.tif") %>% 
  rast()

# optimista

Raster_optimista <- here("..", "..", "MAPAS", "Raster_optimista_caribe.tif") %>% 
  rast()

# pesimista

Raster_pesimista <- here("..", "..", "MAPAS", "Raster_pesimista_caribe.tif") %>% 
  rast()


# escenarios concatenados

raster_escenarios <- c(
  Raster_idoneidad, 
  Raster_optimista, 
  Raster_pesimista
) 
names(raster_escenarios) <- c(
  "Actual",
  "Optimista",
  "Pesimista"
)  


# vector caribe

paises_caribe <- here("..", "..", "Vectores_caribe", "Capa_Mar_Caribe.shp") %>% 
  vect() %>% 
  terra::aggregate(by = "SOVEREIGN1", dissolve = TRUE) 

# extraer valores

valores_extraidos <- terra::extract(raster_escenarios, paises_caribe, df = TRUE)


# Unir los nombres de los países al data.frame extraído
# Primero, creamos un data.frame de referencia con el ID y el nombre del país
ref_paises <- data.frame(ID = 1:nrow(paises_caribe), 
                         pais = paises_caribe[["SOVEREIGN1"]])

# Unimos los datos extraídos con los nombres de los países
datos_anchos <- merge(valores_extraidos, ref_paises, by = "ID")


#ORGANIZAR DATOS PARA EL glm beta 

datos_glm_beta <- datos_anchos %>%
  pivot_longer(cols = c("Actual", "Optimista", "Pesimista"), 
               names_to = "escenario", 
               values_to = "idoneidad") %>%
  select(SOVEREIGN1, escenario, idoneidad) %>%
  na.omit() %>%
  mutate(
    pais = factor(SOVEREIGN1),
    escenario = factor(escenario)
  ) %>% 
  select(pais, escenario, idoneidad)




# transformación de  (Smithson & Verkuilen)

n <- nrow(datos_glm_beta)

datos_glm_tf <- datos_glm_beta %>% 
  mutate(idoneidad_tf = (idoneidad * (n - 1) + 0.5) / n) 

# pixeles por pais, (hay más para nicaragua)
datos_glm_tf %>% 
  group_by(pais, escenario) %>% 
  summarise(n())

# Medidas de tendencia central

TC_DATOS_glm <- datos_glm_tf %>% 
  group_by(pais, escenario) %>% 
  summarise(medias = mean(idoneidad_tf),
            desv_esta = sd(idoneidad_tf)) %>% 
  ungroup()



# EXPLORANDO DATOS EXTREMOS MEDIANTE z-score 

datos_glm_tf %>% 
  select(-idoneidad) %>% 
  group_by(pais, escenario) %>% 
  mutate(z_score = (idoneidad_tf - mean(idoneidad_tf))/sd(idoneidad_tf),
         clasificacion = case_when(abs(z_score) > 3 ~ "Muy inusual",
                                   abs(z_score) > 2 ~ "inusual",
                                   TRUE ~ "NORMAL")) %>% 
  group_by(clasificacion) %>% 
  summarise(n())


ggplot(datos_glm_tf, aes(x = pais, y = idoneidad_tf)) +
  geom_boxplot()


datos_glm_tf %>% 
  group_by(pais, escenario) %>% 
  ggplot(aes(x = idoneidad_tf)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  facet_wrap(~ pais + escenario, scales = "free") +
  labs(title = "Distribución de Idoneidad Transformada por País y Escenario",
       x = "Idoneidad Transformada", y = "Frecuencia") +
  theme_minimal()

# -----------------------------------------------------------------------------
# Aplicación de los métodos del artículo (Mario Morales a, ∗, Jose Lozano b) para probar la homogeneidad de la dispersión
# -----------------------------------------------------------------------------

# m_nulo: Modelo con dispersión homogénea (phi constante)
m_nulo <- betareg(idoneidad_tf ~ pais * escenario, data = datos_glm_tf)
message("Resumen del Modelo Nulo (Dispersión Homogénea):")
summary(m_nulo)

# m_alt1: Modelo con dispersión que varía por 'pais'
m_alt1 <- betareg(idoneidad_tf ~ pais * escenario | pais, data = datos_glm_tf)
message("\nResumen del Modelo Alternativo 1 (Dispersión por País):")
summary(m_alt1)

# m_alt2: Modelo con dispersión que varía por 'escenario'
m_alt2 <- betareg(idoneidad_tf ~ pais * escenario | escenario, data = datos_glm_tf)
message("\nResumen del Modelo Alternativo 2 (Dispersión por Escenario):")
summary(m_alt2)

# Modelo con dispersión que varía por 'pais' Y 'escenario' (más general)
Modelo_glm_beta_con_dispersion_variable <- betareg(idoneidad_tf ~ pais * escenario | pais + escenario, data = datos_glm_tf)
message("\nResumen del Modelo Alternativo General (Dispersión por País y Escenario):")
summary(Modelo_glm_beta_con_dispersion_variable)

# --- Realizar Pruebas de Razón de Verosimilitudes (Likelihood Ratio Tests - LRT) ---

# Prueba 1: ¿Es la dispersión homogénea vs. varía por 'pais'?
lrt_pais <- lrtest(m_nulo, m_alt1)
message("\nLRT: Dispersión Homogénea vs. Dispersión por País")
print(lrt_pais)

# Prueba 2: ¿Es la dispersión homogénea vs. varía por 'escenario'?
lrt_escenario <- lrtest(m_nulo, m_alt2)
message("\nLRT: Dispersión Homogénea vs. Dispersión por Escenario")
print(lrt_escenario)

# Prueba 3: ¿Es la dispersión homogénea vs. varía por 'pais' Y 'escenario' (modelo más general)?
lrt_overall <- lrtest(m_nulo, Modelo_glm_beta_con_dispersion_variable)
message("\nLRT: Dispersión Homogénea vs. Dispersión por País y Escenario (Prueba General)")
print(lrt_overall)

# Interpretación de los resultados (mantenemos la misma lógica)
message("\n--- Interpretación ---")
# Para lrt_overall, el p-valor relevante es el de la última fila (Modelo 2)
if (lrt_overall$`Pr(>Chisq)`[2] < 0.05) {
  message("Se ha encontrado evidencia significativa (p-valor < 0.05) de que la dispersión NO es homogénea y varía según el país y/o el escenario.")
  message("Por lo tanto, el modelo más adecuado es el que permite que el parámetro de dispersión (phi) varíe, como 'Modelo_glm_beta_con_dispersion_variable'.")
} else {
  message("No se ha encontrado evidencia significativa (p-valor >= 0.05) para rechazar la hipótesis de que la dispersión es homogénea.")
  message("En este caso, el modelo 'm_nulo' (con phi constante) podría ser suficiente o preferible por su simplicidad.")
}


# explorando residuos infinitos

residuos_cuantil <- residuals(Modelo_glm_beta_con_dispersion_variable, type = "quantile")

# filas con infinitos
idx_inf <- which(is.infinite(residuos_cuantil))


# revisar estas filas
datos_problema <- datos_glm_tf[idx_inf, ]


# ELIMINAR OBSERVACIONES PROBLEMÁTICAS Y COMPARAR MODELOS


# Crear nuevo dataset excluyendo las filas problemáticas
datos_glm_tf_limpio <- datos_glm_tf[-idx_inf, ]

# Reajustar el modelo con los datos limpios

Modelo_glm_beta_con_dispersion_variable_limpio <- betareg(idoneidad_tf ~ pais * escenario | pais + escenario, data = datos_glm_tf_limpio)
summary(Modelo_glm_beta_con_dispersion_variable_limpio)

# Verificar que ya no hay residuos infinitos
residuos_cuantil_limpio <- residuals(Modelo_glm_beta_con_dispersion_variable_limpio, type = "quantile")
nuevos_idx_inf <- which(is.infinite(residuos_cuantil_limpio))
length(nuevos_idx_inf) # Cuántos son ahora

# Ver qué observaciones siguen siendo problemáticas
datos_problema_restantes <- datos_glm_tf_limpio[nuevos_idx_inf, ]
print(datos_problema_restantes)

# Crear dataset final sin estos 2 residuos infinitos
datos_glm_tf_final <- datos_glm_tf_limpio[-nuevos_idx_inf, ]

# Verificar
cat("Observaciones eliminadas en esta segunda limpieza:", length(nuevos_idx_inf), "\n")
cat("Observaciones finales:", nrow(datos_glm_tf_final), "\n")

# Reajustar el modelo con los datos finales
Modelo_glm_beta_final <- betareg(idoneidad_tf ~ pais * escenario | pais + escenario, 
                                 data = datos_glm_tf_final)

summary(Modelo_glm_beta_final)

# Verificar que ya no hay residuos infinitos
residuos_final <- residuals(Modelo_glm_beta_final, type = "quantile")
infinitos_final <- which(is.infinite(residuos_final))
cat("Residuos infinitos restantes:", length(infinitos_final), "\n")

# GUARDAR MODELO GLM BETA FINAL

write_rds(Modelo_glm_beta_final, 
          here("..", "..",  "Modelos_Entrenados", "ESPECIFICOS", "Modelo_glm_beta_final.rds"))

# GUARDAR BASE DE DATOS FINAL

write_csv(datos_glm_tf_final, here("datos_glm_tf_final.csv"))

# limpiar entorno
rm(list = ls()) 
gc()


# CARGAR MODELO GLM BETA FINAL
Modelo_glm_beta_final <- read_rds(here("..", "..",  "Modelos_Entrenados", "ESPECIFICOS", "Modelo_glm_beta_final.rds"))
summary(Modelo_glm_beta_final)
# USAR EMMEANS PARA POSHOC

# Obtener las Estimated Marginal Means (EMMs)
# 'pais * escenario' le dice a emmeans que calcule las medias para todas las combinaciones de estos factores
# 'type = "response"' es CRUCIAL para obtener los resultados en la escala original de la idoneidad (0-1)

emms_idoneidad <- emmeans(Modelo_glm_beta_final, specs = ~ pais * escenario, type = "response")

# Convertir el objeto emmeans a un dataframe para ggplot
df_emms <- as.data.frame(emms_idoneidad)

# Crear un gráfico de barras con barras de error
ggplot(df_emms, aes(x = pais, y = emmean, fill = escenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Idoneidad del Hábitat Predicha por País y Escenario",
       x = "País", y = "Idoneidad Predicha", fill = "Escenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1) # Asegurar que el eje Y vaya de 0 a 1


# Comparar escenarios dentro de cada país
comparaciones_escenario_por_pais_ajustadas <- pairs(emms_idoneidad, by = "pais", adjust = "tukey")
print(comparaciones_escenario_por_pais_ajustadas)

# Comparar países dentro de cada escenario
comparaciones_pais_por_escenario_ajustadas <- pairs(emms_idoneidad, by = "escenario", adjust = "tukey")
print(comparaciones_pais_por_escenario_ajustadas)
