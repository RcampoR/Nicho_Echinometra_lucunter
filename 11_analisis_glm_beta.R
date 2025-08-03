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


hist(datos_glm_beta$idoneidad) # revisar distribución de idoneidad

min(datos_glm_beta$idoneidad) # revisar mínimo
max(datos_glm_beta$idoneidad) # revisar máximo


# grafico de cajas

ggplot(datos_glm_beta, aes(x = pais, y = idoneidad, fill = escenario)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "white", 
               outlier.stroke = 0.5, linewidth = 0.5) +
  labs(
    x = "País", 
    y = "Idoneidad de Habitat",
    fill = "Escenario"
  ) +
  scale_x_discrete(labels = c("Colombia", "Costa Rica", "Nicaragua", "Panamá")) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D"),
                    labels = c("Actual", "SSP1-1.9", "SSP5-8.5")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "sans"))



# -----------------------------------------------------------------------------
# Aplicación de los métodos del artículo (Mario Morales a, ∗, Jose Lozano b) para probar la homogeneidad de la dispersión
# -----------------------------------------------------------------------------

# m_nulo: Modelo con dispersión homogénea (phi constante)
m_nulo <- betareg(idoneidad ~ pais * escenario, data = datos_glm_beta)
message("Resumen del Modelo Nulo (Dispersión Homogénea):")
summary(m_nulo)

# m_alt1: Modelo con dispersión que varía por 'pais'
m_alt1 <- betareg(idoneidad ~ pais * escenario | pais, data = datos_glm_beta)
message("\nResumen del Modelo Alternativo 1 (Dispersión por País):")
summary(m_alt1)

# m_alt2: Modelo con dispersión que varía por 'escenario'
m_alt2 <- betareg(idoneidad ~ pais * escenario | escenario, data = datos_glm_beta)
message("\nResumen del Modelo Alternativo 2 (Dispersión por Escenario):")
summary(m_alt2)

# Modelo con dispersión que varía por 'pais' Y 'escenario' (más general)
Modelo_glm_beta_con_dispersion_variable <- betareg(idoneidad ~ pais * escenario | pais + escenario, data = datos_glm_beta)
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




# GLM beta final (variable de dispersión por país y escenario)

modelo_glm_beta <- betareg(idoneidad ~ escenario * pais | escenario + pais, 
                            data = datos_glm_beta, 
                            link = "logit")
summary(modelo_glm_beta)



# EXPLORAR RESIDUOS DEL MODELO FINAL
qqnorm(residuals(modelo_glm_beta, type = "pearson")) # QQ-plot de residuos
qqline(residuals(modelo_glm_beta, type = "pearson")) # Línea de referencia



# EXTRAER VALORES PREDICHOS Y RESIDUOS
valores_predichos <- fitted(modelo_glm_beta)
residuos_estandarizados <- residuals(modelo_glm_beta, type = "pearson")

# OPCIÓN 1: GRÁFICO BÁSICO CON PLOT BASE
plot(valores_predichos, residuos_estandarizados,
     xlab = "Valores Predichos",
     ylab = "Residuos Estandarizados",
     main = "Residuos Estandarizados vs Valores Predichos\nModelo Beta Regresión",
     pch = 16, cex = 0.7, col = alpha("blue", 0.6))

# Añadir línea horizontal en y = 0
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Añadir líneas de referencia en ±2
abline(h = c(-2, 2), col = "orange", lty = 3, lwd = 1)

# Añadir curva suavizada (lowess)
lines(lowess(valores_predichos, residuos_estandarizados), col = "darkred", lwd = 2)



# USAR EMMEANS PARA POSHOC

# Obtener las Estimated Marginal Means (EMMs)
# 'pais * escenario' le dice a emmeans que calcule las medias para todas las combinaciones de estos factores
# 'type = "response"' es CRUCIAL para obtener los resultados en la escala original de la idoneidad (0-1)

emms_idoneidad <- emmeans(modelo_glm_beta, specs = ~ pais * escenario, type = "response")

# Comparar escenarios dentro de cada país
comparaciones_escenario_por_pais_ajustadas <- pairs(emms_idoneidad, by = "pais", adjust = "tukey")
print(comparaciones_escenario_por_pais_ajustadas)

# Comparar países dentro de cada escenario
comparaciones_pais_por_escenario_ajustadas <- pairs(emms_idoneidad, by = "escenario", adjust = "tukey")
print(comparaciones_pais_por_escenario_ajustadas)


# guardar datos utilizados en el modelo GLM beta

write_csv(datos_glm_beta, 
           here("datos_glm_beta.csv"))


# guardar modelo GLM beta
saveRDS(modelo_glm_beta, 
        here("..", "..", "Modelos_Entrenados", "ESPECIFICOS", "modelo_glm_beta.rds"))

# leer datos y modelo en un entorno limpio

modelo_glm_beta <- readRDS(here("..", "..", "Modelos_Entrenados", "ESPECIFICOS", "modelo_glm_beta.rds"))

datos_glm_beta <- read_csv(here("datos_glm_beta.csv"))


summary(modelo_glm_beta)
