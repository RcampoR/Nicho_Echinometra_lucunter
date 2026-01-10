library(here)
library(terra)
library(tmap)
library(predicts)
library(tidyverse)
library(randomForest)
library(mgcv)
library(rjava)

rm(list = ls())

# TABLA METRICAS DE EVALUACIÓN
evaluation <- tibble(Model = c("MAXENT", "GLM", "GAM", "RF", "ENSEMBLE"),
                     AUC = c(0.957, 0.9503, 0.969, 0.997, 0.98),
                     TSS = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Kappa = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     TSS_Threshold = c(0.2830035, 0.641, 0.349, 0.664, 0.564))

evaluation$Model <- factor(evaluation$Model,
                           levels = c("MAXENT","GLM", "GAM", "RF", "ENSEMBLE"))


evaluation_plot <- evaluation %>%
  mutate(TSS_equiv = (TSS + 1) / 2) %>%
  select(Model, AUC, TSS_equiv) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# GRAFICAR
ggplot(evaluation_plot, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
    geom_hline(aes(yintercept = 0.9, linetype = "min. AUC"),
             color = "gray12", linewidth = 0.8) +
    geom_hline(aes(yintercept = (0.7 + 1)/2, linetype = "min. TSS"),
             color = "red1", linewidth = 0.8) +
    labs(
    x = "Model",
    y = "AUC Value",
    fill = "Metric"
  ) +
    scale_fill_manual(values = c("AUC" = "#2E86AB",
                               "TSS_equiv" = "#A23B72"),
                    labels = c("AUC", "TSS")) +
    scale_linetype_manual(values = c("min. TSS" = "dashed",
                                   "min. AUC" = "dashed")) +
    scale_y_continuous(
    sec.axis = sec_axis(~ . * 2 - 1, name = "TSS Value") 
  ) +
  
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "sans", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# GUARDAR GRAFICO

ggsave(here("..", "..", "GRAFICAS", "Evaluacion_modelos.png"),
       width = 10, height = 6, dpi = 1000)

dev.off()

# Define la ruta base a la carpeta
pack_variables_base <- here("..", "..", "BIO_MARS_limpias_caribe_50m")

# La lista completa de los nombres "limpios" que deberían ser los nombres de tus archivos .tif
nombres_capas_completos <- c(
  "clorofila_media",
  "salinidad_rango",
  "temperatura_rango",
  "velocidad_corriente_media",
  "ph_rango",
  "batimetria",
  "concavidad",
  "distancia_costa"
)

# Cargar todas las variables usando un bucle y assign()
# Cada SpatRaster se creará en el entorno global con el nombre correspondiente
for (nombre_variable in nombres_capas_completos) {
  ruta_archivo <- here("..", "..", "BIO_MARS_limpias_caribe_50m", paste0(nombre_variable, ".tif"))
  
  if (file.exists(ruta_archivo)) {
    assign(nombre_variable, rast(ruta_archivo), envir = .GlobalEnv)
    message(paste("Cargada:", nombre_variable))
  } else {
    warning(paste("Advertencia: El archivo", ruta_archivo, "no se encontró y no se cargó."))
  }
}

variables_completas <- c(clorofila_media,
                         salinidad_rango,
                         temperatura_rango,
                         velocidad_corriente_media,
                         ph_rango,
                         batimetria,
                         distancia_costa,
                         concavidad)





# Cargar los datos de entrenamiento (necesarios para Random Forest)
sdmdata <- read_csv(here("datos_modelos.csv")) %>%
  rename(pb = presencia_ausencia) %>%
  mutate(pb = as.factor(pb))


# CURVAS DE RESPUESTA

# CARGAR TODOS LOS MODELOS

SDM_maxent <- readRDS(here("Modelos", "SDM_maxent.rds"))

SDM_rf <- readRDS(here("Modelos", "SDM_RF.rds"))

SDM_gam <- readRDS(here("Modelos", "SDM_GAM.rds"))

SDM_glm <- readRDS(here("Modelos", "SDM_glm.rds"))





# CURVAS DE RESPUESTA

valores_extraidos <- read_delim(here("Valores_extraidos_E_lucunter_caribe.csv"))[, -c(9, 10)]
datos_modelos <- read_delim(here("datos_modelos.csv"))


partialResponse(model = SDM_maxent, valores_extraidos, "ph_rango")


# IMPORTANCIA DE LAS VARIABLES

rm(list = ls())


# Cargar los modelos
SDM_maxent <- readRDS(here("Modelos", "SDM_maxent.rds"))
SDM_rf <- readRDS(here("Modelos", "SDM_RF.rds"))
SDM_gam <- readRDS(here("Modelos", "SDM_GAM.rds"))
SDM_glm <- readRDS(here("Modelos", "SDM_glm.rds"))

# Cargar los datos de entrenamiento
sdmdata <- read_csv(here("datos_modelos.csv")) %>%
  rename(pb = presencia_ausencia) %>%
  mutate(pb = as.factor(pb)) 


# --- 2. CALCULAR LA IMPORTANCIA DE LAS VARIABLES CON VARIMPORTANCE ---


# maxent

set.seed(456)

imp_maxent <- varImportance(model = SDM_maxent, 
                            stat = "AUC", n = 40, value = "relative") %>% 
  as.data.frame() 

imp_1 <- imp_maxent %>% 
  transmute(Variable = rownames(imp_maxent),
            Modelo = "maxent", 
            Valor = imp_maxent$.) 


set.seed(456)
# Para GLM
imp_glm <- varImportance(model = SDM_glm, y = sdmdata$pb , x = sdmdata[, -c(1, 10, 11)],
                         stat = "AUC", n = 40, value = "relative") %>% 
  as.data.frame() 


imp_2 <- imp_glm %>% 
  transmute(Variable = rownames(imp_glm),
            Modelo = "glm", 
            Valor = imp_glm$.) 



set.seed(456)
# Para GAM
imp_gam <- varImportance(model = SDM_gam, y = sdmdata$pb , x = sdmdata[, -c(1, 10, 11)],
                         stat = "AUC", n = 40, value = "relative") %>% 
  as.data.frame()


imp_3 <- imp_gam %>% 
  transmute(Variable = rownames(imp_gam),
            Modelo = "gam", 
            Valor = imp_gam$.) 



set.seed(456)
# RF

imp_rf <- varImportance(model = SDM_rf, y = sdmdata$pb , x = sdmdata[, -c(1, 10, 11)],
                        stat = "kappa", n = 40, value = "relative") %>% as.data.frame()

imp_4 <- imp_rf %>% 
  transmute(Variable = rownames(imp_rf),
            Modelo = "rf", 
            Valor = imp_rf$.) 



# Unir y normalizar valores

importancia_unida <- bind_rows(imp_1, imp_2, imp_3, imp_4)

importancia_normalizada <- importancia_unida %>% 
  group_by(Modelo) %>% 
  mutate(v_normalizado = Valor/sum(Valor, na.rm = TRUE)) %>% 
  dplyr::select(-Valor) %>% 
  ungroup() 


# asignar pesos

evaluación <- tibble(Modelo = c("MAXENT", "GLM", "GAM", "RF", "ENSAMBLE"),
                     AUC = c(0.957, 0.9503, 0.969, 0.997, 0.98),
                     TSS = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Kappa = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Umbral_TSS = c(0.2830035, 0.641, 0.349, 0.664, 0.564))


pesos_TSS <- evaluación[, c(1, 3)] %>%
  filter(!Modelo == "ENSAMBLE") %>% 
  transmute(Modelo = Modelo,
            Peso = TSS/sum(TSS))


# Calcular ensamble
ensamble_importancia <- importancia_normalizada %>%
  # Convertir nombres de modelos a mayúsculas para que coincidan con pesos_TSS
  mutate(Modelo = toupper(Modelo)) %>%
  # Unir con los pesos
  left_join(pesos_TSS, by = "Modelo") %>%
  # Calcular importancia ponderada por variable
  mutate(importancia_ponderada = v_normalizado * Peso) %>%
  group_by(Variable) %>%
  summarise(ENSAMBLE = sum(importancia_ponderada, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(ENSAMBLE))


# Cambios en la traducción de las etiquetas para el gráfico (al inglés)

nombres_variables <- c(
  "distancia_costa" = "Distance to Coast",
  "ph_rango" = "pH Range",
  "batimetria" = "Bathymetry",
  "temperatura_rango" = "Temperature Range",
  "clorofila_media" = "Mean Chlorophyll-a",
  "velocidad_corriente_media" = "Mean Current Speed",
  "concavidad" = "Seafloor Concavity",
  "salinidad_rango" = "Salinity Range"
)


ensamble_importancia %>%
  ggplot(aes(x = ENSAMBLE, y = reorder(Variable, ENSAMBLE), fill = ENSAMBLE)) +
  geom_bar(stat = "identity", position = "dodge", orientation = "y", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(ENSAMBLE * 100, 1), "%")),
            hjust = -0.1,
            color = "black",
            size = 3.5,
            fontface = "bold") +
  scale_fill_gradient(
    low = "#D4EBF2",   # azul muy claro
    high = "#005A8D",  # azul profundo
    name = "Importance (%)" # Etiqueta de leyenda traducida
  ) +
  labs(
    y = "Oceanographic Variables", # Eje Y traducido
    x = "Importance" # Eje X traducido
  ) +
  scale_y_discrete(labels = nombres_variables) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "sans", size = 12),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  coord_cartesian(clip = "off")

ggsave(here("..", "..", "GRAFICAS", "importancia_variables.png"),
       dpi = 1000,
       height = 6,
       width = 10)  

dev.off()


# TABLA DE AREAS


datos_escenarios <- tibble(
  pais = rep(c("Panama", "Costa Rica", "Nicaragua", "Colombia"), times = 3),
  escenario = rep(c("Current", "Optimistic", "Pessimistic"), each = 4),
  area_km2 = c(
    # Actual
    3475.25, 250.84, 331.23, 7213.72,
    # Optimista
    109.67, 0.00, 4.96, 1045.22,
    # Pesimista
    2.53, 0.00, 0.84, 131.84
  )
)

datos_escenarios

# GRAFICA

datos_escenarios |> 
  ggplot(aes(x = reorder(pais, -area_km2),
             y = log(area_km2 + 1),
             fill = escenario)) + 
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) + 
  scale_fill_manual(
    values = c(
      "Current"    = "#005A8D",
      "Optimistic" = "#6BAED6",
      "Pessimistic" = "#D4EBF2"
    )
  ) +
  labs(
    x = "Country",
    y = "log(Area (km² + 1))",
    fill = "Stage"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text  = element_text(size = 11),
    text       = element_text(family = "sans"),
    legend.position = "top"
  )


ggsave(here("..", "..", "GRAFICAS", "Areas_idoneas_km2.png"),
       width = 11, height = 7, dpi = 1000)

dev.off()
