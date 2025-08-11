
library(here)
library(terra)
library(tmap)
library(predicts)
library(tidyverse)
library(randomForest)
library(mgcv)

rm(list = ls())



evaluación <- tibble(Modelo = c("MAXENT", "GLM", "GAM", "RF", "ENSAMBLE"),
                     AUC = c(0.957, 0.9503, 0.969, 0.997, 0.98),
                     TSS = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Kappa = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Umbral_TSS = c(0.2830035, 0.641, 0.349, 0.664, 0.564))

evaluación$Modelo <- factor(evaluación$Modelo, 
                            levels = c("MAXENT","GLM", "GAM", "RF", "ENSAMBLE"))


# GRAFICAR evaluación

evaluación %>% 
  pivot_longer(cols = -c(Modelo, Kappa, Umbral_TSS), 
               names_to = "Métrica", 
               values_to = "Valor") %>%
ggplot(aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = 0.7, linetype = "min. TSS"), 
             color = "red1", 
             linewidth = 0.8) +
  geom_hline(aes(yintercept = 0.9, linetype = "min. AUC"), 
             color = "gray12", 
             linewidth = 0.8) +
  labs(
       x = "Modelo",
       y = "Valor de Evaluación"
       ) +
  scale_fill_manual(values = c("AUC" = "#2E86AB",
                             "TSS" = "#A23B72")) +
  scale_linetype_manual(values = c("min. TSS" = "dashed",
                                   "min. AUC" = "dashed")) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title =  element_blank(),
        text = element_text(family = "sans", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


# GUARDAR GRAFICO

ggsave(here("..", "..", "GRAFICAS", "Evaluacion_modelos.png"),
       width = 10, height = 6, dpi = 1000)



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
# Cada SpatRaster se creará en tu entorno global con el nombre correspondiente
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





# Crear un vector con los nuevos nombres de las variables
nombres_variables <- c(
  "distancia_costa" = "Distancia a la Costa",
  "ph_rango" = "Rango de pH",
  "batimetria" = "Batimetría",
  "temperatura_rango" = "Rango de Temperatura",
  "clorofila_media" = "Clorofila-a Media",
  "velocidad_corriente_media" = "Velocidad de Corriente Media",
  "concavidad" = "Concavidad del Fondo",
  "salinidad_rango" = "Rango de Salinidad"
)

ensamble_importancia %>% 
  ggplot(aes(x = ENSAMBLE, y = reorder(Variable, ENSAMBLE))) +
  geom_bar(fill = "#2E86AB",
           stat = "identity", 
           position = "dodge",
           orientation = "y") +
  labs(
    y = "Variables Oceanograficas",
    x = "Importancia",
    
  ) +
  scale_y_discrete(labels = nombres_variables) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title =  element_blank(),
        text = element_text(family = "sans", face = "bold"),
        axis.text.x = element_text(hjust = 1)) 

ggsave(here("..", "..", "GRAFICAS", "importancia_variables.png"),
       dpi = 1000,
       height = 6,
       width = 10)  

dev.off()
                        
  
