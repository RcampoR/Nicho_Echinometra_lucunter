
library(here)
library(terra)
library(tmap)
library(predicts)
library(tidyverse)

rm(list = ls())



evaluación <- tibble(Modelo = c("MAXENT", "GLM", "GAM", "RF", "ENSAMBLE"),
                     AUC = c(0.957, 0.9503, 0.969, 0.997, 0.98),
                     TSS = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Kappa = c(0.796, 0.7592593, 0.852, 0.963, 0.907),
                     Umbral_TSS = c(0.2830035, 0.641, 0.349, 0.664, 0.564))



# GRAFICAR evaluación

evaluación %>% 
  pivot_longer(cols = -c(Modelo, Kappa, Umbral_TSS), 
               names_to = "Métrica", 
               values_to = "Valor") %>%
ggplot(aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray12", linewidth = 0.8) +
  labs(,
       x = "Modelo",
       y = "Valor de Evaluación"
       ) +
  scale_fill_manual(values = c("AUC" = "#2E86AB",
                             "TSS" = "#A23B72")) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title =  element_blank(),
        text = element_text(family = "sans", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


# GUARDAR GRAFICO

ggsave(here("..", "..", "GRAFICAS", "Evaluacion_modelos.png"),
       width = 6, height = 4, dpi = 1000)
