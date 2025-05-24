
#CLOROFILA CRS
crs(Clorofila) <- "EPSG:4326"

crs(Velocidad_corriente) <- "EPSG:4326"

#Tratando el punto flotante

Clorofila[Clorofila < -9999.8 & Clorofila > -10000.0] <- NA

Velocidad_corriente[Velocidad_corriente < -9999.8 & Velocidad_corriente > -10000.0] <- NA



#Remuestreo de capas BIO-ORACLE

Clorofila_remues <- resample(Clorofila, marspec_batimetria, method = "bilinear")

V_corriente_remues <- resample(Velocidad_corriente, marspec_batimetria, method = "bilinear")


# Concatenar todas las capas raster en un solo objeto
variables_raster <- c(
  Clorofila_remues,
  V_corriente_remues,
  marspec_batimetria,
  marspec_Biogeo_1,
  marspec_Biogeo_2,
  marspec_Biogeo_3,
  marspec_Biogeo_4,
  marspec_Biogeo_5,
  marspec_Biogeo_6,
  marspec_Biogeo_7,
  marspec_Biogeo_8,
  marspec_Biogeo_9,
  marspec_Biogeo_10,
  marspec_Biogeo_11,
  marspec_Biogeo_12,
  marspec_Biogeo_13,
  marspec_Biogeo_14,
  marspec_Biogeo_15,
  marspec_Biogeo_16,
  marspec_Biogeo_17
)



