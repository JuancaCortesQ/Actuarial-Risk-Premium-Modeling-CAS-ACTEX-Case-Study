
# Modelo competencia CAS

# ==========================================
# 1. LIBRERIAS E IMPORTACION DE DATOS----
# ==========================================

library(readxl)
library(tidyverse)
library(psych)
library(treemap)
library(ggplot2)
library(dplyr)
library(tseries)
library(writexl)
library(statmod) # Para funciones adicionales de GLM
library(tweedie)

# Codigo
data_entrenamiento <- read_excel("ENTRENAMIENTO.xlsx")
data_validacion<-read_excel("VALIDACION.xlsx")
head(data_entrenamiento,5)
head(data_validacion,5)

# ==========================================
# 2. REVISION DE LA DATA----
# ==========================================


# revision de factores faltantes 
sum(is.na(data_entrenamiento))
sum(is.na(data_validacion))

data_validacion %>%
  select(clase_suscripcion, en_campus, estudios_area, distancia_al_campus) %>%
  summarise_all(~sum(is.na(.)))

# ==========================================
# 3. CODIFICACION Y TARGET----
# ==========================================

# Target (variable objetivo) en este caso el target es la suma de las coberturas como lo establece el caso

data_entrenamiento <- data_entrenamiento %>%
  mutate(TOTAL_LOSS = Contenidos_siniestros_monto + 
           Gastos_Adicionales_siniestros_monto + 
           Resp_Civil_siniestros_monto + 
           Gastos_Medicos_RC_siniestros_monto)

TARGET_VAR <- "TOTAL_LOSS"


CAT_VARS <- c("clase_suscripcion", "en_campus", "estudios_area")


# Para clase suscricpcion
encoding_clase <- data_entrenamiento %>%
  group_by(clase_suscripcion) %>%
  summarise(
    TE_clase_suscripcion = mean(!!sym(TARGET_VAR), na.rm = TRUE)
  )
print("Mapa de Codificación para clase_suscripcion:")
print(encoding_clase)

# para ubicacion del hogar
encoding_ubicacion <- data_entrenamiento %>%
  group_by(en_campus) %>%
  summarise(
    TE_ubicacion_vivienda = mean(!!sym(TARGET_VAR), na.rm = TRUE)
  )
print("Mapa de Codificación para Ubicación (en_campus):")
print(encoding_ubicacion)

# para area de estudios
encoding_area <- data_entrenamiento %>%
  group_by(estudios_area) %>%
  summarise(
    TE_estudios_area = mean(!!sym(TARGET_VAR), na.rm = TRUE)
  )
print("Mapa de Codificación para Area de Estudio:")
print(encoding_area)


# ==========================================
# 4. ENCODING DE ENTRENAMIENTO----
# ==========================================

#Aplicación del Target Encoding entrenamiento----

# Aplicar los 3 mapas al conjunto de entrenamiento
data_entrenamiento <- data_entrenamiento %>%
  left_join(encoding_clase, by = "clase_suscripcion") %>%
  left_join(encoding_ubicacion, by = "en_campus") %>%
  left_join(encoding_area, by = "estudios_area")

# Mostrar las nuevas columnas
head(data_entrenamiento[c("clase_suscripcion", "TE_clase_suscripcion", "en_campus", "TE_ubicacion_vivienda")])
view(data_entrenamiento)

#Aplicación del Target Encoding validacion----
# Aplicar los MISMOS 3 mapas al conjunto de validación
data_validacion <- data_validacion %>%
  left_join(encoding_clase, by = "clase_suscripcion") %>%
  left_join(encoding_ubicacion, by = "en_campus") %>%
  left_join(encoding_area, by = "estudios_area")


# Calculamos la media global del entrenamiento
media_global_entrenamiento <- mean(data_entrenamiento$TOTAL_LOSS, na.rm = TRUE)

# Sustituimos los NA en validación por esa media
data_validacion <- data_validacion %>%
  mutate(
    TE_clase_suscripcion = replace_na(TE_clase_suscripcion, media_global_entrenamiento),
    TE_ubicacion_vivienda = replace_na(TE_ubicacion_vivienda, media_global_entrenamiento),
    TE_estudios_area      = replace_na(TE_estudios_area, media_global_entrenamiento)
  )


# ===============================================
# 5. INTERACCIONES Y RECODIFICACION----
# ===============================================

# Crear las variables binarias (0 y 1)
data_entrenamiento <- data_entrenamiento %>%
  mutate(
    en_campus_bin = ifelse(en_campus == "Dentro de campus", 1, 0),
    mas_inquilinos_bin = ifelse(`2_o_mas_inquilinos` == "Si", 1, 0)
  )

data_validacion <- data_validacion %>%
  mutate(
    en_campus_bin = ifelse(en_campus == "Dentro de campus", 1, 0),
    mas_inquilinos_bin = ifelse(`2_o_mas_inquilinos` == "Si", 1, 0)
  )

# Crear las interacciones
data_entrenamiento <- data_entrenamiento %>%
  mutate(
    TE_clase_x_campus = TE_clase_suscripcion * en_campus_bin,
    inquilinos_x_campus = mas_inquilinos_bin * en_campus_bin
  )

data_validacion <- data_validacion %>%
  mutate(
    TE_clase_x_campus = TE_clase_suscripcion * en_campus_bin,
    inquilinos_x_campus = mas_inquilinos_bin * en_campus_bin
  )

# ===============================================
# 6. CONFIRMACIONES DE LAS TRANSFORMACIONES----
# ===============================================


# Verifica que las nuevas columnas numéricas y las interacciones tengan sentido
cols_check <- c("TE_clase_suscripcion", "en_campus_bin", "mas_inquilinos_bin", "TE_clase_x_campus", "inquilinos_x_campus")

print("Revision en Entrenamiento:")
print(head(data_entrenamiento[cols_check]))

print("Revision en Validación:")
print(head(data_validacion[cols_check]))

# Visualización de las tablas completas
view(data_entrenamiento)
view(data_validacion)

# ==========================================
# 7. MODELADO ACTUARIAL (Tweedie GLM)----
# ==========================================

# Ajustamos el modelo usando las variables binarias y encodings limpios
modelo_actuarial <- glm(
  TOTAL_LOSS ~ TE_clase_suscripcion + 
    TE_estudios_area + 
    TE_clase_x_campus + 
    inquilinos_x_campus +
    en_campus_bin + 
    mas_inquilinos_bin +
    log(distancia_al_campus + 1), 
  family = tweedie(var.power = 1.5, link = 0), #con el 0 es igual que log 
  data = data_entrenamiento
)

# Resumen estadístico del modelo
summary(modelo_actuarial)

# ==========================================
# 8. GENERACIÓN DE PREDICCIONES----
# ==========================================

# Predicción en entrenamiento (para calcular métricas)
data_entrenamiento$prediccion <- predict(modelo_actuarial, newdata = data_entrenamiento, type = "response")

# Predicción en validación (el entregable final)
data_validacion$prediccion <- predict(modelo_actuarial, newdata = data_validacion, type = "response")

# ==========================================
# 9. EVALUACIÓN DE MÉTRICAS (EAM y GINI)----
# ==========================================

# A. (EAM / MAE)
eam_entrenamiento <- mean(abs(data_entrenamiento$TOTAL_LOSS - data_entrenamiento$prediccion), na.rm = TRUE)

# B. Gini
# Lorenz curve
calcular_gini <- function(actual, predicho) {
  df_gini <- data.frame(actual = actual, predicho = predicho) %>%
    arrange(predicho) %>%
    mutate(
      cum_pob = row_number() / n(),
      cum_loss = cumsum(actual) / sum(actual)
    )
  # Gini = 1 - 2 * AUC
  gini <- 1 - 2 * (sum(df_gini$cum_loss) / nrow(df_gini))
  return(abs(gini))
}

gini_final <- calcular_gini(data_entrenamiento$TOTAL_LOSS, data_entrenamiento$prediccion)

print(paste("EAM (Error Medio):", round(eam_entrenamiento, 2)))
print(paste("Coeficiente de Gini:", round(gini_final, 4)))

# ==========================================
# 7. VISUALIZACIÓN: CURVA DE LORENZ ----
# ==========================================

lorenz_data <- data_entrenamiento %>%
  select(TOTAL_LOSS, prediccion) %>%
  arrange(prediccion) %>%
  mutate(
    cum_pob = row_number() / n(),
    cum_loss = cumsum(TOTAL_LOSS) / sum(TOTAL_LOSS)
  )

ggplot(lorenz_data, aes(x = cum_pob, y = cum_loss)) +
  geom_line(color = "#2c3e50", size = 1.2) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "red") + 
  geom_area(fill = "#3498db", alpha = 0.2) + 
  labs(
    title = "Curva de Lorenz: Validación del Modelo Actuarial",
    subtitle = paste("Gini Estimado:", round(gini_final, 4)),
    x = "% Acumulado de Asegurados (Ordenados por Riesgo Predicho)",
    y = "% Acumulado de Siniestros Reales"
  ) +
  theme_minimal()


# ==========================================
# 8. EXPORTACIÓN DE RESULTADOS----
# ==========================================

write_xlsx(data_entrenamiento, "RESULTADOS_ENTRENAMIENTO.xlsx")
write_xlsx(data_validacion, "RESULTADOS_VALIDACION.xlsx")

 

