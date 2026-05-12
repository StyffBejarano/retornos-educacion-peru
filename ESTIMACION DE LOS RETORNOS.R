# ==============================================================================
# PROYECTO: RETORNOS A LA EDUCACIÓN EN PERÚ (2021-2024)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, lmtest, sandwich, broom, stargazer, psych, ggplot2, patchwork)
if(!require(modelsummary)) install.packages("modelsummary")
library(modelsummary)
# 2. CARGA DE DATOS
cols_miem <- c("CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "P207", "P208A", "DOMINIO")
cols_edu  <- c("CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "P301A")
cols_ing  <- c("CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "P513T", "I524A1", 
               "D529T", "I530A", "D536", "I538A1", "D540T", "I541A", "D543", "D544T")

# Carga Año por Año
setwd("C:/DATOS_PANEL/DATOS_2021") 
b21 <- fread("Enaho01-2021-200.csv", select = cols_miem) %>% 
  left_join(fread("Enaho01a-2021-300.csv", select = cols_edu), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  left_join(fread("Enaho01a-2021-500.csv", select = cols_ing), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  mutate(ANO = 2021)

b22 <- fread("C:/DATOS_PANEL/DATOS 2022/Enaho01-2022-200.csv", select = cols_miem) %>% 
  left_join(fread("C:/DATOS_PANEL/DATOS 2022/Enaho01a-2022-300.CSV", select = cols_edu), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  left_join(fread("C:/DATOS_PANEL/DATOS 2022/Enaho01a-2022-500.csv", select = cols_ing), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  mutate(ANO = 2022)

b23 <- fread("C:/DATOS_PANEL/DATOS_2023/Enaho01-2023-200.csv", select = cols_miem) %>% 
  left_join(fread("C:/DATOS_PANEL/DATOS_2023/Enaho01A-2023-300.csv", select = cols_edu), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  left_join(fread("C:/DATOS_PANEL/DATOS_2023/Enaho01a-2023-500.csv", select = cols_ing), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  mutate(ANO = 2023)

b24 <- fread("C:/DATOS_PANEL/DATOS_2024/Enaho01-2024-200.csv", select = cols_miem) %>% 
  left_join(fread("C:/DATOS_PANEL/DATOS_2024/Enaho01A-2024-300.csv", select = cols_edu), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  left_join(fread("C:/DATOS_PANEL/DATOS_2024/Enaho01a-2024-500.csv", select = cols_ing), by = c("CONGLOME","VIVIENDA","HOGAR","CODPERSO")) %>% 
  mutate(ANO = 2024)

base_raw <- bind_rows(b21, b22, b23, b24)
rm(b21, b22, b23, b24) 

# 3. LIMPIEZA Y CONSTRUCCIÓN DE VARIABLES
# A. Limpieza Numérica
cols_num <- c("P207", "P208A", "P301A", "P513T", "I524A1", "D529T", "I530A", "D536")
base_temp <- base_raw %>%
  mutate(across(all_of(cols_num), ~as.numeric(gsub(",", ".", gsub("[^0-9.-]", "", as.character(.))))))

# B. Limpieza NAs Ingresos
vars_principal <- c("I524A1", "D529T", "I530A", "D536")
base_temp <- base_temp %>%
  mutate(across(all_of(vars_principal), ~ ifelse(. %in% c(999999, 999999.0), NA, .)))

# C. Creación Variables Finales
base_final <- base_temp %>%
  mutate(
    # --- DOMINIO ---
    DOMINIO = as.integer(DOMINIO),
    DOMINIO_ = factor(DOMINIO, 
                      levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c("Costa Norte", "Costa Centro", "Costa Sur",
                                 "Sierra Norte", "Sierra Centro", "Sierra Sur",
                                 "Selva", "Lima Metropolitana")),
    # --- SEXO ---
    P207 = as.numeric(P207),
    SEXO = ifelse(P207 == 1, 1, 0),
    SEXO_LABEL = factor(SEXO, levels = c(0, 1), labels = c("Mujer", "Hombre")),
    
    # --- EDUCACIÓN ---
    P301A = as.numeric(P301A),
    EDUC_ANIOS = case_when(
      P301A == 1 ~ 0, P301A == 2 ~ 1, P301A == 3 ~ 4, P301A == 4 ~ 6,
      P301A == 5 ~ 8, P301A == 6 ~ 11, P301A == 7 ~ 13, P301A == 8 ~ 15,
      P301A == 9 ~ 16, P301A == 10 ~ 17, P301A == 11 ~ 19, P301A == 12 ~ 0,
      TRUE ~ NA_real_
    ),
    NIVEL_EDUC = factor(P301A, levels = 1:12,
                        labels = c("Sin nivel", "Inicial", "Primaria Inc.", "Primaria Comp.",
                                   "Secundaria Inc.", "Secundaria Comp.", "Sup. No Univ. Inc.",
                                   "Sup. No Univ. Comp.", "Sup. Univ. Inc.", "Sup. Univ. Comp.",
                                   "Postgrado", "Basica Especial")),
    
    # --- EXPERIENCIA ---
    EXPERIENCIA = P208A - EDUC_ANIOS - 6,
    EXPERIENCIA = ifelse(EXPERIENCIA < 0, 0, EXPERIENCIA),
    EXPERIENCIA2 = EXPERIENCIA^2,
    
    # --- INGRESOS ---
    ING_ANUAL_PRINCIPAL = rowSums(across(all_of(vars_principal)), na.rm = TRUE),
    ING_MENSUAL = ING_ANUAL_PRINCIPAL / 12,        # Para gráficos
    HORAS_ANUAL = P513T * 52,
    ING_HORA = ifelse(HORAS_ANUAL > 0, ING_ANUAL_PRINCIPAL / HORAS_ANUAL, NA_real_),
    LN_ING_HORA = log(ING_HORA)                    # Para regresiones
  ) %>%
  filter(
    !is.na(LN_ING_HORA) & is.finite(LN_ING_HORA),
    !is.na(EDUC_ANIOS),
    EXPERIENCIA >= 0 & EXPERIENCIA < 80,
    !is.na(DOMINIO_)
  )

rm(base_temp)
cat("Base lista con:", nrow(base_final), "observaciones.\n")

# 4. ESTADÍSTICA DESCRIPTIVA Y GRÁFICOS
tabla_promedios <- base_final %>%
  group_by(ANO) %>%
  summarise(
    Mensual = mean(ING_MENSUAL, na.rm=TRUE),
    Anual = mean(ING_ANUAL_PRINCIPAL, na.rm=TRUE),
    Por_Hora = mean(ING_HORA, na.rm=TRUE)
  )

# Gráfico 1: Ingreso Promedio MENSUAL
ggplot(tabla_promedios, aes(x = factor(ANO), y = Mensual)) +
  geom_bar(stat = "identity", fill = "#4E79A7", width = 0.6) +
  geom_text(aes(label = round(Mensual, 0)), vjust = -0.5, fontface="bold") +
  labs(title = "Ingreso Promedio Mensual", x = "Año", y = "Soles") + theme_minimal()

# Gráfico 2: Ingreso Promedio ANUAL
ggplot(tabla_promedios, aes(x = factor(ANO), y = Anual)) +
  geom_bar(stat = "identity", fill = "#F28E2B", width = 0.6) +
  geom_text(aes(label = round(Anual, 0)), vjust = -0.5, fontface="bold") +
  labs(title = "Ingreso Promedio Anual", x = "Año", y = "Soles") + theme_minimal()

# Gráfico 3: Ingreso Promedio POR HORA
ggplot(tabla_promedios, aes(x = factor(ANO), y = Por_Hora)) +
  geom_bar(stat = "identity", fill = "#59A14F", width = 0.6) +
  geom_text(aes(label = round(Por_Hora, 2)), vjust = -0.5, fontface="bold") +
  labs(title = "Ingreso Promedio por Hora", x = "Año", y = "Soles/Hora") + theme_minimal()

# Gráficos por Sexo
tabla_sexo <- base_final %>% group_by(ANO, SEXO_LABEL) %>%
  summarise(Mensual = mean(ING_MENSUAL, na.rm=TRUE), Anual = mean(ING_ANUAL_PRINCIPAL, na.rm=TRUE), Por_Hora = mean(ING_HORA, na.rm=TRUE))

ggplot(tabla_sexo, aes(x = factor(ANO), y = Mensual, fill = SEXO_LABEL)) +
  geom_bar(stat = "identity", position = "dodge") + labs(title = "Ingreso Mensual por Sexo", fill = "Sexo") + theme_minimal()

ggplot(tabla_sexo, aes(x = factor(ANO), y = Anual, fill = SEXO_LABEL)) +
  geom_bar(stat = "identity", position = "dodge") + labs(title = "Ingreso Anual por Sexo", fill = "Sexo") + theme_minimal()

ggplot(tabla_sexo, aes(x = factor(ANO), y = Por_Hora, group = SEXO_LABEL, color = SEXO_LABEL)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) + labs(title = "Ingreso por Hora por Sexo", color = "Sexo") + theme_minimal()

# Gráfico por Zona
tabla_zona <- base_final %>% group_by(DOMINIO_) %>% summarise(Ingreso_Anual = mean(ING_ANUAL_PRINCIPAL, na.rm=TRUE))
ggplot(tabla_zona, aes(x = reorder(DOMINIO_, Ingreso_Anual), y = Ingreso_Anual)) +
  geom_bar(stat = "identity", fill = "darkcyan") + coord_flip() +
  geom_text(aes(label = round(Ingreso_Anual, 0)), hjust = -0.1) + labs(title = "Ingreso Anual por Zona", x="") + theme_minimal()

# Gráfico por Experiencia
ggplot(base_final %>% group_by(EXPERIENCIA) %>% summarise(Ingreso = mean(ING_ANUAL_PRINCIPAL, na.rm=T)), aes(x = EXPERIENCIA, y = Ingreso)) +
  geom_line(color = "darkblue", linewidth = 1) + geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Perfil de Ingresos por Experiencia") + theme_minimal()

# Gráfico por Educación
tabla_educ <- base_final %>% group_by(NIVEL_EDUC) %>% summarise(Ingreso_Anual = mean(ING_ANUAL_PRINCIPAL, na.rm=TRUE))
ggplot(tabla_educ, aes(x = NIVEL_EDUC, y = Ingreso_Anual)) +
  geom_bar(stat = "identity", fill = "#76B7B2") + coord_flip() +
  geom_text(aes(label = round(Ingreso_Anual, 0)), hjust = -0.1, size = 3) + labs(title = "Ingreso Anual por Educación", x="") + theme_minimal()

# 5. REGRESIONES Y ANÁLISIS AÑO POR AÑO
cat("\n=== RESULTADOS INDIVIDUALES POR AÑO (LN_ING_HORA) ===\n")

# Modelo 2021
modelo21 <- lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + SEXO + DOMINIO_, data = filter(base_final, ANO == 2021))
print(summary(modelo21))

# Modelo 2022
modelo22 <- lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + SEXO + DOMINIO_, data = filter(base_final, ANO == 2022))
print(summary(modelo22))

# Modelo 2023
modelo23 <- lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + SEXO + DOMINIO_, data = filter(base_final, ANO == 2023))
print(summary(modelo23))

# Modelo 2024
modelo24 <- lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + SEXO + DOMINIO_, data = filter(base_final, ANO == 2024))
print(summary(modelo24))

# Gráfico de Estabilidad (Betas)
betas_por_anio <- base_final %>% group_by(ANO) %>%
  do(tidy(lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + SEXO + DOMINIO_, data = .), conf.int = TRUE)) %>%
  filter(term == "EDUC_ANIOS")

ggplot(betas_por_anio, aes(x = factor(ANO), y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, size = 0.8) +
  geom_point(size = 4, color = "darkblue") +
  geom_text(aes(label = paste0(round(estimate*100, 2), "%")), vjust = -1.5, fontface = "bold") +
  labs(title = "Estabilidad de los Retornos a la Educación", y = "Retorno (Beta)", x = "Año") + theme_minimal()

# ------------------------------------------------------------------------------
# 1. ESTIMACIÓN DE LOS 5 MODELOS 
# ------------------------------------------------------------------------------

# A) MODELO COMPLETO (Base)
modelo_total <- lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + SEXO + DOMINIO_, 
                   data = base_final)

# B) SIN EXPERIENCIA (Quitar Exp y Exp2)
modelo_sin_experiencia <- lm(LN_ING_HORA ~ EDUC_ANIOS + SEXO + DOMINIO_, 
                             data = base_final)

# C) SIN SEXO (Quitar variable Sexo)
modelo_sin_sexo <- lm(LN_ING_HORA ~ EDUC_ANIOS + EXPERIENCIA + EXPERIENCIA2 + DOMINIO_, 
                      data = base_final)

# D) SOLO EDUCACIÓN (Mincer simple)
modelo_solo_educacion <- lm(LN_ING_HORA ~ EDUC_ANIOS, 
                            data = base_final)

# E) SOLO EXPERIENCIA (Capital humano empírico)
modelo_solo_experiencia <- lm(LN_ING_HORA ~ EXPERIENCIA + EXPERIENCIA2, 
                              data = base_final)

# ------------------------------------------------------------------------------
# 2. IMPRESIÓN DE RESULTADOS INDIVIDUALES (CON ERRORES ROBUSTOS)
# ------------------------------------------------------------------------------
# Esto imprime los "cuadritos" en la consola para cada uno

cat("\n>>> MODELO COMPLETO <<<\n")
print(coeftest(modelo_total, vcov = vcovHC(modelo_total, type = "HC1")))

cat("\n>>> MODELO SIN EXPERIENCIA <<<\n")
print(coeftest(modelo_sin_experiencia, vcov = vcovHC(modelo_sin_experiencia, type = "HC1")))

cat("\n>>> MODELO SIN SEXO <<<\n")
print(coeftest(modelo_sin_sexo, vcov = vcovHC(modelo_sin_sexo, type = "HC1")))

cat("\n>>> MODELO SOLO EDUCACIÓN <<<\n")
print(coeftest(modelo_solo_educacion, vcov = vcovHC(modelo_solo_educacion, type = "HC1")))

cat("\n>>> MODELO SOLO EXPERIENCIA <<<\n")
print(coeftest(modelo_solo_experiencia, vcov = vcovHC(modelo_solo_experiencia, type = "HC1")))

# ------------------------------------------------------------------------------
# 3. TABLA FINAL DE ROBUSTEZ 
# ------------------------------------------------------------------------------
# Aquí unimos todos los modelos en una sola tabla larga para hacer View()
# NOTA: Usamos tidy sobre el coeftest para jalar los errores robustos y p-valores correctos.

tabla_robustez <- bind_rows(
  tidy(coeftest(modelo_total, vcov = vcovHC(modelo_total, type = "HC1"))) %>% mutate(Modelo = "Completo"),
  tidy(coeftest(modelo_sin_experiencia, vcov = vcovHC(modelo_sin_experiencia, type = "HC1"))) %>% mutate(Modelo = "Sin Experiencia"),
  tidy(coeftest(modelo_sin_sexo, vcov = vcovHC(modelo_sin_sexo, type = "HC1"))) %>% mutate(Modelo = "Sin Sexo"),
  tidy(coeftest(modelo_solo_educacion, vcov = vcovHC(modelo_solo_educacion, type = "HC1"))) %>% mutate(Modelo = "Solo Educación"),
  tidy(coeftest(modelo_solo_experiencia, vcov = vcovHC(modelo_solo_experiencia, type = "HC1"))) %>% mutate(Modelo = "Solo Experiencia")
) %>%
  # Ordenamos las columnas para que se vea bien
  select(Modelo, term, estimate, std.error, statistic, p.value) %>%
  rename(Variable = term, Coeficiente = estimate, Error_Std = std.error, t_valor = statistic, P_valor = p.value)

# ------------------------------------------------------------------------------
# 4. VER LA TABLA
# ------------------------------------------------------------------------------
View(tabla_robustez)
  # 7. DIAGNÓSTICO GRÁFICO (NORMALIDAD)
  # Histograma
  p1_hist <- ggplot(base_final, aes(x = LN_ING_HORA)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "skyblue", color = "white", alpha=0.7) +
    geom_density(color = "red", linewidth = 1) + labs(title = "Histograma: Log Ingreso", x="Log(Ingreso)", y="Densidad") + theme_minimal()
  
  # Q-Q Plot
  p1_qq <- ggplot(base_final, aes(sample = LN_ING_HORA)) +
    stat_qq(color = "blue", alpha = 0.5) + stat_qq_line(color = "red") + labs(title = "Q-Q Plot") + theme_minimal()
  
  # Boxplot
  p1_box <- ggplot(base_final, aes(x = "", y = LN_ING_HORA)) +
    geom_boxplot(fill = "skyblue", outlier.colour = "red") + coord_flip() + labs(title = "Boxplot (Outliers)", x="") + theme_minimal()
  
  (p1_hist | p1_qq) / p1_box
