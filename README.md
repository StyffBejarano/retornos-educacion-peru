# Análisis de los Retornos a la Educación en Perú (2021-2024)

## 📌 Descripción
Este proyecto estima la rentabilidad de la educación en el mercado laboral peruano utilizando microdatos de la **ENAHO (INEI)**. Se aplica la **Ecuación de Mincer** para analizar empíricamente cómo el capital humano (años de escolaridad y experiencia) impacta en el ingreso por hora de los trabajadores a nivel nacional.

## 🛠 Metodología y Herramientas
* **Lenguaje:** R (tidyverse, data.table, lmtest, sandwich, ggplot2).
* **Procesamiento de Datos:** Ensamblaje y limpieza de un pool de datos transversales con **204,063 observaciones**, uniendo los módulos de Características de los Miembros, Educación e Ingresos (2021-2024).
* **Modelado Econométrico:** Estimación de 5 modelos de regresión lineal múltiple (MCO) evaluando la estabilidad de los parámetros. Se aplicaron errores estándar robustos (HC1) para mitigar problemas de heterocedasticidad.

## 📊 Principales Hallazgos (Modelo Base)
* **Rentabilidad Educativa:** Se evidencia un retorno positivo y altamente significativo; cada año adicional de educación incrementa el salario por hora en aproximadamente un **7.9%**, *ceteris paribus*.
* **Brecha Salarial por Sexo:** El mercado laboral presenta una fuerte asimetría. Bajo las mismas condiciones de educación y experiencia, los hombres perciben en promedio un **23.8%** más de ingresos por hora que las mujeres.
* **Ciclo de Vida Laboral:** Se comprueba la concavidad del ingreso respecto a la experiencia. Un año adicional de experiencia inicial incrementa el salario en 1.9%, pero presenta rendimientos marginales decrecientes (coeficiente cuadrático negativo).
* **Heterogeneidad Regional:** El área geográfica condiciona fuertemente el salario. Residir en Lima Metropolitana o en la Costa Sur otorga una prima salarial cercana al 14%, mientras que trabajar en zonas como la Sierra Centro implica una penalización salarial relativa del **14.7%** frente a la región base.
