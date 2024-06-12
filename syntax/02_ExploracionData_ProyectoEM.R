#
# Materia:    Estadistica Multivariada
# Fecha:      11/06/2024
# Tema:       Proyecto Final
# Descripion: Base de datos de desaparicion de personas
# Archivo:    02_ExploracionData_ProyectoEM.R
# Autora:     Semiramis G. de la Cruz

#---- LIMPIEZA Y LIBRERIAS -----------------------------------------------------

cat("\014")
rm(list = ls())

# Semilla
set.seed(200222)

#---- DATOS --------------------------------------------------------------------

# Cargar el conjunto de datos
file_in <- "C:/Users/semir/Documents/CIMAT/Estadistica/Proyecto/data/RNPDNO_V5.csv"
RNPDNO_V5 <- read.csv(file_in, header = TRUE)

# Importar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

summary(RNPDNO_V5)
sapply(RNPDNO_V5, class)
colSums(is.na(RNPDNO_V5))
dim(RNPDNO_V5) # [1] 115513, 11


#---- EDA ----------------------------------------------------------------------

# Función para graficar la frecuencia de una variable categórica
grafica_barras <- function(data, variable) {
  freq <- table(data[[variable]])
  df_freq <- as.data.frame(freq)
  names(df_freq) <- c("Var1", "Freq")
  
  # Graficar
  ggplot(data = df_freq, aes(x = Var1, y = Freq, fill = Freq)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Distribución de", variable), x = "", y = "Frecuencia") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_gradient(low = "#DDD5FF", high = "#3F1550") # Degradado en morado
}

grafica_barras(RNPDNO_V5, "SEXO")
grafica_barras(RNPDNO_V5, "ESTADO_CIVIL")
grafica_barras(RNPDNO_V5, "NIVEL_EDUCATIVO")
grafica_barras(RNPDNO_V5, "ESTADO")
grafica_barras(RNPDNO_V5, "EDAD_G")
grafica_barras(RNPDNO_V5, "ESTADO_CIVIL_G")
grafica_barras(RNPDNO_V5, "NIVEL_EDUCATIVO_G")


# Función para graficar un histograma de una variable numérica
grafica_histograma <- function(data, variable, titulo) {
  ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(binwidth = 1, fill = "#DDD5FF", color = "#3F1550", alpha = 0.7) +
    labs(title = titulo, x = variable, y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Función para graficar un diagrama de caja y bigotes de una variable numérica
grafica_caja_bigotes <- function(data, variable, titulo) {
  ggplot(data, aes(y = !!sym(variable))) +
    geom_boxplot(fill = "#DDD5FF", color = "#3F1550", alpha = 0.7) +
    labs(title = titulo, x = "", y = variable) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

RNPDNO_V6 <- RNPDNO_V5    # 115513, 11

# EDAD_C
grafica_caja_bigotes(RNPDNO_V6, "EDAD_C", "Diagrama de Caja y Bigotes de Edades")

RNPDNO_V6 <- RNPDNO_V6 %>%
  filter(EDAD_C <= 100)  # 115498, 11

grafica_caja_bigotes(RNPDNO_V6, "EDAD_C", "Diagrama de Caja y Bigotes de Edades")


# Ejemplo de uso para ANIO_REPORTE
grafica_caja_bigotes(RNPDNO_V6, "ANIO_REPORTE", "Diagrama de Caja y Bigotes del Año del Reporte")

# RNPDNO_V6 <- RNPDNO_V6 %>%
#   filter(ANIO_REPORTE > 1920)
# grafica_caja_bigotes(RNPDNO_V6, "ANIO_REPORTE", "Diagrama de Caja y Bigotes del Año del Reporte")


table(RNPDNO_V6$FLAG)
