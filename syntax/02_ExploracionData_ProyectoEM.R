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
file_in2 <- "C:/Users/semir/Documents/CIMAT/Estadistica/Proyecto/data/RNPDNO_V5_Informada.csv"
RNPDNO_V5 <- read.csv(file_in, header = TRUE)
RNPDNO_V5_Informada <- read.csv(file_in2, header = TRUE)

# Importar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(rlang)
library(svglite)

summary(RNPDNO_V5)
sapply(RNPDNO_V5, class)
colSums(is.na(RNPDNO_V5))
dim(RNPDNO_V5) # [1] 115513, 11


#---- FUNCIONES GRAFICAS -------------------------------------------------------

output_folder <- "C:/Users/semir/Documents/CIMAT/Estadistica/Proyecto/output/"

# Función para graficar la frecuencia de una variable categórica
grafica_barras <- function(data, variable) {
  freq <- table(data[[variable]])
  df_freq <- as.data.frame(freq)
  names(df_freq) <- c("Var1", "Freq")
  
  # Graficar
  p <- ggplot(data = df_freq, aes(x = Var1, y = Freq, fill = Freq)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Distribución de", variable), x = "", y = "Frecuencia") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_gradient(low = "#DDD5FF", high = "#3F1550") # Degradado en morado
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable, "_barras.svg"), 
         plot = p, device = "svg", bg = "transparent")
}

grafica_barras_dobles <- function(data, variable) {
  freq <- table(data[[variable]], data$SEXO)
  df_freq <- as.data.frame(freq)
  names(df_freq) <- c("Var1", "Sexo", "Freq")
  
  # Graficar
  p <- ggplot(data = df_freq, aes(x = Var1, y = Freq, fill = Sexo)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Distribución de", variable, "por Sexo"), x = "", y = "Frecuencia") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values = c("#3F1550", "#DDD5FF")) # Dos colores para hombre y mujer
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable, "_barras_dobles.svg"), 
         plot = p, device = "svg", bg = "transparent")
}

grafica_barras_triples <- function(data, variable, sexo) {
  freq <- table(data[[variable]], data[[sexo]])
  df_freq <- as.data.frame(freq)
  names(df_freq) <- c("Var1", "Sexo", "Freq")
  
  df_freq$Sexo[df_freq$Sexo == "Desconocido"] <- "Desconocido"
  
  # Graficar
  p <- ggplot(data = df_freq, aes(x = Var1, y = Freq, fill = Sexo)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Distribución de", variable, "por Sexo"), x = "", y = "Frecuencia") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values = c("#3F1550", "#DDD5FF", "#A84C70")) # Tres colores para cada categoría de sexo
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable, "_barras_triples.svg"), 
         plot = p, device = "svg", bg = "transparent")
}

# Función para graficar un histograma de una variable numérica
grafica_histograma <- function(data, variable, titulo) {
  p <- ggplot(data, aes(x = !!sym(variable))) +
    geom_histogram(binwidth = 1, fill = "#DDD5FF", color = "#3F1550", alpha = 0.7) +
    labs(title = titulo, x = variable, y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable, "_histograma.svg"), 
         plot = p, device = "svg", bg = "transparent")
}

# Función para graficar un diagrama de caja y bigotes de una variable numérica
grafica_caja_bigotes <- function(data, variable, titulo) {
  ggplot(data, aes(y = !!sym(variable))) +
    geom_boxplot(fill = "#DDD5FF", color = "#3F1550", alpha = 0.7) +
    labs(title = titulo, x = "", y = variable) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}


# Gráfico de dispersión para dos variables numéricas con ggplot
grafica_dispersion <- function(data, variable_x, variable_y, titulo) {
  p <- ggplot(data = data, aes(x = !!sym(variable_x), y = !!sym(variable_y))) +
    geom_point(color = "#3F1550") +
    labs(title = titulo, x = variable_x, y = variable_y) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable_x, "_", variable_y, "_dispersion.svg"), 
         plot = p, device = "svg", bg = "transparent")
}

# Boxplot para comparar una variable numérica entre grupos de una variable categórica con ggplot
grafica_boxplot <- function(data, variable_x, variable_y, titulo, output_folder) {
  p <- ggplot(data = data, aes(x = !!sym(variable_x), y = !!sym(variable_y), fill = !!sym(variable_x))) +
    geom_boxplot() +
    labs(title = titulo, x = variable_x, y = variable_y) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("#3F1550", "#DDD5FF")) # Dos colores para hombre y mujer
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable_x, "_", variable_y, "_boxplot.svg"), 
         plot = p, device = "svg", bg = "transparent")
}

# Boxplot para comparar una variable numérica entre grupos de una variable categórica con ggplot
grafica_boxplot <- function(data, variable_x, variable_y, titulo) {
  p <- ggplot(data = data, aes(x = !!sym(variable_x), y = !!sym(variable_y), fill = !!sym(variable_x))) +
    geom_boxplot() +
    labs(title = titulo, x = variable_x, y = variable_y) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("#A84C70", "#DDD5FF")) # Dos colores para hombre y mujer
  
  # Guardar el gráfico como SVG
  ggsave(filename = paste0(output_folder, variable_x, "_", variable_y, "_boxplot.svg"), 
         plot = p, device = "svg", bg = "transparent")
}



#---- GRAPH ANTES DEL FILTRADO -------------------------------------------------

grafica_barras(RNPDNO_V5, "SEXO")
grafica_barras(RNPDNO_V5, "ESTADO_CIVIL")
grafica_barras(RNPDNO_V5, "NIVEL_EDUCATIVO")
grafica_barras(RNPDNO_V5, "ESTADO")
grafica_barras(RNPDNO_V5, "EDAD_G")
grafica_barras(RNPDNO_V5, "ESTADO_CIVIL_G")
grafica_barras(RNPDNO_V5, "NIVEL_EDUCATIVO_G")
grafica_barras(RNPDNO_V5, "ESTADO_BUSQUEDA")


grafica_barras_triples(RNPDNO_V5, "FLAG", "SEXO")
grafica_barras_triples(RNPDNO_V5, "ESTADO_CIVIL_G", "SEXO")
grafica_barras_triples(RNPDNO_V5, "NIVEL_EDUCATIVO_G", "SEXO")
grafica_barras_triples(RNPDNO_V5, "EDAD_G", "SEXO")
grafica_barras_triples(RNPDNO_V5, "ESTADO", "SEXO")

# No son informativas
# grafica_caja_bigotes(RNPDNO_V5, "EDAD_C", "Diagrama de Caja y Bigotes de Edades")
# grafica_caja_bigotes(RNPDNO_V5, "ANIO_REPORTE", "Diagrama de Caja y Bigotes del Año del Reporte")

# Evitamos este filtro porque la variable no parece relevante
# RNPDNO_V6 <- RNPDNO_V6 %>%
#   filter(ANIO_REPORTE > 1920)
#  grafica_caja_bigotes(RNPDNO_V6, "ANIO_REPORTE", "Diagrama de Caja y Bigotes del Año del Reporte")

 
 #---- GRAPH FILTRANDO TODOS LOS DATOS FALTANTES -------------------------------
 

table(RNPDNO_V5_Informada$SEXO)
table(RNPDNO_V5_Informada$EDAD_C)
table(RNPDNO_V5_Informada$ESTADO_CIVIL)
table(RNPDNO_V5_Informada$NIVEL_EDUCATIVO)
table(RNPDNO_V5_Informada$ESTADO)

grafica_barras_dobles(RNPDNO_V5_Informada, "ESTADO_CIVIL")
grafica_barras_dobles(RNPDNO_V5_Informada, "NIVEL_EDUCATIVO")
grafica_barras_dobles(RNPDNO_V5_Informada, "ESTADO")
grafica_barras_dobles(RNPDNO_V5_Informada, "EDAD_G")
grafica_barras_dobles(RNPDNO_V5_Informada, "ESTADO_CIVIL_G")
grafica_barras_dobles(RNPDNO_V5_Informada, "NIVEL_EDUCATIVO_G")
grafica_barras_dobles(RNPDNO_V5_Informada, "ESTADO_BUSQUEDA")



grafica_histograma(RNPDNO_V5_Informada, "EDAD_C", "Histograma de EDAD")

grafica_dispersion(RNPDNO_V5_Informada, "EDAD_C", "ESTADO", "Relación entre EDAD y ESTADO")

grafica_boxplot(RNPDNO_V5_Informada, "SEXO", "EDAD_C", "EDAD por SEXO")

library(viridis)

# Definir paleta de colores
colores <- viridis(33)

ggplot(RNPDNO_V5_Informada, aes(x = ESTADO_BUSQUEDA, group = ESTADO, color = ESTADO)) +
  geom_point(stat = "count") +
  geom_text(stat = "count", aes(label = ESTADO, vjust = -0.5), size = 3, nudge_y = 0.5) +  # Añadir nombres de estados
  labs(title = "Distribución de estado de búsqueda por sexo",
       x = "Estado de búsqueda", y = "Número de Casos") +
  scale_y_continuous(labels = scales::comma) +  # Mostrar números enteros en el eje y
  scale_color_manual(values = colores) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))















