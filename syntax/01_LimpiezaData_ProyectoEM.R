#
# Materia:    Estadistica Multivariada
# Fecha:      11/06/2024
# Tema:       Proyecto Final
# Descripion: Base de datos de desaparicion de personas
# Archivo:    01_LimpiezaData_ProyectoEM.R
# Autora:     Semiramis G. de la Cruz

#---- LIMPIEZA Y LIBRERIAS -----------------------------------------------------

cat("\014")
rm(list = ls())

# Semilla
set.seed(200222)

#---- DATOS --------------------------------------------------------------------

# Cargar el conjunto de datos
file_in <- "C:/Users/semir/Documents/CIMAT/Estadistica/Proyecto/data/RNPDNO_V3.csv"
RNPDNO_V3 <- read.csv(file_in, header = TRUE)

# Importar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

summary(RNPDNO_V3)
sapply(RNPDNO_V3, class)
colSums(is.na(RNPDNO_V3))
dim(RNPDNO_V3) # [1] 115513, 19

#---- TABLAS -------------------------------------------------------------------

table(RNPDNO_V3$SEXO)

table(RNPDNO_V3$EDAD)

table(RNPDNO_V3$ESTADO_CIVIL)

table(RNPDNO_V3$NIVEL_EDUCATIVO)

table(RNPDNO_V3$ESTADO)

# Nueva copia del dataset
RNPDNO_V4 <- RNPDNO_V3

#---- LIMPIEZA -----------------------------------------------------------------

# Variables FECHAS y EDAD

RNPDNO_V4$FECHA_REPORTE_C <- as.Date(RNPDNO_V4$FECHA_REPORTE, format = "%d/%m/%Y")

RNPDNO_V4$FECHA_NACIMIENTO_C <- as.Date(RNPDNO_V4$FECHA_NACIMIENTO, 
                                      format = "%Y-%m-%d %H:%M:%S")

# Se calcula la EDAD si es posible
RNPDNO_V4$EDAD_DES <- floor(as.numeric(difftime(RNPDNO_V4$FECHA_REPORTE_C, 
                                                RNPDNO_V4$FECHA_NACIMIENTO_C, 
                                                units = "days")/365.25))

# Ejemplo de la correccion IDX_REPORTE: 01-000257-0
RNPDNO_V4 <- RNPDNO_V4 %>% 
  mutate(EDAD_C = if_else(EDAD == -99, 
                          ifelse(is.na(EDAD_DES), 
                                 EDAD, EDAD_DES), EDAD))
# EDAD_C en categorias
RNPDNO_V4 <- RNPDNO_V4 %>%
  mutate(EDAD_G = cut(EDAD_C, 
                        breaks = c(-Inf, 0, 12, 18, 30, 60, Inf),
                        labels = c("Desconocida", "Niñez", "Adolescencia", 
                                   "Juventud", "Adultez", "Adultez Mayor")))

# Reducimos categorias de ESTADO_CIVIL
RNPDNO_V4$ESTADO_CIVIL_G <- ifelse(RNPDNO_V4$ESTADO_CIVIL %in% c("Casada", "Concubina", "Union libre", 
                                                                 "Sociedad en convivencia"), "Relacion",
                                       ifelse(RNPDNO_V4$ESTADO_CIVIL %in% c("Soltera"), "Soltera",
                                              ifelse(RNPDNO_V4$ESTADO_CIVIL %in% c("Divorciada", "Separada"), 
                                                     "Separada/Divorciada", "Desconocido")))
# Reducimos categorias de NIVEL_EDUCATIVO
RNPDNO_V4$NIVEL_EDUCATIVO_G <- ifelse(RNPDNO_V4$NIVEL_EDUCATIVO %in% c("Preescolar", 
                                                                       "Primaria"), "Basico",
                                          ifelse(RNPDNO_V4$NIVEL_EDUCATIVO %in% c("Secundaria", 
                                                                                 "Preparatoria"), "Medio",
                                                 ifelse(RNPDNO_V4$NIVEL_EDUCATIVO %in% c("Licenciatura", 
                                                                                         "Maestria"), "Superior", "Desconocido")))
# Anio del reporte
RNPDNO_V4$ANIO_REPORTE <- format(as.Date(RNPDNO_V4$FECHA_REPORTE_C, 
                                         format = "%Y-%m-%d"), "%Y")
anio_actual <- as.numeric(format(Sys.Date(), "%Y"))


# FLAG de datos faltantes
RNPDNO_V4$FLAG <- ifelse(RNPDNO_V4$EDAD == -99 | RNPDNO_V4$SEXO == "Desconocido" | 
                           RNPDNO_V4$ESTADO_CIVIL == "Desconocido" | 
                           RNPDNO_V4$NIVEL_EDUCATIVO == "Desconocido" , 1, 0)


#---- DATA FINAL ---------------------------------------------------------------

# Nos quedamos con las columnas finales
RNPDNO_V5 <- RNPDNO_V4 %>%              # [1] 115513     12
  select(c("IDX_REPORTE", "FECHA_REPORTE_C", "SEXO", "ESTADO_CIVIL", 
           "NIVEL_EDUCATIVO", "ESTADO", "EDAD_C", "EDAD_G", "ESTADO_CIVIL_G",
           "NIVEL_EDUCATIVO_G", "ANIO_REPORTE", "FLAG" ))


# Filtros para conformar una data sin faltantes y sin outliers en EDAD_C
RNPDNO_V5 <- RNPDNO_V5 %>%
  filter((EDAD_C <= 100 & EDAD_C >= 0) | EDAD_C == -99) # 115492, 11

RNPDNO_V5_Informada <- RNPDNO_V5 %>% 
  filter(FLAG == 0)


# Guardar data final
file_out <- "C:/Users/semir/Documents/CIMAT/Estadistica/Proyecto/data/"
ruta_archivo <- paste0(file_out,  "RNPDNO_V5.csv")
write.csv(RNPDNO_V5, file = ruta_archivo, row.names = FALSE)

ruta_archivo2 <- paste0(file_out,  "RNPDNO_V5_Informada.csv")
write.csv(RNPDNO_V5_Informada, file = ruta_archivo2, row.names = FALSE)



