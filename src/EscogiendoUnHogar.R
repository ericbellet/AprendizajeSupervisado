library(readxl)
library(dplyr)
df <- read_excel("C:/Users/EricBellet/Desktop/AprendizajeSupervisado/data/hogares.xlsx")
df <- na.omit(df)
df$Foto <- NULL

#AGREGO FILAS POR CADA HABITACION DISPONIBLE CON SU CORRESPONDIENTE PRECIO.
df$`Habitaciones Disponibles`
df$Disponibles[grepl("1 singola", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("1 singole", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("1 Singola", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("1 Singola", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("Intero Appartamento", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("Intero appartamento", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("Mini Appartamento", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("intero appartamento", df$`Habitaciones Disponibles`)] <- "1"
df$Disponibles[grepl("Mini Appartamento", df$`Habitaciones Disponibles`)] <- "1"

df$Disponibles[grepl("2 singola", df$`Habitaciones Disponibles`)] <- "2"
df$Disponibles[grepl("2 singole", df$`Habitaciones Disponibles`)] <- "2"
df$Disponibles[grepl("2 Singola", df$`Habitaciones Disponibles`)] <- "2"
df$Disponibles[grepl("2 Singola", df$`Habitaciones Disponibles`)] <- "2"

df$Disponibles[grepl("3 singola", df$`Habitaciones Disponibles`)] <- "3"
df$Disponibles[grepl("3 singole", df$`Habitaciones Disponibles`)] <- "3"
df$Disponibles[grepl("3 Singola", df$`Habitaciones Disponibles`)] <- "3"
df$Disponibles[grepl("3 Singola", df$`Habitaciones Disponibles`)] <- "3"

df$Disponibles[grepl("4 singola", df$`Habitaciones Disponibles`)] <- "4"
df$Disponibles[grepl("4 singole", df$`Habitaciones Disponibles`)] <- "4"
df$Disponibles[grepl("4 Singola", df$`Habitaciones Disponibles`)] <- "4"
df$Disponibles[grepl("4 Singola", df$`Habitaciones Disponibles`)] <- "4"

df$Disponibles[is.na(df$Disponibles)] <- 1

df.expanded <- df[rep(seq_len(nrow(df)), df$Disponibles),]

#DIVIDO EL DATAFRAME EN DOS, MUJERES Y HOMBRES.
df$Sexo <- df$Notas
df$Sexo[grepl("ragazzi/e", df$Sexo)] <- "Ambos"
df$Sexo[grepl("ragazze/i", df$Sexo)] <- "Ambos"
df$Sexo[grepl("ragazzi/ragazze", df$Sexo)] <- "Ambos"
df$Sexo[grepl("ragazze", df$Sexo)] <- "Femenino"
df$Sexo[grepl("ragazzi", df$Sexo)] <- "Masculino"
df$Sexo[!grepl("Ambos", df$Sexo) & !grepl("Masculino", df$Sexo) & !grepl("Femenino", df$Sexo)] <- "Ambos"
dfM <- df[df$Sexo == 'Masculino' | df$Sexo == 'Ambos',]
dfF <- df[df$Sexo == 'Femenino' | df$Sexo == 'Ambos',]
#----------------------------------------------------------
