install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#Instalo automaticamente los paquetes.
install('readxl')
#install('dplyr')
install('jsonlite')
install('pROC')
install('FactoMineR')
install('pscl')
install('MASS')

library(readxl)
#library(dplyr)
library(jsonlite)
library(pROC)
library(FactoMineR)
library(pscl)
library(MASS)
#--------------------------------------API----------------------------------------
# Seleccionar google_api.R en su sistema de archivos
source(file.choose())
#source("C:/Users/EricBellet/Desktop/AprendizajeSupervisado/src/Escogiendo un Hogar/google_api.R")
df <- read_excel("C:/Users/EricBellet/Desktop/AprendizajeSupervisado/data/hogares.xlsx")
df <- na.omit(df)
df$Foto <- NULL

#Inicializamos dataframe y vectores.
dataframe <- data.frame()
Distancia <- vector()
Minutos <- vector()
ori <- vector()

destino =c("Piazzale Aldo Moro")
origen =c("Via Gatteschi")
#Realizo un ciclo para hacer peticiones al API dirección por dirección.
for (origen in df$Dirección){
#Elimino los \n
origen <- strsplit(as.character(origen), "\n")

#Utilizo mi API key
api_key = "AIzaSyD04qKCMM18-iQzY6QK1MSmmii_aVhqUPE"
api_url = get_url(origen, destino, api_key)
datos = get_data(api_url)
#Parseo los datos obtenidos.
timedistancia = parse_data(datos)

#Concateno las distancia y el tiempo que arroja el API.
Distancia <- c(Distancia,timedistancia[1])
Minutos <- c(Minutos,timedistancia[2])
ori <- c(ori, origen)
Distancia <- cbind(Distancia)
Minutos <- cbind(Minutos)
#Guardo todos los datos parseados en un dataframe.
dataframe <- cbind(ori,Distancia,Minutos)
}#endfor
#--------------------------------------FIN API----------------------------------------
#--------------------------------------MINUTOS----------------------------------------
dataframe <- as.data.frame(dataframe)
#Tranformamos todos los tiempos a minutos
enHoras <- grepl("h",dataframe$Minutos)
for (i in 1:length(enHoras)){
  if (enHoras[i] == TRUE){
  num <-  as.numeric(unlist(strsplit(unlist(dataframe$Minutos[i]), "[^0-9]+")))
  dataframe$Minutos[i] <- (num[1]*60) + num[2]
  }else{
    num <-  as.numeric(unlist(strsplit(unlist(dataframe$Minutos[i]), "[^0-9]+")))
    dataframe$Minutos[i] <- num[1]
  }#endif
}#endfor
#Agrego las columnas distancia y tiempo al dataframe que se utilizara para la regresion lineal.
df$Distancia <- dataframe$Distancia
df$Minutos <- dataframe$Minutos
#Elimino las filas cuya direccion el API no encontro.
df <- df[!df$Minutos == "NA", ]
#Asigno un valor de importancia a los tiempos
df$Minutos <- as.numeric(df$Minutos)
df["ValorMinutos"] <- as.factor(ordered(cut(df$Minutos, c(-Inf,15,60,120,180,240,300,360,420,600,Inf)),labels = c("160", "100", "90", "60","50","20","10","5","1","0")))

#Realizo una transformacion a la columna valor minutos para poder utilizarla.

for (i in 1:nrow(df)){
  df$ValorMinutos2[i] <- as.numeric(as.character(df["ValorMinutos"][[1]][i]))
  
}
  

#--------------------------------------FIN MINUTOS----------------------------------------
#--------------------------------------HABITACIONES----------------------------------------
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

#Replico las filas que posee mas de una habitacion disponible.
df <- df[rep(seq_len(nrow(df)), df$Disponibles),]

#Asigno precio a cada habitacion.
i <- 1
while (i != (nrow(df)+1)) {
  array <- na.omit(as.numeric(unlist(strsplit(unlist(df$`Precio Mensual`[i]), 
                                              "[^0-9]+"))))
 if (df$Disponibles[i] == 1){
   df$PrecioTotal[i] <- array[1]
   i <- i + 1
 }else{
   for (j in 1:length(array)) {
    df$PrecioTotal[i + (j-1)] <- array[j]
   }
   i <- i + as.numeric(df$Disponibles[i])
 }

}#endwhile
#--------------------------------------FIN HABITACIONES----------------------------------------
#--------------------------------------PRECIO MENSUAL----------------------------------------
#Creo una columna donde coloco el valor si esta todo incluido o no.
df$TuttoIncluido[grepl("TUTTO INCLUSO", df$`Precio Mensual`)] <- 100
df$TuttoIncluido[grepl("Tutto incluso", df$`Precio Mensual`)] <- 100
df$TuttoIncluido[grepl("tutto incluso", df$`Precio Mensual`)] <- 100
df$TuttoIncluido[is.na(df$TuttoIncluido)] <- 0

#Columna a utilizar para la regresion lineal donde 1 es si tiene todo incluido y 0 es no
df$TodoIncluido[grepl("TUTTO INCLUSO", df$`Precio Mensual`)] <- 1
df$TodoIncluido[grepl("Tutto incluso", df$`Precio Mensual`)] <- 1
df$TodoIncluido[grepl("tutto incluso", df$`Precio Mensual`)] <- 1
df$TodoIncluido[is.na(df$TodoIncluido)] <- 0
#--------------------------------------FIN PRECIO MENSUAL----------------------------------------
#--------------------------------------DESCRIPCION----------------------------------------
#Etiqueto la columna descripcion donde separo por coma y e (solo el conector).
separador <- function(x)
  splat <- unlist(strsplit(x, ", | e "))

df$Descripción2 <- lapply(df$Descripción, separador)
x <- vector()
#Genero columnas que utilizare en la regresion lineal
#Descripcion3 es una columna que genera valor segun los componente de la habitacion.
df$Descripción3 <- 0
df$Pasillo <- 0
df$Cocina <- 0
df$Cuarto <- 0
df$Bagno <- 0
df$Balcon <- 0
df$Comedor <- 0
df$Armario <- 0
df$Salon <- 0
#Obtengo cuantas y cuales habitaciones tiene el apartamento y genero valor.
for (i in 1:nrow(df)) {
   for (j in 1:length(unlist(df$Descripción2[i]))) {
  
    x[1] <- as.numeric(unlist(strsplit(unlist(df$Descripción2[i])[j], 
                             "[^0-9]+")))

    if (is.na(x) == TRUE){
      x[1] <- 1
    }
  
    if (grepl("Ingresso", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Pasillo[i] <- df$Pasillo[i] +x[1]
    }
    if (grepl("ingresso", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Pasillo[i] <- df$Pasillo[i] +x[1]
    }

    if (grepl("cucina", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (20 * x[1])
      df$Cocina[i] <- df$Cocina[i] +x[1]
    }
    
    if (grepl("angolo cottura", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (20 * x[1])
      df$Cocina[i] <- df$Cocina[i] +x[1]
    }
    
    if (grepl("stanze", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (10 * x[1])
      df$Cuarto[i] <- df$Cuarto[i] +x[1]
    }
    
    if (grepl("camere", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (10 * x[1])
      df$Cuarto[i] <- df$Cuarto[i] +x[1]
    }
    
    if (grepl("camera", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (10 * x[1])
      df$Cuarto[i] <- df$Cuarto[i] +x[1]
    }
    
    if (grepl("bagni", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (15 * x[1])
      df$Bagno[i] <- df$Bagno[i] +x[1]
    }
    
    if (grepl("bagno", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (15 * x[1])
      df$Bagno[i] <- df$Bagno[i] +x[1]
    }
    
    if (grepl("disimpegno", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
    }
    
    if (grepl("balcone", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Balcon[i] <- df$Balcon[i] +x[1]
    }
    
    if (grepl("ampiio terrazzo", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Balcon[i] <- df$Balcon[i] +x[1]
    }
    
    if (grepl("sala da pranzo", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (30 * x[1])
      df$Comedor[i] <- df$Comedor[i] +x[1]
    }
    
    if (grepl("doppio soggiorno", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (20 * x[1])
      df$Salon[i] <- df$Salon[i] +x[1]
    }
    
    if (grepl("salotto", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (20 * x[1])
      df$Salon[i] <- df$Salon[i] +x[1]
    }
    
    if (grepl("armario", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Armario[i] <- df$Armario[i] +x[1]
    }
    
    if (grepl("ripostiglio", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Armario[i] <- df$Armario[i] +x[1]
    }
    
    if (grepl("corridoio", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
      df$Pasillo[i] <- df$Pasillo[i] +x[1]
    }
    
    if (grepl("Appartamento su due livelli", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (50 * x[1])
    }
  }#endfor
}#endfor
#MUESTRA UN WARNING QUE EN REALIDAD NO CAUSA NINGUN PROBLEMA
#--------------------------------------FIN DESCRIPCION----------------------------------------
#Categorizo los tipos de habitacion.
df$TipoHabitacion[grepl("Intero Appartamento", df$`Habitaciones Disponibles`)] <- "1"
df$TipoHabitacion[grepl("Intero appartamento", df$`Habitaciones Disponibles`)] <- "1"
df$TipoHabitacion[grepl("intero appartamento", df$`Habitaciones Disponibles`)] <- "1"

df$TipoHabitacion[grepl("monolocale", df$`Habitaciones Disponibles`)] <- "2"
df$TipoHabitacion[grepl("Mini Appartamento", df$`Habitaciones Disponibles`)] <- "2"

df$TipoHabitacion[grepl("posto letto", df$`Habitaciones Disponibles`)] <- "4"
df$TipoHabitacion[grepl("doppia", df$`Habitaciones Disponibles`)] <- "5"
df$TipoHabitacion[grepl("doppie", df$`Habitaciones Disponibles`)] <- "5"

df$TipoHabitacion[grepl("singola", df$`Habitaciones Disponibles`)] <- "3"
df$TipoHabitacion[grepl("singole", df$`Habitaciones Disponibles`)] <- "3"
df$TipoHabitacion[grepl("Singola", df$`Habitaciones Disponibles`)] <- "3"
df$TipoHabitacion[grepl("Singole", df$`Habitaciones Disponibles`)] <- "3"

#-------------------Cual es la mejor habitacion-------------------------
#df$Disponibles <- NULL
#df$Descripción2 <- NULL
df$valorInmobiliario <- df$Descripción3
#df$Descripción3 <- NULL

#El valor de un apto esta relacionado con el tiempo, cuantas habitaciones posee
# y si el precio tiene todo incluido

df$ValorTotal <- df$ValorMinutos2 + df$valorInmobiliario + df$TuttoIncluido

df$ValorMinutos <- NULL
#df$ValorMinutos2 <- NULL
#df$TuttoIncluido <- NULL
#df$valorInmobiliario <- NULL

#Escalo o estandarizo el valor entre 1 y 10.
range01 <- function(x){((x-min(x))/(max(x)-min(x)))*10}
df$ValorTotal <- range01(df$ValorTotal)
df$TipoHabitacion <- as.numeric(df$TipoHabitacion)

df$Disponibles <- as.numeric(df$Disponibles)
df$TipoHabitacion <- as.numeric(df$TipoHabitacion)
#DIVIDO EL DATAFRAME EN DOS, MUJERES Y HOMBRES.
df$Sexo <- df$Notas
df$Sexo[grepl("ragazzi/e", df$Sexo)] <- "Ambos"
df$Sexo[grepl("ragazze/i", df$Sexo)] <- "Ambos"
df$Sexo[grepl("ragazzi/ragazze", df$Sexo)] <- "Ambos"
df$Sexo[grepl("ragazze", df$Sexo)] <- "Femenino"
df$Sexo[grepl("ragazzi", df$Sexo)] <- "Masculino"
df$Sexo[!grepl("Ambos", df$Sexo) & !grepl("Masculino", 
                                          df$Sexo) & !grepl("Femenino", df$Sexo)] <- "Ambos"
dfM <- df[df$Sexo == 'Masculino' | df$Sexo == 'Ambos',]
dfF <- df[df$Sexo == 'Femenino' | df$Sexo == 'Ambos',]
#----------------------------------------------------------
dfM$Sexo <- NULL
dfF$Sexo <- NULL
dfM$Notas <- NULL
dfF$Notas <- NULL

#************************************************************************************
#--------------------------SAMPLING PARA MASCULINO----------------------------------
#************************************************************************************
#Obtengo los valore unicos de mIngreso para los hombres.
valores <- unique(dfM$PrecioTotal)
totalvalores <- nrow(dfM)
probabilidad <- vector()
#Calculo la probabilidad de cada valor de mIngreso.
for (i in 1:length(valores)){
  probabilidad <- c(probabilidad, sum(dfM$PrecioTotal == valores[i]) / totalvalores) 
}
asignarProb <- function(x){
  for (i in 1:length(valores)) {
    if (valores[i] == x){
      return(probabilidad[i])
    }
    
  }
}
#Obtengo un vector de probabilidades para cada valor de PrecioTotal
probabilidades <- lapply(dfM$PrecioTotal, asignarProb)
probabilidades<-unlist(probabilidades)
#**********************************************************************************
#----------Genero un train y test data estratificado para hombres------------------
#**********************************************************************************
set.seed(1)
sets <- sample(nrow(dfM), nrow(dfM)*0.8, prob=probabilidades, replace=F)
trainingM <- dfM[sets,]
testingM <- dfM[-sets,]
#************************************************************************************
#--------------------------SAMPLING PARA FEMENINO----------------------------------
#************************************************************************************
#Obtengo los valore unicos de mIngreso para mujeres
valores <- unique(dfF$PrecioTotal)
totalvalores <- nrow(dfF)
probabilidad <- vector()
#Calculo la probabilidad de cada valor de mIngreso.
for (i in 1:length(valores)){
  probabilidad <- c(probabilidad, sum(dfF$PrecioTotal == valores[i]) / totalvalores) 
}
asignarProb <- function(x){
  for (i in 1:length(valores)) {
    if (valores[i] == x){
      return(probabilidad[i])
    }
    
  }
}
#Obtengo un vector de probabilidades para cada valor de PrecioTotal
probabilidades <- lapply(dfF$PrecioTotal, asignarProb)
probabilidades<-unlist(probabilidades)
#**********************************************************************************
#----------Genero un train y test data estratificado para mujeres------------------
#**********************************************************************************
set.seed(1)
sets <- sample(nrow(dfF), nrow(dfF)*0.8, prob=probabilidades, replace=F)
trainingF <- dfF[sets,]
testingF <- dfF[-sets,]
#---------------------------------------------------------------------------
#************************************************************************************
#----------------------ANALISIS EXPLORATORIO DE LOS DATOS----------------------------
#************************************************************************************
#Selecciono las variables que puedo utilizar para aplicar regresion lineal
#para realizar un analisis exploratorio de los datos

Femenino <- select(dfF, Minutos, TodoIncluido, TipoHabitacion, Pasillo, Cocina, Cuarto, Bagno, Balcon, Comedor, Armario, 
                           Salon, PrecioTotal, ValorTotal)

PCA <- PCA(Femenino)
plot(PCA, choix = "var")

Masculino <- select(dfM, Minutos, TodoIncluido, TipoHabitacion, Pasillo, Cocina,Cuarto, Bagno, Balcon, Comedor, Armario, 
                            Salon, PrecioTotal, ValorTotal)

PCA <- PCA(Masculino)
plot(PCA, choix = "var")

#OBTENGO LA MEJOR HABITACION PARA HOMBRE.
dfM <- dfM[order(dfM$PrecioTotal) , ]
medianaMM <- median(dfM$PrecioTotal)
mejoresHabitacionesM <- subset(dfM , PrecioTotal < medianaMM)
mejorHabitacionesM <- mejoresHabitacionesM[which(mejoresHabitacionesM$ValorTotal == max(mejoresHabitacionesM$ValorTotal)), ]
print(mejorHabitacionesM$Distrito)
print(mejorHabitacionesM$Dirección)
print(mejorHabitacionesM$Descripción)
print(mejorHabitacionesM$PrecioTotal)
print(mejorHabitacionesM$ValorTotal)
#----------------------------------------------------------------

#OBTENGO LA MEJOR HABITACION PARA LAS MUJERES.
dfF <- dfF[order(dfF$PrecioTotal) , ]
medianaMF <- median(dfF$PrecioTotal)
mejoresHabitacionesF <- subset(dfF , PrecioTotal < medianaMF)
mejorHabitacionesF <- mejoresHabitacionesF[which(mejoresHabitacionesF$ValorTotal == max(mejoresHabitacionesF$ValorTotal)), ]
print(mejorHabitacionesF$Distrito)
print(mejorHabitacionesF$Dirección)
print(mejorHabitacionesF$Descripción)
print(mejorHabitacionesF$PrecioTotal)
print(mejorHabitacionesF$ValorTotal)
#----------------------------------------------------------------


#************************************************************************************
#--------------------------REGRESION LINEAL-----------------------------------------
#************************************************************************************
#Selección de variables
#Realice la selección de variables por pasos (forward, backward, both) utilizando la 
#función stepAIC () del paquete MASS.  stepAIC () realiza la selección del modelo paso 
#a paso por la AIC exacta.
#------------------------------MASCULINO------------------------------------------
#Aplico regresion lineal con todas las variables
modeloM1 <- lm(PrecioTotal ~ Minutos + TodoIncluido + TipoHabitacion + Pasillo +
               Cocina + Cuarto + Bagno + Balcon + Comedor + Armario + Salon +
               ValorTotal,data = trainingM)

regresionM1 <- predict(modeloM1, newdata = testingM)

#Evaluo, y busco las mejores variables para predecir
step <- stepAIC(modeloM1, direction="both")
step$anova # display results

#En teoria las mejores variables para predecir son:
modeloM2 <- lm(PrecioTotal ~ Minutos + TodoIncluido + TipoHabitacion + Balcon,data = trainingM)

regresionM2 <- predict(modeloM2, newdata = testingM)

anova(modeloM1, modeloM2)
comparacionM <- cbind(regresionM2,testingM$PrecioTotal )



#-----------------------------FIN MASCULINO------------------------------------------
#------------------------------FEMENINO------------------------------------------
#Aplico regresion lineal con todas las variables
modeloF1 <- lm(PrecioTotal ~ Minutos + TodoIncluido + TipoHabitacion + Pasillo +
                 Cocina + Cuarto + Bagno + Balcon + Comedor + Armario + Salon +
                 ValorTotal,data = trainingF)

regresionF1 <- predict(modeloF1, newdata = testingF)

#Evaluo, y busco las mejores variables para predecir
step <- stepAIC(modeloF1, direction="both")
step$anova # display results

#En teoria las mejores variables para predecir son:
modeloF2 <- lm(PrecioTotal ~ Minutos + TodoIncluido + TipoHabitacion + Balcon,data = trainingF)

regresionF2 <- predict(modeloF2, newdata = testingF)

anova(modeloM1, modeloM2)
comparacionF <- cbind(regresionF2,testingF$PrecioTotal )
#-----------------------------FIN FEMENINO------------------------------------------

#utilizando regresion lineal para decidir el mejor apto.
#OBTENGO LA MEJOR HABITACION PARA HOMBRE.
hombre <- lm(PrecioTotal ~ Minutos + TodoIncluido + TipoHabitacion + Balcon,data = dfM)

hombre <- predict(hombre, newdata = dfM)
dfM$PrediccionPrecio <- cbind(hombre)

dfM <- dfM[order(dfM$PrecioTotal) , ]
medianaMM <- median(dfM$PrecioTotal)
mejoresHabitacionesM <- subset(dfM , PrecioTotal < medianaMM)
mejorHabitacionesM <- mejoresHabitacionesM[which(mejoresHabitacionesM$PrediccionPrecio == max(mejoresHabitacionesM$PrediccionPrecio)), ]
print(mejorHabitacionesM$Distrito)
print(mejorHabitacionesM$Dirección)
print(mejorHabitacionesM$Descripción)
print(mejorHabitacionesM$PrecioTotal)
print(mejorHabitacionesM$ValorTotal)

#OBTENGO LA MEJOR HABITACION PARA LAS MUJERES.
mujer <- lm(PrecioTotal ~ Minutos + TodoIncluido + TipoHabitacion + Balcon,data = dfF)

mujer <- predict(mujer, newdata = dfF)
dfF$PrediccionPrecio <- cbind(mujer)

dfF <- dfF[order(dfF$PrecioTotal) , ]
medianaMF <- median(dfF$PrecioTotal)
mejoresHabitacionesF <- subset(dfF , PrecioTotal < medianaMF)
mejorHabitacionesF <- mejoresHabitacionesF[which(mejoresHabitacionesF$PrediccionPrecio == max(mejoresHabitacionesF$PrediccionPrecio)), ]
print(mejorHabitacionesF$Distrito)
print(mejorHabitacionesF$Dirección)
print(mejorHabitacionesF$Descripción)
print(mejorHabitacionesF$PrecioTotal)
print(mejorHabitacionesF$ValorTotal)




