install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#Instalo automaticamente los paquetes.
install('sqldf')
install('dplyr')
install('class')
install('FactoMineR')
install('caret')
install('rpart')
install('rpart.plot')
install('RWeka')
install('pROC')
install('e1071')
install('som')

#Cargo las librerias.
library(sqldf)
library(dplyr)
library(class)
library(FactoMineR)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(som)
library(RWeka)
library(pROC)

#Leemos el .csv
df <- read.csv("C:/Users/EricBellet/Desktop/AprendizajeSupervisado/data/minable.csv") 

#Seleccionar variables que permitan etiquetar el modo de ingreso de la persona.
# 0 -> Asignado OPSU.
# 1 -> Convenios Interinstitucionales (nacionales e internacionales).
# 2 -> Convenios Internos(Deportistas,artistas, hijos empleados docente y obreros,Samuel Robinson).
# 3 -> Prueba Interna y/o propedeutico.

#Calculamos los ingresos personales del estudiante.
df$IngresosTotalPersonal <- df$aResponsable + df$iActividades + df$aOtros

#Calculamos los gastos personales del estudiante.
df$GastosTotalPersonal <- df$gAlimentacion + df$gTransporte + df$gMedicos + 
  df$gOdontologicos + df$gAlquiler + df$gEstudios + df$gPersonales + df$gRecreacion + df$gOtros

#Calculamos los ingresos del responsable economico del estudiante.
df$IngresosTotalResponsable <- df$irMensual + df$irOtros

#Calculamos los gastos del responsable economico del estudiante.
df$grOdontologicos <- as.numeric(df$grOdontologicos)
df$GastosTotalResponsable <- df$grAlimentacion + df$grTransporte + df$grMedicos + 
  df$grOdontologicos + df$grEducativos + df$grVivienda + df$grServicios + df$grCondominio + 
  df$grOtros

#Eliminamos las columnas que no vamos a utilizar.
df$cIdentidad <- NULL
df$fNacimiento <- NULL
df$eCivil <- NULL
df$jReprobadas <- NULL
df$pReside <- NULL
df$dHabitacion <- NULL
df$cDireccion <- NULL
df$oSolicitudes <- NULL
df$aEconomica <- NULL
df$rating <- NULL
df$sugerencias <- NULL


#Seleccionamos variables predictoras.
df <- select(df, sexo, escuela, aIngreso, sCurso, tGrado, mInscritas,
            pAprobado, eficiencia, mAprobadas, mRetiradas, mReprobadas, pRenovar, beca,
            lProcedencia, lResidencia, tVivienda, rEconomico,
            crFamiliar, IngresosTotalPersonal, GastosTotalPersonal, IngresosTotalResponsable,
            GastosTotalResponsable,mIngreso)

#ANALISIS EXPLORATORIO DE LOS DATOS
PCA <- PCA(df)
sum(df[,"mIngreso"] == 0)
sum(df[,"mIngreso"] == 1)
sum(df[,"mIngreso"] == 2)
sum(df[,"mIngreso"] == 3)

#Obtengo los valore unicos de mIngreso.
valores <- unique(df$mIngreso)
totalvalores <- nrow(df)
probabilidad <- vector()
#Calculo la probabilidad de cada valor de mIngreso.
for (i in 1:length(valores)){
  probabilidad <- c(probabilidad, sum(df$mIngreso == valores[i]) / totalvalores) 
}
asignarProb <- function(x){
  for (i in 1:length(valores)) {
    if (valores[i] == x){
       return(probabilidad[i])
    }
    
  }
}
#Obtengo un vector de probabilidades para cada valor de mIngreso.
probabilidades <- lapply(df$mIngreso, asignarProb)
probabilidades<-unlist(probabilidades)
#**********************************************************************************
#----------Genero un train y test data estratificado-------------------------------
#**********************************************************************************
set.seed(1)
sets <- sample(nrow(df), nrow(df)*0.7, prob=probabilidades, replace=F)
training <- df[sets,]
testing <- df[-sets,]
#----------------------------------------------------------------------------------
#**********************************************************************************
#---------------------------------K-nearest-neighbours-----------------------------
#**********************************************************************************
#Genero las etiquetas.
cl <- training$mIngreso
#Normalizo el training y el testing para poder aplicar knn.
trainingN <-  as.data.frame(lapply(training, function (x) normalize(x)))
testingN <-  as.data.frame(lapply(testing, function (x) normalize(x)))
#testingN$mIngreso <- NULL
#Genero knn, con k=
knnModel<-knn(trainingN, testingN, cl, k = 3, prob=TRUE)

matrizconfusion <- table(testing$mIngreso,knnModel,dnn=c("Valor Real", "Prediccion"))

error <- (sum(knnModel != testing$mIngreso) /nrow(testing))
aciertoknn <- (1-error)*100
print(aciertoknn)
errorknn <- error*100


knnModel<-as.numeric(knnModel)
knnModelROC <- roc(testing$mIngreso, knnModel)
plot(knnModelROC,type="l",col="red")
#**********************************************************************************
#---------------------------------Arboles de Decision-----------------------------
#**********************************************************************************
minsplits <- c(35,35,35,35,35)
cps <- c( 0.01,0.2,0.1,0.0001,0.0000000000001)
minbuckets <- c(50,5,10,50,300)
maxdepths <- c(30,5,10,20,30)
for (i in 1:length(minsplits)){
modelo <- rpart(mIngreso ~ ., data = training, method = "class",
                control = rpart.control(minsplit = minsplits[i], 
                                        cp = cps[i],minbuckets=minbuckets[i],
                                        maxdepth = maxdepths[i]))

arbol <- predict(modelo, newdata = testing,type = "class")

matrizconfusion <- table(testing$mIngreso,arbol,dnn=c("Valor Real", "Prediccion"))
error <- (sum(arbol != testing$mIngreso) /nrow(testing))
aciertoarbol <- (1-error)*100
errorarbol <- error*100
print(aciertoarbol)
print(errorarbol)
}


arbol<-as.numeric(arbol)
arbolROC <- roc(testing$mIngreso, arbol)
plot(arbolROC,type="l",col="green")



#**********************************************************************************
#---------------------------------Reglas de clasificacion--------------------------
#**********************************************************************************
training$mIngreso = as.factor(training$mIngreso)
modelo <- JRip(mIngreso ~ ., training)
reglas <- predict(modelo, testing,type = "class")

matrizconfusion <- table(testing$mIngreso,reglas,dnn=c("Valor Real", "Prediccion"))
error <- (sum(reglas != testing$mIngreso) /nrow(testing))
aciertoreglas <- (1-error)*100
print(acierto)
errorreglas <- error*100

reglas<-as.numeric(reglas)
reglasROC <- roc(testing$mIngreso, reglas)
plot(reglasROC,type="l",col="blue")
#**********************************************************************************
#---------------------------Comparacion entre los modelos--------------------------
#**********************************************************************************
print(paste0("Precisión general de K-nearest-neighbours: ", aciertoknn))
print(paste0("Precisión general de arboles de decisión: ", aciertoarbol))
print(paste0("Precisión general de reglas de clasificación: ", aciertoreglas))

plot(reglasROC,type="l",col="blue")
lines(arbolROC,col="green")
lines(knnModelROC,col="red")
legend(0.1,0.4,legend=c('KNN', 'Reglas de clasificación','Arboles de decisión'),
       col=c('red', 'blue', 'green'), lty=1, cex=0.5)