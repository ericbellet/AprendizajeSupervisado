library(sqldf)
#Funcion que instala los paquetes necesarios para correr el script.
install = function(pkg){
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http:/cran.rstudio.com")
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

install("sqldf")

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
df$GastosTotalResponsable <- df$grAlimentacion + df$grTransporte + df$grMedicos + df$grOdontologicos + df$grEducativos + df$grVivienda + df$grServicios + df$grCondominio + df$grOtros

#Seleccionamos variables predictoras.
df <- select(df, fNacimiento, sexo, crFamiliar, IngresosTotalPersonal, GastosTotalPersonal, IngresosTotalResponsable, GastosTotalResponsable,mIngreso)

#Generamos el training y testing data.            
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))
trainingData <- df[ind==1,]
testingData <- df[ind==2,]                      
                      