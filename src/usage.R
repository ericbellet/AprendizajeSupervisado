library(readxl)
library(dplyr)
library(jsonlite)

# Seleccionar google_api.R en su sistema de archivos
source(file.choose())
df <- read_excel("C:/Users/EricBellet/Desktop/AprendizajeSupervisado/data/hogares.xlsx")
df <- na.omit(df)
df$Foto <- NULL

#Inicializamos dataframe y vectores.
dataframe <- data.frame()
Distancia <- vector()
Minutos <- vector()
ori <- vector()


#destino =c("Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro","Piazzale Aldo Moro")
#origen = c("Piazza Massa Carrara","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi","Via Roberto Ferruzzi")
destino =c("Piazzale Aldo Moro")
for (origen in df$Dirección){
origen <- strsplit(as.character(origen), "\n")
# Colocar su API Key 
api_key = "AIzaSyAjvofBw1RWb-hBEXg9ZXvToXlndevczxg"
api_url = get_url(origen, destino, api_key)
datos = get_data(api_url)
timedistancia = parse_data(datos)


Distancia <- c(Distancia,timedistancia[1])
Minutos <- c(Minutos,timedistancia[2])
ori <- c(ori, origen)
Distancia <- cbind(Distancia)
Minutos <- cbind(Minutos)

dataframe <- cbind(ori,Distancia,Minutos)
}#endfor
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

df$Distancia <- dataframe$Distancia
df$Minutos <- dataframe$Minutos

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
#---------------------------------------
#Etiqueto la columna descripcion.
separador <- function(x)
  splat <- unlist(strsplit(x, ", | e "))

df$Descripción2 <- lapply(df$Descripción, separador)
x <- vector()
df$Descripción3 <- 0
for (i in 1:nrow(df)) {
   for (j in 1:length(unlist(df$Descripción2[i]))) {
  
    x[1] <- as.numeric(unlist(strsplit(unlist(df$Descripción2[i])[j], 
                             "[^0-9]+")))

    if (is.na(x) == TRUE){
      x[1] <- 1
    }
  
    if (grepl("Ingresso", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
    }
    if (grepl("ingresso", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
    }

    if (grepl("cucina", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (20 * x[1])
    }
    
    if (grepl("stanze", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (10 * x[1])
    }
    
    if (grepl("camere", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (10 * x[1])
    }
    
    if (grepl("camera", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (10 * x[1])
    }
    
    if (grepl("bagni", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (15 * x[1])
    }
    
    if (grepl("bagno", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (15 * x[1])
    }
    
    if (grepl("disimpegno", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
    }
    
    if (grepl("balcone", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (5 * x[1])
    }
    
    if (grepl("sala da pranzo", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (30 * x[1])
    }
    
    if (grepl("doppio soggiorno", unlist(df$Descripción2[i])[j]) == TRUE){
      df$Descripción3[i] <- df$Descripción3[i] + (20 * x[1])
    }
  }#endfor
}#endfor

x <- unlist(df$Descripción2[1])
x[1]
Ingresso
cucina
stanze, camere
bagni bagno
cucina
disimpegno
balcone
doppio soggiorno
sala da pranzo
salone doppio

#Coloco etiquetas correspondientes a cada habitacion.
#Todo Incluido.
df$GastosExtras[grepl("TUTTO INCLUSO", df$`Precio Mensual`)] <- "0"
df$GastosExtras[grepl("Tutto incluso", df$`Precio Mensual`)] <- "0"

#Excluidos los costes
df$GastosExtras[grepl("spese escluse", df$`Precio Mensual`)] <- "1"

condominio, acqua, tassa rifiuti e internet inclusi
condominio, acqua, riscaldamento inclusi e tassa rifiuti inclusi
condominio incluso
df$`Precio Mensual`[18]




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

