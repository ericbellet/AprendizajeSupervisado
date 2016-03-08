# Seleccionar google_api.R en su sistema de archivos
source(file.choose())
df <- read_excel("C:/Users/EricBellet/Desktop/AprendizajeSupervisado/data/hogares.xlsx")
df <- na.omit(df)
df$Foto <- NULL

dataframe <- data.frame()
dist <- vector()
time <- vector()
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


dist <- c(dist,timedistancia[1])
time <- c(time,timedistancia[2])
ori <- c(ori, origen)
dist <- cbind(dist)
time <- cbind(time)

dataframe <- cbind(ori,dist,time)

}
