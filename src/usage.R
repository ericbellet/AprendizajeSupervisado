
# Seleccionar google_api.R en su sistema de archivos
source(file.choose())

origen = c("Via Paolo Emilio", "Vancouver BC", "Seattle")
destino =c("Piazzale Aldo Moro", "San Francisco", "Victoria BC")

# Colocar su API Key 
api_key = "AIzaSyAjvofBw1RWb-hBEXg9ZXvToXlndevczxg"

api_url = get_url(origen, destino, api_key)

datos = get_data(api_url)

json = parse_data(datos)
toJSON(data.frame(json))
cat(json)
x <- fromJSON(json)
fromJSON('{"value" : "Z\\u00FCrich"}')
x <- fromJSON(json, simplifyVector = FALSE)
names(x)
x$destination_addresses[2]

json_file <-json

json_file <- fromJSON(json_file)

json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
do.call("rbind", json_file)
