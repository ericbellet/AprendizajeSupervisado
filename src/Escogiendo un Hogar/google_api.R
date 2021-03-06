# Install dependencies in Linux
# sudo apt-get install libcurl4-openssl-dev 

install = function(pkg){
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http:/cran.rstudio.com")
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

install("jsonlite")

fetch_data = function(preamble, list){
  data = preamble
  for(elem in list){
    data = paste0(data, paste0(strsplit(elem, " ")[[1]], collapse = "+"), "|", collapse = "") 
  }
  return(substr(data, 0, nchar(data)-1))
}
#CAMBIAR DRIVING
get_url = function(origins, destinations, key, mode = "driving", language = "es"){
  # install(pkg)
  # url base for distance matrix api
  base = "https://maps.googleapis.com/maps/api/distancematrix/json?"
  
  # This could change, using only some atributes from API
  origin = fetch_data("origins=", origins)
  destination = fetch_data("destinations=", destinations)
  key = fetch_data("key=", key)
  mode = fetch_data("mode=", mode)
  language = fetch_data("language=", language)
  
  # Getting final format for Google API
  api_url = paste(c(base, paste0(c(origin, destination, key, mode, language), collapse = "&")), collapse = "")
  
  return(api_url)
}

get_data = function(api_url){
  return(fromJSON(api_url))
}


parse_data = function(lista){
  # Parsea una lista que representa datos en json.
  #
  # Args:
  #   lista: Lista que representa datos en json con el tiempo y distancia entre direcciones.
  #
  # Returns:
  #   Retorna un arreglo con la distancia y el tiempo entre la direccion del apartamento 
  #   y la universidad Sapienza.

  #Parseo la distancia.
  distancia <- lista$rows$elements[[1]][1,1][1]  
  #Parseo el tiempo.
  tiempo <- lista$rows$elements[[1]][1,2][1]
 #Retorno la concatenacion de la distancia y el tiempo.
   return(c(distancia,tiempo))
 
}