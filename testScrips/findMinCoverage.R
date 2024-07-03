library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)

#________Data_________
df.poblacion <- read_excel('poblacion.xlsx')
rbs.conecel <- read_excel('ResultGeoTOTAL/dbconecel.xlsx')
db.schools <- read_excel('establecimientos/educaciónConecel.xlsx')

#------------WRANGLING----------------------

# crossing bases
we <- apply(rbs.conecel, 1, function(x){
  
  #new around
  my_hashmap <- new.env()
  
  lon1 <- as.numeric(x[['LONGITUD']])
  lat1 <- as.numeric(x[['LATITUD']])
  
  # Define lon2 and lat2 inside the function to create vectors
  lon2 <- as.numeric(db.schools[['LONGITUD']][1])
  lat2 <- as.numeric(db.schools[['LATITUD']][1])
  
  # Calculate distance
  t <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  t <- t / 1000
  my_hashmap$distancia <- t
  my_hashmap$lon <- x[['LONGITUD']]
  my_hashmap$lat <- x[['LATITUD']]
  my_hashmap
})

df <- data.frame(
  distancia = sapply(we, function(env) env$distancia),
  lon = sapply(we, function(env) env$lon),
  lat = sapply(we, function(env) env$lat)
)

min_index <- which.min(df$distancia)
min_env <- we[[min_index]]
print(paste("Distancia mínima:", min_env$distancia))
print(paste("Longitud:", min_env$lon))
print(paste("Latitud:", min_env$lat))


rbs.conecel.4g <- rbs.conecel |> filter(TECNOLOGIA=='LTE')

# FUNCTION to detect min coverage
calculo.cobertura <- function(db1, lon2, lat2){
  wi <- apply(db1, 1, function(x){
    lon1 <- as.numeric(x[['LONGITUD']])
    lat1 <- as.numeric(x[['LATITUD']])
    
    # Define lon2 and lat2 inside the function to create vectors
    lon2 <- as.numeric(lon2)
    lat2 <- as.numeric(lat2)
    
    # Calculate distance
    t <- distHaversine(c(lon1, lat1), c(lon2, lat2))
    t <- t / 1000
    t
  })
  wi
}

db.schools1 <- db.schools |>
  rowwise() |>
  mutate(resultado = min(calculo.cobertura(rbs.conecel, LONGITUD, LATITUD))
  )


#Export tibble with companies classified
#writexl::write_xlsx(db.schools1, 'coberturaEducación/educaciónConecelCobertura.xlsx')

#----------------------------------
func.principal <- function(db.escuelas, db2){
  db.escuelas |>  
    mutate(
      resultado = min(calculo.cobertura(db2, db.escuelas[['LONGITUD']], db.escuelas[['LATITUD']]))
    )
  
}

bd.prueba1 <- Reduce(rbind,
                     lapply(split(db.schools, db.schools$DPA_PROVIN), func.principal, db2 = rbs.conecel[1:2,])
)

#--------other way-------------
lon2 <- (db.schools[['LONGITUD']])
lat2 <- (db.schools[['LATITUD']])

compute2 <- function(x, lon2, lat2) {
  lon1 <- as.numeric(x[['LONGITUD']])
  lat1 <- as.numeric(x[['LATITUD']])
  
  # Calculate distance
  distances <- map2_dbl(lon1, lat1, ~ (distHaversine(c(.x, .y), c(lon2, lat2)) / 1000))
  
  tibble(Longitud = lon1, Latitud = lat1, distancia = distances)
}

result2 <- map_df(list(rbs.conecel), compute2, lon2 = db.schools[['LONGITUD']][1], lat2 = db.schools[['LATITUD']][1])
min(result2$distancia)


db.schools2 <- db.schools |>
  rowwise() |>
  mutate(resultado = min(map_df(list(rbs.conecel[1:2,]), compute2, lon2 = LONGITUD, lat2 = LATITUD)$distancia)
  )



