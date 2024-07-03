library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)
library(magrittr) # to use extract

#________Data_________
rbs.conecel <- read_excel('ResultGeoTOTAL/dbconecel.xlsx')
rbs.otecel <- read_excel('ResultGeoTOTAL/dbotecel.xlsx')
db.salud <- read_excel('establecimientos/saludFiltrada.xlsx')


perro1 <- list(nombre='micha', raza='buldog', edad=10)
perro2 <- list(nombre='artur', raza='we', edad=2)
perro3 <- list(nombre='vera', raza='salchicha', edad=2)

# Unir las listas en una sola lista
perros <- list(perro1, perro2, perro3)

# Encontrar el perro con la mínima edad
min_edad <- min(sapply(perros, function(x) x$edad))

perro_min_edad <- perros[[which(sapply(perros, function(x) x$edad) == min_edad)]]
perros_conciden <- Filter(function(x) x$edad == min_edad, perros)
perros_conciden_tibble <- bind_rows(map(perros_conciden, as_tibble))

# Recuperar el nombre y la raza del perro con la mínima edad
nombre <- perro_min_edad$nombre
raza <- perro_min_edad$raza

# Mostrar los resultados
cat("Nombre:", nombre, "\nRaza:", raza, "\nEdad:", min_edad, "\n")


#------------WRANGLING----------------------


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
    lista <- list(coverage = x[['coverage']], distancia=t, tecnologia = x[['TECNOLOGIA']])
    lista
  })
  wi
}


func.informacion <- function(LONGITUD, LATITUD){
  listas.cal <- calculo.cobertura(rbs.conecel, LONGITUD, LATITUD)
  min_distancia <- min(sapply(listas.cal, function(x) x$distancia))
  indice <- which(sapply(listas.cal, function(x) x$distancia) == min_distancia)
  lista_min_distancia <- listas.cal[[indice[1]]]
  lista_min_distancia
}

func.informacion2 <- function(db1, LONGITUD, LATITUD){
  listas.cal <- calculo.cobertura(db1, LONGITUD, LATITUD)
  cobertura_tibble <- bind_rows(map(listas.cal, as_tibble))
  cobertura_tibble
}


# compare with CONECEL rbs
## OJO ungroup() is very important 
db.salud <- db.salud |>
  rowwise() |>
  mutate(resultado = list(func.informacion2(rbs.conecel, LONGITUD, LATITUD))) |>
  ungroup()

#db.salud <- db.salud |> ungroup()
#db.salud
#db.salud$resultado[[2]][[1]]$nombre
# en tibble db.salud$resultado[[1]]$edad

db.salud <- db.salud |> 
  mutate(distancia_min_km = map_dbl(resultado, ~ min(.x$distancia)))

func.cobertura_km <- function(lista, dmin){
  r <- lista[['coverage']][which(lista[['distancia']] == dmin)]
  r
}

db.salud <- db.salud |>
  mutate(cobertura_km = map_chr(resultado,  ~ paste(func.cobertura_km(.x, min(.x$distancia)), collapse = " ")))

func.cov <- function(lista, dmin){
  r <- lista[['coverage']][which(lista[['distancia']] == dmin)]
  result <- any(sapply(r, function(x) x> dmin))
  result
}

db.salud <- db.salud |>
  mutate(posible_cobertura = map_chr(resultado,  ~ paste(func.cov(.x, min(.x$distancia)), collapse = " ")))

func.tec <- function(lista, dmin){
  r <- lista[['tecnologia']][which(lista[['distancia']] == dmin)]
  r
}

db.salud <- db.salud |>
  mutate(tecnologias = map_chr(resultado,  ~ paste(func.tec(.x, min(.x$distancia)), collapse = " ")))

fun.dat.movi <- function(lista, dmin){
  c <- lista[['coverage']][which(lista[['distancia']] == dmin)]
  t <- lista[['tecnologia']][which(lista[['distancia']] == dmin)]
  result <- map2_lgl(c, t, function(.x, .y) {
    (.x > dmin & .y != 'GSM')
  })
  result.final <- any(result)
}

db.salud <- db.salud |>
  mutate(datos_moviles = map_chr(resultado, ~ paste(fun.dat.movi(.x, min(.x$distancia)), collapse = " ")))


# db.salud[[47]][[1]][['coverage']][[1]]
#resultado[['coverage']][[which(dmin)]]
#test1[which(test1 == x)]
# x2 <- db.salud[[47]][[1]][['distancia']][2]
# which(db.salud[[47]][[1]][['distancia']] == x2)
# db.salud[[47]][[1]][['distancia']][which(db.salud[[47]][[1]][['distancia']] == x2)]


r_min <- 1.5
l1 <- list(1,2,3)
l2 <- list('gsm', 'gsm', 'gsm')
result1 <- map2_chr(l1, l2, function(.x, .y) paste(.x, "y tecnología", .y))
result1

result2 <- map2_lgl(l1, l2, function(.x, .y) {
  (.x > r_min & .y != 'gsm')
})
any(result2)



