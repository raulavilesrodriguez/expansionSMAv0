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
    lista <- list(coverage = x[['coverage']], distancia=round(t, 4), tecnologia = x[['TECNOLOGIA']])
    lista
  })
  wi
}


# function to map all list calculated
func.informacion2 <- function(db1, LONGITUD, LATITUD){
  listas.cal <- calculo.cobertura(db1, LONGITUD, LATITUD)
  cobertura_tibble <- bind_rows(map(listas.cal, as_tibble))
  cobertura_tibble
}


# function to calculate all items with min coverage
func.cobertura_km <- function(lista, dmin){
  r <- lista[['coverage']][which(lista[['distancia']] == dmin)]
  r
}

# TRUE or FALSE if the point is in coverage
func.cov <- function(lista, dmin){
  r <- lista[['coverage']][which(lista[['distancia']] == dmin)]
  result <- any(sapply(r, function(x) as.numeric(x) > dmin), na.rm = FALSE)
  result
}


# function to detect all techonologies
func.tec <- function(lista, dmin){
  r <- lista[['tecnologia']][which(lista[['distancia']] == dmin)]
  r
}

# function to detect if points have coverage of mobile data
fun.dat.movi <- function(lista, dmin){
  c <- lista[['coverage']][which(lista[['distancia']] == dmin)]
  t <- lista[['tecnologia']][which(lista[['distancia']] == dmin)]
  result <- map2_lgl(c, t, function(.x, .y) {
    (as.numeric(.x) > dmin & .y != 'GSM')
  })
  result.final <- any(result)
}



