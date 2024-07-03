library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)

#________Data_________
rbs.conecel <- read_excel('ResultGeoTOTAL/dbconecel.xlsx')
rbs.otecel <- read_excel('ResultGeoTOTAL/dbotecel.xlsx')
db.salud <- read_excel('establecimientos/saludFiltrada.xlsx')

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
    t
  })
  wi
}


# compare with CONECEL rbs
db.salud <- db.salud |>
  rowwise() |>
  mutate(resultado_conecel = min(calculo.cobertura(rbs.conecel, LONGITUD, LATITUD))
  )

# compare with OTECEL rbs
db.salud <- db.salud |>
  rowwise() |>
  mutate(resultado_otecel = min(calculo.cobertura(rbs.otecel, LONGITUD, LATITUD))
  )

db.salud2 <- db.salud |> 
  mutate(cobertura = ifelse(resultado_conecel < 3 | resultado_conecel < 3, 1, 0))

db.salud3 <- db.salud2 |> filter(cobertura == 0)

#Export tibble with companies classified
writexl::write_xlsx(db.salud3, 'coberturaSalud/saludCobertura.xlsx')





