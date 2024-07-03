library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)

#________Data_________
rbs.otecel <- read_excel('ResultGeoTOTAL/dbotecel.xlsx')
db.schools.movi <- read_excel('establecimientos/educaciónOtecel.xlsx')

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

db.schools.movi2 <- db.schools.movi |>
  rowwise() |>
  mutate(resultado = min(calculo.cobertura(rbs.otecel, LONGITUD, LATITUD))
  )


#Export tibble with companies classified
writexl::write_xlsx(db.schools.movi2, 'coberturaEducación/educaciónOTECELCobertura.xlsx')





