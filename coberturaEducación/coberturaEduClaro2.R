library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)
library(magrittr) # to use extract

source(here::here('helpers/cobertura_detection.R'))

#________Data_________
rbs.conecel <- read_excel('ResultGeoTOTAL/dbconecel.xlsx')
db.edu.claro <- read_excel('establecimientos/educaciónConecel.xlsx')


# compare with CONECEL rbs
## OJO ungroup() is very important 
db.edu.claro <- db.edu.claro |>
  rowwise() |>
  mutate(resultado = list(func.informacion2(rbs.otecel, LONGITUD, LATITUD))) |>
  ungroup()

db.edu.claro <- db.edu.claro |> 
  mutate(distancia_min_km = map_dbl(resultado, ~ min(.x$distancia)))

db.edu.claro <- db.edu.claro |>
  mutate(cobertura_km = map_chr(resultado,  ~ paste(func.cobertura_km(.x, min(.x$distancia)), collapse = " ")))

db.edu.claro <- db.edu.claro |>
  mutate(posible_cobertura = map_chr(resultado,  ~ paste(func.cov(.x, min(.x$distancia)), collapse = " ")))

db.edu.claro <- db.edu.claro |>
  mutate(tecnologias = map_chr(resultado,  ~ paste(func.tec(.x, min(.x$distancia)), collapse = " ")))

db.edu.claro <- db.edu.claro |>
  mutate(datos_moviles = map_chr(resultado, ~ paste(fun.dat.movi(.x, min(.x$distancia)), collapse = " ")))

#Export tibble with companies classified
writexl::write_xlsx(db.edu.claro, 'coberturaEducación/educaciónCONECELCobertura2.xlsx')





