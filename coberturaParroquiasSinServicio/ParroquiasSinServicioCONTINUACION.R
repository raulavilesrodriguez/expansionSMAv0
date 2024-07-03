library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents
library(purrr)
library(tidygeocoder)
library(ggplot2)
library(leaflet)
library(geosphere) # to calculate distance min

source(here::here('helpers/propagation.R'))
source(here::here('costeo/costosUnidad.R'))
source(here::here('helpers/cobertura_detection.R'))

#---Data----
parr.sin.servicio <- read_excel("coberturaParroquiasSinServicio/sinservicioClasificadoLatLong FINAL.xlsx")
db.fibra.movi <- read_excel('nodos_fibra/nodos_fibra_otecel.xlsx')
db.fibra.claro <- read_excel('nodos_fibra/nodos_fibra_conecel.xlsx')
rbs.otecel <- read_excel('ResultGeoTOTAL/dbotecel.xlsx')
rbs.conecel <- read_excel('ResultGeoTOTAL/dbconecel.xlsx')


# visualizing using leaflet
leaflet()  %>% 
  addTiles(group = "OSM") %>%
  addMarkers(
    data = parr.sin.servicio,
    lat =  ~ latitude,    
    lng =  ~ longitude,
    label = ~ PARROQUIA.x,
    labelOptions = labelOptions(noHide = TRUE, direction = 'auto')
  )

#----------WRANGLING-----------------
db.fibra.movi <- db.fibra.movi |> group_by(cantón) |>
  distinct()

db.fibra.claro <- db.fibra.claro |> group_by(cantón) |>
  distinct()

class(db.fibra.movi)
class(db.fibra.claro)

# all strings in UPPER CASE
db.fibra.movi <- db.fibra.movi |> 
  mutate(
    cantón = toupper(cantón)
  )

db.fibra.claro <- db.fibra.claro |> 
  mutate(
    cantón = toupper(cantón)
  )

# all strings without accents
db.fibra.movi <- db.fibra.movi |>
  mutate(
    cantón = stri_trans_general(cantón, "Latin-ASCII")
  )

db.fibra.claro <- db.fibra.claro |>
  mutate(
    cantón = stri_trans_general(cantón, "Latin-ASCII")
  )

# all strings without extra spaces
db.fibra.movi <- db.fibra.movi |>
  mutate(
    cantón = str_squish(cantón)
  )

# all strings without extra spaces
db.fibra.claro <- db.fibra.claro |>
  mutate(
    cantón = str_squish(cantón)
  )

parr.sin.servicio.movi <- parr.sin.servicio |>
  filter(Operadora == "OTECEL")

parr.sin.servicio.claro <- parr.sin.servicio |>
  filter(Operadora == "CONECEL")

# looking for schools where are part of an cantón where there are fiber
join_places_fiber.movi <- function(x, y){
  ifelse(x[['CANTÓN']] == y[['cantón']],1,0)
}

matrix.fibra.movi <- apply(parr.sin.servicio.movi, 1, join_places_fiber.movi, y = db.fibra.movi)

col.fibra.s.movi <- apply(matrix.fibra.movi, 2, sum)

parr.sin.servicio.movi$acceso_fibra <- col.fibra.s.movi


join_places_fiber.claro <- function(x, y){
  ifelse(x[['CANTÓN']] == y[['cantón']],1,0)
}

matrix.fibra.claro <- apply(parr.sin.servicio.claro, 1, join_places_fiber.claro, y = db.fibra.claro)

col.fibra.s.claro <- apply(matrix.fibra.claro, 2, sum)

parr.sin.servicio.claro$acceso_fibra <- col.fibra.s.claro


# compare with MOVI rbs
## OJO ungroup() is very important 
parr.sin.servicio.movi <- parr.sin.servicio.movi |>
  rowwise() |>
  mutate(resultado = list(func.informacion2(rbs.otecel, longitude, latitude))) |>
  ungroup()

parr.sin.servicio.movi <- parr.sin.servicio.movi |> 
  mutate(distancia_min_km = map_dbl(resultado, ~ min(.x$distancia)))


# compare with CLARO rbs
## OJO ungroup() is very important 
parr.sin.servicio.claro <- parr.sin.servicio.claro |>
  rowwise() |>
  mutate(resultado = list(func.informacion2(rbs.conecel, longitude, latitude))) |>
  ungroup()

parr.sin.servicio.claro <- parr.sin.servicio.claro |> 
  mutate(distancia_min_km = map_dbl(resultado, ~ min(.x$distancia)))

#Export tibble with distances min Movi
write_xlsx(parr.sin.servicio.movi, 'coberturaParroquiasSinServicio/parroquiasSinServicioMOVI.xlsx')

#Export tibble with distances min CLARO
write_xlsx(parr.sin.servicio.claro, 'coberturaParroquiasSinServicio/parroquiasSinServicioCLARO.xlsx')







