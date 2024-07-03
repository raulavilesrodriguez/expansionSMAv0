library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents

source(here::here('costeo/costosUnidad.R'))

#---Data----
db.fibra.movi <- read_excel('nodos_fibra/nodos_fibra_otecel.xlsx')
db.salud.movi <- read_excel('coberturaSalud/saludOTECELCobertura2.xlsx')

db.fibra.movi <- db.fibra.movi |> group_by(cantón) |>
  distinct()

class(db.fibra.movi)

# all strings in UPPER CASE
db.fibra.movi <- db.fibra.movi |> 
  mutate(
    cantón = toupper(cantón)
  )

db.salud.movi <- db.salud.movi |>
  mutate(
    DPA_DESCAN = toupper(CAN_DESC)
  )


# all strings without accents
db.fibra.movi <- db.fibra.movi |>
  mutate(
    cantón = stri_trans_general(cantón, "Latin-ASCII")
  )

db.salud.movi <- db.salud.movi |>
  mutate(
    DPA_DESCAN = stri_trans_general(CAN_DESC, "Latin-ASCII")
  )


# all strings without extra spaces
db.fibra.movi <- db.fibra.movi |>
  mutate(
    cantón = str_squish(cantón)
  )


db.salud.movi <- db.salud.movi |>
  mutate(
    DPA_DESCAN = str_squish(CAN_DESC)
  )

# looking for schools where are part of an cantón where there are fiber
join_places_fiber <- function(x, y){
  ifelse(x[['CAN_DESC']] == y[['cantón']],1,0)
}

matrix.fibra <- apply(db.salud.movi, 1, join_places_fiber, y = db.fibra.movi)

col.fibra <- apply(matrix.fibra, 2, sum)

db.salud.movi$acceso_fibra <- col.fibra

# order base health centers Claro in descendent order
db.salud.movi <- db.salud.movi[order(-db.salud.movi$distancia_min_km),]

col.fases <- c(
  rep("Fase 1", times = 8)
)

db.salud.movi$fases <- col.fases
db.salud.movi$datos_moviles <- as.logical(db.salud.movi$datos_moviles)

db.salud.movi <- db.salud.movi |> 
  mutate(costeo_min = ifelse(datos_moviles, 0, 
                            ifelse(acceso_fibra == 1 & distancia_min_km < 1, 
                                   fibkm.min.15*distancia_min_km, plan40gb)))

db.salud.movi <- db.salud.movi |> 
  mutate(costeo_max = ifelse(datos_moviles, 0, 
                            ifelse(acceso_fibra == 1 & distancia_min_km < 1, 
                                   fibkm.max.15*distancia_min_km, plan2tb)))

sum(db.salud.movi$costeo_min)
sum(db.salud.movi$costeo_max)


#Export tibble with companies classified
writexl::write_xlsx(db.salud.movi, 'costeo/saludOTECELCosteado.xlsx')

