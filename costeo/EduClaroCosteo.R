library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents

source(here::here('costeo/costosUnidad.R'))

#---Data----
db.fibra.claro <- read_excel('nodos_fibra/nodos_fibra_conecel.xlsx')
db.edu.claro <- read_excel('coberturaEducación/educaciónCONECELCobertura2.xlsx')

db.fibra.claro <- db.fibra.claro |> group_by(cantón) |>
  distinct()

class(db.fibra.claro)

# all strings in UPPER CASE
db.fibra.claro <- db.fibra.claro |> 
  mutate(
    cantón = toupper(cantón)
  )

db.edu.claro <- db.edu.claro |>
  mutate(
    DPA_DESCAN = toupper(DPA_DESCAN)
    )


# all strings without accents
db.fibra.claro <- db.fibra.claro |>
  mutate(
    cantón = stri_trans_general(cantón, "Latin-ASCII")
  )

db.edu.claro <- db.edu.claro |>
  mutate(
    DPA_DESCAN = stri_trans_general(DPA_DESCAN, "Latin-ASCII")
  )


# all strings without extra spaces
db.fibra.claro <- db.fibra.claro |>
  mutate(
    cantón = str_squish(cantón)
  )


db.edu.claro <- db.edu.claro |>
  mutate(
    DPA_DESCAN = str_squish(DPA_DESCAN)
  )

# looking for schools where are part of an cantón where there are fiber
join_places_fiber <- function(x, y){
  ifelse(x[['DPA_DESCAN']] == y[['cantón']],1,0)
}

matrix.fibra <- apply(db.edu.claro, 1, join_places_fiber, y = db.fibra.claro)

col.fibra <- apply(matrix.fibra, 2, sum)

db.edu.claro$acceso_fibra <- col.fibra

# order base schools Claro in descendent order
db.edu.claro <- db.edu.claro[order(-db.edu.claro$distancia_min_km),]

col.fases <- c(
  rep("Fase 1", times = 702), 
  rep("Fase 2", times = 703),
  rep("Fase 3", times = 703),
  rep("Fase 4", times = 703)
  )

db.edu.claro$fases <- col.fases
db.edu.claro$datos_moviles <- as.logical(db.edu.claro$datos_moviles)


db.edu.claro <- db.edu.claro |> 
  mutate(costeo_min = ifelse(datos_moviles, 0, 
                         ifelse(acceso_fibra == 1 & distancia_min_km < 1, 
                                fibkm.min.15*distancia_min_km, plan40gb)))

db.edu.claro <- db.edu.claro |> 
  mutate(costeo_max = ifelse(datos_moviles, 0, 
                            ifelse(acceso_fibra == 1 & distancia_min_km < 1, 
                                   fibkm.max.15*distancia_min_km, plan2tb)))

sum(db.edu.claro$costeo_min)
sum(db.edu.claro$costeo_max)

#Export tibble with companies classified
writexl::write_xlsx(db.edu.claro, 'costeo/educaciónConecelCosteado.xlsx')

