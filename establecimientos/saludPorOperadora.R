library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)

#________Data_________
db.salud <- read_excel('establecimientos/salud.xlsx')

#------------WRANGLING----------------------
# filter schools without signal or bad signal 4G
db.salud.filt <- db.salud |> 
  filter(conecel_lte == '<Nulo>' | conecel_lte == '-140' | is.na(conecel_lte))

db.salud.filt <- db.salud.filt |>
  filter(otecel_lte == '<Nulo>' | otecel_lte == '-140')

db.salud.filt <- db.salud.filt |>
  filter(cnt_lte == 'Sin cobertura 4G' | cnt_lte == '-140')


# Assign company

db.salud.filt$Operadora <- rep(c("OTECEL", "CONECEL"), times = c(8,7))

db.salud.conecel <- db.salud.filt |> filter(Operadora == 'CONECEL')
db.salud.otecel <- db.salud.filt |> filter(Operadora == 'OTECEL')
nrow(db.salud.conecel)
nrow(db.salud.otecel)

writexl::write_xlsx(db.salud.filt, 'establecimientos/saludFiltrada.xlsx')
writexl::write_xlsx(db.salud.conecel, 'establecimientos/saludConecel.xlsx')
writexl::write_xlsx(db.salud.otecel, 'establecimientos/saludOtecel.xlsx')


