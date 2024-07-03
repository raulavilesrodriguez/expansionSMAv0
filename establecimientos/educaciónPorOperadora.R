library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)

#________Data_________
db.schools <- read_excel('establecimientos/educaciónLatLong.xlsx')

#------------WRANGLING----------------------
# filter schools without signal or bad signal 4G
db.schools.filt <- db.schools |> 
  filter(conecel_lte == '<Nulo>' | conecel_lte == '-140')

db.schools.filt <- db.schools.filt |>
  filter(otecel_lte == '<Nulo>' | otecel_lte == '-140')

db.schools.filt <- db.schools.filt |>
  filter(cnt_lte == 'Sin cobertura 4G' | cnt_lte == '-140 ≤Cobertura con niveles < -120')

# function to assign considering the probability
set.seed(224)

addRandomBool <- function(df, p){
  
  n <- ceiling(nrow(df) * p)
  df$Operadora <- sample(rep(c("CONECEL","OTECEL"), times = c(n, nrow(df) - n)))
  
  df
}

db.schools.ope <- Reduce(rbind, 
                         lapply(
                           split(db.schools.filt, db.schools.filt$DPA_DESCAN),
                           addRandomBool,
                           p= 0.5))

db.schools.conecel <- db.schools.ope |> filter(Operadora == 'CONECEL')
db.schools.otecel <- db.schools.ope |> filter(Operadora == 'OTECEL')
nrow(db.schools.conecel)
nrow(db.schools.otecel)

writexl::write_xlsx(db.schools.conecel, 'establecimientos/educaciónConecel.xlsx')
writexl::write_xlsx(db.schools.otecel, 'establecimientos/educaciónOtecel.xlsx')

