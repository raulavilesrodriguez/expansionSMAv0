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

#---Data----
parr.sin.servicio <- read_excel("ResultadosExpansión/sinservicioClasificadoOperadora.xlsx")
class(parr.sin.servicio)
km2.parroquia <- read_excel("poblacion2022/2022_CPV_NACIONAL_DENSIDAD_POBLACIONAL.xlsx",
                            sheet = '3')

km2.parroquia <- km2.parroquia[-c(1:6, 8), -c(7:12)]
colnames(km2.parroquia) <- km2.parroquia[1,]
colnames(km2.parroquia)[6] <- "densidad_pob"
colnames(km2.parroquia)[5] <- "superficie_km2"
km2.parroquia <- km2.parroquia[-1,]

km2.parroquia$densidad_pob <- sapply(km2.parroquia$densidad_pob, as.numeric)
class(km2.parroquia$densidad_pob)


# all strings in UPPER CASE
km2.parroquia <- km2.parroquia |> mutate(
  Provincia = toupper(Provincia),
  Cantón = toupper(Cantón),
  Parroquia = toupper(Parroquia)
)

parr.sin.servicio <- parr.sin.servicio |> mutate(
  PROVINCIA.x = toupper(PROVINCIA.x),
  CANTÓN = toupper(CANTÓN),
  PARROQUIA.x = toupper(PARROQUIA.x)
)


# all strings without accents
km2.parroquia <- km2.parroquia |> mutate(
  Provincia = stri_trans_general(Provincia, "Latin-ASCII"),
  Cantón = stri_trans_general(Cantón, "Latin-ASCII"),
  Parroquia = stri_trans_general(Parroquia, "Latin-ASCII")
)

parr.sin.servicio <- parr.sin.servicio |> mutate(
  PROVINCIA.x = stri_trans_general(PROVINCIA.x, "Latin-ASCII"),
  CANTÓN = stri_trans_general(CANTÓN, "Latin-ASCII"),
  PARROQUIA.x = stri_trans_general(PARROQUIA.x, "Latin-ASCII")
)

# all strings without extra spaces
km2.parroquia <- km2.parroquia |> mutate(
  Provincia = str_squish(Provincia),
  Cantón = str_squish(Cantón),
  Parroquia = str_squish(Parroquia)
)

parr.sin.servicio <- parr.sin.servicio |> mutate(
  PROVINCIA.x = str_squish(PROVINCIA.x),
  CANTÓN = str_squish(CANTÓN),
  PARROQUIA.x = str_squish(PARROQUIA.x)
)

# apply work to dataframe or tibble to extract density by km2
de <- apply(parr.sin.servicio, 1, function(x){
  t <- ifelse(
    x[["PROVINCIA.x"]] == km2.parroquia[["Provincia"]] &
      x[["CANTÓN"]] == km2.parroquia[["Cantón"]] &
      str_detect(x[["PARROQUIA.x"]], km2.parroquia[["Parroquia"]]),
    km2.parroquia[["densidad_pob"]], ''
  )
})

col_density <- apply(de, 2, paste, collapse = "")
parr.sin.servicio$densidad_pob <- col_density
nrow(parr.sin.servicio |> filter(densidad_pob == ""))

area <- apply(parr.sin.servicio, 1, function(x){
  t <- ifelse(
    x[["PROVINCIA.x"]] == km2.parroquia[["Provincia"]] &
      x[["CANTÓN"]] == km2.parroquia[["Cantón"]] &
      str_detect(x[["PARROQUIA.x"]], km2.parroquia[["Parroquia"]]),
    km2.parroquia[["superficie_km2"]], ''
  )
})

col_area <- apply(area, 2, paste, collapse = "")
parr.sin.servicio$superficie_km2 <- col_area
nrow(parr.sin.servicio |> filter(superficie_km2 == ""))


# to generate lat and long by parroquia
parr.sin.servicio <- parr.sin.servicio |>
  mutate(addr = paste(PARROQUIA.x, CANTÓN, PROVINCIA.x, "ECUADOR", sep = ", "))


lat_longs <- parr.sin.servicio |> 
  geocode(addr, method = 'osm', lat = latitude, long = longitude)



lat_longs[,14] <- lapply(lat_longs[,14], as.numeric) 
lat_longs[,15] <- lapply(lat_longs[,15], as.numeric) 
class(lat_longs$superficie_km2)

lat_longs <- lat_longs |>
  mutate(densidad_pob = ifelse(densidad_pob == 0, pob2022 / superficie_km2, densidad_pob))

lat_longs <- lat_longs |>
  mutate(tiempo = ifelse(grupo == "G1", 15, 
                         ifelse(grupo == "G2", 13, 
                                ifelse(grupo == "G3", 11, 
                                       ifelse(grupo == "G4", 9, 15)))))

# SAN PEDRO DE VILCABAMBA
lat_longs[which(lat_longs[['DPA_PARROQ']]==110158), 14] <- 19
lat_longs[which(lat_longs[['DPA_PARROQ']]==110158), 15] <- 67
# TRIUNFO DORADO
lat_longs[which(lat_longs[['DPA_PARROQ']]==190752), 14] <- 20
lat_longs[which(lat_longs[['DPA_PARROQ']]==190752), 15] <- 51
# GARCIA MORENO
lat_longs[which(lat_longs[['DPA_PARROQ']]==220156), 14] <- 11
lat_longs[which(lat_longs[['DPA_PARROQ']]==220156), 15] <- 132
# TUUTINENTZA
lat_longs[which(lat_longs[['DPA_PARROQ']]==140953), 14] <- 5
lat_longs[which(lat_longs[['DPA_PARROQ']]==140953), 15] <- 959
# SAN JOSE DE GUAYUSA
lat_longs[which(lat_longs[['DPA_PARROQ']]==220160), 14] <- 7
lat_longs[which(lat_longs[['DPA_PARROQ']]==220160), 15] <- 458
# SAN LUIS DE ARMENIA
lat_longs[which(lat_longs[['DPA_PARROQ']]==220161), 14] <- 8
lat_longs[which(lat_longs[['DPA_PARROQ']]==220161), 15] <- 313
#ISLA SANTA MARIA (FLOREANA) (CAB. EN PTO. VELASCO IB
lat_longs[which(lat_longs[['DPA_PARROQ']]==200152), 14] <- 1
lat_longs[which(lat_longs[['DPA_PARROQ']]==200152), 15] <- 280
# TIPUTINI
lat_longs[which(lat_longs[['DPA_PARROQ']]==220254), 14] <- 1
lat_longs[which(lat_longs[['DPA_PARROQ']]==220254), 15] <- 1
# PICAIGUA
lat_longs[which(lat_longs[['DPA_PARROQ']]==180160), 14] <- 650
lat_longs[which(lat_longs[['DPA_PARROQ']]==180160), 15] <- 16



write_xlsx(lat_longs, "./costeo/sinservicioClasificadoLatLong.xlsx")
