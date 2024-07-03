library(tidyverse)
library(leaflet)
library(scales)
library(leaflet.extras)
library(hexbin)
library(leaflethex)
library(readxl)
library(here)
library(writexl)
library(purrr)

source(here::here('helpers/grades_to_decimal.R'))
source(here::here('helpers/join_parroquias.R'))
source(here::here('helpers/propagation.R'))

#________Wrangling_________
df.poblacion <- read_excel('poblacion.xlsx')
db.conecel <- read_excel('RBS/01 Rbs_Conecel_2023-12.xlsm')
colnames(db.conecel) <- db.conecel[1,]
db.conecel <- db.conecel[-1,]
db.conecel <- db.conecel[,c(2,3,4,8,11,12,13,14,15,16,17)]

#---Change LATITUDE---
db.conecel <- db.conecel |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.conecel <- grades_to_decimal(db.conecel$LATITUD)
db.conecel$LATITUD <- lat.conecel

#---Change LONGUITUDE---
db.conecel <- db.conecel |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long.conecel <- grades_to_decimal(db.conecel$LONGITUD)
db.conecel$LONGITUD <- long.conecel

colnames(db.conecel)[8] <- "DPA"

#---Join to base poblacion to determinate if is URBAN, RURAL, ETC
tipoConecel <- join_parroquias(db.conecel$DPA, df.poblacion)
#db.conecel <- cbind(db.conecel, as.matrix(tipoConecel))
tipo <- data.frame(map_chr(tipoConecel, paste, collapse = " "))
db.conecel <- cbind(db.conecel, tipo)
colnames(db.conecel)[ncol(db.conecel)] <- "tipo"

# to suppress duplicate to sector x y z
colnames(db.conecel)[1] <- 'NOMBRE DE LA RADIOBASE'
db.conecel <- db.conecel |> group_by(`NOMBRE DE LA RADIOBASE`) |>
  distinct()

#Add Propagation radio[Km]
colnames(db.conecel)[2] <- "banda"

db.conecel <- db.conecel |> 
  mutate(coverage = ifelse(tipo=='RURAL' & banda=='850', rural850, 
                    ifelse(banda=='850', urbana850, 
                    ifelse(tipo=='RURAL' & banda=='1900', rural1900,
                    ifelse(banda=='1900', urbana1900,
                    ifelse(tipo=='RURAL' & banda=='1700', ruralAWS, urbanaAWS))))))



db.conecel2G <- db.conecel |> filter(TECNOLOGIA=='GSM')
db.conecel3G <- db.conecel |> filter(TECNOLOGIA=='UMTS')
db.conecel4G <- db.conecel |> filter(TECNOLOGIA=='LTE')

#----Ploting-----
map <- leaflet() %>% addTiles()
# Create a leaflet map
map <- map %>%
  addCircles(
    data = db.conecel2G,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#FF9209",
    fillOpacity = 0.2,
    group = "2G",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel3G,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "blue",
    fillOpacity = 0.2,
    group = "3G",
    weight = 0  # Set weight to 0 to remove the border
  )

map <- map %>%
  addCircles(
    data = db.conecel4G,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "red",
    fillOpacity = 0.2,
    group = "4G",
    weight = 0  # Set weight to 0 to remove the border
  )

# Add the layers control
map %>% addLayersControl(
  overlayGroups = c("2G", "3G", "4G"),
  options = layersControlOptions(collapsed = FALSE)
)


# Export db Conecel
writexl::write_xlsx(db.conecel, './ResultGeoTOTAL/dbconecel.xlsx')
