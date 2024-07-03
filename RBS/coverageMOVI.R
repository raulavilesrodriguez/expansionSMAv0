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
db.otecel.GSM850 <- read_excel('RBS/OTECEL-RBS diciembre 2023.xlsx',
                               sheet = 'GSM 850')
db.otecel.UMTS850 <- read_excel('RBS/OTECEL-RBS diciembre 2023.xlsx',
                                sheet = 'UMTS 850')
db.otecel.LTE850 <- read_excel('RBS/OTECEL-RBS diciembre 2023.xlsx',
                               sheet = 'LTE 850')
db.otecel.GSM1900 <- read_excel('RBS/OTECEL-RBS diciembre 2023.xlsx',
                                sheet = 'GSM 1900')
db.otecel.UMTS1900 <- read_excel('RBS/OTECEL-RBS diciembre 2023.xlsx',
                                 sheet = 'UMTS 1900')
db.otecel.LTE1900 <- read_excel('RBS/OTECEL-RBS diciembre 2023.xlsx',
                                sheet = 'LTE 1900')

colnames(db.otecel.GSM850) <- db.otecel.GSM850[1,]
db.otecel.GSM850 <- db.otecel.GSM850[-1,]
db.otecel.GSM850 <- db.otecel.GSM850[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.otecel.UMTS850) <- db.otecel.UMTS850[1,]
db.otecel.UMTS850 <- db.otecel.UMTS850[-1,]
db.otecel.UMTS850 <- db.otecel.UMTS850[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.otecel.LTE850) <- db.otecel.LTE850[1,]
db.otecel.LTE850 <- db.otecel.LTE850[-1,]
db.otecel.LTE850 <- db.otecel.LTE850[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.otecel.GSM1900) <- db.otecel.GSM1900[1,]
db.otecel.GSM1900 <- db.otecel.GSM1900[-1,]
db.otecel.GSM1900 <- db.otecel.GSM1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.otecel.UMTS1900) <- db.otecel.UMTS1900[1,]
db.otecel.UMTS1900 <- db.otecel.UMTS1900[-1,]
db.otecel.UMTS1900 <- db.otecel.UMTS1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.otecel.LTE1900) <- db.otecel.LTE1900[1,]
db.otecel.LTE1900 <- db.otecel.LTE1900[-1,]
db.otecel.LTE1900 <- db.otecel.LTE1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

#---Change LATITUDE---
# GSM 850
db.otecel.GSM850 <- db.otecel.GSM850 |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.otecel.GSM850 <- grades_to_decimal(db.otecel.GSM850$LATITUD)
db.otecel.GSM850$LATITUD <- lat.otecel.GSM850

# UMTS 850
db.otecel.UMTS850 <- db.otecel.UMTS850 |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.otecel.UMTS850 <- grades_to_decimal(db.otecel.UMTS850$LATITUD)
db.otecel.UMTS850$LATITUD <- lat.otecel.UMTS850

# LTE 850
db.otecel.LTE850 <- db.otecel.LTE850 |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.otecel.LTE850 <- grades_to_decimal(db.otecel.LTE850$LATITUD)
db.otecel.LTE850$LATITUD <- lat.otecel.LTE850

# GSM 1900
db.otecel.GSM1900 <- db.otecel.GSM1900 |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.otecel.GSM1900 <- grades_to_decimal(db.otecel.GSM1900$LATITUD)
db.otecel.GSM1900$LATITUD <- lat.otecel.GSM1900

# UMTS 1900
db.otecel.UMTS1900 <- db.otecel.UMTS1900 |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.otecel.UMTS1900 <- grades_to_decimal(db.otecel.UMTS1900$LATITUD)
db.otecel.UMTS1900$LATITUD <- lat.otecel.UMTS1900

# LTE 1900
db.otecel.LTE1900 <- db.otecel.LTE1900 |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.otecel.LTE1900 <- grades_to_decimal(db.otecel.LTE1900$LATITUD)
db.otecel.LTE1900$LATITUD <- lat.otecel.LTE1900


#---Change LONGUITUDE---
# GSM 850
db.otecel.GSM850 <- db.otecel.GSM850 |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long1 <- grades_to_decimal(db.otecel.GSM850$LONGITUD)
db.otecel.GSM850$LONGITUD <- long1

# UMTS 850
db.otecel.UMTS850 <- db.otecel.UMTS850 |> mutate(LONGITUD  = (strsplit(LONGITUD, "[°'']")))
long2 <- grades_to_decimal(db.otecel.UMTS850$LONGITUD)
db.otecel.UMTS850$LONGITUD <- long2

# LTE 850
db.otecel.LTE850 <- db.otecel.LTE850 |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long3 <- grades_to_decimal(db.otecel.LTE850$LONGITUD)
db.otecel.LTE850$LONGITUD <- long3

# GSM 1900
db.otecel.GSM1900 <- db.otecel.GSM1900 |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long4 <- grades_to_decimal(db.otecel.GSM1900$LONGITUD)
db.otecel.GSM1900$LONGITUD <- long4

# UMTS 1900
db.otecel.UMTS1900 <- db.otecel.UMTS1900 |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long5 <- grades_to_decimal(db.otecel.UMTS1900$LONGITUD)
db.otecel.UMTS1900$LONGITUD <- long5

# LTE 1900
db.otecel.LTE1900 <- db.otecel.LTE1900 |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long6 <- grades_to_decimal(db.otecel.LTE1900$LONGITUD)
db.otecel.LTE1900$LONGITUD <- long6


colnames(db.otecel.GSM850)[5] <- "DPA"
colnames(db.otecel.UMTS850)[5] <- "DPA"
colnames(db.otecel.LTE850)[5] <- "DPA"
colnames(db.otecel.GSM1900)[5] <- "DPA"
colnames(db.otecel.UMTS1900)[5] <- "DPA"
colnames(db.otecel.LTE1900)[5] <- "DPA"

colnames(db.otecel.GSM850) <- colnames(db.otecel.UMTS850)
colnames(db.otecel.LTE850) <- colnames(db.otecel.UMTS850)
colnames(db.otecel.GSM1900) <- colnames(db.otecel.UMTS850)
colnames(db.otecel.UMTS1900) <- colnames(db.otecel.UMTS850)
colnames(db.otecel.LTE1900) <- colnames(db.otecel.UMTS850)


db.otecel <- rbind(db.otecel.GSM850,
                   db.otecel.UMTS850,
                   db.otecel.LTE850,
                   db.otecel.GSM1900,
                   db.otecel.UMTS1900,
                   db.otecel.LTE1900
)


#---Join to base poblacion to determinate if is URBAN, RURAL, ETC
tipoOtecel <- join_parroquias(db.otecel$DPA, df.poblacion)
#db.otecel <- cbind(db.otecel, as.matrix(tipoOtecel))
tipo.o <- data.frame(map_chr(tipoOtecel, paste, collapse = " "))
db.otecel <- cbind(db.otecel, tipo.o)
colnames(db.otecel)[ncol(db.otecel)] <- "tipo"

# to suppress duplicate to sector x y z
db.otecel <- db.otecel |> group_by(`NOMBRE DE LA RADIOBASE`) |>
  distinct()

#Add Propagation radio[Km]
colnames(db.otecel)[2] <- "banda"

db.otecel <- db.otecel |> 
  mutate(coverage = ifelse(tipo=='RURAL' & banda=='850', rural850, 
                           ifelse(banda=='850', urbana850, 
                                  ifelse(tipo=='RURAL' & banda=='1900', rural1900,
                                         ifelse(banda=='1900', urbana1900,
                                                ifelse(tipo=='RURAL' & banda=='1700', ruralAWS, urbanaAWS))))))


db.otecel2G <- db.otecel |> filter(TECNOLOGIA=='GSM')
db.otecel3G <- db.otecel |> filter(TECNOLOGIA=='UMTS')
db.otecel4G <- db.otecel |> filter(TECNOLOGIA=='LTE')


#----Ploting-----
map <- leaflet() %>% addTiles()
# Create a leaflet map
map <- map %>%
  addCircles(
    data = db.otecel2G,
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
    data = db.otecel3G,
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
    data = db.otecel4G,
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

# Export db Otecel
writexl::write_xlsx(db.otecel, './ResultGeoTOTAL/dbotecel.xlsx')
