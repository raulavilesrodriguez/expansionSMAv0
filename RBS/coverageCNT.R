library(tidyverse)
library(leaflet)
library(scales)
library(leaflet.extras)
library(hexbin)
library(leaflethex)
library(readxl)
library(here)
library(writexl)

source(here::here('helpers/grades_to_decimal.R'))
source(here::here('helpers/join_parroquias.R'))
source(here::here('helpers/propagation.R'))


#________Wrangling_________
df.poblacion <- read_excel('poblacion.xlsx')
db.cnt.LTE700 <- read_excel('RBS/01 Rbs_Cnt_diciembre_2023.xlsm',
                            sheet = 'LTE(700)')
db.cnt.UMTS1900 <- read_excel('RBS/01 Rbs_Cnt_diciembre_2023.xlsm',
                              sheet = 'UMTS(1900)')
db.cnt.LTE1900 <- read_excel('RBS/01 Rbs_Cnt_diciembre_2023.xlsm',
                             sheet = 'LTE 1900')
db.cnt.AWS1700 <- read_excel('RBS/01 Rbs_Cnt_diciembre_2023.xlsm',
                             sheet = 'AWS (17002100)')


colnames(db.cnt.LTE700) <- c(1:52)
db.cnt.LTE700 <- db.cnt.LTE700[-1,]
db.cnt.LTE700 <- db.cnt.LTE700[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.cnt.UMTS1900) <- c(1:52)
db.cnt.UMTS1900 <- db.cnt.UMTS1900[-1,]
db.cnt.UMTS1900 <- db.cnt.UMTS1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.cnt.LTE1900) <- c(1:52)
db.cnt.LTE1900 <- db.cnt.LTE1900[-1,]
db.cnt.LTE1900 <- db.cnt.LTE1900[,c(2,3,4,8,11,12,13,14,15,16,17)]

colnames(db.cnt.AWS1700) <- c(1:53)
db.cnt.AWS1700 <- db.cnt.AWS1700[-1,]
db.cnt.AWS1700 <- db.cnt.AWS1700[,c(2,3,4,8,11,12,13,14,15,16,17)]

db.cnt <- rbind(
  db.cnt.LTE700,
  db.cnt.UMTS1900,
  db.cnt.LTE1900,
  db.cnt.AWS1700
)

colnames(db.cnt) <- c(
  "NOMBRE DE LA RADIOBASE",
  "banda",
  "TECNOLOGIA",
  "CENTRAL A LA QUE SE CONECTA (MSC)",
  "PROVINCIA",
  "CANTON",
  "PARROQUIA",
  "DPA",
  "DIRECCCION",
  "LATITUD",
  "LONGITUD"
)

# to suppress duplicate to sector x y z
db.cnt <- db.cnt |> group_by(LATITUD, LONGITUD, `NOMBRE DE LA RADIOBASE`) |>
  distinct()

# Drpo rows NAs
dropito <- c(which(is.na(db.cnt$LATITUD)))
db.cnt <- db.cnt[-dropito, ]


#---Change LATITUDE---
db.cnt <- db.cnt |> mutate(LATITUD = (strsplit(LATITUD, "[°'']")))
lat.cnt <- grades_to_decimal(db.cnt$LATITUD)
db.cnt$LATITUD <- lat.cnt

#---Change LONGUITUDE---
db.cnt <- db.cnt |> mutate(LONGITUD = (strsplit(LONGITUD, "[°'']")))
long.cnt <- grades_to_decimal(db.cnt$LONGITUD)
db.cnt$LONGITUD <- long.cnt

colnames(db.cnt)[8] <- "DPA"

#---Join to base poblacion to determinate if is URBAN, RURAL, ETC
tipoCnt <- join_parroquias(db.cnt$DPA, df.poblacion)

x23 <- c()
n <- length(tipoCnt)
x <- 1
while (n > 0) {
  ifelse(class(tipoCnt[[x]]) == 'character', 
         x23 <- c(x23, tipoCnt[[x]]), 
         x23 <- c(x23, 'RURAL'))
  x <- x + 1
  n <- n - 1
  print(x)
}

db.cnt$tipo <- x23

#Add Propagation radio[Km]
colnames(db.cnt)[2] <- "banda"

db.cnt <- db.cnt |> 
  mutate(coverage = ifelse(tipo=='RURAL' & banda=='1900', rural1900, 
                           ifelse(banda=='1900', urbana1900, 
                                  ifelse(tipo=='RURAL' & banda=='700', rural700,
                                         ifelse(banda=='700', urbana700,
                                                ifelse(tipo=='RURAL' & banda=='1700', ruralAWS, urbanaAWS))))))


# delete the directios that are wrong writen and duplicate the rbs
db.cnt <- db.cnt |> select(-DIRECCCION)
db.cnt <- db.cnt |> group_by(LATITUD, LONGITUD, `NOMBRE DE LA RADIOBASE`) |>
  distinct()


db.cnt3G <- db.cnt |> filter(TECNOLOGIA == 'UMTS')
db.cnt4G <- db.cnt |> filter(TECNOLOGIA == 'LTE')

#----Ploting-----
map <- leaflet() %>% addTiles()
# Create a leaflet map
map <- map %>%
  addCircles(
    data = db.cnt3G,
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
    data = db.cnt4G,
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
  overlayGroups = c("3G", "4G"),
  options = layersControlOptions(collapsed = FALSE)
)


# Export db CNT
writexl::write_xlsx(db.cnt, './ResultGeoTOTAL/dbcnt.xlsx')

