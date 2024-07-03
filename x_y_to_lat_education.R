library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(terra) # to tranform TO LONGITUDE AND LATITUDE
library(sp) # to transform TO LONGITUDE AND LATITUDE

df.education <- read_excel('./establecimientos/educación.xlsx')

#---SCRIPT TO TRANSFORM PROJECTED SYSTEMS X Y TO LONGITUDE AND LATITUDE

# Define the projection information for Zone 17S in Ecuador
utm_zone <- "+proj=utm +zone=17 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

x <- df.education$X
y <- df.education$Y

# Combine the coordinates into a matrix
xy <- cbind(x, y)

# Create a SpatVector object with the projected coordinates
pts <- vect(xy, crs = utm_zone)

# Transform the projected coordinates to longitude and latitude
pts_ll <- project(pts, "+proj=longlat +datum=WGS84")

# Display the transformed coordinates (longitude and latitude)
longlat <- geom(pts_ll)[, c("x", "y")]
longlat

df.education$LONGITUD <- longlat[,1]
df.education$LATITUD <- longlat[,2]

#Export tibble education
writexl::write_xlsx(df.education, './establecimientos/educaciónLatLong.xlsx')


