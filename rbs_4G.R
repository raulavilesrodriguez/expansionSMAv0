library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinyjs)
library(rsconnect)
library(DT)
library(here)
library(readxl)
library(stringr)
library(highcharter)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust) # to find the ideal number of clusters
library(writexl)


#----Read Total file-------
df.total <- read_excel('dftotalSMA.xlsx')
str(df.total)

df.total <- df.total |> mutate(
  Cuatro_G = CONECEL_4G + OTECEL_4G + CNT_4G
)

db.4g <- df.total |> filter(Cuatro_G != 0)

porc.4g <- (sum(db.4g$pob2022) / sum(df.total$pob2022)) * 100
porc.4g
