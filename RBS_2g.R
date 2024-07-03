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
  DOS_G = CONECEL_2G + OTECEL_2G
)

db.2g <- df.total |> filter(DOS_G != 0)

porc.2g <- (sum(db.2g$pob2022) / sum(df.total$pob2022)) * 100
porc.2g
