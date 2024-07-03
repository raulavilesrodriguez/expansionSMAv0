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
  TRES_G = CONECEL_3G + OTECEL_3G + CNT_3G
)

db.3g <- df.total |> filter(TRES_G != 0)

porc.3g <- (sum(db.3g$pob2022) / sum(df.total$pob2022)) * 100
porc.3g
