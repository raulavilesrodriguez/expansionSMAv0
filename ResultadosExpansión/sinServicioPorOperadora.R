library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)

db.sinservicio <- read_excel('ResultadosExpansión/sinservicioClasificado.xlsx')
head(db.sinservicio)

set.seed(1986) 

# function to assign considering the probability
addRandomBool <- function(df, p){
  
  n <- ceiling(nrow(df) * p)
  df$Operadora <- sample(rep(c("CONECEL","OTECEL"), times = c(n, nrow(df) - n)))
  
  df
}

#---To Assign to each company considering their probability---
bd.prueba <- Reduce(rbind, 
       lapply(split(db.sinservicio, db.sinservicio$grupo), addRandomBool, p = 0.5))


#-----TESTER-------
# G1
nrow(bd.prueba |> filter(grupo == 'G1', Operadora == 'OTECEL'))
nrow(bd.prueba |> filter(grupo == 'G1', Operadora == 'CONECEL'))
# G2
nrow(bd.prueba |> filter(grupo == 'G2', Operadora == 'OTECEL'))
nrow(bd.prueba |> filter(grupo == 'G2', Operadora == 'CONECEL'))
# G3
nrow(bd.prueba |> filter(grupo == 'G3', Operadora == 'OTECEL'))
nrow(bd.prueba |> filter(grupo == 'G3', Operadora == 'CONECEL'))
# G4
nrow(bd.prueba |> filter(grupo == 'G4', Operadora == 'OTECEL'))
nrow(bd.prueba |> filter(grupo == 'G4', Operadora == 'CONECEL'))
# Total
nrow(bd.prueba |> filter(Operadora == 'OTECEL'))
nrow(bd.prueba |> filter(Operadora == 'CONECEL'))


#Export tibble with companies classified
writexl::write_xlsx(bd.prueba, 'ResultadosExpansión/sinservicioClasificadoOperadora.xlsx')

# Resume
resume.claro <- bd.prueba |> filter(Operadora == 'CONECEL') |> 
  group_by(grupo) |>
  summarise(
    Grupo_nombre = first(grupo),
    grupo = n(),
    Periodo = first(periodo),
    Tecnología = '4G o superiores'
  ) |> rename(No_Parroquias = grupo)

resume.claro

resume.movi <- bd.prueba |> filter(Operadora == 'OTECEL') |> 
  group_by(grupo) |>
  summarise(
    Grupo_nombre = first(grupo),
    grupo = n(),
    Periodo = first(periodo),
    Tecnología = '4G o superiores'
  ) |> rename(No_Parroquias = grupo)

resume.movi

#Export Summaries
writexl::write_xlsx(resume.claro, 'ResultadosExpansión/sinservicio_CONECEL_Summary.xlsx')
writexl::write_xlsx(resume.movi, 'ResultadosExpansión/sinservicio_OTECEL_Summary.xlsx')










