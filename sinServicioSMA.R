library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)

#________Wrangling_________
df.rbs <- read_excel(
  '1.2-Radiobases-por-operador-y-tecnologia-nivel-provincial_Dic23.xlsx',
  sheet = 'RBSxPARQ')
df.poblacion <- read_excel('poblacion.xlsx')

# transform to numeric the population variable
colnames(df.poblacion)[colnames(df.poblacion) == '2020'] <- 'poblacion' 
df.poblacion[,9] <- lapply(df.poblacion[,9], as.numeric)
df.poblacion$poblacion <- ifelse(is.na(df.poblacion$poblacion), 0, df.poblacion$poblacion)
df.poblacion <- df.poblacion |> mutate(poblacion = round(poblacion, 0))

# change DPA to numeric to compare with db.rbs
df.poblacion[,5] <- sapply(df.poblacion[,5], as.numeric)

#df.rbs
df.rbs <- df.rbs[-c(1:8, 1054),]
df.rbs[1,(5:11)]='CONECEL S.A.'
df.rbs[1,(12:17)]='OTECEL S.A.'
df.rbs[1,(18:21)]='CNT E.P'
df.rbs[1, which(is.na(df.rbs[1, ]))] = ''
new_names <- sapply(df.rbs[1:2, ], paste, collapse=" ")
colnames(df.rbs) <- new_names
df.rbs <- df.rbs[-(1:2),]

# transform of character to numeric
df.rbs[,-c(1:3)] <- lapply(df.rbs[,-c(1:3)], as.numeric)
colnames(df.rbs)[4] <- "DPA_PARROQ"

df.rbs2 <- df.rbs |>
  mutate(
    CONECEL_2G = `CONECEL S.A. GSM 850` + `CONECEL S.A. GSM 1900`,
    CONECEL_3G = `CONECEL S.A. UMTS 850` + `CONECEL S.A. UMTS 1900`,
    CONECEL_4G = `CONECEL S.A. LTE 850` + `CONECEL S.A. LTE 1700` + `CONECEL S.A. LTE 1900`,
    OTECEL_2G = `OTECEL S.A. GSM 850` + `OTECEL S.A. GSM 1900`,
    OTECEL_3G = `OTECEL S.A. UMTS 850` + `OTECEL S.A. UMTS 1900`,
    OTECEL_4G = `OTECEL S.A. LTE 1900` + `OTECEL S.A. LTE 850`,
    CNT_3G = `CNT E.P UMTS 1900`,
    CNT_4G = df.rbs[["CNT E.P LTE\r\n700"]] + df.rbs[["CNT E.P LTE\r\n1700"]] + 
      df.rbs[["CNT E.P LTE 1900"]]
  )

df.rbs2 <- df.rbs2 |> 
  mutate(TOTAL = CONECEL_2G + CONECEL_3G + CONECEL_4G +
           OTECEL_2G + OTECEL_3G + OTECEL_4G +
           CNT_3G + CNT_4G)


df.sin <- df.rbs2 |>
  filter(TOTAL==0)

colnames(df.sin)[2] <- 'CANTÓN'

# Variable internet fixed
df.internet.fijo <- read_excel('./internetFijo/dfinternetFINAL.xlsx')
df.internet.fijo$DPA_PARROQ <- sapply(df.internet.fijo$DPA_PARROQ, as.numeric)
df.internet.fijo <- df.internet.fijo |> group_by(DPA_PARROQ) |>
  summarise(
    cuentasInt = sum(cuentasInt)
  )

# join tibbles
df.sin2 <- left_join(df.sin, df.internet.fijo, by = "DPA_PARROQ")
df.sin2[which(is.na(df.sin2[, 31])), 31] = 0

df.sin2 <- df.sin2[, -c(5:30)]

#Export tibble witout mobile service telecom
writexl::write_xlsx(df.sin2, 'dfsinservicioSMA.xlsx')

# Export db TOTAL with and without service mobile telecom
df.total <- left_join(df.rbs2, df.internet.fijo, by = "DPA_PARROQ")
class(df.total$cuentasInt)
df.total[which(is.na(df.total[, 31])), 31] = 0
# delete duplicates
df.total <- df.total[!duplicated(df.total$DPA_PARROQ), ]

writexl::write_xlsx(df.total, 'dftotalSMA.xlsx')


