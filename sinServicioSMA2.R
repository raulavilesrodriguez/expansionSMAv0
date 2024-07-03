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


# join tibbles
df.total <- full_join(df.poblacion, df.rbs2, by = "DPA_PARROQ")

df.sin <- df.total |>
  filter(TIPO != 'URBANO', TOTAL==0)

df.sin <- df.sin[, -c(7,10:38)]
colnames(df.sin)[2] <- 'PROVINCIA'
colnames(df.sin)[4] <- 'CANTÓN'
colnames(df.sin)[6] <- 'PARROQUIA'

# Variable internet fixed
df.internet.fijo <- read_excel('./internetFijo/dfinternetFINAL.xlsx')
df.internet.fijo$DPA_PARROQ <- sapply(df.internet.fijo$DPA_PARROQ, as.numeric)
df.internet.fijo <- df.internet.fijo |> group_by(DPA_PARROQ) |>
  summarise(
    cuentasInt = sum(cuentasInt)
  )

# Variable fixed telephony
df.telefonia.fija <- read_excel('STF/dfSTF.xlsx')
df.telefonia.fija$DPA_PARROQ <- sapply(df.telefonia.fija$DPA_PARROQ, as.numeric)

# join tibbles without service of SMA
df.sin2 <- left_join(df.sin, df.internet.fijo, by = "DPA_PARROQ")
df.sin2[which(is.na(df.sin2[, 9])), 9] = 0

df.sin2 <- left_join(df.sin2, df.telefonia.fija, by = "DPA_PARROQ")
df.sin2[which(is.na(df.sin2[, 10])), 10] = 0

df.pob2022 <- read_excel('./poblacion2022/pob2022FINAL.xlsx')
df.pob2022$DPA_PARROQ <- sapply(df.pob2022$DPA_PARROQ, as.numeric)
df.sin2 <- left_join(df.sin2, df.pob2022, by = "DPA_PARROQ")
df.sin2[which(is.na(df.sin2[, 14])), 14] = 0

df.sin2 <- df.sin2[, -c(11:13)]
colnames(df.sin2)[8] <- "pob2020"

#Export tibble witout mobile service telecom
writexl::write_xlsx(df.sin2, 'dfsinservicioSMA.xlsx')

#-----Export db TOTAL with and without service mobile telecom----
df.total <- full_join(df.total, df.internet.fijo, by = "DPA_PARROQ")
class(df.total$cuentasInt)
df.total[which(is.na(df.total[, 39])), 39] = 0
df.total[which(is.na(df.total[, 38])), 38] = 0

df.total <- full_join(df.total, df.telefonia.fija, by = "DPA_PARROQ")
df.total[which(is.na(df.total[, 40])), 40] = 0

df.total <- full_join(df.pob2022, df.total, by = "DPA_PARROQ")
df.total[which(is.na(df.total[, 4])), 4] = 0

# Test Base of Data
test.pob2022 <- ifelse(sum(df.total$pob2022)== 16938986, 'OK', 'Error')
test.pob2022
test.rbs <- ifelse(sum(df.total$TOTAL)== 20877, 'OK', 'Error')
test.rbs
test.internet.fijo <- ifelse(sum(df.total$cuentasInt) == 2889020, 'OK', 'Error')
test.internet.fijo
test.telf.fija <- ifelse(sum(df.total$lineStf) == 1434441, 'OK', 'Error')
test.telf.fija

duplicados <- df.total[duplicated(df.total$DPA_PARROQ), ]
duplicados
# delete duplicates
#df.total <- df.total[!duplicated(df.total$DPA_PARROQ), ]

writexl::write_xlsx(df.total, 'dftotalSMA.xlsx')

total.parroquias <- nrow(df.total |> filter(TIPO != 'URBANO'))
total.parroquias

parroquias.con.servicio <- total.parroquias - nrow(df.sin2)
parroquias.con.servicio

# % without service
porc.sin.servicio <- nrow(df.sin2) / total.parroquias
porc.sin.servicio


rbs2G <- df.total |> filter(TIPO != 'URBANO') |> 
  mutate(rbs2G = CONECEL_2G + OTECEL_2G) |> select(rbs2G) |>
  filter(rbs2G != 0)

rbs3G <- df.total |> filter(TIPO != 'URBANO') |> 
  mutate(rbs3G = CONECEL_3G + OTECEL_3G + CNT_3G) |> select(rbs3G) |>
  filter(rbs3G != 0)

rbs4G <- df.total |> filter(TIPO != 'URBANO') |> 
  mutate(rbs4G = CONECEL_4G + OTECEL_4G + CNT_4G) |> select(rbs4G) |>
  filter(rbs4G != 0)

