library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)


df.internet <- read_excel('Diciembre_2023_internet_fijo.xls')
colnames(df.internet)[14] <- 'internetFijo'
df.internet <- df.internet[, c(4:6, 14)]
df.internet <- df.internet |> group_by(PROVINCIA,Cantón,PARROQUIA) |>
  summarise(
    cuentasInt = sum(internetFijo)
  )
df.internet <- df.internet[-1,]

df.internet.dpa <- read_excel('./internetFijo/dfinternet._dpa.xlsx')

df.internet.dpa <- df.internet.dpa |> select(-c(cuentasInt))

#join df.internet with DPA in the base df.internet.dpa
df.internet2 <- left_join(df.internet, df.internet.dpa, by = c("PROVINCIA", "Cantón", "PARROQUIA"))

writexl::write_xlsx(df.internet2, './internetFijo/dfinternet.xlsx')

