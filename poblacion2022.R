library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents



db.pob2022 <- read_excel('poblacion2022/01_2022_CPV_Estructura_poblacional.xlsx',
                         sheet = '1.2'
                         )

db.poblacion2020 <- read_excel('poblacion.xlsx')

#-------Wrangling to DB of poblation 2022-----------
db.pob2022 <- db.pob2022[-c(1:9), -c(1, 6, 7)]
colnames(db.pob2022) <- c("PROVINCIA", "CANTON", "PARROQUIA", "pob2022")

# filter row with Total name
db.pob2022 <- db.pob2022 |> filter(
  str_detect(PARROQUIA, regex("^Total"), negate = TRUE)
  )

db.pob2022$pob2022 <- sapply(db.pob2022$pob2022, as.numeric)
class(db.pob2022$pob2022)

# all strings in UPPER CASE
db.pob2022 <- db.pob2022 |> mutate(
  PROVINCIA = toupper(PROVINCIA),
  CANTON = toupper(CANTON),
  PARROQUIA = toupper(PARROQUIA)
  )

# all strings without accents
db.pob2022 <- db.pob2022 |> mutate(
  PROVINCIA = stri_trans_general(PROVINCIA, "Latin-ASCII"),
  CANTON = stri_trans_general(CANTON, "Latin-ASCII"),
  PARROQUIA = stri_trans_general(PARROQUIA, "Latin-ASCII")
)

db.poblacion2020 <- db.poblacion2020 |> mutate(
  DPA_DESPRO = stri_trans_general(DPA_DESPRO, "Latin-ASCII"),
  DPA_DESCAN =stri_trans_general(DPA_DESCAN, "Latin-ASCII"),
  DPA_DESPAR = stri_trans_general(DPA_DESPAR, "Latin-ASCII")
)

# all strings without extra spaces
db.pob2022 <- db.pob2022 |> mutate(
  PROVINCIA = str_squish(PROVINCIA),
  CANTON = str_squish(CANTON),
  PARROQUIA = str_squish(PARROQUIA)
)

db.poblacion2020 <- db.poblacion2020 |> mutate(
  DPA_DESPRO = str_squish(DPA_DESPRO),
  DPA_DESCAN = str_squish(DPA_DESCAN),
  DPA_DESPAR = str_squish(DPA_DESPAR)
)

# apply work to dataframe or tibble to extract DPA
we <- apply(db.pob2022, 1, function(x){
  t <- ifelse(
    x[["PROVINCIA"]] == db.poblacion2020[["DPA_DESPRO"]] &
      str_detect(db.poblacion2020[["DPA_DESPAR"]], x[["PARROQUIA"]]),
    db.poblacion2020[["DPA_PARROQ"]], ''
    )
})

ve <- apply(we, 2, paste, collapse="")
db.pob2022$DPA_PARROQ <- ve

writexl::write_xlsx(db.pob2022, './poblacion2022/pob2022.xlsx')






