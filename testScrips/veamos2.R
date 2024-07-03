library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(purrr)
library(geosphere)
library(magrittr) # to use extract

#________Data_________
db.salud <- read_excel('establecimientos/saludFiltrada.xlsx')


perro1 <- list(nombre='micha', raza='buldog', edad=10)
perro2 <- list(nombre='artur', raza='we', edad=2)
perro3 <- list(nombre='vera', raza='salchicha', edad=5)

# Unir las listas en una sola lista
perros <- list(perro1, perro2, perro3)

# Encontrar el perro con la mínima edad
perros_conciden_tibble <- bind_rows(map(perros, as_tibble))

db.salud <- db.salud |>
  rowwise() |>
  mutate(resultado = list(perros_conciden_tibble)) |>
  ungroup()

db.salud <- db.salud |>
  mutate(edad_min = map_dbl(resultado, ~ min(.x$edad)))

db.salud <- db.salud |>
  mutate(nombres = map_chr(resultado, ~ paste(.x$nombre, collapse = " ")))


db.salud <- db.salud |>
  mutate(nom_unico = map_lgl(resultado, ~ any(.x$raza == "salchicha")))

db.salud <- db.salud |>
  mutate(nom_unico2 = map(resultado, ~ any(.x$raza == "salchicha")))

# correct style to extract character
edades_caracter <- map_chr(perros, ~ as.character(.x$edad))
