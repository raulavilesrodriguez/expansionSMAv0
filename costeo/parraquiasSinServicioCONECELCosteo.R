library(tidyverse)
library(dplyr)
library(readxl)
library(here)
library(writexl)
library(stringr)
library(stringi) # to remove accents
library(purrr)
library(tidygeocoder)
library(ggplot2)
library(leaflet)


source(here::here('helpers/propagation.R'))
source(here::here('costeo/costosUnidad.R'))

#----Data----
parr.sin.servicio.claro <- read_excel("./coberturaParroquiasSinServicio/parroquiasSinServicioCLARO.xlsx")


coverage.rbs <- 2.6 * (rural850)^2

parr.sin.servicio.claro <- parr.sin.servicio.claro |> 
  mutate(mercado_potencial = round(densidad_pob * coverage.rbs, 0))

parr.sin.servicio.claro <- parr.sin.servicio.claro |>
  mutate(income_anual = 
           ifelse(pob2022 > mercado_potencial, (mercado_potencial * 0.1* arpu.conecel * 12)/15,
                  (pob2022 * 0.1* arpu.conecel * 12)/15))


anualizar.capex <- function(tasa.descuento, tiempo){
  x <- (tasa.descuento*(1+tasa.descuento)^tiempo) 
  y <- ((1+tasa.descuento)^tiempo) - 1
  r <- x / y
  r
}


parr.sin.servicio.claro <- parr.sin.servicio.claro |>
  rowwise() |>
  mutate(capex.min = ifelse(acceso_fibra == 1,
                            (sitio.min+enode.min+fibkm.capex.min*distancia_min_km),
                            (sitio.min+enode.min)
                            )) |>
  mutate(
    capex.min = capex.min * anualizar.capex(tasa.descuento, tiempo)
  )


parr.sin.servicio.claro <- parr.sin.servicio.claro |>
  rowwise() |>
  mutate(
    opex.min = ifelse(acceso_fibra == 1,
                      (rbs.mantenimineto + fibkm.mantenimiento),
                      (rbs.mantenimineto + 2 * plan40gb)
                      )
  ) |>
  mutate(costo.min.anual = capex.min + opex.min)



parr.sin.servicio.claro <- parr.sin.servicio.claro |>
  rowwise() |>
  mutate(
    income_flujo_min = list(c(seq(from = income_anual, by = 0.0, length.out = tiempo))),
    costo_flujo_min = list(c(seq(from = costo.min.anual, by = 0.0, length.out = tiempo)))
  ) |>
  mutate(flujo_min = list(map2(income_flujo_min, costo_flujo_min, ~ .x - .y))) |>
  ungroup()


# Función para calcular el VPN
calcular_vpn <- function(flujo, tasa.descuento) {
  sum(flujo / (1 + tasa.descuento) ^ seq_along(flujo))
}

parr.sin.servicio.claro <- parr.sin.servicio.claro |>
  rowwise() |>
  mutate(
    vpn = calcular_vpn(unlist(flujo_min), tasa.descuento)
  )

sum(parr.sin.servicio.claro$vpn)
