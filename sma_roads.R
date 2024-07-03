library(readxl)
library(tidyverse)
library(ggplot2)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(modelsummary)

# import the data
data_roads <- read_excel('carreteras/NIVELES DE SEÑAL PROMEDIO MTOP NACIONAL.xlsx')
data_roads
class(data_roads)

# helper functions
niveles2g3g <- function(x) {
  ifelse(x == '-85', 'ALTA', 
         ifelse(x == '-95', 'MEDIA',
                ifelse(x == '-100'| x == '-105', 'BAJO', 'INDEFINIDO')))
}

niveles4g <- function(x) {
  ifelse(x == '-100', 'ALTA', 
         ifelse(x == '-120', 'MEDIA',
                ifelse(x == '-140', 'BAJO', 'INDEFINIDO')))
}

jerarquia <- function(x) {
  ifelse(x == 'ARTERIAL', 5, 
         ifelse(x == 'COLECTORA', 3, 1))
}


# mutate the tibble
data_roads <- data_roads %>% 
  mutate(conecel_gsm_level= niveles2g3g(conecel_gsm), 
         conecel_umts_level = niveles2g3g(conecel_umts),
         conecel_lte_level = niveles4g(conecel_lte),
         otecel_gsm_level= niveles2g3g(otecel_gsm), 
         otecel_umts_level = niveles2g3g(otecel_umts),
         otecel_lte_level = niveles4g(otecel_lte),
         cnt_gsm_level= niveles2g3g(cnt_gsm), 
         cnt_umts_level = niveles2g3g(cnt_umts),
         cnt_lte_level = niveles4g(cnt_lte))


data_roads <- data_roads |>
  mutate(importancia = jerarquia(jerarquia))
data_roads$importancia <- replace(data_roads$importancia, is.na(data_roads$importancia), 1)

# to display the internal structure of tibble
str(data_roads)



# exploring the data
data_roads |> ggplot(aes(conecel_lte_level, ubicacion, col=provincia)) +  
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


roads.conecel <- data_roads %>% select(id, nombre, importancia,
  promedio, longitud, conecel_gsm, conecel_umts, conecel_lte)

roads.conecel <- roads.conecel |> filter(conecel_lte == -140)

# lenght of km without 4G signal - CONECEL
sum(roads.conecel$longitud)

#Check for data-types of columns
#kmeans can only be used on numerical columns, because it needs 
#to compute the mean.
View(sapply(roads.conecel, class))
roads.conecel[,3] <- lapply(roads.conecel[,3], as.numeric)

df.conecel <- roads.conecel |> select(-c(id, nombre, conecel_lte))

# normalize data
row.names(df.conecel) <- roads.conecel$id
head(df.conecel)

df.conecel <- scale(df.conecel)
head(df.conecel)


#K-means clustering function
set.seed(2)
#aqui podemos usar df_conecel fitrando en df las columnas de conecel
k1 <- kmeans(df.conecel, centers = 4, nstart = 25)  
str(k1)
k1

fviz_cluster(k1, data = df.conecel)

roads.conecel <- roads.conecel |> mutate(grupo = k1$cluster)

#Export tibble
writexl::write_xlsx(roads.conecel, "./ResultadosExpansión/carreteras_conecel.xlsx")

resumen.conecel <- roads.conecel |> group_by(grupo) |>
  summarise(
    Longitud = sum(longitud),
    Promedio = mean(promedio),
    Grupo_nombre = first(grupo),
    grupo = n(),
  ) |> rename(No_tramos = grupo)

writexl::write_xlsx(resumen.conecel, './ResultadosExpansión/resumen_carreteras_conecel.xlsx')

datasummary_skim(resumen.conecel)
