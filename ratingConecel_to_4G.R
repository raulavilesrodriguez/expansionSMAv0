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

df.total[which(is.na(df.total[, 34])), 34] = 0
df.total[which(is.na(df.total[, 35])), 35] = 0

# testing rbs 2G
ifelse(sum(df.total |> select(CONECEL_2G)) == 2130, 'TRUE', 'ERROR')

rbs2G.Conecel <- df.total |>  
  select(-c(OTECEL_2G, OTECEL_3G, OTECEL_4G, CNT_3G, CNT_4G, TOTAL)) |> 
  filter(CONECEL_2G !=0) |> 
  mutate(porc.2G.4G = CONECEL_2G / (CONECEL_2G + CONECEL_4G))

rbs2G.Conecel <- rbs2G.Conecel[, -c(6:33, 35, 37, 38)]
rbs2G.Conecel <- rbs2G.Conecel |> filter(porc.2G.4G > 0.5)

# testing rbs 3G
ifelse(sum(df.total |> select(CONECEL_3G)) == 2986, 'TRUE', 'ERROR')

rbs3G.Conecel <- df.total |>  
  select(-c(OTECEL_2G, OTECEL_3G, OTECEL_4G, CNT_3G, CNT_4G, TOTAL)) |> 
  filter(CONECEL_3G !=0) |> 
  mutate(porc.3G.4G = CONECEL_3G / (CONECEL_3G + CONECEL_4G))

rbs3G.Conecel <- rbs3G.Conecel[, -c(6:34, 37, 38)]
rbs3G.Conecel <- rbs3G.Conecel |> filter(porc.3G.4G > 0.5)

# ----------Rating 2G radio bases to 4G ---------------- 
df.2g <- rbs2G.Conecel |> select(pob2022, porc.2G.4G)
# Replace NA with 0
df.2g[is.na(df.2g)] <- 0
row.names(df.2g) <- rbs2G.Conecel$DPA_PARROQ

# normalize data
df.2g <- scale(df.2g)
head(df.2g)

sapply(df.2g, class)


#---NBUST METHOD---
set.seed(123)
res<-NbClust(df.2g, distance = "euclidean", min.nc=2, max.nc=10, 
             method = "ward.D", index = "all")

# ------------MODEL to 2G-------------------------
set.seed(446)
k1 <- kmeans(df.2g, centers = 5, nstart = 25)  
str(k1)
k1

fviz_cluster(k1, geom="point", data = df.2g)

dataFinal2 <- rbs2G.Conecel |> mutate(cluster = k1$cluster)

#testing
count(dataFinal2[which(dataFinal2$cluster == 1),])
count(dataFinal2[which(dataFinal2$cluster == 2),])
count(dataFinal2[which(dataFinal2$cluster == 3),])
count(dataFinal2[which(dataFinal2$cluster == 4),])
count(dataFinal2[which(dataFinal2$cluster == 5),])


rbs2g.cluster1 <- sum(dataFinal2 |> filter(cluster ==1) |> select(CONECEL_2G))
rbs2g.cluster2 <- sum(dataFinal2 |> filter(cluster ==2) |> select(CONECEL_2G))
rbs2g.cluster3 <- sum(dataFinal2 |> filter(cluster ==3) |> select(CONECEL_2G))
rbs2g.cluster4 <- sum(dataFinal2 |> filter(cluster ==4) |> select(CONECEL_2G))
rbs2g.cluster5 <- sum(dataFinal2 |> filter(cluster ==5) |> select(CONECEL_2G))

num.rbs2g <- sum(rbs2g.cluster1+ rbs2g.cluster2 + rbs2g.cluster3 + rbs2g.cluster4 + rbs2g.cluster5)


# ----------Rating 3G radio bases to 4G ---------------- 
df.3g <- rbs3G.Conecel |> select(pob2022, porc.3G.4G)
# Replace NA with 0
df.3g[is.na(df.3g)] <- 0
row.names(df.3g) <- rbs3G.Conecel$DPA_PARROQ

# normalize data
df.3g <- scale(df.3g)
head(df.3g)

sapply(df.3g, class)


#---NBUST METHOD---
set.seed(12345)
res<-NbClust(df.3g, distance = "euclidean", min.nc=2, max.nc=10, 
             method = "ward.D", index = "all")

# ------------MODEL to 3G-------------------------
set.seed(666)
k1 <- kmeans(df.3g, centers = 3, nstart = 25)  
str(k1)
k1

fviz_cluster(k1, geom="point", data = df.3g)

dataFinal3 <- rbs3G.Conecel |> mutate(cluster = k1$cluster)

#testing
count(dataFinal3[which(dataFinal3$cluster == 1),])
count(dataFinal3[which(dataFinal3$cluster == 2),])
count(dataFinal3[which(dataFinal3$cluster == 3),])



rbs3g.cluster1 <- sum(dataFinal3 |> filter(cluster ==1) |> select(CONECEL_3G))
rbs3g.cluster2 <- sum(dataFinal3 |> filter(cluster ==2) |> select(CONECEL_3G))
rbs3g.cluster3 <- sum(dataFinal3 |> filter(cluster ==3) |> select(CONECEL_3G))


num.rbs3g <- sum(rbs3g.cluster1+ rbs3g.cluster2 + rbs3g.cluster3)

