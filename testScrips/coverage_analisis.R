library(geosphere)

# Definir las coordenadas de los puntos
lat1 <- 0.285944444444444
lon1 <- -78.1113055555555
lat2 <- 0.285944444444444
lon2 <- -78.02

# Crear un vector con las coordenadas de cada punto
punto1 <- c(lon1, lat1)
punto2 <- c(lon2, lat2)

# Calcular la distancia usando la fórmula de haversine
distancia <- distHaversine(punto1, punto2)

# Convertir la distancia a kilómetros (la función distHaversine devuelve la distancia en metros)
distancia_km <- distancia / 1000

# Imprimir la distancia
print(paste("La distancia entre los dos puntos es de", round(distancia_km, 2), "km"))

