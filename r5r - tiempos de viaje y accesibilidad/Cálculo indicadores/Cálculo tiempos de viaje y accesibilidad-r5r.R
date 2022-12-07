#Matriz de tiempo de viajes y accesibilidad con r5r - Ciudad de Cordoba

##Agrego memoria para JAVA (requiere la instalacion previa de Java SE Development Kit version >= 11.0.8)
options(java.parameters = "-Xmx2G")

##Librerias
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(xlsx)
library(akima)
library(dplyr)
library(mapview)
mapviewOptions(platform = 'leafgl')

##Datos
###GTFS, mapa base OSM, hospitales como puntos de interes y centroides de area de radios censales como origenes
data_path <- file.path("D:/~") ##Donde se encuentran los archivos
list.files(data_path) ##Verificacion de archivos
poi <- fread(file.path(data_path, "Hospitales publicos.csv")) 
points <- fread(file.path(data_path, "Centroides radios con %NBI.csv")) ##Obtenidos a través de QGIS del shapefile de radios del Censo 2010

#Construccion de una red para el ruteo con el archivo de OMS y el GTFS, indicando la direccion de los archivos
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

##Calculo de matriz de tiempos de viaje
#Variables de entrada
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 833 #En metros
max_trip_duration <- 120 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("10-04-2019 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S") ##Se repite el proceso para 2021 modificando el GTFS y esta fecha

ttm <- travel_time_matrix(r5r_core = r5r_core,
                                origins = points,
                                destinations = poi,
                                mode = mode,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration,
                                percentiles=percentiles,
                                time_window=time_window,
                                verbose = FALSE)
write.xlsx(ttm,"~/TTM.xlsx")

##Calculo de accesibilidad
#Variables de entrada
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 833 #En metros
travel_time_cutoff <- 61 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("10-04-2019 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S") ##Se repite el proceso para 2021 modificando el GTFS y esta fecha

access <- accessibility(r5r_core,
                        origins = points,
                        destinations = points,
                        mode = mode,
                        opportunities_colname = "Hospitales",
                        decay_function = "step",
                        cutoffs = travel_time_cutoff,
                        departure_datetime = departure_datetime,
                        max_walk_dist = max_walk_dist,
                        time_window = time_window,
                        percentiles = percentiles,
                        verbose = FALSE)
write.xlsx(access,"~/Oportunidades acumuladas.xlsx")

#Limpiar despues del uso
stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
