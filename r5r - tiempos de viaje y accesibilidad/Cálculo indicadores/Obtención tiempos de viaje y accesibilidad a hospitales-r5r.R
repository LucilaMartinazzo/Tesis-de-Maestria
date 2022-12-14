#Matriz de tiempo de viajes y accesibilidad con r5r - Ciudad de Cordoba

##Instalacion de paquetes de CRAN
install.packages('r5r')
install.packages('mapview')
install.packages('akima')

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

##Archivos de base
###GTFS, mapa base OSM, hospitales como puntos de interes y centroides de area de radios censales como origenes
data_path <- file.path("D:/~") ##Donde se encuentran los archivos
list.files(data_path) #Verificacion de archivos
poi <- fread(file.path(data_path, "Hospitales públicos.csv"))
points <- fread(file.path(data_path, "Centroides radios con %NBI.csv")) ##Obtenidos a través de QGIS del shapefile de radios del Censo 2010

##Construccion de una red para el ruteo con el archivo de OMS y el GTFS, indicando la direccion de los archivos
r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)

##Calculo de matriz de tiempos de viaje
###Variables de entrada 2019
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 417 #En metros
max_trip_duration <- 120 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("10-04-2019 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

ttm19_417 <- travel_time_matrix(r5r_core = r5r_core,
                                origins = points,
                                destinations = poi,
                                mode = mode,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration,
                                percentiles=percentiles,
                                time_window=time_window,
                                verbose = FALSE)
write.xlsx(ttm19_417,"~/TTM 2019-417m.xlsx")

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 833 #En metros
max_trip_duration <- 120 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("10-04-2019 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

ttm19_833 <- travel_time_matrix(r5r_core = r5r_core,
                                origins = points,
                                destinations = poi,
                                mode = mode,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration,
                                percentiles=percentiles,
                                time_window=time_window,
                                verbose = FALSE)
write.xlsx(ttm19_833,"~/TTM 2019-833m.xlsx")

###Variables de entrada 2021
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 417 #En metros
max_trip_duration <- 120 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("14-04-2021 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

ttm21_417 <- travel_time_matrix(r5r_core = r5r_core,
                          origins = points,
                          destinations = poi,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_dist = max_walk_dist,
                          max_trip_duration = max_trip_duration,
                          percentiles=percentiles,
                          time_window=time_window,
                          breakdown = TRUE,
                          verbose = FALSE)
write.xlsx(ttm21_417,"~/TTM 2019-833m.xlsx")

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 833 #En metros
max_trip_duration <- 120 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("14-04-2021 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

ttm21_833 <- travel_time_matrix(r5r_core = r5r_core,
                                 origins = points,
                                 destinations = poi,
                                 mode = mode,
                                 departure_datetime = departure_datetime,
                                 max_walk_dist = max_walk_dist,
                                 max_trip_duration = max_trip_duration,
                                 percentiles=percentiles,
                                 time_window=time_window,
                                 verbose = FALSE)
write.xlsx(ttm21_833,"~/TTM 2021-833m.xlsx")

##Calculo de accesibilidad
####Variables de entrada 2019
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 417 #En metros
travel_time_cutoff <- 61 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("10-04-2019 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

access19_417 <- accessibility(r5r_core,
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
write.xlsx(access19_417,"~/Oportunidades acumuladas 2019-417m.xlsx")

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 833 #En metros
travel_time_cutoff <- 61 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("10-04-2019 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

access19_833 <- accessibility(r5r_core,
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
write.xlsx(access19_833,"~/Oportunidades acumuladas 2019-833m.xlsx")

####Variables de entrada 2021
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 417 #En metros
travel_time_cutoff <- 61 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("14-04-2021 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

access21_417 <- accessibility(r5r_core,
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
write.xlsx(access21_417,"~/Oportunidades acumuladas 2021-417m.xlsx")

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 833 #En metros
travel_time_cutoff <- 61 #En minutos
percentiles <- 85
time_window <- 120 #En minutos
departure_datetime <- as.POSIXct("14-04-2021 09:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

access21_833 <- accessibility(r5r_core,
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
write.xlsx(access21_833,"~/Oportunidades acumuladas 2021-833m v2.xlsx")


#Limpiar despues del uso
stop_r5(r5r_core)
rJava::.jgc(R.gc = TRUE)
rm(list=ls())
