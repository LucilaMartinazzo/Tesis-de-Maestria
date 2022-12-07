#Filtrado base GTFS 2019 con Queries de SQL para aplicacion Metodologia European Commission

##Librerias
library(RSQLite)
library(xlsx)

##Datos
setwd("~/GTFS 2019 editado")  ##Fue editado para agregar los valores de intervalos faltantes en las lineas Aerobus, B20 y B31 (ver punto 4.3.2.1.)
stops_data<-read.csv("stops.txt",header = TRUE,sep = ',')
stops_times_data<-read.csv("stop_times.txt",header = TRUE,sep = ',')
frequency_data<-read.csv("frequencies.txt",header = TRUE,sep = ',')
trip_data<-read.csv("trips.txt",header = TRUE,sep = ',')
shape_data<-read.csv("shapes.txt",header = TRUE,sep = ',')
route_data<-read.csv("routes.txt",header = TRUE,sep = ',')

##Crear database en memoria para correr busquedas
con<-dbConnect(RSQLite::SQLite(),":memory:",loadable.extensions=TRUE)

##Crear tablas GTFS
dbWriteTable(con,"stops",stops_data)
dbWriteTable(con,"stop_times",stops_times_data)
dbWriteTable(con,"frequency",frequency_data)
dbWriteTable(con,"trips",trip_data)
dbWriteTable(con,"shapes",shape_data)
dbWriteTable(con,"routes",route_data)

##Stops data asociadas con stops_times, trips y routes
stops_data_2<-dbGetQuery(con,"select b.stop_id, stop_code,stop_name,stop_lat,stop_lon, d.route_short_name from stops a, stop_times b, trips c, routes d where a.stop_id=b.stop_id and b.trip_id=c.trip_id and c.route_id=d.route_id and service_id=24")
dbWriteTable(con,"stops_data_2",stops_data_2)

##Intervalo y regularidad promedio por ruta
avg_frequency<-dbGetQuery(con, "select d.route_id, d.route_short_name, avg(headway_secs) as avg_headway_secs, 3600/avg(headway_secs) as veh_h from trips c, routes d, frequency e where c.route_id=d.route_id and c.trip_id=e.trip_id and service_id in (19,20,22,23,24) group by d.route_short_name")
dbWriteTable(con,"avg_frequency",avg_frequency)

##Union de tablas de paradas y rutas con los intervalos y regularidad
avg_frequency_stops<-dbGetQuery(con, "select stop_id, stop_code, stop_name, stop_lat, stop_lon, stops_data_2.route_short_name, avg_headway_secs, veh_h from stops_data_2 inner join avg_frequency on avg_frequency.route_short_name=stops_data_2.route_short_name")
dbWriteTable(con,"avg_frequency_stops",avg_frequency_stops)

##Tabla agrupada por stop_id para exportar a QGis y aplicar la metodologia
grouped_stops<-dbGetQuery(con,"select stop_id, stop_code, stop_name, stop_lat, stop_lon, group_concat(route_short_name,',') as route, sum(veh_h) as veh_h from avg_frequency_stops group by stop_id")
dbWriteTable(con,"grouped_stops",grouped_stops)
verificacion<-dbGetQuery(con, "select * from stops where stop_id not in (select stop_id from grouped_stops)")
write.csv(grouped_stops,"D:/Documents/Tesis Maestr?a/Bases de datos/Paradas agrupadas.csv")

#Desconectarse de la base
dbDisconnect(con)
