#Grafico relaciones bases GTFS 2019

rm(list=ls())

##Librerias
install.packages("dm")
library(dm)
install.packages("DiagrammeR")
library(DiagrammeR)

##Datos
setwd("~/GTFS CBA editado")
agency<-read.csv("agency.txt",sep = ',',header = TRUE)
calendar<-read.csv("calendar.txt",sep = ',',header = TRUE)
feed_info<-read.csv("feed_info.txt",sep = ',',header = TRUE)
frequencies<-read.csv("frequencies.txt",sep = ',',header = TRUE)
routes<-read.csv("routes.txt",sep = ',',header = TRUE)
shapes<-read.csv("shapes.txt",sep = ',',header = TRUE)
stop_times<-read.csv("stop_times.txt",sep = ',',header = TRUE)
stops<-read.csv("stops.txt",sep = ',',header = TRUE)
trips<-read.csv("trips.txt",sep = ',',header = TRUE)

##Armado base dm
GTFS19_dm_no_keys <- dm(agency, calendar, feed_info, frequencies, routes, shapes, stop_times, stops, trips)

###Claves primarias
n_occur <- data.frame(table(trips$trip_id))

dm_enum_pk_candidates(
  dm = GTFS19_dm_no_keys,
  table = agency)
dm_enum_pk_candidates(
  dm = GTFS19_dm_no_keys,
  table = calendar)
dm_enum_pk_candidates(
  dm = GTFS19_dm_no_keys,
  table = routes)
dm_enum_pk_candidates(
  dm = GTFS19_dm_no_keys,
  table = stops)
dm_enum_pk_candidates(
  dm = GTFS19_dm_no_keys,
  table = trips)


GTFS19_dm_only_pks <-
  GTFS19_dm_no_keys %>%
  dm_add_pk(agency, agency_id) %>%
  dm_add_pk(calendar, service_id) %>%
  dm_add_pk(routes, route_id) %>%
  dm_add_pk(stops, stop_id) %>%
  dm_add_pk(trips, trip_id) %>% 
  dm_add_pk(shapes, shape_id)
GTFS19_dm_only_pks

###Claves ajenas
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = routes,
  ref_table = agency)
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = trips,
  ref_table = calendar)
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = trips,
  ref_table = routes)
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = trips,
  ref_table = routes)
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = stop_times,
  ref_table = stops)
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = frequencies,
  ref_table = trips)
dm_enum_fk_candidates(
  dm = GTFS19_dm_only_pks,
  table = stop_times,
  ref_table = trips)

GTFS19_dm_all_keys <-
  GTFS19_dm_only_pks %>%
  dm_add_fk(routes, agency_id, agency) %>%
  dm_add_fk(trips, service_id, calendar) %>%
  dm_add_fk(trips, route_id, routes) %>%
  dm_add_fk(stop_times, stop_id, stops) %>%
  dm_add_fk(frequencies, trip_id, trips) %>%
  dm_add_fk(stop_times, trip_id, trips) %>% 
  dm_add_fk(trips, shape_id, shapes)
GTFS19_dm_all_keys

###Visualizacion
windowsFonts(Cambria=windowsFont("Cambria"))
pal <- c('#efc847','#d8854a','#cc644c','#b52150', '#a90052')

GTFS19_dm_all_keys %>%
  dm_set_colors(
    '#b52150' = agency, 
    '#b52150' = calendar,
    '#cc644c' = feed_info,
    '#b52150' = frequencies,
    '#b52150' = routes,
    '#b52150' = shapes,
    '#b52150' = stop_times,
    '#b52150' = stops,
    '#b52150' = trips
  )%>% 
  dm_draw(rankdir = "LR", view_type = "all")

#Grafico relaciones bases GTFS 2021

rm(list=ls())

##Librerias
install.packages("dm")
library(dm)
install.packages("DiagrammeR")
library(DiagrammeR)

##Datos
setwd("~/GTFS 04-2021")
agency<-read.csv("agency.txt",sep = ',',header = TRUE)
calendar<-read.csv("calendar.txt",sep = ',',header = TRUE)
calendar_dates<-read.csv("calendar_dates.txt",sep = ',',header = TRUE)
fare_attributes<-read.csv("fare_attributes.txt",sep = ',',header = TRUE)
fare_rules<-read.csv("fare_rules.txt",sep = ',',header = TRUE)
routes<-read.csv("routes.txt",sep = ',',header = TRUE)
shapes<-read.csv("shapes.txt",sep = ',',header = TRUE)
stop_times<-read.csv("stop_times.txt",sep = ',',header = TRUE)
stops<-read.csv("stops.txt",sep = ',',header = TRUE)
trips<-read.csv("trips.txt",sep = ',',header = TRUE)

##Armado base dm
GTFS21_dm_no_keys <- dm(agency, calendar, calendar_dates, fare_attributes, fare_rules, routes, shapes, stop_times, stops, trips)

###Claves primarias
dm_enum_pk_candidates(
  dm = GTFS21_dm_no_keys,
  table = agency)
dm_enum_pk_candidates(
  dm = GTFS21_dm_no_keys,
  table = calendar)
dm_enum_pk_candidates(
  dm = GTFS21_dm_no_keys,
  table = fare_attributes)
dm_enum_pk_candidates(
  dm = GTFS21_dm_no_keys,
  table = routes)
dm_enum_pk_candidates(
  dm = GTFS21_dm_no_keys,
  table = stops)
dm_enum_pk_candidates(
  dm = GTFS21_dm_no_keys,
  table = trips)


GTFS21_dm_only_pks <-
  GTFS21_dm_no_keys %>%
  dm_add_pk(agency, agency_id) %>%
  dm_add_pk(calendar, service_id) %>%
  dm_add_pk(fare_attributes, fare_id) %>%
  dm_add_pk(routes, route_id) %>%
  dm_add_pk(stops, stop_id) %>%
  dm_add_pk(trips, trip_id) %>% 
  dm_add_pk(shapes, shape_id)
GTFS21_dm_only_pks

###Claves ajenas
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = fare_attributes,
  ref_table = agency)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = routes,
  ref_table = agency)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = calendar_dates,
  ref_table = calendar)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = trips,
  ref_table = calendar)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = fare_rules,
  ref_table = fare_attributes)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = fare_rules,
  ref_table = routes)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = trips,
  ref_table = routes)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = stop_times,
  ref_table = stops)
dm_enum_fk_candidates(
  dm = GTFS21_dm_only_pks,
  table = stop_times,
  ref_table = trips)

GTFS21_dm_all_keys <-
  GTFS21_dm_only_pks %>%
  dm_add_fk(fare_attributes, agency_id, agency) %>%
  dm_add_fk(routes, agency_id, agency) %>%
  dm_add_fk(calendar_dates, service_id, calendar) %>%
  dm_add_fk(trips, service_id, calendar) %>%
  dm_add_fk(fare_rules, fare_id, fare_attributes) %>%
  dm_add_fk(fare_rules, route_id, routes) %>%
  dm_add_fk(trips, route_id, routes) %>%
  dm_add_fk(stop_times, stop_id, stops) %>%
  dm_add_fk(stop_times, trip_id, trips) %>% 
  dm_add_fk(trips, shape_id, shapes)
GTFS21_dm_all_keys

###Visualizacion
windowsFonts(Cambria=windowsFont("Cambria"))
pal <- c('#efc847','#d8854a','#cc644c','#b52150', '#a90052')

GTFS21_dm_all_keys %>%
  dm_set_colors(
    '#b52150' = agency, 
    '#b52150' = calendar,
    '#b52150' = calendar_dates,
    '#cc644c' = fare_attributes,
    '#cc644c' = fare_rules,
    '#b52150' = routes,
    '#b52150' = shapes,
    '#b52150' = stop_times,
    '#b52150' = stops,
    '#b52150' = trips
  )%>% 
  dm_draw(rankdir = "LR", view_type = "all")

rm(list=ls())