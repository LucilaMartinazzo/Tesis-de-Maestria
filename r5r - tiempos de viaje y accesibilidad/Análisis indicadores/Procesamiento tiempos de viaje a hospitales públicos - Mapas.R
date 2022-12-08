#Procesamiento tiempos de viaje a hospitales publicos - Mapas

rm(list=ls())

##Librerias
library(tidyverse)
library(dplyr)
library(leaflet)
library(patchwork)
library(xlsx)
library(sf)
library(mapview)
library(gstat)
library(tmap)
library(osmdata)
library(tmaptools)
library(OpenStreetMap)
library(spdep)

##Datos
setwd("D:/~")
CAT1_417_2019<-read.csv("CAT1_417_2019.csv",sep = ',',header = TRUE)
CAT1_417_2021<-read.csv("CAT1_417_2021.csv",sep = ',',header = TRUE)
CAT1_833_2019<-read.csv("CAT1_833_2019.csv",sep = ',',header = TRUE)
CAT1_833_2021<-read.csv("CAT1_833_2021.csv",sep = ',',header = TRUE)
CAT2_417_2019<-read.csv("CAT2_417_2019.csv",sep = ',',header = TRUE)
CAT2_417_2021<-read.csv("CAT2_417_2021.csv",sep = ',',header = TRUE)
CAT2_833_2019<-read.csv("CAT2_833_2019.csv",sep = ',',header = TRUE)
CAT2_833_2021<-read.csv("CAT2_833_2021.csv",sep = ',',header = TRUE)
CAT3_417_2019<-read.csv("CAT3_417_2019.csv",sep = ',',header = TRUE)
CAT3_417_2021<-read.csv("CAT3_417_2021.csv",sep = ',',header = TRUE)
CAT3_833_2019<-read.csv("CAT3_833_2019.csv",sep = ',',header = TRUE)
CAT3_833_2021<-read.csv("CAT3_833_2021.csv",sep = ',',header = TRUE)
radios <- st_read("~/Radios censales Dpto Capital con %NBI.shp")
radios$link <- as.integer(radios$link)
ejido <- st_read("~/EjidoMunicipal.shp")
hospitales <- st_read("~/Hospitales Públicos.shp")
hospitales_CAT1 <- filter(hospitales, Categoría == "Alto riesgo con terapia intensiva especializada")
hospitales_CAT2 <- filter(hospitales, Categoría == "Alto riesgo con terapia intensiva")
hospitales_CAT3 <- filter(hospitales, Categoría == "Bajo riesgo con internación simple")

#Union de tablas
CAT1_417_2019 <- left_join(radios,CAT1_417_2019,by = "link")
CAT1_417_2021 <- left_join(radios,CAT1_417_2021,by = "link")
CAT1_833_2019 <- left_join(radios,CAT1_833_2019,by = "link")
CAT1_833_2021 <- left_join(radios,CAT1_833_2021,by = "link")
CAT2_417_2019 <- left_join(radios,CAT2_417_2019,by = "link")
CAT2_417_2021 <- left_join(radios,CAT2_417_2021,by = "link")
CAT2_833_2019 <- left_join(radios,CAT2_833_2019,by = "link")
CAT2_833_2021 <- left_join(radios,CAT2_833_2021,by = "link")
CAT3_417_2019 <- left_join(radios,CAT3_417_2019,by = "link")
CAT3_417_2021 <- left_join(radios,CAT3_417_2021,by = "link")
CAT3_833_2019 <- left_join(radios,CAT3_833_2019,by = "link")
CAT3_833_2021 <- left_join(radios,CAT3_833_2021,by = "link")

#Mapas estaticos
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ###Modo estatico
pal <- c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052') ###Paleta QGIS
windowsFonts(Cambria=windowsFont("Cambria"))
scales::show_col(c('#f0eb54','#efc847','#d8854a','#b52150', '#a90052'))

##Tiempos promedio por quintiles - caminata maxima 417 m
hospitales_icon <- tmap_icons(c("first-aid-99069_640.png"))
map_CAT1_417_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT1_417_2019) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT1 417m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT1)+
  tm_symbols(size = 0.08,
    shape = hospitales_icon)
tmap_save(map_CAT1_417_2019, filename="Mapa_prueba.png", height=5, width=4, units="in", dpi=300)
map_CAT1_417_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT1_417_2021) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT1 417m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT1)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT2_417_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT2_417_2019) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT2 417m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT2)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT2_417_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT2_417_2021) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT2 417m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT2)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT3_417_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_417_2019) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT3 417m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT3)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT3_417_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_417_2021) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT3 417m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT3)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
mapa_417 <- tmap_arrange(map_CAT1_417_2019,map_CAT1_417_2021,map_CAT2_417_2019,map_CAT2_417_2021,map_CAT3_417_2019,map_CAT3_417_2021,ncol=2,nrow = 3)
tmap_save(mapa_417, filename="Mapa_417_2019_2021 v2.png", height=12, width=6, units="in", dpi=300)

##Tiempos promedio por quintiles - caminata maxima 833 m
map_CAT1_833_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT1_833_2019) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT1 833m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT1)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT1_833_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT1_833_2021) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT1 833m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT1)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT2_833_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT2_833_2019) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT2 833m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT2)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT2_833_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT2_833_2021) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT2 833m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT2)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT3_833_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_833_2019) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT3 833m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT3)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_CAT3_833_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(CAT3_833_2021) + 
  tm_polygons(col = "avg_tiempo_viaje",  
              title = "Tiempos de viaje promedio CAT3 833m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.6, 
              breaks = c(5,20,40,60,90,120),
              textNA = "S/D",
              legend.format = c(text.separator = "-"),
              border.alpha = 0.5,
              lwd = 0.2)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1,
            legend.text.size = 0.8)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))+
  tm_shape(hospitales_CAT3)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
mapa_833 <- tmap_arrange(map_CAT1_833_2019,map_CAT1_833_2021,map_CAT2_833_2019,map_CAT2_833_2021,map_CAT3_833_2019,map_CAT3_833_2021,ncol=2,nrow = 3)
tmap_save(mapa_833, filename="Mapa_833_2019_2021 v2.png", height=12, width=6, units="in", dpi=300)

##Verificacion NA
sum(is.na(CAT1_417_2019$avg_tiempo_viaje))
sum(is.na(CAT1_417_2021$avg_tiempo_viaje))
sum(is.na(CAT1_833_2019$avg_tiempo_viaje))
sum(is.na(CAT1_833_2021$avg_tiempo_viaje))
sum(is.na(CAT2_417_2019$avg_tiempo_viaje))
sum(is.na(CAT2_417_2021$avg_tiempo_viaje))
sum(is.na(CAT2_833_2019$avg_tiempo_viaje))
sum(is.na(CAT2_833_2021$avg_tiempo_viaje))
sum(is.na(CAT3_417_2019$avg_tiempo_viaje))
sum(is.na(CAT3_417_2021$avg_tiempo_viaje))
sum(is.na(CAT3_833_2019$avg_tiempo_viaje))
sum(is.na(CAT3_833_2021$avg_tiempo_viaje))

rm(list=ls())
