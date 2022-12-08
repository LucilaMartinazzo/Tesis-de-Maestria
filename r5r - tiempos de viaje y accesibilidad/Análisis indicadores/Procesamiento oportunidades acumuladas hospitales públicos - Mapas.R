#Procesamiento oportunidades acumuladas hospitales publicos - Mapas

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
OA_417_2019<-read.csv("Oportunidades acumuladas 2019-417m.csv",sep = ',',header = TRUE)
OA_417_2021<-read.csv("Oportunidades acumuladas 2021-417m.csv",sep = ',',header = TRUE)
OA_833_2019<-read.csv("Oportunidades acumuladas 2019-833m.csv",sep = ',',header = TRUE)
OA_833_2021<-read.csv("Oportunidades acumuladas 2021-833m.csv",sep = ',',header = TRUE)

radios <- st_read("~/Radios censales Dpto Capital con %NBI.shp")
radios$link <- as.integer(radios$link)
ejido <- st_read("~/EjidoMunicipal.shp")
hospitales <- st_read("~/Hospitales Públicos.shp")

#Union de tablas
OA_417_2019 <- left_join(radios,OA_417_2019,by = "link")
OA_417_2021 <- left_join(radios,OA_417_2021,by = "link")
OA_833_2019 <- left_join(radios,OA_833_2019,by = "link")
OA_833_2021 <- left_join(radios,OA_833_2021,by = "link")

#Mapas estaticos
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ###Modo estatico
pal <- c('#efc847','#d8854a','#cc644c','#b52150', '#a90052') ###Paleta QGIS
windowsFonts(Cambria=windowsFont("Cambria"))
scales::show_col(c('#efc847','#d8854a','#cc644c','#b52150', '#a90052'))
hospitales_icon <- tmap_icons(c("first-aid-99069_640.png"))

map_OA_417_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(OA_417_2019) + 
  tm_polygons(col = "accessibility",  
              title = "Oportunidades alcanzadas en 60 minutos - 417m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.7, 
              breaks = c(0,3,6,9,12,14),
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
  tm_shape(hospitales)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
tmap_save(map_OA_417_2019, filename="Mapa_prueba OA.png", height=5, width=4, units="in", dpi=300)
map_OA_417_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(OA_417_2021) + 
  tm_polygons(col = "accessibility",  
              title = "Oportunidades alcanzadas en 60 minutos - 417m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.7, 
              breaks = c(0,3,6,9,12,14),
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
  tm_shape(hospitales)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
mapa_417 <- tmap_arrange(map_OA_417_2019,map_OA_417_2021,ncol=2,nrow = 1)
tmap_save(mapa_417, filename="MapaOA_417_2019_2021.png", height=4, width=6, units="in", dpi=300)

map_OA_833_2019 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(OA_833_2019) + 
  tm_polygons(col = "accessibility",  
              title = "Oportunidades alcanzadas en 60 minutos - 833m - 2019", 
              palette = pal ,
              n=5,
              alpha = 0.7, 
              breaks = c(0,3,6,9,12,14),
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
  tm_shape(hospitales)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
map_OA_833_2021 <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(OA_833_2021) + 
  tm_polygons(col = "accessibility",  
              title = "Oportunidades alcanzadas en 60 minutos - 833m - 2021", 
              palette = pal ,
              n=5,
              alpha = 0.7, 
              breaks = c(0,3,6,9,12,14),
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
  tm_shape(hospitales)+
  tm_symbols(size = 0.08,
             shape = hospitales_icon)
mapa_833 <- tmap_arrange(map_OA_833_2019,map_OA_833_2021,ncol=2,nrow = 1)
tmap_save(mapa_833, filename="MapaOA_833_2019_2021.png", height=4, width=6, units="in", dpi=300)

rm(list=ls())
