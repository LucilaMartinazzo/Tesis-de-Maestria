#Criterios de evaluacion a traves de indicadores de la demanda

rm(list=ls())

##Librerias
library(sf)
library(dplyr)
library(mapview)
library(gstat)
library(tmap)
library(tidyverse)
library(osmdata)
library(tmaptools)
library(OpenStreetMap)
library(spdep)

##Datos
setwd("D:/~")
EOD <- st_read("~/Zonas EOD según radios censales.shp")
ejido <- st_read("~/EjidoMunicipal.shp")

##Union de tablas
viajes <- read.csv("~/Viajes.csv",sep = ',',header = TRUE)
viajes <- rename(viajes,Zona.EOD = Zona)
EOD <- inner_join(EOD,viajes,by = "Zona.EOD")


##Mapas estaticos
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') ###Modo estatico
pal <- c('#efc847','#d8854a','#cc644c','#b52150', '#a90052') ###Paleta QGIS
windowsFonts(Cambria=windowsFont("Cambria"))
scales::show_col(c('#efc847','#d8854a','#cc644c','#b52150', '#a90052'))

###Tasas por persona por zona
EOD1<-mutate(EOD,tasa_V=V_total/totalpobl)
EOD1 %>% 
  summarise(mean(tasa_V))
map_tasas <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD1) + 
  tm_polygons(col = "tasa_V",  
              title = "Viajes por persona", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1.69,4),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(map_tasas, filename="Tasa de viajes por persona v4.png", height=7, width=10, units="in", dpi=300)
filtrado_tasa <- EOD1 %>% 
  filter(tasa_V>=1.69)

EOD2<-mutate(EOD,tasa_V_Md_Bus=(V_Md_Bus_M+V_Md_Bus_F)/totalpobl)
EOD2 %>% 
  summarise(mean(tasa_V_Md_Bus))
map_tasas <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD2) + 
  tm_polygons(col = "tasa_V_Md_Bus",  
              title = "Viajes por persona en\ntransporte público", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,0.56,2.5),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(map_tasas, filename="Tasa de viajes por persona en TP.png", height=7, width=10, units="in", dpi=300)

filtrado_tasa <- filtrado_tasa %>%
  mutate(tasa_V_Md_Bus=(V_Md_Bus_M+V_Md_Bus_F)/totalpobl) %>% 
  mutate(clase = case_when(tasa_V>=1.69 & tasa_V_Md_Bus>=0.56 ~ "Tasa mayor al promedio"))
map_tasas_mayor <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "grey",
              alpha = 0.1)+
  tm_shape(filtrado_tasa) + 
  tm_polygons(col = "clase",  
              title = "Tasas de viajes por persona\nen transporte público", 
              palette = '#a90052', 
              n=5,
              alpha = 0.5,
              textNA = "Tasa menor al promedio")+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(map_tasas_mayor, filename="Tasa de viajes por persona mayor-mayor.png", height=7, width=10, units="in", dpi=300)

##Exportar
st_write(filtrado_tasa, "Zonas filtradas por tasas.gpkg")
