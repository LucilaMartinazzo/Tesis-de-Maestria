#Analisis de resultados EOD 2009

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
setwd("D:/.")
EOD <- st_read("./Zonas EOD según radios censales.shp")   #Base Zonas EOD 2009
ejido <- st_read("./EjidoMunicipal.shp")

##Union de tablas
viajes <- read.csv("./Viajes.csv",sep = ',',header = TRUE)
viajes <- rename(viajes,Zona.EOD = Zona)
EOD <- inner_join(EOD,viajes,by = "Zona.EOD")


##Mapas estaticos
data(ejido)
osm_CBA <- read_osm(ejido, ext=1.1)
tmap_mode('plot') # Modo estatico
pal <- c('#efc847','#d8854a','#cc644c','#b52150', '#a90052') # Paleta para todo el trabajo
windowsFonts(Cambria=windowsFont("Cambria"))

##Zonificacion
map_zonasEOD <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_borders(col = '#b52150')+
  tm_fill(col = '#d8854a',  
          alpha = 0.3)+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(map_zonasEOD, filename="Zonas EOD 2009.png", height=6.5, width=6.5, units="in", dpi=300)

##Viajes totales
###Total por zona
map_V_total <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_total",  
              title = "Viajes totales", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(quantile(EOD$V_total, prob =  seq(0, 1, 1/5)) ),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(map_V_total, filename="Total viajes v2.png", height=7, width=10, units="in", dpi=300)

###Tasas por persona por zona
EOD1<-mutate(EOD,tasa_V=V_total/totalpobl)
map_tasas <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD1) + 
  tm_polygons(col = "tasa_V",  
              title = "Viajes por persona", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(quantile(EOD1$tasa_V, prob =  seq(0, 1, 1/5)) ),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "right",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
tmap_save(map_tasas, filename="Tasa de viajes por persona.png", height=7, width=10, units="in", dpi=300)

map_V_total_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_total_M",  
              title = "Viajes totales\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,5000,10000,15000,25000,55000),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_total_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_total_F",  
              title = "Viajes totales\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,5000,10000,15000,25000,55000),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
mapaVG <- tmap_arrange(map_V_total_M,map_V_total_F,ncol=2,nrow = 1)
tmap_save(mapaVG, filename="Total viajes por género.png", height=7, width=10, units="in", dpi=300)

###Porcentaje de viajes totales por genero
EOD1<-mutate(EOD,porc_VM=100*V_total_M/V_total)
map_PV_total_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD1) + 
  tm_polygons(col = "porc_VM",  
              title = "Porcentaje de viajes totales\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(quantile(EOD1$porc_VM, prob =  seq(0, 1, 1/5)) ))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
EOD1<-mutate(EOD,porc_VF=100*V_total_F/V_total)
map_PV_total_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD1) + 
  tm_polygons(col = "porc_VF",  
              title = "Porcentaje de viajes totales\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(quantile(EOD1$porc_VF, prob =  seq(0, 1, 1/5)) ))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
mapaPVG <- tmap_arrange(map_PV_total_M,map_PV_total_F,ncol=2,nrow = 1)
tmap_save(mapaPVG, filename="Porcentaje total viajes por genero.png", height=7, width=10, units="in", dpi=300)

###Tasas por persona por genero
EOD1<-mutate(EOD,tasa_V_M=V_total_M/varon)
map_tasas_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "tasa_V_M",  
              title = "Viajes por persona\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(quantile(EOD1$tasa_V_M, prob =  seq(0, 1, 1/5)) ))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
EOD1<-mutate(EOD,tasa_V_F=V_total_F/mujer)
map_tasas_F <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "tasa_V_F",  
              title = "Viajes por persona\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(quantile(EOD1$tasa_V_F, prob =  seq(0, 1, 1/5)) ))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
mapa_tasas_G <- tmap_arrange(map_tasas_M,map_tasas_F,ncol=2,nrow = 1)
tmap_save(mapa_tasas_G, filename="Tasa de viajes por persona por genero.png", height=7, width=10, units="in", dpi=300)

##Viajes por motivo
###Cantidad de viajes por genero
map_V_Mt_T_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Mt_T_M",  
              title = "Viajes totales por trabajo\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,2000,4000,6000,11500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Mt_T_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Mt_T_F",  
              title = "Viajes totales por trabajo\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,2000,4000,6000,7500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Mt_E_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Mt_E_M",  
              title = "Viajes totales por estudio\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,1500,2000,3000,6500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Mt_E_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Mt_E_F",  
              title = "Viajes totales por estudio\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,1500,2000,3000,9500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Mt_MC_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Mt_MC_M",  
              title = "Viajes totales por cuidado\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,500,1500,2000,3000,5000),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Mt_MC_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Mt_MC_F",  
              title = "Viajes totales por cuidado\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,500,1500,2000,3000,7000),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
# map_V_Mt_Soc_M <-
#   tm_shape(osm_CBA) +
#   tm_rgb() +
#   tm_shape(EOD) +
#   tm_polygons(col = "V_Mt_Soc_M",
#               title = "Viajes totales sociales\n(Sexo Masculino)",
#               palette = pal ,
#               n=5,
#               alpha = 0.5,
#               breaks = c(quantile(EOD$V_Mt_Soc_M, prob =  seq(0, 1, 1/5)) ))+
#   tm_layout(frame = FALSE,
#             fontfamily = "Cambria",
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             legend.title.size = 1.5,
#             legend.text.size = 1.2)+
#   tm_compass(position = c("right", "top")) +
#   tm_scale_bar(position = c("right", "bottom"))
# map_V_Mt_Soc_F <-
#   tm_shape(osm_CBA) +
#   tm_rgb() +
#   tm_shape(EOD) +
#   tm_polygons(col = "V_Mt_Soc_F",
#               title = "Viajes totales sociales\n(Sexo Femenino)",
#               palette = pal ,
#               n=5,
#               alpha = 0.5,
#               breaks = c(quantile(EOD$V_Mt_Soc_F, prob =  seq(0, 1, 1/5)) ))+
#   tm_layout(frame = FALSE,
#             fontfamily = "Cambria",
#             legend.outside = TRUE,
#             legend.outside.position = "bottom",
#             legend.title.size = 1.5,
#             legend.text.size = 1.2)+
#   tm_compass(position = c("right", "top")) +
#   tm_scale_bar(position = c("right", "bottom"))
mapa_Mt_G <- tmap_arrange(map_V_Mt_T_M,map_V_Mt_T_F,map_V_Mt_E_M,map_V_Mt_E_F,map_V_Mt_MC_M,map_V_Mt_MC_F,ncol=2,nrow = 3)
tmap_save(mapa_Mt_G, filename="Total viajes por motivo por género.png", height=16, width=8, units="in", dpi=300)

##Mapas por modo
###Cantidad de viajes por genero
map_V_Md_Bus_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Md_Bus_M",  
              title = "Viajes totales en colectivo/trolebús\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,2000,4000,6000,8000,13500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Md_Bus_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Md_Bus_F",  
              title = "Viajes totales en colectivo/trolebús\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,2000,4000,6000,8000,16500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Md_Auto_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Md_Auto_M",  
              title = "Viajes totales en auto\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,3000,5000,7000,20500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Md_Auto_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Md_Auto_F",  
              title = "Viajes totales en auto\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,3000,5000,7000,18500),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Md_Peat_M <- 
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Md_Peat_M",  
              title = "Viajes totales a pie\n(Sexo Masculino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,3000,5000,8000,16000),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
map_V_Md_Peat_F <-
  tm_shape(osm_CBA) +
  tm_rgb() +
  tm_shape(EOD) + 
  tm_polygons(col = "V_Md_Peat_F",  
              title = "Viajes totales a pie\n(Sexo Femenino)", 
              palette = pal ,
              n=5,
              alpha = 0.5, 
              breaks = c(0,1000,3000,5000,8000,18000),
              legend.format = c(text.separator = "-"))+
  tm_layout(frame = FALSE,
            fontfamily = "Cambria",
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = 1.5,
            legend.text.size = 1.2)+
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"))
mapa_Md_G <- tmap_arrange(map_V_Md_Bus_M, map_V_Md_Bus_F, map_V_Md_Auto_M, map_V_Md_Auto_F, map_V_Md_Peat_M, map_V_Md_Peat_F, ncol=2, nrow = 3)
tmap_save(mapa_Md_G, filename="Total viajes por modo por género.png", height=16, width=8, units="in", dpi=300)

rm(list=ls())