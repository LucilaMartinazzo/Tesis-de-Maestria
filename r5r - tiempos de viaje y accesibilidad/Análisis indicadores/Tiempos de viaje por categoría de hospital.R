#Agrupamiento de resultados de tiempo de viaje por categoría de hospital

##Librerias
library(RSQLite)
library(leaflet)
library(ggplot2)
library(xlsx)
library(dplyr)
library(Hmisc)

##Datos
setwd("~/Analisis de tiempos de viaje y accesibilidad")
radios_censales<-read.csv("Centroides radios con %NBI.csv",sep = ',',header = TRUE) ##Obtenidos a través de QGIS del shapefile de radios del Censo 2010
TTM<-read.csv("Matriz de tiempos de viaje a hospitales.csv",sep = ',',header = TRUE)

##Crear base de datos
con<-dbConnect(RSQLite::SQLite(),":memory:")
dbWriteTable(con,"radios",radios_censales)
dbWriteTable(con,"TTM",TTM)

##Categorizacion de datos
CAT1<-dbGetQuery(con, "select Origen as id, travel_time from TTM where Destino in (1,2,3,4)")
CAT2<-dbGetQuery(con, "select Origen as id, travel_time from TTM where Destino in (5,6,7,11,14)")
CAT3<-dbGetQuery(con, "select Origen as id, travel_time from TTM where Destino in (8,9,10,13,15)")
dbWriteTable(con,"CAT1",CAT1)
dbWriteTable(con,"CAT2",CAT2)
dbWriteTable(con,"CAT3",CAT3)
CAT1_ag<-dbGetQuery(con, "select Origen as id, avg(travel_time) as TT_promedio from TTM where Destino in (1,2,3,4) group by Origen")
CAT2_ag<-dbGetQuery(con, "select Origen as id, avg(travel_time) as TT_promedio from TTM where Destino in (5,6,7,11,14) group by Origen")
CAT3_ag<-dbGetQuery(con, "select Origen as id, avg(travel_time) as TT_promedio from TTM where Destino in (8,9,10,13,15) group by Origen")
dbWriteTable(con,"CAT1_ag",CAT1_ag)
dbWriteTable(con,"CAT2_ag",CAT2_ag)
dbWriteTable(con,"CAT3_ag",CAT3_ag)

##Union de tablas
HE<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI, 100-porc_hogaresNBI as porc_hogaresSinNBI, travel_time from radios inner join CAT1 on radios.id=CAT1.id")
HE_ag<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, TT_promedio from radios inner join CAT1_ag on radios.id=CAT1_ag.id")
HTI<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI, 100-porc_hogaresNBI as porc_hogaresSinNBI, travel_time from radios inner join CAT2 on radios.id=CAT2.id")
HTI_ag<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, TT_promedio from radios inner join CAT2_ag on radios.id=CAT2_ag.id")
HIS<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI, 100-porc_hogaresNBI as porc_hogaresSinNBI, travel_time from radios inner join CAT3 on radios.id=CAT3.id")
HIS_ag<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, TT_promedio from radios inner join CAT3_ag on radios.id=CAT3_ag.id")

##Categorizacion en quintiles por % de hogares sin NBI
###CAT 1 - HE
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(HE_ag$porc_hogaresSinNBI, HE_ag$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
HE_ag$quintilNBI <- sapply(HE_ag$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = HE_ag, sum)
###CAT 2 - HTI
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(HTI_ag$porc_hogaresSinNBI, HTI_ag$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
HTI_ag$quintilNBI <- sapply(HTI_ag$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = HTI_ag, sum)
###CAT 3 - HIS
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(HIS_ag$porc_hogaresSinNBI, HIS_ag$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
HIS_ag$quintilNBI <- sapply(HIS_ag$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = HIS_ag, sum)

##CategorizaciOn en quintiles por tiempo de viaje
###CAT 1 - HE
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(HE_ag$TT_promedio, HE_ag$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
HE_ag$quintilTT <- sapply(HE_ag$TT_promedio, ApplyQuintiles)
aggregate(hogares ~ quintilTT, data = HE_ag, sum)
write.xlsx(HE_ag,"~/TT_HE_agrupados.xlsx")
###CAT 2 - HTI
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(HTI_ag$TT_promedio, HTI_ag$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
HTI_ag$quintilTT <- sapply(HTI_ag$TT_promedio, ApplyQuintiles)
aggregate(hogares ~ quintilTT, data = HTI_ag, sum)
write.xlsx(HTI_ag,"~/TT_HTI_agrupados.xlsx")
###CAT 3 - HIS
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(HIS_ag$TT_promedio, HIS_ag$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
HIS_ag$quintilTT <- sapply(HIS_ag$TT_promedio, ApplyQuintiles)
aggregate(hogares ~ quintilTT, data = HIS_ag, sum)
write.xlsx(HIS_ag,"~/TT_HIS_agrupados.xlsx")

##Graficos
devtools::install_github('Mikata-Project/ggthemr',force=TRUE)
library(ggthemr)
ggthemr('grape',layout = "scientific")

###Curvas de densidad
ggplot(data=HE_ag, aes(x=TT_promedio))+
  geom_density(mapping=aes(group=quintilNBI,fill=quintilNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="Distribucion de los tiempos de viajes a hospitales especializados por quintil de \n% hogares sin NBI")+
  facet_wrap(~quintilNBI,ncol = 1)
ggplot(data=HTI_ag, aes(x=TT_promedio))+
  geom_density(mapping=aes(group=quintilNBI,fill=quintilNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="Distribucion de los tiempos de viajes a hospitales con terapia intensiva por quintil\nde % hogares sin NBI")+
  facet_wrap(~quintilNBI,ncol = 1)
ggplot(data=HIS_ag, aes(x=TT_promedio))+
  geom_density(mapping=aes(group=quintilNBI,fill=quintilNBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="Distribucion de los tiempos de viajes a hospitales con internacion simple por\nquintil de % hogares sin NBI")+
  facet_wrap(~quintilNBI,ncol = 1)

###Boxplot TT_promedio by quintil_porc_NBI
tiff("~/P4_TT_HE_boxplot.tif", res=1000, compression = "lzw", height=4200, width=4200, units="px")
par(ps = 11, cex = 0.80, cex.main = 0.80, mar=c(3.8,4.2,0.8,0.8), mgp=c(2.4,0.4,0), oma=c(0,0,0,0))
boxplot(HE_ag$TT_promedio ~ HE_ag$quintilNBI, main="", xlab="Quintiles de hogares segun\n % hogares sin NBI", ylab="Tiempo de viaje promedio a hospitales especializados [min.]", ylim=c(20,80))
dev.off()
tiff("~/P4_TT_HTI_boxplot.tif", res=1000, compression = "lzw", height=4200, width=4200, units="px")
par(ps = 11, cex = 0.80, cex.main = 0.80, mar=c(3.8,4.2,0.8,0.8), mgp=c(2.4,0.4,0), oma=c(0,0,0,0))
boxplot(HTI_ag$TT_promedio ~ HTI_ag$quintilNBI, main="", xlab="Quintiles de hogares segun\n % hogares sin NBI", ylab="Tiempo de viaje promedio a hospitales con terapia intensiva [min.]", ylim=c(20,80))
dev.off()
tiff("~/P4_TT_HIS_boxplot.tif", res=1000, compression = "lzw", height=4200, width=4200, units="px")
par(ps = 11, cex = 0.80, cex.main = 0.80, mar=c(3.8,4.2,0.8,0.8), mgp=c(2.4,0.4,0), oma=c(0,0,0,0))
boxplot(HIS_ag$TT_promedio ~ HIS_ag$quintilNBI, main="", xlab="Quintiles de hogares segun\n % hogares sin NBI", ylab="Tiempo de viaje promedio a hospitales con internaci?n simple [min.]", ylim=c(20,80))
dev.off()

##Desconectar base
dbDisconnect(con)
