#Procesamiento tiempos de viaje a hospitales públicos

rm(list=ls())

##Libreríis
library(RSQLite)
library(leaflet)
library(ggplot2)
library(xlsx)
library(dplyr)
library(Hmisc)

##Datos
setwd("D:/~")
radios_censales<-read.csv("Centroides radios con %NBI.csv",sep = ',',header = TRUE) ##Obtenidos a través de QGIS del shapefile de radios del Censo 2010
TTM_417_2019<-read.csv("TTM 2019-417m.csv",sep = ',',header = TRUE)
TTM_833_2019<-read.csv("TTM 2019-833m.csv",sep = ',',header = TRUE)
TTM_417_2021<-read.csv("TTM 2021b-417m.csv",sep = ',',header = TRUE)
TTM_833_2021<-read.csv("TTM 2021b-833m.csv",sep = ',',header = TRUE)

##Crear base de datos
con<-dbConnect(RSQLite::SQLite(),":memory:")
dbWriteTable(con,"radios",radios_censales)
dbWriteTable(con,"TTM_417_2019",TTM_417_2019)
dbWriteTable(con,"TTM_833_2019",TTM_833_2019)
dbWriteTable(con,"TTM_417_2021",TTM_417_2021)
dbWriteTable(con,"TTM_833_2021",TTM_833_2021)

##Categorizacion de datos
CAT1_417_2019_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_417_2019 where destino in (1,2,3,4) group by origen")
CAT2_417_2019_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_417_2019 where destino in (5,6,7,11,14) group by origen")
CAT3_417_2019_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_417_2019 where destino in (8,9,10,13,15) group by origen")
dbWriteTable(con,"CAT1_417_2019_ag",CAT1_417_2019_ag)
dbWriteTable(con,"CAT2_417_2019_ag",CAT2_417_2019_ag)
dbWriteTable(con,"CAT3_417_2019_ag",CAT3_417_2019_ag)
CAT1_833_2019_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_833_2019 where destino in (1,2,3,4) group by origen")
CAT2_833_2019_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_833_2019 where destino in (5,6,7,11,14) group by origen")
CAT3_833_2019_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_833_2019 where destino in (8,9,10,13,15) group by origen")
dbWriteTable(con,"CAT1_833_2019_ag",CAT1_833_2019_ag)
dbWriteTable(con,"CAT2_833_2019_ag",CAT2_833_2019_ag)
dbWriteTable(con,"CAT3_833_2019_ag",CAT3_833_2019_ag)
CAT1_417_2021_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_417_2021 where destino in (1,2,3,4) group by origen")
CAT2_417_2021_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_417_2021 where destino in (5,6,7,11,14) group by origen")
CAT3_417_2021_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_417_2021 where destino in (8,9,10,13,15) group by origen")
dbWriteTable(con,"CAT1_417_2021_ag",CAT1_417_2021_ag)
dbWriteTable(con,"CAT2_417_2021_ag",CAT2_417_2021_ag)
dbWriteTable(con,"CAT3_417_2021_ag",CAT3_417_2021_ag)
CAT1_833_2021_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_833_2021 where destino in (1,2,3,4) group by origen")
CAT2_833_2021_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_833_2021 where destino in (5,6,7,11,14) group by origen")
CAT3_833_2021_ag<-dbGetQuery(con, "select origen as id, avg(tiempo_viaje) as avg_tiempo_viaje from TTM_833_2021 where destino in (8,9,10,13,15) group by origen")
dbWriteTable(con,"CAT1_833_2021_ag",CAT1_833_2021_ag)
dbWriteTable(con,"CAT2_833_2021_ag",CAT2_833_2021_ag)
dbWriteTable(con,"CAT3_833_2021_ag",CAT3_833_2021_ag)

##Union de tablas
CAT1_417_2019<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT1_417_2019_ag on radios.id=CAT1_417_2019_ag.id")
CAT1_833_2019<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT1_833_2019_ag on radios.id=CAT1_833_2019_ag.id")
CAT1_417_2021<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT1_417_2021_ag on radios.id=CAT1_417_2021_ag.id")
CAT1_833_2021<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT1_833_2021_ag on radios.id=CAT1_833_2021_ag.id")
CAT2_417_2019<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT2_417_2019_ag on radios.id=CAT2_417_2019_ag.id")
CAT2_833_2019<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT2_833_2019_ag on radios.id=CAT2_833_2019_ag.id")
CAT2_417_2021<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT2_417_2021_ag on radios.id=CAT2_417_2021_ag.id")
CAT2_833_2021<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT2_833_2021_ag on radios.id=CAT2_833_2021_ag.id")
CAT3_417_2019<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT3_417_2019_ag on radios.id=CAT3_417_2019_ag.id")
CAT3_833_2019<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT3_833_2019_ag on radios.id=CAT3_833_2019_ag.id")
CAT3_417_2021<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT3_417_2021_ag on radios.id=CAT3_417_2021_ag.id")
CAT3_833_2021<-dbGetQuery(con, "select radios.id as link, lon, lat, hogares, hogaresNBI, porc_hogaresNBI,100-porc_hogaresNBI as porc_hogaresSinNBI, avg_tiempo_viaje from radios inner join CAT3_833_2021_ag on radios.id=CAT3_833_2021_ag.id")

##Categorizacion en quintiles por % de hogares sin NBI
###CAT1
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_417_2019$porc_hogaresSinNBI, CAT1_417_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_417_2019$quintilNBI <- sapply(CAT1_417_2019$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT1_417_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_417_2021$porc_hogaresSinNBI, CAT1_417_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_417_2021$quintilNBI <- sapply(CAT1_417_2021$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT1_417_2021, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_833_2019$porc_hogaresSinNBI, CAT1_833_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_833_2019$quintilNBI <- sapply(CAT1_833_2019$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT1_833_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_833_2021$porc_hogaresSinNBI, CAT1_833_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_833_2021$quintilNBI <- sapply(CAT1_833_2021$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT1_833_2021, sum)

###CAT2
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_417_2019$porc_hogaresSinNBI, CAT2_417_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_417_2019$quintilNBI <- sapply(CAT2_417_2019$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT2_417_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_417_2021$porc_hogaresSinNBI, CAT2_417_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_417_2021$quintilNBI <- sapply(CAT2_417_2021$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT2_417_2021, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_833_2019$porc_hogaresSinNBI, CAT2_833_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_833_2019$quintilNBI <- sapply(CAT2_833_2019$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT2_833_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_833_2021$porc_hogaresSinNBI, CAT2_833_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_833_2021$quintilNBI <- sapply(CAT2_833_2021$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT2_833_2021, sum)

###CAT3
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_417_2019$porc_hogaresSinNBI, CAT3_417_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_417_2019$quintilNBI <- sapply(CAT3_417_2019$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT3_417_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_417_2021$porc_hogaresSinNBI, CAT3_417_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_417_2021$quintilNBI <- sapply(CAT3_417_2021$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT3_417_2021, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_833_2019$porc_hogaresSinNBI, CAT3_833_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_833_2019$quintilNBI <- sapply(CAT3_833_2019$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT3_833_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_833_2021$porc_hogaresSinNBI, CAT3_833_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_833_2021$quintilNBI <- sapply(CAT3_833_2021$porc_hogaresSinNBI, ApplyQuintiles)
aggregate(hogares ~ quintilNBI, data = CAT3_833_2021, sum)

##Categorizacion en quintiles por tiempo de viaje
###CAT1
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_417_2019$avg_tiempo_viaje, CAT1_417_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_417_2019$quintil_tiempo_viaje <- sapply(CAT1_417_2019$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT1_417_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_417_2021$avg_tiempo_viaje, CAT1_417_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_417_2021$quintil_tiempo_viaje <- sapply(CAT1_417_2021$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT1_417_2021, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_833_2019$avg_tiempo_viaje, CAT1_833_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_833_2019$quintil_tiempo_viaje <- sapply(CAT1_833_2019$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT1_833_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT1_833_2021$avg_tiempo_viaje, CAT1_833_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT1_833_2021$quintil_tiempo_viaje <- sapply(CAT1_833_2021$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT1_833_2021, sum)

###CAT2
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_417_2019$avg_tiempo_viaje, CAT2_417_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_417_2019$quintil_tiempo_viaje <- sapply(CAT2_417_2019$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT2_417_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_417_2021$avg_tiempo_viaje, CAT2_417_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_417_2021$quintil_tiempo_viaje <- sapply(CAT2_417_2021$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT2_417_2021, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_833_2019$avg_tiempo_viaje, CAT2_833_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_833_2019$quintil_tiempo_viaje <- sapply(CAT2_833_2019$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT2_833_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT2_833_2021$avg_tiempo_viaje, CAT2_833_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT2_833_2021$quintil_tiempo_viaje <- sapply(CAT2_833_2021$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT2_833_2021, sum)

###CAT3
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_417_2019$avg_tiempo_viaje, CAT3_417_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_417_2019$quintil_tiempo_viaje <- sapply(CAT3_417_2019$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT3_417_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_417_2021$avg_tiempo_viaje, CAT3_417_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_417_2021$quintil_tiempo_viaje <- sapply(CAT3_417_2021$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT3_417_2021, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_833_2019$avg_tiempo_viaje, CAT3_833_2019$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_833_2019$quintil_tiempo_viaje <- sapply(CAT3_833_2019$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT3_833_2019, sum)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(wtd.quantile(CAT3_833_2021$avg_tiempo_viaje, CAT3_833_2021$hogares, probs = seq(0, 1, by = 0.20), na.rm=TRUE)), 
      labels=c("Q1","Q2","Q3","Q4","Q5"))
}
CAT3_833_2021$quintil_tiempo_viaje <- sapply(CAT3_833_2021$avg_tiempo_viaje, ApplyQuintiles)
aggregate(hogares ~ quintil_tiempo_viaje, data = CAT3_833_2021, sum)

write.xlsx(CAT1_417_2019,"~/CAT1_417_2019.xlsx")
write.xlsx(CAT1_417_2021,"~/CAT1_417_2021.xlsx")
write.xlsx(CAT1_833_2019,"~/CAT1_833_2019.xlsx")
write.xlsx(CAT1_833_2021,"~/CAT1_833_2021.xlsx")

write.xlsx(CAT2_417_2019,"~/CAT2_417_2019.xlsx")
write.xlsx(CAT2_417_2021,"~/CAT2_417_2021.xlsx")
write.xlsx(CAT2_833_2019,"~/CAT2_833_2019.xlsx")
write.xlsx(CAT2_833_2021,"~/CAT2_833_2021.xlsx")

write.xlsx(CAT3_417_2019,"~/CAT3_417_2019.xlsx")
write.xlsx(CAT3_417_2021,"~/CAT3_417_2021.xlsx")
write.xlsx(CAT3_833_2019,"~/CAT3_833_2019.xlsx")
write.xlsx(CAT3_833_2021,"~/CAT3_833_2021.xlsx")

##Desconectar base
dbDisconnect(con)
