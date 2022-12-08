#Procesamiento tiempos de viaje a hospitales publicos - Graficos

rm(list=ls())

##Librerias
library(leaflet)
library(ggplot2)
library(dplyr)
library(patchwork)
library(xlsx)

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

##Detalles graficos
devtools::install_github('Mikata-Project/ggthemr',force=TRUE)
library(ggthemr)
palette_btext <- define_palette(background = '#e8ece5', 
                              text = c('black', 'black'), 
                              line = c('#bbb8ab', '#bbb8ab'),
                              gridline = '#bbb8ab',
                              swatch = (c('#14828C','#efc847','#d8854a','#cc644c','#b52150', '#a90052')),
                              gradient = c(low='#efc847', high='#a90052'))

ggthemr(palette_btext,layout = "scientific",text_size   = 11)
windowsFonts(Cambria=windowsFont("Cambria"))
scales::show_col(c('#14828C','#efc847','#d8854a','#cc644c','#b52150', '#a90052'))

###Densidad de probabilidad
d_CAT1_417_2019 <- 
  ggplot(data=CAT1_417_2019, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT1\nHospitales especializados")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT2_417_2019 <- 
  ggplot(data=CAT2_417_2019, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT2\nHospitales con terapia intensiva")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT3_417_2019 <- 
  ggplot(data=CAT3_417_2019, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT3\nHospitales con internación simple")+
  facet_wrap(~quintil_NBI,ncol = 1)

d_CAT1_417_2019 + theme(text = element_text(family = "Cambria")) +
  d_CAT2_417_2019 + theme(text = element_text(family = "Cambria")) +
  d_CAT3_417_2019 + theme(text = element_text(family = "Cambria"))
  
d_CAT1_417_2021 <- 
  ggplot(data=CAT1_417_2021, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT1\nHospitales especializados")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT2_417_2021 <- 
  ggplot(data=CAT2_417_2021, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT2\nHospitales con terapia intensiva")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT3_417_2021 <- 
  ggplot(data=CAT3_417_2021, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT3\nHospitales con internación simple")+
  facet_wrap(~quintil_NBI,ncol = 1)

d_CAT1_417_2021 + theme(text = element_text(family = "Cambria")) +
  d_CAT2_417_2021 + theme(text = element_text(family = "Cambria")) +
  d_CAT3_417_2021 + theme(text = element_text(family = "Cambria"))

d_CAT1_833_2019 <- 
  ggplot(data=CAT1_833_2019, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT1\nHospitales especializados")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT2_833_2019 <- 
  ggplot(data=CAT2_833_2019, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT2\nHospitales con terapia intensiva")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT3_833_2019 <- 
  ggplot(data=CAT3_833_2019, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT3\nHospitales con internación simple")+
  facet_wrap(~quintil_NBI,ncol = 1)

d_CAT1_833_2019 + theme(text = element_text(family = "Cambria")) +
  d_CAT2_833_2019 + theme(text = element_text(family = "Cambria")) +
  d_CAT3_833_2019 + theme(text = element_text(family = "Cambria"))

d_CAT1_833_2021 <- 
  ggplot(data=CAT1_833_2021, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT1\nHospitales especializados")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT2_833_2021 <- 
  ggplot(data=CAT2_833_2021, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT2\nHospitales con terapia intensiva")+
  facet_wrap(~quintil_NBI,ncol = 1)
d_CAT3_833_2021 <- 
  ggplot(data=CAT3_833_2021, aes(x=avg_tiempo_viaje))+
  geom_density(mapping=aes(group=quintil_NBI,fill=quintil_NBI,alpha=1/5),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Tiempo de viaje promedio [minutos]",y="",title="CAT3\nHospitales con internación simple")+
  facet_wrap(~quintil_NBI,ncol = 1)

d_CAT1_833_2021 + theme(text = element_text(family = "Cambria")) +
  d_CAT2_833_2021 + theme(text = element_text(family = "Cambria")) +
  d_CAT3_833_2021 + theme(text = element_text(family = "Cambria"))

##Detalles graficos
devtools::install_github('Mikata-Project/ggthemr',force=TRUE)
library(ggthemr)
palette_bbtext <- define_palette(background = '#e8ece5', 
                                text = c('black', 'black'), 
                                line = c('#bbb8ab', '#bbb8ab'),
                                gridline = '#bbb8ab',
                                swatch = (c('black','#cc644c','#b52150', '#a90052')),
                                gradient = c(low='#efc847', high='#a90052'))

ggthemr(palette_bbtext,layout = "scientific",text_size   = 11)
windowsFonts(Cambria=windowsFont("Cambria"))

###Boxplot
b_CAT1_417_2019 <-
  ggplot(data=CAT1_417_2019, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 417m - 2019\nCAT1 Hospitales especializados")
b_CAT2_417_2019 <-
  ggplot(data=CAT2_417_2019, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 417m - 2019\nCAT2 Hospitales con terapia intensiva")
b_CAT3_417_2019 <-
  ggplot(data=CAT3_417_2019, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 417m - 2019\nCAT3 Hospitales con internación simple")

b_CAT1_417_2019 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_417_2019 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_417_2019 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))

b_CAT1_417_2021 <-
  ggplot(data=CAT1_417_2021, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 417m - 2021\nCAT1 Hospitales especializados")
b_CAT2_417_2021 <-
  ggplot(data=CAT2_417_2021, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 417m - 2021\nCAT2 Hospitales con terapia intensiva")
b_CAT3_417_2021 <-
  ggplot(data=CAT3_417_2021, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 417m - 2021\nCAT3 Hospitales con internación simple")

b_CAT1_417_2021 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_417_2021 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_417_2021 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))

b_CAT1_833_2019 <-
  ggplot(data=CAT1_833_2019, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 833m - 2019\nCAT1 Hospitales especializados")
b_CAT2_833_2019 <-
  ggplot(data=CAT2_833_2019, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 833m - 2019\nCAT2 Hospitales con terapia intensiva")
b_CAT3_833_2019 <-
  ggplot(data=CAT3_833_2019, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 833m - 2019\nCAT3 Hospitales con internación simple")

b_CAT1_833_2019 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_833_2019 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_833_2019 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))

b_CAT1_833_2021 <-
  ggplot(data=CAT1_833_2021, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 833m - 2021\nCAT1 Hospitales especializados")
b_CAT2_833_2021 <-
  ggplot(data=CAT2_833_2021, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 833m - 2021\nCAT2 Hospitales con terapia intensiva")
b_CAT3_833_2021 <-
  ggplot(data=CAT3_833_2021, aes(y=avg_tiempo_viaje, x=quintil_NBI))+
  geom_boxplot(mapping=aes(group=quintil_NBI),show.legend = FALSE,na.rm = FALSE)+
  labs(x="Quintiles de hogares por %NBI",y="Tiempo de viaje promedio [minutos]",title="Caminata máxima 833m - 2021\nCAT3 Hospitales con internación simple")

b_CAT1_833_2021 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT2_833_2021 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))+
  b_CAT3_833_2021 + theme(text = element_text(family = "Cambria")) + scale_y_continuous(name="Tiempo de viaje promedio [minutos]", breaks=c(20,40,60,80,100,120), limits=c(20,120))

##Estadisticos resumen
t_CAT1_417_2019 <- CAT1_417_2019 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2019) %>% 
  mutate(caminata=417)
t_CAT2_417_2019 <- CAT2_417_2019 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2019) %>% 
  mutate(caminata=417) %>% 
  rows_append(t_CAT1_417_2019)
t_417_2019 <- CAT3_417_2019 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2019) %>% 
  mutate(caminata=417) %>% 
  rows_append(t_CAT2_417_2019)
write.xlsx(t_417_2019,"~/t_417_2019.xlsx")

t_CAT1_417_2021 <- CAT1_417_2021 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2021) %>% 
  mutate(caminata=417)
t_CAT2_417_2021 <- CAT2_417_2021 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2021) %>% 
  mutate(caminata=417) %>% 
  rows_append(t_CAT1_417_2021)
t_417_2021 <- CAT3_417_2021 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2021) %>% 
  mutate(caminata=417) %>% 
  rows_append(t_CAT2_417_2021)
write.xlsx(t_417_2021,"~/t_417_2021.xlsx")

t_CAT1_833_2019 <- CAT1_833_2019 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2019) %>% 
  mutate(caminata=833)
t_CAT2_833_2019 <- CAT2_833_2019 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2019) %>% 
  mutate(caminata=833) %>% 
  rows_append(t_CAT1_833_2019)
t_833_2019 <- CAT3_833_2019 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2019) %>% 
  mutate(caminata=833) %>% 
  rows_append(t_CAT2_833_2019)
write.xlsx(t_833_2019,"~/t_833_2019.xlsx")

t_CAT1_833_2021 <- CAT1_833_2021 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=1) %>% 
  mutate(anio=2021) %>% 
  mutate(caminata=833)
t_CAT2_833_2021 <- CAT2_833_2021 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=2) %>% 
  mutate(anio=2021) %>% 
  mutate(caminata=833) %>% 
  rows_append(t_CAT1_833_2021)
t_833_2021 <- CAT3_833_2021 %>% 
  group_by(quintil_NBI) %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje)) %>% 
  mutate(CAT=3) %>% 
  mutate(anio=2021) %>% 
  mutate(caminata=833) %>% 
  rows_append(t_CAT2_833_2021)
write.xlsx(t_833_2021,"~/t_833_2021.xlsx")

CAT1_417_2019 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT2_417_2019 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT3_417_2019 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))

CAT1_417_2021 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT2_417_2021 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT3_417_2021 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))

CAT1_833_2019 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT2_833_2019 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT3_833_2019 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))

CAT1_833_2021 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT2_833_2021 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))
CAT3_833_2021 %>% 
  summarise(mean = mean(avg_tiempo_viaje), median(avg_tiempo_viaje), sd(avg_tiempo_viaje),min(avg_tiempo_viaje),max(avg_tiempo_viaje))

rm(list=ls())
