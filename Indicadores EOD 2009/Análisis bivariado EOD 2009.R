#Analisis bivariado resultados EOD 2009
#Fuente: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

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
library(rstudioapi)
library(magrittr)
library(lintr)
library(viridis)
library(cowplot)
library(rmarkdown)
windowsFonts(Cambria=windowsFont("Cambria"))

##Datos
setwd("D:/~")
EOD <- st_read("~/Zonas EOD según radios censales.shp")

##Union de tablas
viajes <- read.csv("~/Viajes.csv",sep = ',',header = TRUE)
viajes <- rename(viajes,Zona.EOD = Zona)
EOD <- inner_join(EOD,viajes,by = "Zona.EOD")

##Tema basico mapas
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Cambria",
                          color = "Black"),
      ###Remover todos los ejes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      ###Añadir grilla
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      ###Colores de fondo
      plot.background = element_rect(fill = "#F5FCEB",
                                     color = NA),
      panel.background = element_rect(fill = "#FCF7F9",
                                      color = NA),
      legend.background = element_rect(fill = "#FCF7F9",
                                       color = NA),
      ###Bordes y margenes
      plot.margin = unit(c(.2, .2, .2, .2), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      ###Titulos
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "Black"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "Black"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "Black",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      ###Leyendas
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

##Mapa
###Cuantiles para %HogaresNBI
quantiles_NBI <- EOD %>%
  pull(X.hogaresNB) %>%
  quantile(probs = seq(0, 1, length.out = 4))

###Cuantiles cantidad de viajes
quantiles_viajes <- EOD %>%
  pull(V_total) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_total_M <- EOD %>%
  pull(V_total_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_total_F <- EOD %>%
  pull(V_total_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Mt_T_M <- EOD %>%
  pull(V_Mt_T_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Mt_T_F <- EOD %>%
  pull(V_Mt_T_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Mt_E_M <- EOD %>%
  pull(V_Mt_E_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Mt_E_F <- EOD %>%
  pull(V_Mt_E_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Mt_MC_M <- EOD %>%
  pull(V_Mt_MC_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Mt_MC_F <- EOD %>%
  pull(V_Mt_MC_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Md_Bus_M <- EOD %>%
  pull(V_Md_Bus_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Md_Bus_F <- EOD %>%
  pull(V_Md_Bus_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Md_Auto_M <- EOD %>%
  pull(V_Md_Auto_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Md_Auto_F <- EOD %>%
  pull(V_Md_Auto_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Md_Peat_M <- EOD %>%
  pull(V_Md_Peat_M) %>%
  quantile(probs = seq(0, 1, length.out = 4))
quantiles_V_Md_Peat_F <- EOD %>%
  pull(V_Md_Peat_F) %>%
  quantile(probs = seq(0, 1, length.out = 4))

###Escala de color para las dos variables
###rojo para %HogaresNBI y azul para cantidad de viajes
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high NBI, high viajes
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low NBI, high viajes
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium NBI, medium viajes
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high NBI, low viajes
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low NBI, low viajes
) %>%
  gather("group", "fill")

###Corto la base en los grupos definidos anteriormente y hago left_join del color de relleno
EOD %<>%
  mutate(
    NBI_quantiles = cut(
      X.hogaresNB,
      breaks = quantiles_NBI,
      include.lowest = TRUE
    ),
    viajes_quantiles = cut(
      V_Md_Peat_F,
      breaks = quantiles_V_Md_Peat_F,
      include.lowest = TRUE
    ),
    group = paste(
      as.numeric(NBI_quantiles), "-",
      as.numeric(viajes_quantiles)
    )
  ) %>%
  left_join(bivariate_color_scale, by = "group")

###Dibujo el mapa
map <- ggplot(
  data = EOD
) +
  geom_sf(
    aes(
      fill = fill
    ),
    color = "black",
    size = 0.1
  ) +
  scale_fill_identity() +
  # add the theme
  theme_map()

###Leyenda del mapa
bivariate_color_scale %<>%
  separate(group, into = c("X.hogaresNB", "V_Md_Peat_F"), sep = " - ") %>%
  mutate(X.hogaresNB = as.integer(X.hogaresNB),
         V_Md_Peat_F = as.integer(V_Md_Peat_F))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = X.hogaresNB,
      y = V_Md_Peat_F,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Mayor %HogaresNBI",
       y = "Mayor N° viajes") +
  theme_map() +
  theme(
    axis.title = element_text(size = 7)
  ) +
  # quadratic tiles
  coord_fixed()

###Plot
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.078, 0, 0.2, 0.2)

legend

rm(list=ls())