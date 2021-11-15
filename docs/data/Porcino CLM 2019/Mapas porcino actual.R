library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(writexl)
library(sf)
library(viridis)
library(rmarkdown)
library(tidygeocoder)
#Load geospatial data

setwd("C:/Victor/KCL/Thesis/R")

CLM_provinces <- st_read("Provincias.shp") 

CLM_provinces %>%
  ggplot(aes(fill=NOMBRE, ))+
  scale_fill_brewer(palette = "Reds") +
  geom_sf(color = "black", size = 0.1)

setwd("C:/Victor/KCL/Thesis/R/Website/Fighting_for_our_lives_MA_Thesis/data/Porcino CLM 2019")
villages_macrogranjas <- read_excel("porcino +2000.xlsx")

#Clean the excel
villages_macrogranjas_municipality_split <- villages_macrogranjas %>% 
  separate(Municipality,c("MUNICIPIO","ARTICLE"),sep=",")

#Geocode
villages_geocoded <- villages_macrogranjas_municipality_split %>%
  geocode("MUNICIPIO", method = 'osm', lat = latitude , long = longitude, min_time=2)


#Tamaño de puntos por animales en granja: Good one


CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=Animals, colour="pink", alpha=0.01)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30)) +
  guides(colour = FALSE, alpha = FALSE)+
  xlab("Longitude")+
  ylab("Latitude")
ggsave("map_existing_2019.png")


CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=Animals, colour="pink", alpha=0.01)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30)) +
  guides(colour = FALSE, alpha = FALSE)+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Map 3: Existing macrogranjas across villages of CLM (2019)")
ggsave("map_title_2019.png")

#EXTRA STUFF
villages_geocoded <- villages_geocoded %>% select(-ARTICLE, -...6)

head(CLM_provinces)

CLM_provinces %>%
  ggplot(aes(fill="firebrick"))+
  geom_sf(color = "white", size = 0.1) +
  geom_point(aes(villages_geocoded, color="black", alpha=0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-10, 5), ylim = c(35, 44))

CLM_provinces %>%
  ggplot(aes(fill="reds"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))

CLM_provinces <- fortify(CLM_provinces)

#Tamaño de puntos por animales en granja alpha uniforme
CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=ANIMALES, colour="pink")) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +

mybreaks <- c(2500,25000,50000,75000,100000,125000)

labels = c("2000-2499", "2499-24999", "25000-49999","50000-74999", "75000-124999", "150000+")

scale_size_continuous(name="Animals", range=c(2000,127000),breaks=mybreaks, labels = c("2000-2499", "2499-24999", "25000-49999","50000-74999", "75000-124999", "150000+"))+

CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=Animals, colour="pink", alpha=0.01)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30)) +
  guides(colour = FALSE, alpha = FALSE)


CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=Animals, colour="pink", alpha=0.5)) +
  scale_size_continuous(range=c(2000, 127000), breaks=c(2500,25000,50000,75000,100000,125000))
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30)) +
  guides(colour = FALSE, alpha = FALSE)



CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=Animals, colour="pink", alpha=0.5)) +
  scale_size_continuous(range=c(2000, 127000), breaks=c(2500,25000,50000,75000,100000,125000))+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))


#Todos los puntos tienen el mismo tamaño, pero el alpha es diferente
CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, colour="pink", alpha=0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))

CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, colour="pink", alpha=0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))


#Todos los puntos tienen el mismo tamaño, el alpha es igual para todos
CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, colour="pink")) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))

#Mapa, todos los puntos del mismo alpha y nombre de las provincias.
CLM_provinces %>%
  ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_sf_text(data = CLM_provinces, aes(label = NOMBRE), nudge_x = -0.36, nudge_y = -0.135) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, colour="pink")) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))
ggsave("mapa_nombres.png", width=1200, length=900)

