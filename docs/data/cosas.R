library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(sf)
library(ggspatial)
library(tidygeocoder)

#Geocoding
setwd("C:/Victor/KCL/Thesis/R/Website/Fighting_for_our_lives_MA_Thesis/data/Porcino CLM 2019")

#Good version
#Read the excel
villages_macrogranjas <- read_excel("porcino +2000.xlsx")

villages_macrogranjas_municipality <- villages_macrogranjas %>%
  select(Municipality)
#Clean the excel
villages_macrogranjas_municipality_split <- villages_macrogranjas %>% 
  separate(Municipality,c("MUNICIPIO","ARTICLE"),sep=",")

#Geocode
villages_geocoded <- villages_macrogranjas_municipality_split %>%
  geocode("MUNICIPIO", method = 'osm', lat = latitude , long = longitude, min_time=2)





villages1 <- villages_macrogranjas %>%
  select(MUNICIPIO)

villages_macrogranjas1 <- villages1 %>%
  strsplit(MUNICIPIO, ",")

villages_split <- villages_macrogranjas %>% 
  separate(MUNICIPIO,c("MUNICIPIO","ARTICLE"),sep=",")

villages_geocoded <- villages_split %>%
  geocode("MUNICIPIO", method = 'osm', lat = latitude , long = longitude, min_time=2)

villages_geocoded <- villages_geocoded %>% select(-ARTICLE, -...6)

villages_geocoded

slaughter_houses <- read_excel("Mataderos.xlsx")

slaughter_houses_split <- slaughter_houses %>% 
  separate(MUNICIPIO,c("MUNICIPIO","ARTICLE"),sep=",")

slaughter_houses_geocoded <- slaughter_houses_split %>%
  geocode("MUNICIPIO", method = 'osm', lat = latitude , long = longitude, min_time=2) 

provinces <- st_read("ll_provinciales_inspire_peninbal_etrs89.shp")

provinces <- fortify(provinces)

province_clm <- provinces %>%
  grepl("Castilla la mancha", NAME_BOUND)

provinces_clm <- provinces%>%
  filter(CODNUT2 == "ES42")

provinces_clm %>% ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=ANIMALES, colour="pink", alpha=0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))
#Municipalities

municipios <- st_read("Municipios_IGN.shp")

castilla_la_mancha <- municipios%>%
  filter(CODNUT2 == "ES42")

castilla_la_mancha %>% ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=ANIMALES, colour="pink", alpha=0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))

castilla_la_mancha %>% ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=ANIMALES, colour=ORIENTACION, alpha=0.5)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))

castilla_la_mancha %>% ggplot(aes("gray33"))+
  geom_sf(color = "black", size = 0.1) +
  geom_point(data=villages_geocoded, aes(x=longitude, y=latitude, size=ANIMALES)) +
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-5.5, -0.5), ylim = c(38, 41.30))

write_xlsx(villages_geocoded, "villages_geocoded.xlsx")

write_xlsx(slaughter_houses_geocoded, "slaughter_houses_geocoded.xlsx")

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_nominatim_search <- function(search_query_url, country_url,
                                 language_url, email_url) {
  # load libraries
  library(RCurl)
  # nominatim search api url
  url_nominatim_search_api <- "https://nominatim.openstreetmap.org/search/"
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  # parameters
  if (!is.null(country_url)) {
    country_url <- paste0("&countrycodes=", country_url)
  }
  parameters_url <- paste0("?format=json",
                           "&addressdetails=1&extratags=1&limit=1",
                           country_url, "&accept-language=", language_url,
                           "&email=", email_url)
  # construct search request for geocode
  url_nominatim_search_call <- paste0(url_nominatim_search_api,
                                      search_query_url, parameters_url)
  return(url_nominatim_search_call)
}
