library(rgal)
library(raster)
library(ggplot2)
library(dplyr)
library(sf)

setwd("C:/Users/lucie/Documents/R/data/")


path.base <- "G:/My Drive/Oaks_ClimateSensitivity/"
path.t <- paste(path.base, "Traits/", sep="")

murica.b <- st_read("Neon-DS-Site-Layout-Files/Neon-DS-Site-Layout-Files/US-Boundary-Layers/US-Boundary-Dissolved-States.shp")
murica.s <- st_read("Neon-DS-Site-Layout-Files/Neon-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp")
america <- st_read("us_states.shp")
alba <- st_read("litt802av.shp")

setwd("G:/My Drive/Oaks_ClimateSensitivity/Maps")


range <- st_read("Quercus_alba.shp")

america.sf <- spTransform(america, crs = "") 
                       

ggplot()+
  geom_sf(data = america)+
  geom_sf(data = alba, color = "purple", alpha = 0.3)+
  ggtitle("murica")+
  coord_sf()


setwd(path.t)
bien.dat <- read.csv("BIEN_fulllist.csv")

species <- googlesheets4::sheets_find("Oaks_Handpicked_Option")

species.df <- quer.dat <- data.frame(googlesheets4::sheets_read(species, range='Oaks_Handpicked_Option'))

specieslist <- "Quercus michauxii"
  
bien.mod <- bien.dat[(bien.dat$species %in% specieslist),]

bien.mod <- subset(bien.mod, select=c(1,6:7))

bien.mod <- bien.mod[!(is.na(bien.mod$latitude)== T | is.na(bien.mod$longitude)) == T,]

bien.lat <- unique(bien.mod[, 1:3])

bien.lat$latitude <- celestial::deg2dms(bien.lat$latitude)
bien.lat$longitude <- celestial::deg2dms(bien.lat$longitude, NS = TRUE)

Oak_Locations <- st_as_sf(bien.lat, coords = c("latitude", "longitude"), 
                          crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

path.d <- "G:/My Drive/Distributions_TreeSpecies/in-use_occurrence_compiled/"

setwd(path.d)

fia <- read.csv("fia_compiled.csv")

fia.sf <- st_as_sf(fia, coords = c("decimalLatitude", "decimalLongitude"), 
                   crs = "+init=epsg:43 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


ggplot() +
  geom_sf(data = fia.sf) +
  geom_sf(data = murica.b, size=2)+
  geom_sf(data = murica.s)+
  ggtitle("Map of unique oak locations for chosen species")


Que.phellos <- BIEN::BIEN_occurrence_species(species = "Quercus fusiformis")
Que.phellos <- Que.phellos[!(is.na(Que.phellos$latitude)== T | is.na(Que.phellos$longitude)) == T,]

Que <- st_as_sf(Que.phellos, coords = c("latitude", "longitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

crs(murica.b)

