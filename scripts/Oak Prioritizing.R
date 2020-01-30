#Script for Prioritizing oaks
library(googlesheets4)
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(ape)
library(ggplot2)

path.base <- "G:/My Drive/Oaks_ClimateSensitivity/"
path.t <- paste(path.base, "Traits/", sep="")

setwd(path.base)

#grabbing the file from google drive and making it a workable data frame
quer.df <- sheets_find("Quercus Collection Metadata")
quer.dat <- data.frame(sheets_read(quer.df, range='QuercusCollection'))
colnames(quer.dat) <- c("Species", "Common Name", "Subgenus", "Section", "Numtrees", "Wild Origin", "Garden Origin",
                        "Geographic Distribution", "Phenology", "Dendrometer Bands", "ITRDB", "NPN", "TRY_Traits", 
                        "BIEN_Traits", "BIEN_Map", "FIA_Map", "Notes")

quer.dat <- quer.dat[!(quer.dat$Phenology == "N"),]


#Grabbing the BIEN trait file for comparison
setwd(path.t)
bien.dat <- read.csv("BIEN_fulllist.csv")

#creating a list of species and traits to include in ordination
#quercus.mod <- subset(bien.dat, select= c("species", "trait_name"))
#quercus.s <- distinct(quercus.mod)

#quercus.sp <- data.frame(ddply(quercus.s,~species,summarise,number=length(unique(trait_name))))
#quercus.sp <- quercus.sp[!(quercus.sp$number < 10),]

#quercus.t <- data.frame(ddply(quercus.s,~trait_name,summarise,number=length(unique(species))))
#quercus.t <- quercus.t[!(quercus.t$number < 13),]

#traitlist <- quercus.t$trait_name

specieslist <- quer.dat$Species
traitlist <- list("leaf area per leaf dry mass", "seed mass", "stem wood density", "whole plant height",
                  "leaf nitrogen content per leaf dry mass", "maximum whole plant longevity")

#removing oak species and traits we aren't concerned with from BIEN
bien.mod <- bien.dat[(bien.dat$species %in% specieslist & bien.dat$trait_name %in% traitlist), ]
bien.mod <- bien.mod[!(is.na(bien.mod$latitude)== T | is.na(bien.mod$longitude)) == T,]
bien.mod$trait_value <- as.character(bien.mod$trait_value)

bien.mod <- subset(bien.mod, select=c(1:4,6:7))

#Converting to character then number YOU MUST CONVERT TO CHARCTER FIRST OR VALUES CHANGE
bien.mod$trait_value <- as.numeric(bien.mod$trait_value)

bien.lat <- bien.mod[(duplicated(bien.mod$latitude & bien.mod$longitude)),]


#currently this removes non numeric traits e.g. flower color, whole plant dispersal syndrome
#This is useful for now for narrowing traits but remember their exclusion
bien.agg <- aggregate(trait_value~species+trait_name+project_pi, data=bien.mod, median)

#Here is the actual ordination script#
bien.ord <- spread(bien.agg, trait_name, trait_value)

#removing any remaining NA's leftover from trimming of species and traits
View(bien.ord)
bien.ord <- na.omit(bien.ord)

bien.orddata <- bien.ord[,-c(1)]

set.seed(4)
bien.mds <- metaMDS(comm = bien.orddata, distance = "bray", trace = FALSE) 
bien.xy <- data.frame(bien.mds$points)
bien.xy$species <- bien.ord$species


ggplot(bien.xy, aes(MDS1, MDS2, color = bien.xy$species)) + geom_point() + theme_bw()

bien.mds$stress

View(bien.qualchart)

#Creating a data.frame of the qualitative traits that aren't run in ordination
bien.qual <- bien.mod
bien.qual$trait_value <- gsub('[0-9]+', '', bien.qual$trait_value)
bien.qual$trait_value <- gsub('[[:punct:]]+', '', bien.qual$trait_value)
bien.qual <- bien.qual[!((bien.qual$trait_value=="")|(bien.qual$trait_value=="e")),]
bien.qualagg <- bien.qual %>% 
  group_by(species, trait_name) %>% 
  summarise(trait_value = paste(unique(trait_value), collapse = ', '))

bien.qualchart <- spread(bien.qualagg, trait_name, trait_value)

#grabbing the TRY trait file
setwd(path.t)
try.df <- sheets_find("TRY_Quercus_Traits")
try.dat <- data.frame(sheets_read(try.df))
colnames(try.dat)

#removing oak species we aren't concerned with from TRY
try.mod <- try.dat[try.dat$species %in% specieslist, ]


