#Script for Prioritizing oaks
library(googlesheets4)
library(plyr)
library(dplyr)
library(tidyr)
library(vegan)
library(ape)

path.base <- "G:/My Drive/Oaks_ClimateSensitivity/"
path.t <- paste(path.base, "Traits/", sep="")

setwd(path.base)

#grabbing the file from google drive and making it a workable data frame
quer.df <- sheets_find("Quercus Collection Metadata")
quer.dat <- data.frame(sheets_read(quer.df, range='QuercusCollection'))
colnames(quer.dat) <- c("Species", "Common Name", "Subgenus", "Section", "Numtrees", "Wild Origin", "Garden Origin",
                        "Geographic Distribution", "Phenology", "Dendrometer Bands", "ITRDB", "TRY_Traits", 
                        "BIEN_Traits", "Notes")

#grabbing the TRY trait file
setwd(path.t)
try.df <- sheets_find("TRY_Quercus_Traits")
try.dat <- data.frame(sheets_read(try.df))
colnames(try.dat)

#removing oak species we aren't concerned with from TRY
try.mod <- try.dat[try.dat$species %in% specieslist, ]


#Grabbing the BIEN trait file for comparison
setwd(path.t)
bien.dat <- read.csv("BIEN_fulllist.csv")
colnames(bien.dat)

#removing oaks we are not doing phenology monitoring for#
quer.org <- quer.dat[!(quer.dat$Phenology =="N"),]

#removing oaks that don't have both TRY and BIEN traits. Since none have BIEN but not TRY this works for only BIEN
quer.org <- na.omit(quer.org)

#creating a list of species and traits to include in ordination
quercus.mod <- subset(bien.dat, select= c("species", "trait_name"))
quercus.s <- quercus.mod %>% distinct()
quercus.sp <- data.frame(ddply(quercus.s,~species,summarise,number=length(unique(trait_name))))
quercus.sp <- quercus.sp[!(quercus.sp$number < 15),]

quercus.t <- data.frame(ddply(quercus.s,~trait_name,summarise,number=length(unique(species))))
quercus.t <- quercus.t[!(quercus.t$number < 20),]

specieslist <- quercus.sp$species
traitlist <- quercus.t$trait_name

#removing oak species we aren't concerned with from BIEN
bien.mod <- bien.dat[(bien.dat$species %in% specieslist & bien.dat$trait_name %in% traitlist), ]
bien.mod <- subset(bien.mod, select=c(1:4))

#Converting to character then number YOU MUST CONVERT TO CHARCTER FIRST OR VALUES CHANGE
bien.mod$trait_value <- as.numeric(as.character(bien.mod$trait_value))

#currently this removes non numeric traits e.g. flower color, whole plant dispersal syndrome
#This is useful for now for narrowing traits but remember their exclusion
bien.agg <- aggregate(trait_value~species+trait_name, data=bien.mod, median)

#Here is the actual ordination script#
bien.ord <- spread(bien.agg, trait_name, trait_value)

#removing any remaining NA's leftover from trimming of species and traits
View(bien.ord)
bien.ord <- na.omit(bien.ord)

bien.orddata <- bien.ord[,-c(1)]

bien.mds <- metaMDS(comm = bien.orddata, distance = "bray", trace = FALSE, autotransform = FALSE) 
 
plot(bien.mds$points)

bien.xy <- data.frame(bien.mds$points)
bien.xy$species <- bien.ord$species

library(ggplot2)
ggplot(bien.xy, aes(MDS1, MDS2, color = bien.xy$species)) + geom_point() + theme_bw()


#test case
bien.ordt <- spread(bien.agg, trait_name, trait_value)
bien.ordsm <- bien.ordt[c(2,6,13,17,19,29,30,33),c(2:4,19)]
View(bien.ordsm)

bien.modt$trait_value <- as.numeric(as.character(bien.modt$trait_value))
bien.aggt <- aggregate(trait_value~trait_name+species, data=bien.modt, mean)
View(bien.aggt)

