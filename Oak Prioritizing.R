#Script for Prioritizing oaks
library(googlesheets4)
library(plyr)
library(dplyr)


path.base <- "G:/My Drive/Oaks_ClimateSensitivity/"
path.t <- paste(path.base, "Traits/", sep="")

setwd(path.base)

#grabbing the file from google drive and making it a workable data frame
quer.df <- sheets_find("Quercus Collection Metadata")
quer.dat <- data.frame(sheets_read(quer.df, range='QuercusCollection'))
colnames(quer.dat) <- c("Species", "Common Name", "Subgenus", "Section", "Numtrees", "Wild Origin", "Garden Origin",
                        "Geographic Distribution", "Phenology", "Dendrometer Bands", "ITRDB", "TRY_Traits", 
                        "BIEN_Traits", "Notes")
#removing oaks we are not doing phenology monitoring for#
quer.org <- quer.dat[!(quer.dat$Phenology =="N"),]

#removing oaks that don't have both TRY and BIEN traits. Temporary solution will likely need tweaking
quer.org <- na.omit(quer.org)

specieslist <- quer.org$Species
#grabbing the TRY trait file for comparison
setwd(path.t)
try.df <- sheets_find("TRY_Quercus_Traits")
try.dat <- data.frame(sheets_read(try.df))
colnames(try.dat)

#removing oak species we aren't concerned with from TRY
try.mod <- try.dat[try.dat$species %in% specieslist, ]

#Grabbing the BIEN trait file for comparison
bien.dat <- read.csv("BIEN_fulllist.csv")
colnames(bien.dat)

#removing oak species we aren't concerned with from BIEN
bien.mod <- bien.dat[bien.dat$species %in% specieslist, ]
bien.mod <- subset(bien.mod, select=c(1:4))

#Converting to character then number YOU MUST CONVERT TO CHARCTER FIRST OR VALUES CHANGE
bien.mod$trait_value <- as.numeric(as.character(bien.mod$trait_value))

bien.agg <- aggregate(trait_value~species+trait_name, data=bien.mod, median)
View(bien.agg)
#currently this removes non numeric traits e.g. flower color, whole plant dispersal syndrome


#test case
bien.modt <- bien.dat[bien.dat$species == "Quercus acutissima", ]
bien.modt <- bien.modt[bien.modt$trait_name == "seed mass", ]
View(bien.modt)

bien.modt$trait_value <- as.numeric(as.character(bien.modt$trait_value))
bien.aggt <- aggregate(trait_value~trait_name+species, data=bien.modt, mean)
View(bien.aggt)

