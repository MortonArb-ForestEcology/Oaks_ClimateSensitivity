#Script for Prioritizing oaks
library(googlesheets4)
library(plyr)
library(dplyr)


path.q <- "G:/My Drive/Oaks_ClimateSensitivity/"

setwd(path.q)

#grabbing the file from google drive and making it a workable data frame
quer.df <- sheets_find("Quercus Collection Metadata")
quer.dat <- data.frame(sheets_read(quer.df, range='QuercusCollection'))

colnames(quer.dat)

colnames(quer.dat) <- c("Species", "Common Name", "Subgenus", "Section", "Numtrees", "Wild Origin", "Garden Origin",
                        "Geographic Distribution", "Phenology", "Dendrometer Bands", "ITRDB", "TRY Traits", 
                        "BIEN Traits", "Notes")
#removing oaks we are not doing phenology monitoring for#
quer.org <- quer.dat[!(quer.dat$Phenology =="N"),]


