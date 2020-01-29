#Script to take the large files from BIEN and convert them to workable csvs
library(BIEN)
library(tidyverse)
library(dplyr)

dr <- "G:/My Drive/Oaks_ClimateSensitivity/Traits/BIEN_traits"

genus <- "Quercus"
species <- "velutina"

#download step. This will take a few minutes if they file is large
traits <- paste(genus, species, sep=" ")
qa <- BIEN_trait_species(traits)
View(qa)

#download step to get all the range maps (using specieslist from prioritizing script since this should only run 1 time)
ranges <- as.character(specieslist)
qr <- BIEN_ranges_species(ranges)
View(qr)


filename <- paste(genus, "_", species, "_traits.csv", sep="")
write.csv(qr, file.path(dr, file = filename), row.names=FALSE)

#removing duplicates (same species and same trait) this does merge measurements so only used to reduce space.
mod <- qa[!duplicated(qa$trait_name),]
View(mod)