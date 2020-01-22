#Script to take the large files from BIEN and convert them to workable csvs
library(BIEN)
library(tidyverse)
library(dplyr)

dr <- "G:/My Drive/Oaks_ClimateSensitivity/Traits/BIEN_traits"

genus <- "Quercus"
species <- "stellata"

#download step. This will take a few minutes if they file is large
traits <- paste(genus, species, sep=" ")
qa <- BIEN_trait_species(traits)
View(qa)

#removing duplicates (same species and same trait) this does merge measurements.
mod <- qa[!duplicated(qa$trait_name),]
View(mod)

filename <- paste(genus, "_", species, "_traits.csv", sep="")
write.csv(mod, file.path(dr, file = filename), row.names=FALSE)
