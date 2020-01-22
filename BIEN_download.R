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

filename <- paste(genus, "_", species, "_traits.csv", sep="")
write.csv(qa, file.path(dr, file = filename), row.names=FALSE)

#removing duplicates (same species and same trait) this does merge measurements so only used to reduce space.
mod <- qa[!duplicated(qa$trait_name),]
View(mod)