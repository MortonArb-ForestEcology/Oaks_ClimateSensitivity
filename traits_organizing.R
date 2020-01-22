#Organizing the trait data to make decisions

library(tidyverse)
library(readbulk)
library(plyr)
library(ggplot2)
library(googlesheets4)

dir <- "G:/My Drive/Oaks_ClimateSensitivity/Traits/"

setwd(dir)

##Section for working with BIEN##
quercus <-read_bulk(directory = "BIEN_traits", extension = ".csv", header = FALSE, skip=1,) # Combine all data
colnames(quercus) <-  c("species", "trait_name","trait_value", "unit", "method", "latitude", "longitude", "elevation_m", 
                        "url_source", "project_pi","project_pi_contect", "access", "id", "unknown", "file")

quercus.mod <- subset(quercus, select= c("species", "trait_name", "file"))

##Section for working with TRY##
quercus.goog <- sheets_find("TRY_Quercus_Traits")
quercus.try <- data.frame(sheets_read(quercus.goog))

quercus.mod <- subset(quercus.try, select= c("species", "trait_name", "traitID"))


#Start here after beginning with either BIEN or TRY#

quercus.t <- quercus.mod %>% distinct()

View(quercus.t)

quercus.t$species <- as.character(quercus.t$species)
quercus.t$trait_name <-  as.character(quercus.t$trait_name)

#View frequency of traits relvative to species and vice versa
ddply(quercus.t,~species,summarise,number=length(unique(trait_name)))
quercus.trait <- data.frame(ddply(quercus.t,~trait_name,summarise,number=length(unique(species))))

quercus.trait <- quercus.trait %>% arrange(desc(number))

#This can be used to create a chart showing which traits have data for which  species#
library(data.table) 
quercus.freq = data.table(quercus.t)

quercus.freq[, freq := .N, by = trait_name]

quercus.freq <- quercus.freq %>% arrange(desc(freq))

ggplot(quercus.freq, aes(x=species, y=trait_name,))+
  geom_tile(color='white')+
  theme(axis.text.x = element_text(size=8, angle=60, vjust=0.6))+
  ggtitle("What trait data do we have for species")
  

write.csv(quercus.trait, file.path(dir, file = "BIEN_traitlist.csv"), row.names=FALSE)

  
  
  