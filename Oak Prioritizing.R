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
quercus.sp <- quercus.sp[!(quercus.sp$number < 10),]

quercus.t <- data.frame(ddply(quercus.s,~trait_name,summarise,number=length(unique(species))))
quercus.t <- quercus.t[!(quercus.t$number < 23),]

specieslist <- quercus.sp$species
traitlist <- quercus.t$trait_name

#removing oak species we aren't concerned with from BIEN
bien.mod <- bien.dat[(bien.dat$species %in% specieslist & bien.dat$trait_name %in% traitlist), ]
bien.mod <- subset(bien.mod, select=c(1:4))
bien.mod$trait_value <- as.character(bien.mod$trait_value)

#Creating a data.frame of the qualitative traits that aren't run in ordination
bien.qual <- bien.mod
bien.qual$trait_value <- gsub('[0-9]+', '', bien.qual$trait_value)
bien.qual$trait_value <- gsub('[[:punct:]]+', '', bien.qual$trait_value)
bien.qual <- bien.qual[!((bien.qual$trait_value=="")|(bien.qual$trait_value=="e")),]
bien.qualagg <- bien.qual %>% 
                  group_by(species, trait_name) %>% 
                  summarise(trait_value = paste(unique(trait_value), collapse = ', '))

bien.qualchart <- spread(bien.qualagg, trait_name, trait_value)

#Converting to character then number YOU MUST CONVERT TO CHARCTER FIRST OR VALUES CHANGE
bien.mod$trait_value <- as.numeric(bien.mod$trait_value)

#currently this removes non numeric traits e.g. flower color, whole plant dispersal syndrome
#This is useful for now for narrowing traits but remember their exclusion
bien.agg <- aggregate(trait_value~species+trait_name, data=bien.mod, median)

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


library(ggplot2)
ggplot(bien.xy, aes(MDS1, MDS2, color = bien.xy$species)) + geom_point() + theme_bw()

bien.mds$stress

View(bien.qualchart)


##This is where I'm playing around with ordiantion techniques and transformations
data(varechem)
data(varespec)

dist <- vegdist(bien.orddata, method = "bray")

#Stolen script for visualizing the stress based on different dimensions
NMDS.scree <- function(x) { 
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}
NMDS.scree(dist)


set.seed(2)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
NMDS2 <- metaMDS(bien.orddata, k = 2, trymax = 100, trace = F)
NMDS2

NMDS3 <- metaMDS(bien.orddata, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")


#grabbing the TRY trait file
setwd(path.t)
try.df <- sheets_find("TRY_Quercus_Traits")
try.dat <- data.frame(sheets_read(try.df))
colnames(try.dat)

#removing oak species we aren't concerned with from TRY
try.mod <- try.dat[try.dat$species %in% specieslist, ]


