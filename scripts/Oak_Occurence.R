#This script is to download Daymet data for any single points of interest.
library(dplyr)

#setting file path
path.save <- "G:/My Drive/Oaks_ClimateSensitivity/Occurrence/"

#retrieving the occurence points from BIEN
#qo <- BIEN::BIEN_occurrence_species("Quercus acutissima", only.new.world = FALSE) #, native.status = TRUE, natives.only = FALSE)


setwd(path.save)
species <- "Q_georgiana"
ystart <- 1980
yend <- 2018


#loading species list of occurence points
q.occur <- read.csv(paste(species, "_spatial_data.csv", sep=""))

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- q.occur[(c=1),(c=8:9)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]

#Writing the csv file of lat and longs because batch function needs to read a file instea of a dataframe
#write.csv(q.lat, file.path(path.save, file = "test_lat.csv"), row.names=FALSE)

#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
#lat.df <- daymetr::download_daymet_batch(file_location = "test_lat.csv",
                      #start = ystart,
                      #end = yend,
                      #internal = TRUE)

daymet.df <- daymetr::download_daymet(lat = 34.8101, lon = -86.9814, start = ystart, end = yend, internal = TRUE, simplify = TRUE)

#calculating annual max temperature for each point
daymet.tmax <- daymet.df %>% group_by(year, measurement) %>% summarise(max = max(value))
daymet.tmax <- daymet.tmax[(daymet.tmax$measurement == "tmax..deg.c."),]

#calculating annual min temeperature for each point
daymet.tmin <- daymet.df %>% group_by(year, measurement) %>% summarise(min = min(value))
daymet.tmin <- daymet.tmin[(daymet.tmin$measurement == "tmin..deg.c."),]

#calculating day of first hard freeze (might work these into one loop but unsure how right now)
daymet.frz <- daymet.df[(daymet.df$measurement == "tmin..deg.c." & daymet.df$value <=0),]

year.frz <- ystart
rows <- 1
for(i in rows:nrow(daymet.frz)){
  if(daymet.frz[i, "year"] == year.frz){
    if(daymet.frz[i, "yday"] > 171){
      daymet.frz[i, "first.freeze"] <- TRUE
      year.frz <- year.frz + 1
    } 
  }
  rows <- rows+1
}
ffreeze.df <- daymet.frz[!(is.na(daymet.frz$last.freeze)),]

#calculating day of last hard freeze (make it add to next freeze and this can be one loop)
daymet.lfrz <- daymet.df[(daymet.df$measurement == "tmin..deg.c." & daymet.df$value <=0),]

year.lfrz <- ystart
l.rows <- 1
for(i in l.rows:nrow(daymet.lfrz)){
  if(daymet.lfrz[i, "year"] == year.lfrz){
    if(daymet.lfrz[i, "yday"] < 171){
        daymet.lfrz[i, "last.freeze"] <- 1
        daymet.lfrz[i-1, "last.freeze"] <- 0
    } else{year.lfrz <- year.lfrz+1}
  }
  l.rows <- l.rows+1
}

lfreeze.df <- daymet.lfrz[(daymet.lfrz$last.freeze == 1),]