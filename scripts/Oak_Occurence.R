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
q.lat <- q.occur[(c=1:3),(c=8:9)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]

#Writing the csv file of lat and longs because batch function needs to read a file instea of a dataframe
write.csv(q.lat, file.path("C:/Users/lucie/Documents/", file = "test_lat.csv"), row.names=FALSE)

#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = "test_lat.csv",
                      start = ystart,
                      end = yend,
                      internal = T)

lat.df <- do.call(rbind.data.frame, lapply(lat.list, as.data.frame, stringsAsFactors = F))



daymet.df <- daymetr::download_daymet(lat = 34.8101, lon = -86.9814, start = ystart, end = yend, internal = TRUE, simplify = TRUE)

daymet.df <- tidyr::spread(daymet.df, measurement, value)

#Max consecutive days with and withou precipitation
year.precip <- ystart
prows <- 1
w.p <- 0
wo.p <- 0
for(i in prows:nrow(daymet.df)){
    if(daymet.df[i, "prcp..mm.day."] != 0){
      w.p <- w.p + 1
      wo.p <- 0
    }else {w.p <- 0
            wo.p <- wo.p+1}
  daymet.df[i, "days.precip"] <- w.p
  daymet.df[i, "days.wo.precip"] <- wo.p
}

daymet.mod <-  daymet.df %>% group_by(year) %>% mutate(max.wo.precip = max(days.wo.precip), 
                                                       days.precip = max(days.precip),
                                                       max.precip =max(prcp..mm.day.),
                                                       max.temp = max(tmax..deg.c.),
                                                       min.temp = min(tmin..deg.c.))

#Subsetting the dry periods
year.dry <- ystart
drows <- 1
set <- ""
for(t in drows:nrow(daymet.mod)){
  if(daymet.mod[t, "year"] == year.dry){
    if(daymet.mod[t, "days.wo.precip"] == daymet.mod[t, "max.wo.precip"]){
      v <- as.numeric(daymet.mod[t, "max.wo.precip"])
      daymet.mod[(((t-v)+1):t), "dry"] <- 1
    }
  }else{year.dry <- year.dry +1}
   drows <- drows+1
}

daymet.dry <- daymet.mod[!(is.na(daymet.mod$dry)==T),]

daymet.fin <- daymet.dry %>% group_by(year) %>% summarise(dry.max = max(tmax..deg.c.),
                                                       dry.min = min(tmin..deg.c.),
                                                       days.wo.precip = max(max.wo.precip), 
                                                       days.precip = max(days.precip),
                                                       max.precip = max(max.precip),
                                                       max.temp = max(max.temp) ,
                                                       min.temp = min(min.temp))



#calculating last freeze and first freeze of every year
daymet.frz <- daymet.df[(daymet.df$tmin..deg.c. <=0),]

year.frz <- ystart
rows <- 1
for(i in rows:nrow(daymet.frz)){
  if(daymet.frz[i, "year"] == year.frz){
    if(daymet.frz[i, "yday"] < 171){
      daymet.frz[i, "freeze"] <- "last.freeze"
      daymet.frz[i+1, "freeze"] <- "first.freeze"
      daymet.frz[i-1, "freeze"] <- 0
    } else{year.frz <- year.frz+1}
  }
  rows <- rows+1
}
freeze.df <- daymet.frz[!(daymet.frz$freeze == 0 | is.na(daymet.frz$freeze) == T),]
#seperating the last freeze and first freeze of each year
freeze.df <- tidyr::spread(freeze.df, freeze, yday)
freeze.df <- freeze.df %>% group_by(year) %>% summarise(first.freeze = max(first.freeze, na.rm = T),
                                                        last.freeze = max(last.freeze, na.rm = T))


#merging them together for the final data frame  
daymet.done <- merge(daymet.fin, freeze.df, by =c("year"))

