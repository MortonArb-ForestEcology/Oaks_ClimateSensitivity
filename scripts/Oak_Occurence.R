#This script is to download Daymet data for any single points of interest.
library(dplyr)

#setting file path
path.points <- "G:/My Drive/Oaks_ClimateSensitivity/Occurrence/"

setwd(path.points)
species <- "Q_arkansana"
ystart <- 1980
yend <- 2018

occurencefile <- paste(species, "occurence_points.csv", sep="")

#loading species list of occurence points
q.occur <- read.csv(paste(species, "_spatial_data.csv", sep=""))

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- q.occur[,(c=8:9)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]

#Writing the csv file of lat and longs because batch function needs to read a file instea of a dataframe
write.csv(q.lat, file.path("C:/Users/lucie/Documents", file = occurencefile), row.names=FALSE)


#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = occurencefile,
                      start = ystart,
                      end = yend,
                      internal = T)

lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]


df.output <- data.frame()

#loop to create dataframe containing values of interest for every year at every occurence points
i <- 1
YR <- ystart
for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  
  #Loop that goes through every year for each point
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    df.yr <- df.tmp[df.tmp$year==YR,]
    w.p <- 0
    wo.p <- 0
    rows <- 1
    
    #loop that determines the length of dry and wet periods as well as yearlong temp stats
    for(t in rows:nrow(df.yr)){
      if(df.yr[t, "prcp..mm.day."] != 0){
        w.p <- w.p + 1
        wo.p <- 0
      }else {w.p <- 0
      wo.p <- wo.p+1}
      df.yr[t, "days.precip"] <- w.p
      df.yr[t, "days.wo.precip"] <- wo.p
    }
    df.dry <-  df.yr %>% mutate(max.wo.precip = max(days.wo.precip), 
                                days.precip = max(days.precip),
                                max.precip =max(prcp..mm.day.),
                                max.temp = max(tmax..deg.c.),
                                min.temp = min(tmin..deg.c.))
    
    #loop seperating out the dry period so summary statistics can be done on that section
    drows <- 1
    for(d in drows:nrow(df.dry)){
      if(df.dry[d, "days.wo.precip"] == df.dry[d, "max.wo.precip"]){
        v <- as.numeric(df.dry[d, "max.wo.precip"])
        df.dry[(((d-v)+1):d), "dry"] <- 1
      }
      drows <- drows+1
    }
    
    df.dry <- df.dry[!(is.na(df.dry$dry)==T),]
    
    df.fin <- df.dry %>%  summarise(year = max(year),
                                    dry.max = max(tmax..deg.c.),
                                    dry.min = min(tmin..deg.c.),
                                    dry.mean.max = mean(tmax..deg.c.),
                                    dry.mean.min = mean(tmin..deg.c.),
                                    days.wo.precip = max(max.wo.precip), 
                                    days.precip = max(days.precip),
                                    max.precip = max(max.precip),
                                    max.temp = max(max.temp) ,
                                    min.temp = min(min.temp))
    
    df.frz <- df.yr[(df.yr$tmin..deg.c. <=0),]
    
    #Final loop to determine the freeze values. 
    frows <- 1
    for(f in frows:nrow(df.frz)){
      if(df.frz[f, "yday"] < 171){
        df.frz[f, "freeze"] <- "last.freeze"
        df.frz[f+1, "freeze"] <- "first.freeze"
        df.frz[f-1, "freeze"] <- 0
      }else{df.frz[f, "freeze"] <- "first.freeze"
            df.frz[f+1, "freeze"] <- 0
        
      }
      frows <- frows+1
    }
    freeze.df <- df.frz[!(df.frz$freeze == 0 | is.na(df.frz$freeze) == T),]
    
    #seperating the last freeze and first freeze of each year
    freeze.df <- tidyr::spread(freeze.df, freeze, yday)
    freeze.df <- freeze.df %>%  summarise(year = max(year, na.rm = T),
                                          first.freeze = max(first.freeze, na.rm = T),
                                          last.freeze = max(last.freeze, na.rm = T))
    freeze.df[freeze.df == "-Inf"] <- NA
    
    #merging them together for the final data frame  
    
    daymet.done <- merge(df.fin, freeze.df, by=c("year"))
    daymet.done$latitude <- lat.list[[i]]$latitude
    daymet.done$longitude <- lat.list[[i]]$longitude
    daymet.done$altitude <- lat.list[[i]]$altitude 
    df.output <- rbind(df.output, daymet.done)
    
  }
}

path.out <- "D:/lfitzpatrick/Oak_Daymet"
filename <- paste(species, "_daymet_data.csv", sep="")

write.csv(df.output, file.path(path.out, file = filename), row.names = F)

#Could defintiely clean up parts of this for efficiency, CUrrently runs at about 1.2 points per second
#Could clean by not using rbind to build the final dataframe. I can't figure out how but IM sure CHristy knows
#COuld also clean by not using a loop for the dry period loop or the freeze which sounds possible





daymet.df <- daymetr::download_daymet(lat = 30.26521083, lon = -85.62025028, start = ystart, end = yend, internal = TRUE, simplify = TRUE)

daymet.df <- tidyr::spread(daymet.df, measurement, value)

df.output <- data.frame(latitude=numeric(),
                        longitude=numeric(),
                        altitude=numeric(),
                        year=numeric(),
                        dry.max=numeric(),
                        dry.min=numeric(),
                        dry.mean.max=numeric(),
                        dry.mean.min=numeric(),
                        days.wo.precip=numeric(),
                        days.precip=numeric(),
                        max.precip=numeric(),
                        max.temp=numeric(),
                        min.temp=numeric(),
                        first.freeze=numeric(),
                        last.freeze=numeric())


