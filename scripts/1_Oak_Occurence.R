#This script is to download Daymet data for any single points of interest.
library(dplyr)

#setting file path
path.points <- "G:/My Drive/Oaks_ClimateSensitivity/Occurrence/"
setwd(path.points)
species <- "Q_imbricaria"
ystart <- 1980

#make sure the yend of the data matches what you enter. Sometimes daymet truncates and this varibale will become wrong later in the script
yend <- 2018

occurencefile <- paste(species, "_occurence_points.csv", sep="")

#loading species list of occurence points
q.occur <- read.csv(paste(species, "_spatial_data.csv", sep=""))

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- q.occur[,c("decimalLatitude","decimalLongitude")]
q.lat <- na.omit(q.lat)

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]

#Writing the csv file of lat and longs because batch function needs to read a file instead of a dataframe
write.csv(q.lat, file.path(path.points, file = occurencefile), row.names=FALSE)


#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = occurencefile,
                      start = ystart,
                      end = yend,
                      internal = T)

#removing failed downloads 
lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]


#loop to create dataframe containing values of interest for every year at every occurence points

#creating a progress bar for the loop
pb <- txtProgressBar(min=0, max=length(lat.list)*((yend-ystart)+1), style=3)
pb.ind=0

#initializing the output dataframe
i <- 1
df.output <- data.frame(latitude=rep(lat.list[[i]]$latitude, ((yend-ystart)+1)) ,
                        longitude=rep(lat.list[[i]]$longitude, ((yend-ystart)+1)),
                        altitude=rep(lat.list[[i]]$altitude, ((yend-ystart)+1)))
count <- 1
YR <- ystart
for(i in seq_along(lat.list)){
  df.tmp <- lat.list[[i]]$data
  
  #Loop that goes through every year for each point
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
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
    df.sum <-  df.yr %>% mutate(max.wo.precip = max(days.wo.precip), 
                                days.precip = max(days.precip),
                                max.precip =max(prcp..mm.day.),
                                max.temp = max(tmax..deg.c.),
                                min.temp = min(tmin..deg.c.))
    
    #pulling out the values of the first and last freeze
    l.freeze <- df.yr[df.yr$yday==max(df.yr[df.yr$yday<180 & !is.na(df.yr$tmin..deg.c.) 
                                               & df.yr$tmin..deg.c. < 0, c("yday")]),]$yday
    f.freeze <- df.yr[df.yr$yday==min(df.yr[df.yr$yday>180 & !is.na(df.yr$tmin..deg.c.) 
                                                & df.yr$tmin..deg.c. < 0, c("yday")]),]$yday
    df.sum$last.freeze <- ifelse(length(l.freeze) == 0, NA, l.freeze)
    df.sum$first.freeze <- ifelse(length(f.freeze) == 0, NA, f.freeze)
    
    #loop seperating out the dry period so summary statistics can be done on that section
    drows <- 1
    for(d in drows:nrow(df.sum)){
      if(df.sum[d, "days.wo.precip"] == df.sum[d, "max.wo.precip"]){
        v <- as.numeric(df.sum[d, "max.wo.precip"])
        df.sum[(((d-v)+1):d), "dry"] <- 1
        break
      }
      drows <- drows+1
    }
    
    df.sum <- df.sum[!(is.na(df.sum$dry)==T),]
    
    #conversion to dataframe that output will be based on
    df.dry <- df.sum %>%  summarise(dry.max = max(tmax..deg.c.),
                                    dry.min = min(tmin..deg.c.),
                                    dry.mean.max = mean(tmax..deg.c.),
                                    dry.mean.min = mean(tmin..deg.c.),
                                    days.wo.precip = max(days.wo.precip), 
                                    days.precip = max(days.precip),
                                    max.precip =max(max.precip),
                                    max.temp = max(tmax..deg.c.),
                                    min.temp = min(tmin..deg.c.),
                                    last.freeze = max(last.freeze),
                                    first.freeze = max(first.freeze),
                                    year = max(year))
                                    
    
    #merging them together for the final data frame
    df.output[count, "latitude"] <- lat.list[[i]]$latitude
    df.output[count, "longitude"] <- lat.list[[i]]$longitude
    df.output[count, "altitude"] <- lat.list[[i]]$altitude
    df.output[count, "year"] <- df.dry$year
    df.output[count, "days.wo.precip"] <- df.dry$days.wo.precip
    df.output[count, "days.precip"] <- df.dry$days.precip
    df.output[count, "max.precip"] <- df.dry$max.precip
    df.output[count, "max.temp"] <- df.dry$max.temp
    df.output[count, "min.temp"] <- df.dry$min.temp
    df.output[count, "first.freeze"] <- df.dry$first.freeze
    df.output[count, "last.freeze"] <- df.dry$last.freeze
    df.output[count, "dry.max"] <- df.dry$dry.max
    df.output[count, "dry.min"] <- df.dry$dry.min
    df.output[count, "dry.mean.max"] <- df.dry$dry.mean.max
    df.output[count, "dry.mean.min"] <- df.dry$dry.mean.min
    
    count <- count + 1
  }
}

path.out <- "D:/lfitzpatrick/Oak_Daymet"
filename <- paste(species, "_daymet_data.csv", sep="")

write.csv(df.output, file.path(path.out, file = filename), row.names = F)



#Keeping these incase of testing single points
daymet.df <- daymetr::download_daymet(lat = 33.59, lon = -92.88, start = ystart, end = yend, internal = TRUE, simplify = TRUE)

daymet.df <- tidyr::spread(daymet.df, measurement, value)



