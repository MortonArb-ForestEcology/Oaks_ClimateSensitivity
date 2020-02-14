#This script is to download Daymet data for any single points of interest.

#setting file path

path.save <- "C:/Users/lucie/Documents/"

#retrieving the occurence points from BIEN
qo <- BIEN::BIEN_occurrence_species("Quercus acutissima", only.new.world = FALSE) #, native.status = TRUE, natives.only = FALSE)

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
q.lat <- qo[(c=1:5),(c=2:3)]

#creating a proxy "site" column because the batch function needs it
q.lat$site <- "Daymet"
q.lat <- q.lat[,c(3,1,2)]

#Writing the csv file of lat and longs because batch function needs to read a file instea of a dataframe
write.csv(q.lat, file.path(path.save, file = "test_lat.csv"), row.names=FALSE)

#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.df <- daymetr::download_daymet_batch(file_location = "test_lat.csv",
                      start = 2000,
                      end = 2010,
                      internal = TRUE)


#df <- download_daymet(lat = 37.08000, lon = -81.92000, start = 2000, end = 2010, internal = TRUE, simplify = TRUE)





