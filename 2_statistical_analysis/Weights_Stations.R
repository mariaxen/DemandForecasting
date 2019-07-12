#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")
if(!exists("foo", mode="function")) source("Import_clean_metadata.R")

#Set your working directory (wd)
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Data')
#Import your weather stations
Stations <- list()
for (i in 1:6){
  Stations[[i]] <- read.xlsx('Stations.xlsx', sheetIndex = i, stringsAsFactors=FALSE)
}
Stations <- do.call("rbind", Stations)
Stations$Station.name <- str_replace_all(Stations$Station.name, "[^a-zA-Z0-9]", " ")

#Distances of weather stations - postcodes
distance.list <- list()
W_posts <- unique(final$postcode)

distances <- data.frame(matrix(ncol = nrow(Stations), nrow = length(W_posts)), stringsAsFactors = FALSE)
colnames(distances) <- Stations$Station.name
rownames(distances) <- W_posts

for (k in 1:length(Stations$Station.name)){
  for (i in 1:length(W_posts)){
    distances[i,k] <- specify_decimal(dist_station_post(W_posts[i], as.character(Stations$Station.name[k]))/1000,2) 
  }
}

#Keep only the stations that are the closest to a postcode
closest_stations <- colnames(distances)[apply(distances,1,which.min)]
Stations <- Stations[Stations$Station.name %in% closest_stations,]

#Find closest stations to each property
closest <- as.data.frame(apply(distances,1,which.min))
colnames(closest) <- "Station_Name"

for (k in 1:nrow(distances)){
  closest$SecStation[k] <- which.min(distances[,-closest$Station[k]][k,])
} 

closest$Station_Name  <- colnames(distances)[closest$Station]
closest$SecStation <- colnames(distances)[closest$SecStation]
closest$postcodes <- rownames(closest)

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')