##This script imports and prepares the weather dataset for the analysis

#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")

#Increase memory limit
memory.limit(17500000000000)

#Set your working directory (wd)
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Data')
#Import your weather stations
Stations <- list()
for (i in 1:6){
  Stations[[i]] <- read.xlsx('Stations.xlsx', sheetIndex = i, stringsAsFactors=FALSE)
}
Stations <- do.call("rbind", Stations)
Stations$Station.name <- str_replace_all(Stations$Station.name, "[^a-zA-Z0-9]", " ")

#Set your working directory
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Data/MIDAS')

#Import your weather data
folders <- list.files()

for (i in 1:length(folders)){
  wd <- paste('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Data/MIDAS/',folders[i], sep = "")
  setwd(wd)
  files <- list.files()
  Weather_Data <- list()
  headers <- unlist(strsplit(readChar("Column_Headers.txt", file.info("Column_Headers.txt")$size), split=","))
  headers <- gsub("[[:space:]]", "", headers)
  max_col <- length(headers)
  
  for (k in 1:(length(files)-1)){
  Weather_Data[[k]] <- read.table(files[k], sep=",", fill = TRUE, row.names=NULL, header = FALSE)
  Weather_Data[[k]] <- Weather_Data[[k]][,1:max_col]
  }
  Weather_Data <- do.call("rbind", Weather_Data)
  colnames(Weather_Data) <- headers
  Weather_Data <- Weather_Data[Weather_Data[,"SRC_ID"] %in% Stations[,"src_id"] , ]
  Weather_Data$Station_Name <- Stations[match(Weather_Data$SRC_ID, Stations$src_id),2]
  Weather_Data <- Weather_Data[colSums(!is.na(Weather_Data)) > 0]
  assign(folders[i], Weather_Data)
}

#Daily_Rainfall_Data (mm)
Daily_Rainfall_Data <- as.data.frame(Daily_Rainfall_Data[,c(3,4,7,8,10,16)])     
Daily_Rainfall_Data <- Daily_Rainfall_Data[Daily_Rainfall_Data$OB_DAY_CNT == 1, ] 
colnames(Daily_Rainfall_Data) <- c("Date", "Q_Checked", "Days", "Station_ID", "Rainfall", "Station_Name")
Daily_Rainfall_Data$Date <- unlist(lapply(strsplit(as.character(Daily_Rainfall_Data$Date), " "), '[[', 2))
Daily_Rainfall_Data <- Daily_Rainfall_Data[Daily_Rainfall_Data$Q_Checked == 1,]
Daily_Rainfall_Data <- Daily_Rainfall_Data[,c(1,5,6)]

#Daily_Weather_Observation_Data (sunshine duration)
Daily_Weather_Observation_Data <- as.data.frame(Daily_Weather_Observation_Data[,c(1,4,5,7,9:17,29,31)])     
Spot_Weather_Observation_Data  <- Daily_Weather_Observation_Data[Daily_Weather_Observation_Data$OB_HOUR_COUNT == 0, ] 
Spot_Weather_Observation_Data <- Spot_Weather_Observation_Data[colSums(!is.na(Spot_Weather_Observation_Data)) > 0]
Daily_Weather_Observation_Data <- Daily_Weather_Observation_Data[Daily_Weather_Observation_Data$OB_HOUR_COUNT == 24, ]
Daily_Weather_Observation_Data <- Daily_Weather_Observation_Data[colSums(!is.na(Daily_Weather_Observation_Data)) > 0]
colnames(Daily_Weather_Observation_Data) <- c("Date", "Hours", "Q_Checked", "Station_ID", "CS_Sunshine_Dur", "Snow_ID","Hail_ID", 
                                              "Thunder_ID", "Gale_ID", "WMO_Sunshine_Dur", "Derived_D_Sun_dur","Station_Name")
Daily_Weather_Observation_Data$Date <- unlist(lapply(strsplit(as.character(Daily_Weather_Observation_Data$Date), " "), '[[', 1))
Daily_Weather_Observation_Data$Sunshine_Dur <- ifelse(!is.na(Daily_Weather_Observation_Data$CS_Sunshine_Dur),
                                                        Daily_Weather_Observation_Data$CS_Sunshine_Dur, 
                                                        Daily_Weather_Observation_Data$WMO_Sunshine_Dur)
Daily_Weather_Observation_Data <- Daily_Weather_Observation_Data[Daily_Weather_Observation_Data$Q_Checked == 1,]
Daily_Weather_Observation_Data <- Daily_Weather_Observation_Data[,c(1,5:13)]
Daily_Weather_Observation_Data <- unique(Daily_Weather_Observation_Data)

#Hourly_Soil_Temperature_Data
Hourly_Soil_Temperature_Data <- as.data.frame(Soil_Temperature_Data[,c(3,5,6,8:10,16)])     
colnames(Hourly_Soil_Temperature_Data) <- c("Date", "Q_Checked", "Station_ID", "Amount_10CM", "Amount_30CM", "Amount_100CM", "Station_Name")
Hourly_Soil_Temperature_Data$Time <- unlist(lapply(strsplit(as.character(Hourly_Soil_Temperature_Data$Date), " "), '[[', 3))
Hourly_Soil_Temperature_Data$Date <- unlist(lapply(strsplit(as.character(Hourly_Soil_Temperature_Data$Date), " "), '[[', 2))
Hourly_Soil_Temperature_Data <- Hourly_Soil_Temperature_Data[Hourly_Soil_Temperature_Data$Q_Checked == 1,]
Hourly_Soil_Temperature_Data <- Hourly_Soil_Temperature_Data[,c(1,4:8)]

#Hourly_Mean_Wind_Data (wind direction & speed)
Hourly_Mean_Wind_Data <- as.data.frame(Mean_Wind_Data[,c(1,4,7,9:13,21)])     
Hourly_Mean_Wind_Data <- Hourly_Mean_Wind_Data[Hourly_Mean_Wind_Data$OB_HOUR_COUNT == 1, ] 
colnames(Hourly_Mean_Wind_Data) <- c("Date", "Hours", "Station_ID", "Wind_dir", "Wind_speed", "Gust_dir", "Gust_speed", "Max_Gust_time", 
                              "Station_Name")
Hourly_Mean_Wind_Data$Time <- unlist(lapply(strsplit(as.character(Hourly_Mean_Wind_Data$Date), " "), '[[', 2))
Hourly_Mean_Wind_Data$Date <- unlist(lapply(strsplit(as.character(Hourly_Mean_Wind_Data$Date), " "), '[[', 1))
Hourly_Mean_Wind_Data <- Hourly_Mean_Wind_Data [, c(1,10,4,5,9)]
Hourly_Mean_Wind_Data <- Hourly_Mean_Wind_Data[!duplicated(Hourly_Mean_Wind_Data), ]

#Global_Radiation_Observations (Hourly and Daily amounts of radiation)
Global_Radiation_Observations <- as.data.frame(Global_Radiation_Observations[,c(3,4,5,7,9,15)])     
colnames(Global_Radiation_Observations) <- c("Date", "Hours", "Q_Checked", "Station_ID", "Radiation", "Station_Name")
Hourly_Radiation_Observations <- Global_Radiation_Observations[Global_Radiation_Observations$Hours == 1, ] 
Hourly_Radiation_Observations$Time <- unlist(lapply(strsplit(as.character(Hourly_Radiation_Observations$Date), " "), '[[', 3))
Hourly_Radiation_Observations$Date <- unlist(lapply(strsplit(as.character(Hourly_Radiation_Observations$Date), " "), '[[', 2))
Hourly_Radiation_Observations <- Hourly_Radiation_Observations[!duplicated(Hourly_Radiation_Observations), ]
Hourly_Radiation_Observations <- Hourly_Radiation_Observations[Hourly_Radiation_Observations$Q_Checked==1,]
Hourly_Radiation_Observations <- Hourly_Radiation_Observations[,c(1,5:7)]

#Daily_Radiation_Observations
Daily_Radiation_Observations  <- Global_Radiation_Observations[Global_Radiation_Observations$Hours == 24, ] 
Daily_Radiation_Observations$Date <- unlist(lapply(strsplit(as.character(Daily_Radiation_Observations$Date), " "), '[[', 2))
Daily_Radiation_Observations <- Daily_Radiation_Observations[Daily_Radiation_Observations$Q_Checked==1,]
Daily_Radiation_Observations <- Daily_Radiation_Observations[,c(1,5:6)]

#Hourly_Weather_Observation_Data(wind speed/direction,visibility,pressure,air temp,humidity,sunshine dur,snow depth,derived sun dur)
Hourly_Weather_Observation_Data <- as.data.frame(Hourly_Weather_Observation_Data[,c(1,5,6,8:11, 21,22, 36, 38, 44,86,88,90,92)])     
colnames(Hourly_Weather_Observation_Data) <- c("Date", "Q_Checked", "Station_ID", "Wind_Speed_Unit", "Observation_Type", "Wind_Direction", 
                                               "Wind_Speed", "Visibility", "Pressure", "Air_Temperature", "Wet_bulb_Temperature",
                                               "Sun_duration_wmo", "Humidity", "Snow_depth", "Derived_H_Sun_dur", "Station_Name")
Hourly_Weather_Observation_Data$Time <- unlist(lapply(strsplit(as.character(Hourly_Weather_Observation_Data$Date), " "), '[[', 2))
Hourly_Weather_Observation_Data$Date <- unlist(lapply(strsplit(as.character(Hourly_Weather_Observation_Data$Date), " "), '[[', 1))

#Daily_Temperature_Data (temperature of the past 12 and the past 24 hours, min, max)
Daily_Temperature_Data <- as.data.frame(Daily_Temperature_Data[,c(1,4,5,7,9:12,19)])     
colnames(Daily_Temperature_Data) <- c("Date", "Hours", "Q_Checked", "Station_ID", "Max_Air", "Min_Air", "Min_Grass", "Min_Conc", 
                                      "Station_Name")
Past12_Temperature_Data <- Daily_Temperature_Data[Daily_Temperature_Data$Hours == 12, ]
Past12_Temperature_Data$Time <- unlist(lapply(strsplit(as.character(Past12_Temperature_Data$Date), " "), '[[', 2))
Past12_Temperature_Data$Date <- unlist(lapply(strsplit(as.character(Past12_Temperature_Data$Date), " "), '[[', 1))
Past12_Temperature_Data_09 <- Past12_Temperature_Data[Past12_Temperature_Data$Time=="09:00",]
Past12_Temperature_Data_09 <- Past12_Temperature_Data_09[,c(1,5,6,9)]
colnames(Past12_Temperature_Data_09) <- c("Date", "Max_Air_21_09", "Min_Air_21_09", "Station_Name")
Past12_Temperature_Data_21 <- Past12_Temperature_Data[Past12_Temperature_Data$Time=="21:00",]
Past12_Temperature_Data_21 <- Past12_Temperature_Data_21[,c(1,5,6,9)]
colnames(Past12_Temperature_Data_21) <- c("Date", "Max_Air_09_21", "Min_Air_09_21", "Station_Name")

Past24_Temperature_Data <- Daily_Temperature_Data[Daily_Temperature_Data$Hours == 24, ]
Past24_Temperature_Data$Time <- unlist(lapply(strsplit(as.character(Past24_Temperature_Data$Date), " "), '[[', 2))
Past24_Temperature_Data$Date <- unlist(lapply(strsplit(as.character(Past24_Temperature_Data$Date), " "), '[[', 1))
Past24_Temperature_Data <- Past24_Temperature_Data[,c(1,5,6,9)]
colnames(Past24_Temperature_Data) <- c("Date", "Max_Air_09_09", "Min_Air_09_09", "Station_Name")
Daily_Temperature_Data  <-  merge(merge(Past12_Temperature_Data_21, Past12_Temperature_Data_09), Past24_Temperature_Data, all = TRUE)

#Create Hourly
Hourly <- merge(merge(merge(Hourly_Mean_Wind_Data, Hourly_Radiation_Observations, all = TRUE),
                 Hourly_Weather_Observation_Data, all = TRUE),Hourly_Soil_Temperature_Data, all=TRUE)

Hourly <- Hourly[, sapply(Hourly, function(col) length(unique(col))) > 5]
Hourly$Week  <- lubridate::week(ymd(Hourly$Date))
Hourly$Month <- lubridate::month(ymd(Hourly$Date))
Hourly$Year  <- lubridate::year(ymd(Hourly$Date))

#Create daily
Daily <- merge(merge(merge(Daily_Rainfall_Data, Daily_Weather_Observation_Data, all = TRUE), 
                     Daily_Radiation_Observations, all = TRUE), Daily_Temperature_Data, all = TRUE)

Daily$Week  <- lubridate::week(ymd(Daily$Date))
Daily$Month <- lubridate::month(ymd(Daily$Date))
Daily$Year  <- lubridate::year(ymd(Daily$Date))

#Add hourly aggregated columns
Daily <- merge(aggregate(Wind_speed ~ Date+Station_Name, Hourly, mean, na.rm=TRUE, na.action=NULL), Daily, all =TRUE)
#Add hourly aggregated columns
Daily <- merge(aggregate(Air_Temperature ~ Date+Station_Name, Hourly, max, na.rm=TRUE, na.action=NULL), Daily, all =TRUE)
Daily <- merge(aggregate(Humidity     ~ Date+Station_Name, Hourly, mean, na.rm=TRUE, na.action=NULL), Daily, all =TRUE)
Daily <- merge(aggregate(Amount_10CM  ~ Date+Station_Name, Hourly, mean, na.rm=TRUE, na.action=NULL), Daily, all =TRUE)
Daily <- merge(aggregate(Amount_30CM  ~ Date+Station_Name, Hourly, mean, na.rm=TRUE, na.action=NULL), Daily, all =TRUE)
Daily <- merge(aggregate(Amount_100CM ~ Date+Station_Name, Hourly, mean, na.rm=TRUE, na.action=NULL), Daily, all =TRUE)