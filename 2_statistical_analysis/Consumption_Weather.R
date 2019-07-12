#Load clean data and weather data
load("~/Desktop/R_files/Data/Clean_Data.RData")
load("~/Desktop/R_files/Data/Weather.RData")

#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")

#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')
if(!exists("foo", mode="function")) source("Weights_Stations.R")

#Weekends, Time_d, Season, Gardens, Acorn_groups, Occupants, rateable, Status, Consumers
db <- df(df.list, "all", "all", "all", "all", "all", "all", "all", "all", "all")
db$Time <- as.character(db$Time)

#Weighted averages for the weather stations
weightsT <- as.data.frame(table(final$postcode))
closest <- data.frame(cbind(rownames(distances), colnames(distances)[apply(distances, 1, which.min)]))
weightsT <- merge(weightsT, closest, by.x = "Var1", by.y = "X1")
colnames(weightsT) <- c("Postcodes", "Properties", "Station")

#Choose weights for the weather stations
postcodes <- unique(db$Postcode)
weightsT <- weightsT[weightsT$Postcodes %in% postcodes,]
weightsT <- aggregate(Properties~Station, weightsT, sum)
weightsT$Station <- as.character(weightsT$Station) 
Weather  <- Daily[Daily$Station_Name %in% weightsT$Station, ]

#Replace all inf with NA
Weather <- cbind(Weather[1:2], do.call(data.frame , lapply(Weather[3:ncol(Weather)], 
                                                           function(x) replace(x, is.infinite(x), NA))))

#Give weighst to weather df
if (nrow(Weather)>0){
  Weather$Weights <- NA
  for (i in 1:nrow(weightsT)){
    if (nrow(Weather[Weather$Station_Name == weightsT$Station[i], ])>0){
      Weather[Weather$Station_Name == weightsT$Station[i],]$Weights <- weightsT$Properties[i]   
    }
  }
}

#Aggregate weather by day based on weights
Weather   <- 
  ddply(Weather, .(Date), plyr::summarize,  # so by asset class invoke following function
        Sunshine_Dur = weighted.mean(Sunshine_Dur, Weights, na.rm = T),
        Radiation = weighted.mean(Radiation, Weights, na.rm = T),
        Rainfall = weighted.mean(Rainfall, Weights, na.rm = T),
        CS_Sunshine_Dur = weighted.mean(CS_Sunshine_Dur, Weights, na.rm = T),
        Snow_ID = weighted.mean(Snow_ID, Weights, na.rm = T),
        Hail_ID = weighted.mean(Hail_ID, Weights, na.rm = T),
        Thunder_ID = weighted.mean(Thunder_ID, Weights, na.rm = T),
        Gale_ID = weighted.mean(Gale_ID, Weights, na.rm = T),
        WMO_Sunshine_Dur = weighted.mean(WMO_Sunshine_Dur, Weights, na.rm = T),
        Derived_D_Sun_dur = weighted.mean(Derived_D_Sun_dur, Weights, na.rm = T),
        Wind_speed = weighted.mean(Wind_speed, Weights, na.rm = T),
        Air_Temperature = weighted.mean(Air_Temperature, Weights, na.rm = T),
        Humidity = weighted.mean(Humidity, Weights, na.rm = T),
        Amount_10CM = weighted.mean(Amount_10CM, Weights, na.rm = T),
        Amount_30CM = weighted.mean(Amount_30CM, Weights, na.rm = T),
        Amount_100CM = weighted.mean(Amount_100CM, Weights, na.rm = T),
        Max_Air_09_21 = weighted.mean(Max_Air_09_21, Weights, na.rm = T),
        Min_Air_09_21 = weighted.mean(Min_Air_09_21, Weights, na.rm = T),
        Max_Air_21_09 = weighted.mean(Max_Air_21_09, Weights, na.rm = T),
        Min_Air_21_09 = weighted.mean(Min_Air_21_09, Weights, na.rm = T),
        Max_Air_09_09 = weighted.mean(Max_Air_09_09, Weights, na.rm = T),
        Min_Air_09_09 = weighted.mean(Min_Air_09_09, Weights, na.rm = T)
  )

#Create column for aggregated rainfall
Weather <- transform(Weather, Rain = ifelse(Rainfall <= 0.2, 1, 0))
Weather$Rain_Agg <- with(Weather, ave(Rain, cumsum(Rain == 0), FUN = cumsum))

#Combine consumption and weather
Correl <- Cor_Individual(db, "AbsConsumption", Weather, "perday")

#Exclude days with consumption less than 50litres
Correl <- Correl[Correl$Cons>50, ]
Correl$Date <- as.Date(Correl$Date)

#Add socio-economic variables
Correl <- merge(Correl, final, by = "ID", all = TRUE)

#Add weekday
Correl$Weekday <- weekdays(as.Date(Correl$Date))

#Identify weekends and holidays
Correl$Weekend <- chron::is.weekend(as.Date(Correl$Date))
Correl$Holiday <- is.holiday(as.Date(Correl$Date), chron(as.character(holidayLONDON(year = 2008:2017)),
                                                         format = "y-m-d"))

Correl <- transform(Correl, Working_D = ifelse(Weekend==TRUE, FALSE, ifelse(Holiday == TRUE, FALSE, TRUE)))  

Correl$ConsPC <- Correl$Cons/Correl$Occupants 
Correl <- Correl[!is.na(Correl$Date), ]
Correl$Season <- getSeason(as.numeric(Correl$Month))

#Add Weather variables
Weather$Date   <- as.Date(Weather$Date)
Weather$Week   <- lubridate::week(Weather$Date)
Weather$Month  <- data.frame(do.call('rbind',strsplit(as.character(Weather$Date),'-',fixed=TRUE)))$X2
Weather$Year   <- data.frame(do.call('rbind',strsplit(as.character(Weather$Date),'-',fixed=TRUE)))$X1
Weather$Season <- getSeason(as.numeric(Weather$Month))

colnames(Correl)[c(42, 47, 52)] <-   c("Metering_Status", "Rateable_Value", "Type_of_Day")
colnames(Correl)[c(4,17, 20, 27)] <- c("Sunshine", "Soil_T", "Air_T", "Days_without_Rain")
colnames(Weather)[c(2, 4, 12, 14, 15, 18, 25)] <- 
  c("Sunshine", "Rainfall", "Wind_speed", "Humidity", "Soil_T", "Air_T", "Days_without_Rain")

Correl$postcode_areas <- substr(Correl$postcode, start = 1, stop = 2)

Correl <- Correl[ Correl$Garden.Size != "No Garden", , drop=FALSE] 
Correl$Garden.Size <- factor(Correl$Garden.Size)

Correl[Correl$occupancy == "medium" & !is.na(Correl$occupancy), "occupancy"] <- "medium_occ"
Correl[Correl$occupancy == "high" & !is.na(Correl$occupancy), "occupancy"] <- "high_occ"
Correl[Correl$occupancy == "low" & !is.na(Correl$occupancy), "occupancy"] <- "low_occ"

Correl[Correl$ACORN == "Financially Stretched" & !is.na(Correl$ACORN), "ACORN"] <- "Financially_Stretched"

Correl[,"ID"] <- as.factor (Correl[,"ID"])
Correl$Occupants <- round(Correl$Occupants)

Correl[Correl$Occupants == 1 & !is.na(Correl$Occupants), "Occupants"] <- "occ_1"
Correl[Correl$Occupants == 2 & !is.na(Correl$Occupants), "Occupants"] <- "occ_2"
Correl[Correl$Occupants == 3 & !is.na(Correl$Occupants), "Occupants"] <- "occ_3"
Correl[Correl$Occupants == 4 & !is.na(Correl$Occupants), "Occupants"] <- "occ_4"
Correl[Correl$Occupants == 5 & !is.na(Correl$Occupants), "Occupants"] <- "occ_5"
Correl[Correl$Occupants == 6 & !is.na(Correl$Occupants), "Occupants"] <- "occ_6"
Correl[Correl$Occupants == 7 & !is.na(Correl$Occupants), "Occupants"] <- "occ_7"
Correl[Correl$Occupants == 8 & !is.na(Correl$Occupants), "Occupants"] <- "occ_8"
Correl[Correl$Occupants == 9 & !is.na(Correl$Occupants), "Occupants"] <- "occ_9"