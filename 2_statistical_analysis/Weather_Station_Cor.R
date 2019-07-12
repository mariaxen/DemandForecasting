#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_clean_Weather.R")

#Temperature correlations
Weather <- Daily
#Choose the weather variables you want to correlate
variable <- c("Air_Temperature", "Amount_10CM")
Weather <- completeFun(Daily, variable)

#Get all the station names in the weather dataset
station_names <- unique(Weather$Station_Name)
#Create an empty data frame to store the correlations
Stations_Correlations <- data.frame(matrix(ncol = length(station_names), nrow = length(station_names)), stringsAsFactors = FALSE)
colnames(Stations_Correlations) <- station_names
rownames(Stations_Correlations) <- station_names

Weather <- cbind(Weather[1:2], do.call(data.frame , lapply(Weather[3:ncol(Weather)], 
                                                           function(x) replace(x, is.infinite(x), NA))))

for (i in 1:length(station_names)){
  for (k in 1:length(station_names)){
    M <- merge(Weather[Weather$Station_Name==station_names[i],],Weather[Weather$Station_Name==station_names[k],],by="Date")    

    #Temperature correlations
    W.lmT <- lm(Sunshine_Dur.x~Sunshine_Dur.y, data=M)
    Stations_Correlations[i,k] <- summary(W.lmT)$adj.r.squared
              }
}

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/PhD')
write.xlsx(Stations_Correlations, "Postcodes.xlsx", sheetName="postcodes")

#Rainfall correlations
Stations_Correlations <- data.frame(matrix(ncol = length(station_names), nrow = length(station_names)), stringsAsFactors = FALSE)
colnames(Stations_Correlations) <- station_names
rownames(Stations_Correlations) <- station_names

for (i in 1:length(station_names)){
  for (k in 1:length(station_names)){
    M <- merge(Weather[Weather$Station_Name==station_names[i],],Weather[Weather$Station_Name==station_names[k],],by="Date")    

    W.lmP <- lm(Precipitation_mm.x~Precipitation_mm.y,data=M)
    Stations_Correlations[i,k] <- summary(W.lmP)$r.squared
  }
}

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/PhD')
write.xlsx(Stations_Correlations, "Postcodes.xlsx", sheetName="postcodes")

#HOURLY
#Humidity correlations
Weather <- Hourly_data
station_names <- station_names <- c("AMESBURY", "BOSCOMBE DOWN" , "BOURNEMOUTH","BRISTOL", "DUNKESWELL AERODROME",  
                                    "FILTON",  "ISLE OF PORTLAND", "LARKHILL", "LYNEHAM")


Stations_Correlations <- data.frame(matrix(ncol = length(station_names), nrow = length(station_names)), stringsAsFactors = FALSE)
colnames(Stations_Correlations) <- station_names
rownames(Stations_Correlations) <- station_names

for (i in 1:length(station_names)){
  for (k in 1:length(station_names)){
    M <- merge(Weather[Weather$Station_Name==station_names[i],],Weather[Weather$Station_Name==station_names[k],],by="DateTime")    
    
    if (nrow(M)>1){
      #Humidity correlations
      W.lmT <- lm(RelativeHumidity.x~RelativeHumidity.y,data=M)
      Stations_Correlations[i,k] <- summary(W.lmT)$r.squared
      
      #Humidity correlations
      W.lmT <- lm(RelativeHumidity.y~RelativeHumidity.x,data=M)
      Stations_Correlations[k,i] <- summary(W.lmT)$r.squared
    }else{
      NA
    }
  }
}

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/PhD')
write.xlsx(Stations_Correlations, "Postcodes.xlsx", sheetName="postcodes")

#Temperature correlations
Stations_Correlations <- data.frame(matrix(ncol = length(station_names), nrow = length(station_names)), stringsAsFactors = FALSE)
colnames(Stations_Correlations) <- station_names
rownames(Stations_Correlations) <- station_names

for (i in 1:length(station_names)){
  for (k in 1:length(station_names)){
    M <- merge(Weather[Weather$Station_Name==station_names[i],],Weather[Weather$Station_Name==station_names[k],],by="DateTime")    
    
    if (nrow(M)>1){
      #Humidity correlations
      W.lmT <- lm(Temp.x~Temp.y,data=M)
      Stations_Correlations[i,k] <- summary(W.lmT)$r.squared
      
      #Humidity correlations
      W.lmT <- lm(Temp.y~Temp.x,data=M)
      Stations_Correlations[k,i] <- summary(W.lmT)$r.squared
    }else{
      NA
    }
  }
}

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/PhD')
write.xlsx(Stations_Correlations, "Postcodes.xlsx", sheetName="postcodes")