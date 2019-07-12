#Load workspace data
load ("//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/Data/Clean_Data.RData")
load ("//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/Data/Weather.RData")

#Run your function through another script#####
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")
if(!exists("foo", mode="function")) source("Import_clean_metadata.R")

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')
if(!exists("foo", mode="function")) source("Weights_Stations.R")
if(!exists("foo", mode="function")) source("Properties_High_Cons.R")

#############
#Increase memory limit
memory.limit(1750000000)

#Only for summer months
#Correl_Summer <- Correl[Correl$Season == "Summer", ]

#Configure your model
dots <- c("Sunshine", "Humidity", "Soil_T", "Air_T" , "Wind_speed", 
          "Rainfall", "Days_without_Rain")   

group_var <- c("Date")

social <- c("Metering_Status", "Council.tax.Band", "Garden.Size",  
            "ACORN", "occupancy", "Rateable_Value")

temporal <- c("Type_of_Day", "Season", "Month", "Weekday")

pastConsumption <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7")

#Create groups
dataS <- prepare_df(group_var, social, dots, 1, Correl, Weather, removeOutliers = TRUE)

#Choose what are the aggregation criteria and the number of days in the future you want to predict
splitCol <- social
futureD <- 1
HH <- prepare_HH(dataS, futureD, splitCol, temporal)
predDay <- paste0("X",7+futureD)

nrow(HH)
range(dataS$length)
mean(dataS$length)
HH$Type_of_Day <- as.factor(HH$Type_of_Day)

dataS[, temporal] <- Correl[, temporal][match(dataS$Date, Correl$Date),]
dataS$Months <- factor(dataS$Month, levels=c(1:12))
dataS$Weekdays <- factor(dataS$Weekday, levels=c('Monday', 'Tuesday', 'Wednesday',
                                                 'Thursday', 'Friday', 'Saturday','Sunday'))
dataS$Seasons <- factor(dataS$Season, levels=c('Autumn', 'Winter', 'Spring',
                                               'Summer'))