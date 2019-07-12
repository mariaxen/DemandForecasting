#Load clean data or run Import_Consumption and Clean_Consumption
load("~/Desktop/R_files/Data/Clean_Data.RData")

#Run your function through another script
setwd('~/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')
if(!exists("foo", mode="function")) source("Consumption_Weather.R")

#For aggregating all households, postcode <- NULL, or postcode_areas, postcode
postcode <- "postcode_areas"
minHHs <- 20

#Choose level of spatial aggregation and minimum amount of households in each group
CV <- list(Correl$Date, Correl[,postcode])

#List your variables
gardens <- c("Large", "Medium", "Small")
occupancy <- c("high_occ", "medium_occ", "low_occ")
tax <- c( "A", "D", "F_G_H", "C",  "B",  "E")
status <- c("Measured",  "Unmeasured")
ACORN <- c( "Affluent", "Financially_Stretched", "Comfortable")
rateable <- c("high", "medium", "low")
Occupants <- c("occ_1", "occ_2", "occ_3", "occ_4", "occ_5", "occ_6", "occ_7", "occ_8", "occ_9")

temporal <- c("Type_of_Day", "Weekday", "Month", "Season")
pastConsumption <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7")
weather <- c("Sunshine", "Humidity", "Soil_T", "Air_T", 
             "Rainfall", "Days_without_Rain")   
social <- c(status, tax, gardens, ACORN, occupancy, rateable)

#Create a list with one dataframe for each day
CC <- split(Correl, CV)
#Remove consumption NAs
CC <- lapply(CC, function (x) x <- x[!is.na(x$ConsPC), ])
#Remove groups with less than minHHs number of houses
CC <- CC[(lapply(CC, nrow)>minHHs)]

#Create an empty list
DDD <- list()
#Choose your variables
variabls <- c("Month", "Weekday", "Type_of_Day", "Date", postcode)

#Turn each dataframe in the list into one row that shows what is the percentage of occurence 
#of each one of the household variables for each group of houses
for (kk in 1:length(CC)){
  
  DD <- data.frame(Date=as.Date(1:1, origin=Sys.Date()),
                   Month=integer(1),
                   Weekday=character(1),
                   Type_of_Day=logical(1),
                   stringsAsFactors=FALSE)
  
  for (i in 1:length(variabls)){
    
    DD[1, variabls[i]] <- CC[[kk]][1, variabls[i]]  
    DD[1, "ConsPC"] <- mean(CC[[kk]][, "ConsPC"])  
  }
  
  GS  <- round(prop.table(table((CC[[kk]])$Garden.Size))*100, 1)
  CT  <- round(prop.table(table((CC[[kk]])$Council.tax.Band))*100, 1)
  MS  <- round(prop.table(table((CC[[kk]])$Metering_Status))*100, 1)
  AC  <- round(prop.table(table((CC[[kk]])$ACORN))*100, 1)
  RV  <- round(prop.table(table((CC[[kk]])$Rateable_Value))*100, 1)
  OC  <- round(prop.table(table((CC[[kk]])$occupancy))*100, 1)
  OCC <- round(prop.table(table((CC[[kk]])$Occupants))*100, 1)
  
  addCol <- c(GS, CT, MS, AC, RV, OC, OCC)
  
  DD <- cbind(DD, setNames(lapply(addCol, function(x) x=x), names(addCol)) )
  
  DDD[[kk]] <- DD
}

#Turn the list into a dataframe
FF <- rbindlist(DDD, fill = TRUE)

FFF <- list()

#Divide based on postcodes depending on the level of spatial aggregation you want
if (!is.null(postcode)){
  FFF <- split(FF, FF[,..postcode])
}else{FFF[[1]] = FF}

#Get mean number of households in each group
meanHHs <- mean(unlist(lapply(CC, NROW)))