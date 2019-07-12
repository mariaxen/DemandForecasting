##This script is used to clean and prepare the consumption dataset for analysis

#Set the working directory
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')

#Run other scripts you need
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")
if(!exists("foo", mode="function")) source("Import_clean_metadata.R")

#Remove rows with initial identical recordings
df.list <- lapply(df.list, removedupl, "V3")

#Change 15-30 min recordings to hourly recordings
rounded_hours <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "1 hour"), "%H:%M:00")
df.list <- lapply(df.list, function(x) x[x$V2 %in% rounded_hours,])

#Find absolute consumption
df.list <- lapply(df.list, absolute, "V3")
#Exclude readings with consumption higher than a threshold 
df.list <- lapply(df.list, replcap, 450)
#Create columns for week, month, year, and season
df.list <- lapply(df.list, function(x) cbind(x, Week = lubridate::week(x$V1)))
df.list <- lapply(df.list, function(x) cbind(x, data.frame(do.call('rbind',strsplit(as.character(x$V1),'-',fixed=TRUE)))))
df.list <- lapply(df.list, function(x) cbind(x, Season = getSeason(as.numeric(x$X2))))
df.list <- lapply(df.list, function(x) cbind(x, Y_M = paste(x$X1,x$X2)))
#Name your columns
df.list <- lapply(df.list, function(x) if (nrow(x)>0) {setNames(x, c('Date','Time','Consumption','AbsConsumption',
                                                                       'Week','Year','Month','Day','Season',"Y_M"))}
                  else{setNames(x, c('Date','Time','Consumption','AbsConsumption','Week'))})

#Remove rows with more than 24 repetitions, i.e. consumption that remain unchanged for 24 hours 
df.list <- lapply(df.list, removerep, 24)
#Exclude leaking days (where less than 10% of the recordings were 0) & days with less than 12 hours of data
good_days <- non.leak.days(df.list, 0.10)
for (i in 1:length(df.list)){
  df.list[[i]] <- df.list[[i]][df.list[[i]]$Date %in% good_days[[i]],] 
  }
#Exclude leaking months (where less than 10% of the recordings were 0) & months with less than 1 day of data
good_months <- non.leak.months(df.list, 0.20)
for (i in 1:length(df.list)){
 df.list[[i]] <- df.list[[i]][df.list[[i]]$Y_M %in% good_months[[i]],] 
}
#Identify weekends and holidays
df.list <- lapply(df.list, function(x) cbind(x, Weekend = chron::is.weekend(as.Date(x$Date))))
df.list <- lapply(df.list, function(x) cbind(x, Holiday = is.holiday(as.Date(x$Date),  
                  chron(as.character(holidayLONDON(year = 2008:2017)),format = "y-m-d"))))
#Add property ID
for (i in 1:length(names(df.list))){
  if (nrow(df.list[[i]])>0){
    df.list[[i]]$ID <- names(df.list)[[i]]
  }
}
#Add the postcode
df.list <- lapply(df.list, function(x) cbind(x, Postcode = final[final$ID==as.numeric(substring(x$ID[[1]],3)),]$postcode))
#Add weighted consumption, based on the number of occupants in each household
for (i in 1:nrow(final)){
  if(nrow(df.list[[i]])>0){
    df.list[[i]]$WeightedCons <- df.list[[i]]$AbsConsumption/ as.numeric(as.character(final$Occupants[i]))
  }
}