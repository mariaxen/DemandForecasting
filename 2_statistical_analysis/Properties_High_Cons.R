#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("User_defined_functions.R")

#Load workspace data
load ("//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/Data/Clean_Data.RData")

#Aggregate consumption per household, year, and month
df.list.C <- lapply(df.list, function(x) if (nrow(x)>0) {merge(aggregateSum(x, "AbsConsumption", "Y_M", FUN = sum),
                                                                 repDF(x, "Y_M"), by.x=1, by.y=1)})
#Name the columns
df.list.C <- lapply(df.list.C, function(x) if (!is.null(x)) {setnames(x, c('Y_M','AbsConsumption', "Freq"))})

#Get the ID for each house and the number of recordings for each month
for (i in 1:length(names(df.list.C))){
  if (length(df.list.C[[i]])>0){
    df.list.C[[i]]$ID <- names(df.list)[[i]]
    df.list.C[[i]]$Freq <- as.numeric(df.list.C[[i]]$Freq)
  }
}

#Create a column for average daily consumption in the corresponding month and difference between
#each month and consumption in the month with the min consumption for each property (seasonal consumption)
df.list.C <- lapply(df.list.C, function(x) cbind(x, "Cons" = x$AbsConsumption/x$Freq*24, 
                                          "Seasonal" =  x$AbsConsumption/x$Freq*24 - min(x$AbsConsumption/x$Freq*24)))

#Identify the range of daily consumption among all months
df.list.range     <- lapply(df.list.C, rangeL    , "Cons")
#The mean consumption among all months
df.list.mean      <- lapply(df.list.C, meanL     , "Cons")
#The mean difference in consumption between each month and the min consumption month (mean seasonal consumption)
df.list.summerC   <- lapply(df.list.C, summeravg , "Seasonal")

#Create a dataframe with the mean consumption 
summerC <- as.data.frame(do.call("rbind", df.list.summerC))
summerC <- setDT(summerC, keep.rownames = TRUE)[]
colnames(summerC) <- c("ID","Con_Summer")
summerC$ID <- as.numeric (gsub("ID", "", summerC$ID))

High_cons <- summerC[summerC$Con_Summer > 120,]$ID
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')