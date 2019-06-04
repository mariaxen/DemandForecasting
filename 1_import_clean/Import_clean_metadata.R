#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')
if(!exists("foo", mode="function")) source("User_defined_functions.R")

#Set your working directory (wd)
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Data')#Import your data in a data frame
metadata  <- read.xlsx("household_metadata.xls", sheetIndex = 1)
occupancy <- read.xlsx("household_metadata.xls", sheetIndex = 3)
occupancy <- occupancy[!duplicated(occupancy), ]

#Import additional data for each file
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Data/Consumption')
#Get the file names in your wd
files  <-list.files()
fileNo <- length(files)
consdata <- data.frame("meterID" = numeric(fileNo), "postcode" = character(fileNo), "latitude" = numeric(fileNo), "longitude" = numeric(fileNo),"Status" = character(fileNo), "Units" = character(fileNo),stringsAsFactors=FALSE)

for (i in 1:length(files)) {
  for (k in 1:6) {
    consdata[i,k] <- t(read.table(files[i], header = FALSE, nrows = 6))[2,k]
  }
  consdata$Property[i] <- i
}
consdata <- plyr::rename(consdata, c("meterID"="ID"))

#Combine all your data
characteristics <- merge(metadata,occupancy)
final <- merge(characteristics,consdata, all = F, stringsAsFactors=FALSE)
final$Occupants <- as.numeric(as.character(final$Occupants))
final$Rateable.Value <- as.numeric(as.character(final$Rateable.Value))

final[final=="Not Available"] <- NA
final[final=="0"] <- NA

final$Gardens[final$Garden.Size=="No Garden"] <- FALSE
final$Gardens[final$Garden.Size=="Small" | final$Garden.Size=="Medium" | final$Garden.Size=="Large"] <- TRUE

final[c("ACORN", "Rateable", "occupancy")] <- NA

final[final$Acorn.Group %in% c("A","B","C","D","E"),]$ACORN <- "Affluent"
final[final$Acorn.Group %in% c("F","G","H","I","J"),]$ACORN <- "Comfortable"
final[final$Acorn.Group %in% c("K","L","M","N", "O", "P","U"),]$ACORN <- "Stretched"

final$Rateable.Value <- as.numeric(final$Rateable.Value)
final$Rateable[final$Rateable.Value>193] <- "high"
final$Rateable[final$Rateable.Value<193 & final$Rateable.Value>135] <- "medium"
final$Rateable[final$Rateable.Value<139] <- "low"

final$Council.tax.Band <- as.character(final$Council.tax.Band)
final[final$Council.tax.Band %in% c("A","B","C"),]$Council.tax.Band <- "A-C"
final[final$Council.tax.Band %in% c("D","E"),]$Council.tax.Band <- "D-E"
final[final$Council.tax.Band %in% c("F","G", "H"),]$Council.tax.Band <- "F-H"

final$Occupants <- as.numeric(final$Occupants)
final$occupancy[final$Occupants>=3] <- "3+"
final$occupancy[final$Occupants>=2 & final$Occupants<3] <- "2"
final$occupancy[final$Occupants<2] <- "1"

final$ID <- as.character(paste0("ID", final$ID))

#final$Occupants <- as.numeric(final$Occupants)
#final$Occupants[final$Occupants>=4.5] <- "5+"
#final$Occupants[final$Occupants>=3.5 & final$Occupants<4.5] <- "4"
#final$Occupants[final$Occupants>=2.5 & final$Occupants<3.5] <- "3"
#final$Occupants[final$Occupants>=1.5 & final$Occupants<2.5] <- "2"
#final$Occupants[final$Occupants>=0.5 & final$Occupants<1.5] <- "1"

#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')