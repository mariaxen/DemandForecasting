#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files')

#Run the necessary scripts 
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("functions.R")
if(!exists("foo", mode="function")) source("Stations.R")
if(!exists("foo", mode="function")) source("Properties.R")
if(!exists("foo", mode="function")) source("Metadata_Final.R")

#Increase memory limit
memory.limit(17500000000000)

#Create Segmentations of Consumption

#Choose a subset of the data based on the below
#(df.list, Weekends, Time of the day, Season, Gardens, Acorn, Occupants, rateable, Status, Consumers)
testall <- df(df.list, "all", "all", "all",  "all", "all", "all","all", "all", "all")
test2   <- df(df.list, "all", "all", "summer",  "all", "all", "medium","all", "Measured", "all")
test3   <- df(df.list, FALSE, "evening", "all",  "all", "all", "all","all", "all", "high")
test4   <- df(df.list, FALSE, "evening", "summer",  "all", "all", "all","all", "all", "all")
test5   <- df(df.list, FALSE, "all", "all",  "all", "Affluent", "all","all", "Unmeasured", "high")
test6   <- df(df.list, FALSE, "all", "summer",  "all", "all", "medium","all", "all", "all")

#Choose the dataset you want and name it testNN 
testNN <- testall

#Aggregate consumption based on date and house ID
testNN <- aggregate(formula(paste0("AbsConsumption", "~Date+ID")),testNN,FUN = (mean))

#Create table x, which shows the number of households for each day in the data
x <- table(testNN$Date)

#Keep only days that have above yy number of households
yy <- 60
testNN <- testNN[testNN$Date %in% names(x[x >= yy]), ]

#Remove NA values
testNN <- na.omit(testNN)
#Add postcode, week, month, and year
testNN <- cbind(testNN, Postcode = final$postcode[match(testNN$ID, paste('ID', final$ID, sep = ''))])
testNN <- cbind(testNN, Week  = lubridate::week(testNN$Date))
testNN <- cbind(testNN, Month = lubridate::month(testNN$Date))
testNN <- cbind(testNN, Year = lubridate::year(testNN$Date))

#Get all the postcodes of the houses in the dataset 
weightsT <- as.data.frame(table(final$postcode))

#Find which weather stations are the closest to which postcode
closest  <- data.frame(cbind(rownames(distances), colnames(distances)[apply(distances, 1, which.min)]))
#Create a data frame with the weather station that is the closest to each postcode and the number of properties that are in it
weightsT <- merge(weightsT, closest, by.x = "Var1", by.y = "X1")
#Name the columns
colnames(weightsT) <- c("Postcodes", "Properties", "Station")

#Combine the weather and consumption information based on the weights for each weather station
testNN <- Cor(testNN, "AbsConsumption", distances, Daily , weightsT, final, "perday")

#Choose the variables for x, colour, and size for your scatterplots
#The y axis is always per capita consumption (PCC)
xvariable <- colnames(testNN)[14]
sizevar   <- colnames(testNN)[5]
colourvar <- colnames(testNN)[3]

#Below are your ready-made x axis titles
#"Sunshine Duration (hours/day)", "Radiation (Mj/m2/day)", 
#"Rainfall (mm/day)", "Humidity (%)", 
#"Soil Temperature (°C)", "Air Temperature (°C)")

#Divide the radiation by 1,000 to change the units
testNN$Radiation <- testNN$Radiation/1000

#Create your scatterplot!
scatters <- 
  ggplot() + geom_point(data = testNN, aes_string(x = xvariable, y = "Cons", size = sizevar,
                                                  colour = colourvar)) +
  theme(axis.title.x = element_text(color="black", size=23),
        axis.title.y = element_text(color="black", size=23),
        axis.text = element_text(size=16),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16),
        plot.title   = element_text(color="black", size=15))+
  ylim(250, 650)+
  xlab("Air Temperature (°C)") +
  ylab("Consumption (L/property/day)") +
  labs(size="Rainfall (mm/day)", colour="Sunshine Duration\n(hours/day)") +
  guides(colour = guide_colourbar(order = 2),
         size = guide_legend(order = 1))+
  geom_smooth(data=testNN, aes_string(x = xvariable, y = "Cons"), fill="red",
              colour="red", method = "lm",formula = y~poly(x,2), se = FALSE)

#Save your scatterplot
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("AirT_All.png", scatters, width = 8.5, height = 6.5, dpi = 900)