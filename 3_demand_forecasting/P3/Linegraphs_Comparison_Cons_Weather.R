#Choose the number of days to predict into the future
Days_pred <- 1
predDay <- paste("X", Days_pred+7, sep ="")

#Aggregate spatially by postcode
HH <- prepare_HH_post(Correl, Days_pred, list(Correl$Date, Correl$postcode_areas), "postcode_areas")
HH <- as.data.frame(unclass(HH))
HH <- na.omit(HH)

#Divide calibration and validation datasets
k <- nrow(HH)
HH_cal <- HH[1:round(k*6/10),]
HH_val <- HH[round(k*6/10):k,]

#Choose your variables
temporal <- c("Type_of_Day", "Weekday", "Month")
variabless <- c(pastConsumption)
variabless <- c(pastConsumption)
variabless <- c(pastConsumption, temporal, "Sunshine")
variabless <- c(pastConsumption, temporal, "Sunshine")
variabless <- c(pastConsumption, temporal, "Air_T")
variabless <- c(pastConsumption, temporal, "Soil_T")

#Prepare the model input 
variables <- paste(variabless, collapse = "+")
rf.form   <- as.formula(paste(predDay, variables, sep="~"))

#Train the model
fit <-  randomForest(rf.form, 
                     data=HH_cal,
                     ntree = 1000,
                     nodesize =25,
                     mtry = length(variabless), 
                     localImp = TRUE)

#Make a prediction and calculate the error for the calibration dataset
HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[,predDay]
HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, predDay] *100

#Make a prediction and calculate the error for the validation dataset
HH_val[ , "Prediction"] <- predict(fit, HH_val)
HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, predDay]
HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, predDay] *100
HH_val[ , "error_naive"] <- mean(unlist(HH_val[ , day_pr])) - HH_val[, predDay]
HH_val[ , "error_perc_naive"] <- HH_val[ , "error_naive"]/HH_val[, predDay] *100

#Calculate the MAPE and NSE for the calibration and validation datasets
c(MAPE(HH_cal[ , "Prediction"], HH_cal[, day_pr]),
  NSE (HH_cal[ , "Prediction"], HH_cal[, day_pr]),
  MAPE(HH_val[ , "Prediction"], HH_val[, day_pr]),
  NSE (HH_val[ , "Prediction"], HH_val[, day_pr]))

#Save your dataframe to compare
HH_val_cons            <- HH_val
HH_val_cons_temp       <- HH_val
HH_val_cons_temp_sun   <- HH_val
HH_val_cons_temp_AirT  <- HH_val
HH_val_cons_temp_SoilT <- HH_val

#Create a new dataframe that contains the results of all data frames
test_data <- data.frame(
  Date = HH_val$Date,
  PredictionReal = HH_val_temp[,day_pr],
  Prediction_temp = HH_val_cons_temp$Prediction,
  Prediction_temp_Sun = HH_val_cons_temp_sun$Prediction,
  Prediction_temp_AirT = HH_val_cons_temp_AirT$Prediction,
  Prediction_temp_SoilT = HH_val_cons_temp_SoilT$Prediction
)

#Add columns for month and year
test_data[,"Month"] <- Correl[,"Month"][match(test_data$Date, Correl$Date)]
test_data[,"Year"]  <- Correl[, "Year"][match(test_data$Date, Correl$Date)]

#Create subset dataframes by Choosing the month and year
test_data6 <- melt(test_data[test_data$Month==6 & test_data$Year ==2016,-c((length(colnames(test_data))-1):length(colnames(test_data)))], "Date")
test_data10<- melt(test_data[test_data$Month==10 & test_data$Year ==2016,-c((length(colnames(test_data))-1):length(colnames(test_data)))], "Date")
test_data8 <- melt(test_data[test_data$Month==8 & test_data$Year ==2017,-c((length(colnames(test_data))-1):length(colnames(test_data)))], "Date")

#Choose the colours for the linegraph
cols <- c("black", "forestgreen", "firebrick", "gold")
#darkslateblue

#Choose the names for the linegraph
colsL <- c("Actual.Consumption" , "Past.Consumption Temporal" , "Past.Consumption Temporal Sunshine.Hours",
           "Past.Consumption Temporal Air.Temperature" )

z <- unique (factor (test_data6$variable))
test_data6 <- mutate(test_data6, Method = if_else(variable == "PredictionReal", "solid", "twodash")) 

#Create the plot
p <- 
  ggplot (test_data6, aes (x = Date, y= value, color = factor (variable), linetype = factor(Method)))+ 
  geom_line(size = 1) + 
  scale_color_manual(name="Predictive Variables", breaks = z, 
                     labels = c(str_wrap(colsL, 5)),
                     values = c(cols) )  +
  scale_linetype_manual(values=c("solid", rep("longdash", length(cols)-1)))+
  xlab('Dates (2016)') +
  ylab('Per Capita Consumption (L/property/day)') + 
  guides(linetype = FALSE)  +
  theme(legend.direction = 'vertical', 
        legend.position = 'right',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(2.5, 'lines'))

#Save the plot
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("NoCons_Oct_Weather.png", p, width = 6, height = 3.5, dpi = 900)