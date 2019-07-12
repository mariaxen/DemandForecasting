#Rename columns to prepare for nice graphs
pastConsumption <- c("d.1", "d.2","d.3", "d.4", "d.5","d.6", "d.7")
colnames(HH)[1:7] <- pastConsumption

#Clean and prepare your dataframe
HH <- HH[order(HH$Date),]
HH <- as.data.frame(unclass(HH))
HH <- na.omit(HH)

#Divide calibration and validation datasets
k <- nrow(HH)
HH_cal <- HH[1:round(k*7/10),]
HH_val <- HH[round(k*7/10):k,]

#Choose your variables
variables <- c(pastConsumption, temporal, "Sunshine", "Humidity", "Soil_T", "Air_T", 
               "Rainfall", "Days_without_Rain", social)

#Prepare your model inputs in the correct format
variables <- paste(variables, collapse = "+")
rf.form   <- as.formula(paste(predDay, variables, sep="~"))

#Train your random forest model
fit <-  randomForest(rf.form, 
                     data=HH_cal,
                     ntree = 300,
                     nodesize = 300,
                     mtry = 5,
                     importance = TRUE,
                     localImp = TRUE
)

#Train your linear model
fit_lm <- lm(rf.form, data=HH_cal)

#Make a prediction
HH_cal[ , "Prediction"]       <- predict(fit, HH_cal)
HH_cal[ , "error"]            <- HH_cal[ , "Prediction"] - HH_cal[,predDay]
HH_cal[ , "error_perc"]       <- HH_cal[ , "error"]/HH_cal[, predDay] *100

HH_val[ , "Prediction"]       <- predict(fit, HH_val)
HH_val[ , "error"]            <- HH_val[ , "Prediction"] - HH_val[, predDay]
HH_val[ , "square_error"]     <- HH_val[ , "error"]^2
HH_val[ , "error_perc"]       <- HH_val[ , "error"]/HH_val[, predDay] *100
HH_val[ , "error_naive"]      <- mean(unlist(HH_val[ , predDay])) - HH_val[, predDay]
HH_val[ , "error_perc_naive"] <- HH_val[ , "error_naive"]/HH_val[, predDay] *100

#Get some metrics
c(
  MAPE    (HH_cal[ , "Prediction"], HH_cal[, predDay]),
  MSE     (HH_cal[ , "Prediction"], HH_cal[, predDay]),
  rsquare (HH_cal[ , "Prediction"], HH_cal[, predDay]),
  RMSE    (HH_cal[ , "Prediction"], HH_cal[, predDay]),
  MAPE    (HH_val[ , "Prediction"], HH_val[, predDay]),
  MSE     (HH_val[ , "Prediction"], HH_val[, predDay]),
  rsquare (HH_val[ , "Prediction"], HH_val[, predDay]),
  RMSE    (HH_val[ , "Prediction"], HH_val[, predDay]))

#Save different models
HH_val_temp <- HH_val
HH_val_temp_T_Sun <- HH_val
HH_val_temp_T_Sun_status_tax <- HH_val
HH_val_temp_status_tax <- HH_val

#Combine different models
test_data <- data.frame(
  Date = HH_val$Date,
  PredictionReal = HH_val_temp[,day_pr],
  Prediction_temp = HH_val_temp$Prediction,
  Prediction_temp_T_Sun = HH_val_temp_T_Sun$Prediction,
  Prediction_temp_T_Sun_status_tax = HH_val_temp_T_Sun_status_tax$Prediction,
  Prediction_temp_status_tax = HH_val_temp_status_tax$Prediction
)