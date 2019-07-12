gardens <- c("Large", "Medium", "Small")
occupancy <- c("high_occ", "medium_occ", "low_occ")
tax <- c( "A", "D", "F_G_H", "C",  "B",  "E")
status <- c("Measured",  "Unmeasured")
ACORN <- c( "Affluent", "Financially_Stretched", "Comfortable")
rateable <- c("high", "medium", "low")

variabless <- c("Type_of_Day", "Weekday", "Month", "Season", occupancy, tax, gardens, ACORN, 
                rateable, status, "Sunshine", "Rainfall", "Air_T", "Soil_T", "Humidity", "Days_without_Rain", "Wind_Speed")
######
Days_pred <- 1
predDay <- paste("X", Days_pred+7, sep ="")

HH <- prepare_HH_post(Correl, Days_pred, list(Correl$Date), NULL)
HH <- as.data.frame(unclass(HH))
HH <- na.omit(HH)

k <- nrow(HH)
HH_cal <- HH[1:round(k*6/10),]
HH_val <- HH[round(k*6/10):k,]

temporal <- c("Type_of_Day", "Weekday", "Month", "Season")

variabless <- c(temporal)
variabless <- c(temporal, rateable)
variabless <- c(temporal, tax)
variabless <- c(temporal, gardens)
variabless <- c(temporal, occupancy)
variabless <- c(temporal, ACORN)
variabless <- c(temporal, status)

variables <- paste(variabless, collapse = "+")
rf.form   <- as.formula(paste(predDay, variables, sep="~"))
#####
fit <-  randomForest(rf.form, 
                     data=HH_cal,
                     ntree = 1000,
                     nodesize = 50,
                     mtry = length(variabless), 
                     localImp = TRUE)

#####Cal/Val data#####
#Choose cal/val dataset
#day_pr <- "ConsPC"

HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[,predDay]
HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, predDay] *100

HH_val[ , "Prediction"] <- predict(fit, HH_val)
HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, predDay]
HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, predDay] *100
HH_val[ , "error_naive"] <- mean(unlist(HH_val[ , day_pr])) - HH_val[, predDay]
HH_val[ , "error_perc_naive"] <- HH_val[ , "error_naive"]/HH_val[, predDay] *100

#Daily - anytime #########
HH_val_temp <- HH_val
HH_val_temp_rateable <- HH_val
HH_val_temp_tax <- HH_val
HH_val_temp_gardens <- HH_val
HH_val_temp_occupancy <- HH_val
HH_val_temp_ACORN <- HH_val
HH_val_temp_status <- HH_val

test_data <- data.frame(
  Date = HH_val$Date,
  PredictionReal = HH_val[,day_pr],
  Prediction_temp = HH_val_temp$Prediction,
  Prediction_temp_rateable = HH_val_temp_rateable$Prediction,
  Prediction_temp_tax = HH_val_temp_tax$Prediction,
  Prediction_temp_gardens = HH_val_temp_gardens$Prediction,
  Prediction_temp_occupancy = HH_val_temp_occupancy$Prediction,
  Prediction_temp_ACORN = HH_val_temp_ACORN$Prediction,
  Prediction_temp_status = HH_val_temp_status$Prediction
)

test_data[,"Month"] <- Correl[,"Month"][match(test_data$Date, Correl$Date)]
test_data[,"Year"]  <- Correl[, "Year"][match(test_data$Date, Correl$Date)]

test_data6 <- melt(test_data[test_data$Month==6 & test_data$Year ==2016,-c((length(colnames(test_data))-1):length(colnames(test_data)))], "Date")
test_data10<- melt(test_data[test_data$Month==10 & test_data$Year ==2016,-c((length(colnames(test_data))-1):length(colnames(test_data)))], "Date")

cols <- c("black", "forestgreen", "firebrick", "gold", "darkslateblue", "green2", "orange2")

colsL <- c("Actual.Consumption" , "Past.Consumption Temporal" , "Past.Consumption Temporal Rateable.Value",
           "Past.Consumption Temporal Tax.Band", "Past.Consumption Temporal Garden.Size",
           "Past.Consumption Temporal ACORN", "Past.Consumption Temporal Occupancy", "Past.Consumption Temporal Status")

z <- unique (factor (test_data6$variable))
test_data6  <- mutate(test_data6, Method = if_else(variable == "PredictionReal", "solid", "twodash")) 
test_data10 <- mutate(test_data10, Method = if_else(variable == "PredictionReal", "solid", "twodash")) 

#p <- 
ggplot (test_data6, aes (x = Date, y= value, color = factor (variable)))+ 
  geom_line(size = 1) + 
  scale_color_manual(name="Predictive Variables", breaks = z, 
                     labels = c(str_wrap(colsL, 5)),
                     values = c(cols) )  +
  xlab('Dates (2016)') +
  ylab('Per Capita Consumption (L/property/day)') + 
  guides(linetype = FALSE)  +
  theme(legend.direction = 'vertical', 
        legend.position = 'right',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(2.5, 'lines'))

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("NoCons_Oct_P.png", p, width = 6, height = 3.5, dpi = 900)