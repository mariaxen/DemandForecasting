#Choose certain months and years to compare, 10-2016, 8-2017
test_data[,"Month"] <- Correl[,"Month"][match(test_data$Date, Correl$Date)]
test_data[,"Year"]  <- Correl[, "Year"][match(test_data$Date, Correl$Date)]

#test_data7 <- test_data[test_data$Month==8 & test_data$Year ==2017,]
test_data7 <- test_data[test_data$Month==6 & test_data$Year ==2016,]
test_data7 <- test_data[test_data$Month==10 & test_data$Year ==2016,]

#Choose colour for each model
cols <- c("Actual Consumption" = "black", "Temporal" ="red", "Temporal+Air Temperature+Sunshine" ="hotpink",
          "Temporal+Tax Band" ="green2","Temporal+Air Temperature+Sunshine+Tax Band" ="yellow")

#Plot model comparisons
p <-  
  ggplot(data = test_data7, aes(x = Date)) + 
  geom_line(aes(y = PredictionReal, color = "Actual Consumption"), linetype = "solid", size = 1) +
  geom_line(aes(y = Prediction_temp, color = "Temporal"), linetype = "twodash", size = 1) +
  geom_line(aes(y = Prediction_temp_T_Sun, color = "Temporal+Air Temperature+Sunshine"), linetype = "twodash", size = 1) +
  geom_line(aes(y = Prediction_temp_status_tax, color = "Temporal+Tax Band"), linetype = "twodash", size = 1) +
  geom_line(aes(y = Prediction_temp_T_Sun_status_tax, color = "Temporal+Air Temperature+Sunshine+Tax Band"), linetype = "twodash", size = 1) +
  xlab('Dates (2016)') +
  ylab('Per Capita Consumption')+  
  scale_colour_manual(name="Predictive Variables", values=cols)

#Save the plot
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("NoCons_June_All.png", p, width = 7, height = 4, dpi = 900)

#Plot consumption measured VS predicted for each season
p <-  
  ggplot(HH_val, aes(X8, Prediction,colour = Season, shape = Season))+ 
  geom_point(size = 1.5) +
  xlab('Per Capita Consumption (L/day) - Measured') +
  ylab('Per Capita Consumption (L/day) - Predicted')+
  coord_fixed(ratio=1, xlim = c(120,160), ylim = c(120,160))+
  scale_y_continuous(breaks=seq(110,160,20))+
  scale_x_continuous(breaks=seq(110,260,20))

#Save plot
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("seasons.png", p, width = 7, height = 3.7, dpi = 900)