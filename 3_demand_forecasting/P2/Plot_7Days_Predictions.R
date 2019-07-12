weather <- c("Sunshine", "Humidity", "Soil_T", "Air_T" , "Rainfall", "Days_without_Rain")   

social <- c("Metering_Status", "Council.tax.Band", "Garden.Size",  
            "ACORN", "occupancy", "Rateable_Value")

temporal <- c("Type_of_Day", "Season", "Month", "Weekday")

######################
#Model 1
variables1 <- c(pastConsumption, temporal, social, weather)
#Model 2
variables2 <- c(temporal, social, weather)
#Model 3 
variables3 <- c(pastConsumption, temporal)
#Model 4
variables4 <- c(temporal)
#Model 5 
variables5 <- c(pastConsumption)
#Model 6 
variables6 <- c(temporal, social)
#Model 7 
variables7 <- c(pastConsumption, "Type_of_Day")
#Model 8 
variables8 <- c(social, "Type_of_Day")

#Choose which model you want to get the accuracy for 
variables <- variables7

######
MAPE <- NA
for (n in 1:7){
  MAPE[n] <- round(MAPE_days_J(dataS, variables, splitCol, temporal, n, 1000, 150, 
                               round(sqrt(length(variables))+0.5))[7]*100, 1)
}

R2_7_LM <- data.frame(MAPE, 1:7)

#MAPE <- round(rep(MAPE(unlist(mean(dataS$ConsPC)), unlist(dataS$ConsPC)),7)*100, 1)
#MAPE_Naive <- data.frame(MAPE, 1:7)

R2  <- round(rep(rsquare(c(rep(unlist(mean(dataS$ConsPC)), nrow(dataS)), 130), c(unlist(dataS$ConsPC), 130)),7)*100, 1)
R2_Naive <- data.frame(R2, 1:7)

cohort <- factor(rep(LETTERS[1:4], each = 10))

Weather7 <- 
  ggplot()+
#  geom_line(data=MAPE_Naive, aes(y=MAPE,x= X1.7,colour="Naive Model"),size=1, linetype = 1) +
  geom_line(data=MAPE_1,     aes(y=MAPE,x= X1.7,colour="Model 1: Past Consumption+Household+Temporal+Weather"),size=1, linetype = 4) +
  geom_line(data=MAPE_2,     aes(y=MAPE,x= X1.7,colour="Model 2: Household+Temporal+Weather"),size=1, linetype = 2) +
  geom_line(data=MAPE_5,     aes(y=MAPE,x= X1.7,colour="Model 3: Past Consumption"),size=1, linetype = 4) +
  geom_line(data=MAPE_3,     aes(y=MAPE,x= X1.7,colour="Model 4: Past Consumption+Temporal"),size=1, linetype = 4) +
  geom_line(data=MAPE_4,     aes(y=MAPE,x= X1.7,colour="Model 5: Temporal"),size=1, linetype = 1) +
  geom_line(data=MAPE_6,     aes(y=MAPE,x= X1.7,colour="Model 6: Household+Temporal"),size=1, linetype = 2) +
  geom_line(data=MAPE_7,     aes(y=MAPE,x= X1.7,colour="Model 7: Past Consumption+Type of Day"),size=1, linetype = 4) +
  geom_line(data=MAPE_7_LM,  aes(y=MAPE,x= X1.7,colour="Model 7(LM): Past Consumption+Type of Day"),size=1, linetype = 3) +
  geom_line(data=MAPE_8,     aes(y=MAPE,x= X1.7,colour="Model 8: Household+Type of Day"),size=1, linetype = 2) +
  geom_line(data=MAPE_8_LM,  aes(y=MAPE,x= X1.7,colour="Model 8(LM): Household+Type of Day"),size=1, linetype = 3) +
  scale_color_discrete(name = "Explanatory Variables:") + 
  labs(x="Forecast Horizon (days)", y = "MAPE (%)")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim = c(17, 30), xlim = c(1,7))+
  theme(legend.position="right",
        legend.key.size = unit(1.7, "cm"),
        legend.key.height = unit(0.8,"cm"))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7))+
  guides(colour = guide_legend(override.aes = list(linetype=c(4,2,4,4,1,2,3,4,3,2))))

Weather7 <- 
  ggplot()+
#  geom_line(data=R2_Naive, aes(y=R2,x= X1.7,colour="Naive Model"),size=1, linetype = 1) +
  geom_line(data=R2_1,     aes(y=MAPE,x= X1.7,colour="Model 1: Past Consumption+Household+Temporal+Weather"),size=1, linetype = 4) +
  geom_line(data=R2_2,     aes(y=MAPE,x= X1.7,colour="Model 2: Household+Temporal+Weather"),size=1, linetype = 2) +
  geom_line(data=R2_5,     aes(y=MAPE,x= X1.7,colour="Model 3: Past Consumption"),size=1, linetype = 4) +
  geom_line(data=R2_3,     aes(y=MAPE,x= X1.7,colour="Model 4: Past Consumption+Temporal"),size=1, linetype = 4) +
  geom_line(data=R2_4,     aes(y=MAPE,x= X1.7,colour="Model 5: Temporal"),size=1, linetype = 1) +
  geom_line(data=R2_6,     aes(y=MAPE,x= X1.7,colour="Model 6: Household+Temporal"),size=1, linetype = 2) +
  geom_line(data=R2_7,     aes(y=MAPE,x= X1.7,colour="Model 7: Past Consumption+Type of Day"),size=1, linetype = 4) +
  geom_line(data=R2_7_LM,  aes(y=MAPE,x= X1.7,colour="Model 7(LM): Past Consumption+Type of Day"),size=1, linetype = 3) +
  geom_line(data=R2_8,     aes(y=MAPE,x= X1.7,colour="Model 8: Household+Type of Day"),size=1, linetype = 2) +
  geom_line(data=R2_8_LM,  aes(y=MAPE,x= X1.7,colour="Model 8(LM): Household+Type of Day"),size=1, linetype = 3) +
  scale_color_discrete(name = "Explanatory Variables:") + 
  labs(x="Forecast Horizon (days)", y = "R2 (%)")+
  theme(plot.title = element_text(hjust=0.5),
        legend.key.size = unit(1.7, "cm"),
        legend.key.height = unit(0.8,"cm"))+
  coord_cartesian(ylim = c(0, 60), xlim = c(1,7))+
  theme(legend.position="right")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7))+
  guides(colour = guide_legend(override.aes = list(linetype=c(4,2,4,4,1,2,3,4,3,2))))

#Save your result
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("R2_RF.png", Weather7, width = 8, height = 4, dpi = 800)