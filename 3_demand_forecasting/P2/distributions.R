##Create your distribution plot for the variable of your choice (e.g. Seasons)
##First you need to create the dataset 'dataS' using the R script 'dataS'
##When you choose your social variables to use for grouping consumption, use the one that you want to see the distribution for

occupancy <-   
  ggplot(dataS, aes(x=ConsPC, fill = occupancy))+geom_density(alpha = .3)+
  scale_y_continuous(labels= scales::percent)+
  labs(x="PCC (l/day)", y = "Density (%)")+
  ggtitle("a. Occupancy")+
  theme(plot.title = element_text(size=10, face = "bold"))+
  xlim(80, 260)+ 
  scale_y_continuous(limits=c(0, 0.06), labels = scales::percent_format(accuracy = 1))+
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  guides(shape = guide_legend(override.aes = list(size = 2)))+ 
  theme(legend.title = element_text(size = 8), legend.text = element_text(size = 8))
#+
#  facet_wrap(~occupancy, ncol = 2)

  setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
#  ggsave("Months.png", Months, width = 3, height = 2.5, dpi = 1000)
  ggsave("occupancy.png", occupancy, width = 3, height = 2.5, dpi = 900)

##Create distribution plots for the following weather variables
  
  #Choose the date range for the weather variables to be the same as for the consumption 
  rangeW <- range(dataS$Date)
  
  #Create a new weather dataframe for the date range of interest 
  WeatherDis <- Weather[Weather$Date>=rangeW[1] & Weather$Date<=rangeW[2], ]
  #Test the result is what you expected
  range(WeatherDis$Date)
  
  #Create a distribution plot for each weather variable and save the result
  Air <-   
    ggplot(WeatherDis, aes(x=Date, y=Air_T)) + 
    geom_point(aes(color=Season))+
    labs(x="Dates", y = "Air Temperature (°C)")+
    theme(plot.title = element_text(size=10, face = "bold")) 
  
  Soil <-   
    ggplot(WeatherDis, aes(x=Date, y=Soil_T)) + 
    geom_point(aes(color=Season))+
    labs(x="Dates", y = "Soil Temperature (°C)")+
    theme(plot.title = element_text(size=10, face = "bold")) 
  
  Rainf <-   
    ggplot(WeatherDis, aes(x=Date, y=Rainfall)) + 
    geom_point(aes(color=Season))+
    labs(x="Dates", y = "Rainfall (mm)")+
    theme(plot.title = element_text(size=10, face = "bold")) 
  
  DaysWR <-   
    ggplot(WeatherDis, aes(x=Date, y=Days_without_Rain)) + 
    geom_point(aes(color=Season))+
    labs(x="Dates", y = "Days without Rain")+
    theme(plot.title = element_text(size=10, face = "bold")) 
  
  Humi <- 
    ggplot(WeatherDis, aes(x=Date, y=Humidity)) + 
    geom_point(aes(color=Season))+
    labs(x="Dates", y = "Humidity (%)")+
    theme(plot.title = element_text(size=10, face = "bold")) 
  
  Suns <-   
    ggplot(WeatherDis, aes(x=Date, y=Sunshine)) + 
    geom_point(aes(color=Season))+
    labs(x="Dates", y = "Sunshine (hours)")+
    theme(plot.title = element_text(size=10, face = "bold")) 
  
  #Save each figure on the desktop under the corresponding name
  setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
  ggsave("Air.png", Air, width = 3.5, height = 2.5, dpi = 600)