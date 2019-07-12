#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")

#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')
if(!exists("foo", mode="function")) source("Weights_Stations.R")
if(!exists("foo", mode="function")) source("Properties_High_Conss.R")

#setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/Journal_papers/Journal_1')
#all = read.csv("all_J.csv", header = TRUE)

########SCATTERPLOTS############
#Sunshine_Dur [3]
#Radiation [4]
#Rainfall [5]
#Humidity [15]
#Amount_10CM [16]
#Max_Air_09_21 [19]
#"Sunshine Duration (hours/day)", "Radiation (Mj/m2/day)", "Rainfall (mm/day)"
#"Humidity (%)", "Soil Temperature (°C)", "Air Temperature (°C)")

#Increase memory limit
memory.limit(17500000000000)
#(df.list, Weekends, Time_d, Season, Gardens, Acorn_groups, Occupants, rateable, Status, Consumers)
testall <- df(df.list, "all", "all", "all",  "all", "all", "all","all", "all", "all")

test33 <- df(df.list, TRUE , "evening", "summer",  "all", "Affluent", "all","all", "Unmeasured", "high") # sunshine
test3  <- df(df.list, FALSE, "evening", "summer",  "all", "Affluent", "all","all", "Unmeasured", "high") # sunshine

test44 <- df(df.list, FALSE, "all", "all",  "all", "Affluent"     , "all","all", "Measured" , "high") # temp soil
test4  <- df(df.list, FALSE, "all", "all",  "all", "Affluent", "all","all", "Unmeasured", "high") # temp soil

test22 <- df(df.list, FALSE, "all", "all",  "all", "Financially Stretched", "all","all", "Unmeasured", "high") # temp air
test2  <- df(df.list, FALSE, "all", "all",  "all", "Affluent"             , "all","all", "Unmeasured", "high") # temp air

test55 <- df(df.list, FALSE, "all", "winter",  "all", "all", "all","high", "Unmeasured", "high") #hum
test5  <- df(df.list, FALSE, "all", "summer",  "all", "all", "all","high", "Unmeasured", "high") #hum

test66 <- df(df.list, "all", "all", "all",  "all", "all", "all","all", "all", "all") #Rainfall
test6  <- df(df.list, FALSE, "all", "summer",  "all", "Affluent", "all","all", "Unmeasured", "high") #Rainfall

test22 <- df(df.list, FALSE, "all", "all",  "all", "Financially Stretched", "all","all", "all", "all") # temp air
test2  <- df(df.list, FALSE, "all", "all",  "all", "Affluent"             , "all","all", "all", "all") # temp air

testT <- df(df.list, TRUE, "night", "summer",  "Large", "Affluent", "medium","all", "all", "all")

#testNNall <- Scatter_Correl(testall, final, 1)

testNNall <- Scatter_Correl(testT, final, 1)

#Choose segmentation
segmentation <- testT

####
testNN <- Scatter_Correl(segmentation, final, 2)
testNNF <- merge(testNNall, testNN, all=TRUE)
#testNNF$Radiation <- testNNF$Radiation/1000
testNNF$Cons <- testNNF$Cons/4

#Make the plot
xvariable <- colnames(testNN)[16]
sizevar   <- colnames(testNN)[15]
colourvar <- colnames(testNN)[3]

p1 <-  ggplot() + geom_point(data = testNNF, aes_string(x = xvariable, y = "Cons", size = sizevar,
                                                         colour = colourvar))+
  geom_smooth(data=testNNF, aes_string(x = xvariable, y = "Cons"), fill="red",
              colour="red", method = "lm",formula = y~x, se = FALSE)

p <-
  p1 + facet_wrap( ~ type, nrow = 1) + 
  theme(axis.title.x = element_text(color="black", size = 20),
        axis.title.y = element_text(color="black", size = 20),
        axis.text.x  = element_text(color="black", size = 15),
        axis.text.y  = element_text(color="black", size = 15),
        legend.text  = element_text(color="black", size = 15),
        legend.title = element_text(color="black", size = 16),
        strip.text = element_text(size=17))+
#  ylim(250, 475)+
  xlab("Soil Temperature (°C)") +
  ylab("Average Consumption (L/property/day)") +
  labs(size="Humidity (%)", colour="Sunshine Duration\n(hours/day)") +
  guides(colour = guide_colourbar(order = 2),
         size = guide_legend(order = 1))+
  scale_size_continuous(breaks = c(60,75,90), range=c(3,6)
                        ) 

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("scatter_Soil_example.png", p1, width = 9, height = 5, dpi = 900)

write.csv(testTT, file = "2017_06-25.csv",row.names=FALSE)
