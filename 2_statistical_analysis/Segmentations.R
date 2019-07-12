#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/1_import_clean')
if(!exists("foo", mode="function")) source("Import_Libraries.R")
if(!exists("foo", mode="function")) source("User_defined_functions.R")
if(!exists("foo", mode="function")) source("Import_clean_metadata.R")

#Run your function through another script
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop/R_files/SCRIPTS/2_statistical_analysis')
if(!exists("foo", mode="function")) source("Weights_Stations.R")
if(!exists("foo", mode="function")) source("Properties_High_Cons.R")

#Increase memory limit
memory.limit(17500000000000)

#Choose your variables
#Weekends, Time_d, Season, Gardens, Acorn_groups, Occupants, rateable, Status, Consumers
#Choose all the available categories for each variable
weekends <- c("all", TRUE, FALSE)
time_d <- c("all", "morning", "afternoon", "evening", "night")
season <- c("all", "summer", "autumn", "winter", "spring")
gardens <- c("all", "Large", "Medium", "Small")
ACORN <- c("all","Affluent","Comfortable","Financially Stretched")
Occupancy <- c("all", "high","medium","low")
rateable <- c( "all","high","medium","low")
metered <- c("all","Measured", "Unmeasured")
consumers <- c("all", "high")

#Create a data frame with all the possible combinations
combinations <- as.data.frame(expand.grid(weekends, time_d, season, gardens, 
                                          ACORN, Occupancy, rateable, metered, tax))
colnames(combinations) <- c("Weekends", "Time_of_Day","Season","Garden_Sizes", 
                            "ACORN","Occupancy","Rateable", "Status", "Tax_band")

# create progress bar
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = nrow(combinations), width = 300)

#Populate your data frame with the results
dfT <- list()

for (i in 1:nrow(combinations)){

  a <- df(df.list, combinations[i,1], combinations[i,2], combinations[i,3], 
                 combinations[i,4], combinations[i,5], combinations[i,6],combinations[i,7], 
                 combinations[i,8], combinations[i,9])
  
  if (!is.null(a)){
    dfT[[i]] <- a
    dfT[[i]] <- aggregate(formula(paste0("AbsConsumption", "~Date+ID")),dfT[[i]],FUN = (mean))
    #Exclude days with less than 60 properties
    x <- table(dfT[[i]]$Date)
    dfT[[i]] <- dfT[[i]][dfT[[i]]$Date %in% names(x[x >= 60]), ]
  }else{
    dfT[[i]] <- NULL
  }
  
  # create progress bar  
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, label=paste( round(i/nrow(combinations)*100, 0), "% done"))
}

for (i in 1:nrow(combinations)){
  if (!is.null(dfT[[i]])){
    dfT[[i]] <- na.omit(dfT[[i]])
    dfT[[i]] <- cbind(dfT[[i]], Postcode = final$postcode[match(dfT[[i]]$ID, paste('ID', final$ID, sep = ''))])
    dfT[[i]] <- cbind(dfT[[i]], Week  = lubridate::week(dfT[[i]]$Date))
    dfT[[i]] <- cbind(dfT[[i]], Month = lubridate::month(dfT[[i]]$Date))
    dfT[[i]] <- cbind(dfT[[i]], Year = lubridate::year(dfT[[i]]$Date))
  }
}

#Weighted averages
weightsT <- as.data.frame(table(final$postcode))
closest <- data.frame(cbind(rownames(distances), colnames(distances)[apply(distances, 1, which.min)]))
weightsT <- merge(weightsT, closest, by.x = "Var1", by.y = "X1")
colnames(weightsT) <- c("Postcodes", "Properties", "Station")

newcols <- c("Properties","Days", "Weeks", "Months")
combinations[, newcols] <- NA

#You can choose to create also the correlations past 3 days, past 5 days, weekly, and monthly
Correl.D   <- list()
#Correl.D_3 <- list()
#Correl.D_5 <- list()
#Correl.W   <- list()
#Correl.M   <- list()

Correl.D[[1]] <- Cor(dfT[[1]], "AbsConsumption", distances, Daily , weightsT, final, "perday")
#Columns from Correl.D
cols_weather <- c(3:5,14:16,19,21,23)
for (k in cols_weather){
#  combinations[,c(paste0(colnames(Correl.D[[1]])[k],   "_Gradient_D"), paste0(colnames(Correl.D[[1]])[k], "_R2_D"), paste0(colnames(Correl.D[[1]])[k], "_p_D"))] <- NA
#  combinations[,c(paste0(colnames(Correl.D[[1]])[k], "_Gradient_D_3"), paste0(colnames(Correl.D[[1]])[k], "_R2_D_3"), paste0(colnames(Correl.D[[1]])[k], "_p_D_3"))] <- NA
#  combinations[,c(paste0(colnames(Correl.D[[1]])[k], "_Gradient_D_5"), paste0(colnames(Correl.D[[1]])[k], "_R2_D_5"), paste0(colnames(Correl.D[[1]])[k], "_p_D_5"))] <- NA
#  combinations[,c(paste0(colnames(Correl.D[[1]])[k],   "_Gradient_W"), paste0(colnames(Correl.D[[1]])[k], "_R2_W"), paste0(colnames(Correl.D[[1]])[k], "_p_W"))] <- NA
#  combinations[,c(paste0(colnames(Correl.D[[1]])[k],   "_Gradient_M"), paste0(colnames(Correl.D[[1]])[k], "_R2_M"), paste0(colnames(Correl.D[[1]])[k], "_p_M"))] <- NA
  combinations[,c(paste0(colnames(Correl.D[[1]])[k],   "_spearman_r"), paste0(colnames(Correl.D[[1]])[k], "_spearman_p"))] <- NA
}

for (i in 1:nrow(combinations)){
  
  if (length(dfT[[i]])!= 0){
  if (length(unique(dfT[[i]]$Date)) > 5){
    
    #Daily
    Correl.D[[i]]   <- Cor(dfT[[i]], "AbsConsumption", distances, Daily , weightsT, final, "perday")
#    if (!is.null(Correl.D[[i]]) && length(unique(Correl.D[[i]]$Cons))>3){
    #Daily_3
#    Correl.D_3[[i]] <- lagged_weather(Correl.D[[i]], 3)
    #Daily_5
#    Correl.D_5[[i]] <- lagged_weather(Correl.D[[i]], 5)
    }
    #Weekly
#    Correl.W[[i]]   <- Cor(dfT[[i]], "AbsConsumption", distances, Daily , weightsT, final, "perweek")
    #Monthly
#    Correl.M[[i]]   <- Cor(dfT[[i]], "AbsConsumption", distances, Daily , weightsT, final, "permonth")
  }}else{
    Correl.D[[i]]   <- 0
#    Correl.D_3[[i]] <- 0
#    Correl.D_5[[i]] <- 0
#    Correl.W[[i]]   <- 0
#    Correl.M[[i]]   <- 0
  }
#  }

# create progress bar
pb <- tkProgressBar(title = "progress bar", min = 0,
                    max = nrow(combinations), width = 300)


for (i in 1:nrow(combinations)){
  
  for (n in cols_weather){
    
    variable   <- colnames(Correl.D[[i]])[n]
#    variable_3 <- paste0(colnames(Correl.D[[i]])[n], "_3")
#    variable_5 <- paste0(colnames(Correl.D[[i]])[n], "_5")
    
    if(!is.null(variable)){
#      compare_v <- paste0("Cons", " ~ ", variable)
#      compare_v_3 <- paste0("Cons", " ~ ", variable, "_3")
#      compare_v_5 <- paste0("Cons", " ~ ", variable, "_5")
      
            Correl.D[[i]][,variable][which(!is.finite(Correl.D[[i]][,variable]))] <- NA
      #      Correl.W[[i]][,variable][which(!is.finite(Correl.W[[i]][,variable]))] <- NA
      #      Correl.M[[i]][,variable][which(!is.finite(Correl.M[[i]][,variable]))] <- NA
      
      #Daily      
      if (!is.null(Correl.D[[i]]) && length(unique(Correl.D[[i]]$Cons))>3 && length(unique(Correl.D[[i]][,variable]))>3){
#        assign(paste0("model_D_", variable), lm(as.formula(compare_v), data = data.frame(Correl.D[[i]]), na.action=na.omit))
        assign(paste0("model_D_", variable), cor.test(Correl.D[[i]][,variable], Correl.D[[i]][,"Cons"], method = "spearman"))
      } else {
        assign(paste0("model_D_", variable), NA)
        }
      
      #Daily_3      
#      if (!is.null(Correl.D_3[[i]]) && length(unique(Correl.D_3[[i]]$Cons))>3 && length(unique(Correl.D_3[[i]][,variable_3]))>3){
#        assign(paste0("model_D_", variable_3), lm(as.formula(compare_v_3), data = data.frame(Correl.D_3[[i]]), na.action=na.omit))
#      } else {
#        assign(paste0("model_D_", variable_3), NA)}
      
      #Daily_5      
#      if (!is.null(Correl.D_5[[i]]) && length(unique(Correl.D_5[[i]]$Cons))>3 && length(unique(Correl.D_5[[i]][,variable_5]))>3){
#        assign(paste0("model_D_", variable_5), lm(as.formula(compare_v_5), data = data.frame(Correl.D_5[[i]]), na.action=na.omit))
#      } else {
#        assign(paste0("model_D_", variable_5), NA)}
      
      #Weekly
#      if (!is.null(Correl.W[[i]]) && length(unique(Correl.W[[i]]$Cons))>3 && length(unique(Correl.W[[i]][,variable]))>3){
#        assign(paste0("model_W_", variable), lm(as.formula(compare_v), data = data.frame(Correl.W[[i]]), na.action=na.omit))
#      } else {
#        assign(paste0("model_W_", variable), NA)}
      
      #Monthly
#      if (!is.null(Correl.M[[i]]) && length(unique(Correl.M[[i]]$Cons))>3 && length(unique(Correl.M[[i]][,variable]))>3){
#        assign(paste0("model_M_", variable), lm(as.formula(compare_v), data = data.frame(Correl.M[[i]]), na.action=na.omit))
#      } else {
#        assign(paste0("model_M_", variable), NA)}
      
      #Write results
      #Save your results in the table
#      combinations[i, c(paste0(variable, "_Gradient_D"), paste0(variable, "_R2_D"), paste0(variable, "_p_D")
            combinations[i, c(paste0(variable, "_spearman_r"), paste0(variable, "_spearman_p")
                        #,
#                        paste0(variable, "_Gradient_D_3"), paste0(variable, "_R2_D_3"), paste0(variable, "_p_D_3"),
#                        paste0(variable, "_Gradient_D_5"), paste0(variable, "_R2_D_5"), paste0(variable, "_p_D_5"),
#                        paste0(variable, "_Gradient_W"), paste0(variable, "_R2_W"), paste0(variable, "_p_W"),
#                        paste0(variable, "_Gradient_M"), paste0(variable, "_R2_M"), paste0(variable, "_p_M")
)] <- 
        c(
          #r
          if (!is.na(get(paste0("model_D_", variable)))  &&  is.numeric(get(paste0("model_D_", variable))$estimate)){
          format(round(as.numeric(get(paste0("model_D_", variable))$estimate), 3),nsmall=3)
          }else{NA},
          #p
          if (!is.na(get(paste0("model_D_", variable)))  &&  is.numeric(get(paste0("model_D_", variable))$p.value)){
          format(round(as.numeric(get(paste0("model_D_", variable))$p.value),  3),nsmall=3)
          }else{NA}
          
          #Daily
#          #Gradient
#          if (!is.na(get(paste0("model_D_", variable)))  &&  is.numeric(get(paste0("model_D_", variable))$coefficients)){
#            format(round(as.numeric(get(paste0("model_D_", variable))$coefficients[2]),3),nsmall=3)
#          }else{NA},
#          #R2
#          if (!is.na(get(paste0("model_D_", variable)))  &&  is.numeric(get(paste0("model_D_", variable))$coefficients)){
#            format(round(as.numeric(summary(get(paste0("model_D_", variable)))$adj.r.squared), 3),nsmall=3)
#          }else{NA},
          #p
#          if (!is.na(get(paste0("model_D_", variable)))  &&  is.numeric(get(paste0("model_D_", variable))$coefficients)){
#            format(round(as.numeric(lmp(get(paste0("model_D_", variable)))),3),nsmall=3)
#          }else{NA},
          #Daily_3
          #Gradient
 #         if (!is.na(get(paste0("model_D_", variable_3)))  &&  is.numeric(get(paste0("model_D_", variable_3))$coefficients)){
#            format(round(as.numeric(get(paste0("model_D_", variable_3))$coefficients[2]),3),nsmall=3)
#          }else{NA},
          #R2
#          if (!is.na(get(paste0("model_D_", variable_3)))  &&  is.numeric(get(paste0("model_D_", variable_3))$coefficients)){
#            format(round(as.numeric(summary(get(paste0("model_D_", variable_3)))$adj.r.squared), 3),nsmall=3)
#          }else{NA},
          #p
#          if (!is.na(get(paste0("model_D_", variable_3)))  &&  is.numeric(get(paste0("model_D_", variable_3))$coefficients)){
#            format(round(as.numeric(lmp(get(paste0("model_D_", variable_3)))),3),nsmall=3)
#          }else{NA},
          #Daily_5
          #Gradient
#          if (!is.na(get(paste0("model_D_", variable_5)))  &&  is.numeric(get(paste0("model_D_", variable_5))$coefficients)){
#            format(round(as.numeric(get(paste0("model_D_", variable_5))$coefficients[2]),3),nsmall=3)
#          }else{NA},
          #R2
#          if (!is.na(get(paste0("model_D_", variable_5)))  &&  is.numeric(get(paste0("model_D_", variable_5))$coefficients)){
#            format(round(as.numeric(summary(get(paste0("model_D_", variable_5)))$adj.r.squared), 3),nsmall=3)
#          }else{NA},
          #p
#          if (!is.na(get(paste0("model_D_", variable_5)))  &&  is.numeric(get(paste0("model_D_", variable_5))$coefficients)){
#            format(round(as.numeric(lmp(get(paste0("model_D_", variable_5)))),3),nsmall=3)
#          }else{NA},
          #Weekly
          #Gradient
#          if (!is.na(get(paste0("model_W_", variable)))  &&  is.numeric(get(paste0("model_W_", variable))$coefficients)){
#            format(round(as.numeric(get(paste0("model_W_", variable))$coefficients[2]),3),nsmall=3)
#          }else{NA},
          #R2
#          if (!is.na(get(paste0("model_W_", variable)))  &&  is.numeric(get(paste0("model_W_", variable))$coefficients)){
#            format(round(as.numeric(summary(get(paste0("model_W_", variable)))$adj.r.squared), 3),nsmall=3)
#          }else{NA},
          #p
#          if (!is.na(get(paste0("model_W_", variable)))  &&  is.numeric(get(paste0("model_W_", variable))$coefficients)){
#            format(round(as.numeric(lmp(get(paste0("model_W_", variable)))),3),nsmall=3)
#          }else{NA},
          #Monthly
#          if (!is.na(get(paste0("model_M_", variable)))  &&  is.numeric(get(paste0("model_M_", variable))$coefficients)){
#            format(round(as.numeric(get(paste0("model_M_", variable))$coefficients[2]),3),nsmall=3)
#          }else{NA},
          #R2
#          if (!is.na(get(paste0("model_M_", variable)))  &&  is.numeric(get(paste0("model_M_", variable))$coefficients)){
#            format(round(as.numeric(summary(get(paste0("model_M_", variable)))$adj.r.squared), 3),nsmall=3)
#          }else{NA},
          #p
#          if (!is.na(get(paste0("model_M_", variable)))  &&  is.numeric(get(paste0("model_M_", variable))$coefficients)){
#            format(round(as.numeric(lmp(get(paste0("model_M_", variable)))),3),nsmall=3)
#          }else{NA}
)
      
      #Save your results in the table
#      combinations[i, c("Properties","Days", "Weeks", "Months")] <- 
#        c(
#          #Properties
#          length(unique(na.omit(dfT[[i]])$ID)), 
#          #Days
#          if (!is.null(Correl.D[[i]])){nrow(Correl.D[[i]])}else{0},
#          #Weeks
#          if (!is.null(Correl.W[[i]])){nrow(Correl.W[[i]])}else{0},
#          #Months
#          if (!is.null(Correl.M[[i]])){nrow(Correl.M[[i]])}else{0}) 
    }
  }
  # create progress bar  
  Sys.sleep(0.1)
  setTkProgressBar(pb, i, label=paste( round(i/nrow(combinations)*100, 0), "% done"))
  
}

#Save your result
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
write.csv(x = combinations, file = "Rate_all.csv")