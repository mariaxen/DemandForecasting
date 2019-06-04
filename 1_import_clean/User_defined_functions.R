importData <- function(name, i){
  x <- try(read.table(name, header=FALSE, skip = i), silent = TRUE)
  if(inherits(x, "try-error"))
    return(NULL)
  else
    return(x)
}

removedupl <- function(df,col){
  #Start from the 1st row
  k <- 1
  s <- 1;
  
  while (k==1 && s <nrow(df)) {      
    #If the consumption value is the same for the first two rows
    if (!is.na(df[s,col]) && !is.na(df[s+1,col]) &&  df[s,col] == df[s+1,col]) {
      s <- s+1
    }else{
      k <- 2
    }
  }
  df <- df[-(1:s),]
  
  #Return the resulting data frame
  return(df)
}

remove_outliers <- function(df,col) {
  x <- df[,col]
  if(nrow(df)>0){
    l <- x[!x %in% 0]
    qnt <- quantile(l, probs=c(.01, .99), na.rm = TRUE)
    #  H <- (qnt[2]-qnt[1])/4
    #  y <- x
    #  x[x < (qnt[1] - H)] <- NA
    #  x[x > (qnt[2] + H)] <- NA
    x[x < (qnt[1])] <- NA
    x[x > (qnt[2])] <- NA
    df[,col] <- x
  }
  df
}

outersect <- function(a,b) setdiff(union(a,b), intersect(a,b))

replcap <- function(df, cap) {
  if (nrow(df)>0){
    df$AbsoluteConsumption[df$AbsoluteConsumption<0 | df$AbsoluteConsumption>cap] <- NA
    df <- na.omit(df)
  }
  df
}

zeros <- function(Data) {
  a <- as.data.frame(table(Data$Precipitation))[1,2]
  return(a)
}

removerep <- function (df,number){
  if (nrow(df)>0){
    repeats <- as.data.frame(table(df$Consumption))
    remov <- repeats[repeats$Freq>number,]
    df <- df [!df$Consumption %in% remov$Var1,]
  }
  df
}

absolute <- function(df, Col){
  if (!is.null(df) && nrow(df) > 0){
    cbind(df, AbsoluteConsumption = c(0,(diff(as.numeric(as.character(df[,Col]))))))
  }else{
    cbind(df, AbsoluteConsumption = character()) 
  }
}

my_correlation_plot <- function(df, x_string, y_string, postcodes, weather_station, Subset, variable) {
  
  cor.k <- Kendall(df[,x_string], df[,y_string])
  
  ggplot() + geom_point(data = df, aes_string(x = x_string, y = y_string), color = 'black') +
    geom_smooth(data=df, aes_string(x = x_string, y = y_string), fill="red",
                colour="red", size=1,method = "lm",formula = y ~ poly(x, 2))+
    theme(axis.title.x = element_text(color="black", size=18),
          axis.title.y = element_text(color="black", size=18),
          plot.title   = element_text(color="black", size=15))+
    labs(x=sprintf("%s", variable), 
         y = "Average Consumption (L/property/day)",
         title= paste("Postcodes:", paste(postcodes, collapse=","),"\n and",paste(weather_station, collapse=", "),"\nAcorn Groups:", paste(acorn_groups, collapse=", ")))+ 
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", x=c(min(df[,x_string]), min(df[,x_string])), hjust = c(0,-4.5), y= Inf, vjust=4.4, label = c(paste("Kendall_rho ==", format(round(as.numeric(cor.k[1]),3), nsmall =4)),paste("p ==", format(round(as.numeric(cor.k[2]),3), nsmall =4))), parse=T, color = 'black', size = 6)+
    annotate("text", x=min(df[,x_string]), hjust = 0, y= c(Inf,Inf), vjust=c(2,3.2), label = c(paste("Number_of_days ==", nrow(df)),paste("Properties ==", length(Subset))), parse=T, color = 'black', size = 6)+
    ylim(75, 675)
}

correlation_plot <- function(Correl, x_string, y_string, Properties, Days, variable) {
  
  #  cor.k <- Kendall(Correl[,x_string], Correl[,y_string])
  #  cor.s <- cor.test(Correl[,x_string], Correl[,y_string], method = "spearman")
  model <- lm(Correl[,x_string] ~ poly(Correl[,y_string],2))
  
  ggplot() + geom_point(data = Correl, aes_string(x = x_string, y = y_string), color = 'black') +
    geom_smooth(data=Correl, aes_string(x = x_string, y = y_string), fill="red",
                colour="red", size=1,method = "lm",formula = y ~ poly(x, 2), se = FALSE)+
    theme(axis.title.x = element_text(color="black", size=18),
          axis.title.y = element_text(color="black", size=18),
          plot.title   = element_text(color="black", size=15))+
    labs(x=sprintf("%s", variable), 
         y = "Average Consumption (L/capita/day)")+ 
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", x=c(3, 3), y= Inf, hjust = 0, vjust=4.4, label = c(paste("R_square ==", format(round(as.numeric(summary(model)$adj.r.squared),2), nsmall =3))), parse=T, color = 'black', size = 6)+
    annotate("text", x=3, y= c(Inf,Inf), hjust = 0, vjust=c(2,3.2), label = c(paste("Days ==", Days),paste("Properties ==", Properties)), parse=T, color = 'black', size = 6)
  #   ylim(50, 300)+
  #   xlim(2,33)
}

my_season_plot <- function(df, x_string, y_string, postcodes, weather_station, Subset, variable) {
  
  f <- paste(as.name(x_string), "~", as.name(y_string))
  
  #Choose months to plot
  wintermonths <- c(12,01,02)
  summermonths <- c(06,07,08)
  
  winter <- df[df$Month %in% wintermonths,]
  summer <- df[df$Month %in% summermonths,]
  
  cor.k.summer <- Kendall(summer[,x_string], summer[,y_string])
  cor.k.winter <- Kendall(winter[,x_string], winter[,y_string])
  
  ggplot() +     
    geom_point(data = summer, aes_string(x = x_string, y = y_string), color = 'red')+   
    geom_smooth(data=summer, aes_string(x = x_string, y = y_string), fill="red",
                colour="red", size=1,method = "loess")+
    geom_point(data = winter, aes_string(x = x_string, y = y_string), color = 'blue') +
    geom_smooth(data=winter, aes_string(x = x_string, y = y_string), fill="blue",
                colour="blue", size=1, method = "loess")+
    theme(axis.title.x = element_text(face="bold", color="black", size=12),
          axis.title.y = element_text(face="bold", color="black", size=12),
          plot.title = element_text(face="bold", color = "black", size=14))+
    labs(x=sprintf("%s", variable), 
         y = "Average Consumption (L/property/day)",
         title= paste("Postcodes:", paste(postcodes, collapse=", "),"\n and",paste(weather_station, collapse=", "),"station \n","Acorn Groups:", paste(acorn_groups, collapse=", ")))+ 
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", x=c(min(df[,x_string]), min(df[,x_string])), hjust = c(0,-2.5), y= Inf, vjust=5.6, label = c(paste("Kendall_rho ==", format(round(as.numeric(cor.k.summer[1]),3), nsmall =4)),paste("p ==", format(round(as.numeric(cor.k.summer[2]),3), nsmall =4))), parse=T, color = 'red', size = 5)+
    annotate("text", x=c(min(df[,x_string]), min(df[,x_string])), hjust = c(0,-2.5), y= Inf, vjust=4.4, label = c(paste("Kendall_rho ==", format(round(as.numeric(cor.k.winter[1]),3), nsmall =4)),paste("p ==", format(round(as.numeric(cor.k.winter[2]),3), nsmall =4))), parse=T, color = 'blue', size = 5)+
    annotate("text", x=min(df[,x_string]), hjust = 0, y= c(Inf,Inf), vjust=c(2,3.2), label = c(paste("Number_of_days ==", nrow(winter)+nrow(summer)),paste("Properties ==", length(Subset))), parse=T, color = 'black', size = 5)
}

Subsetdf <- function(df_stats, include) {
  if(include[1] == "all"){
    as.numeric(row.names(df_stats))
  } else{
    na.omit(as.numeric(row.names(df_stats)[apply(df_stats, 1, function(u) any(u %in% include))]))
  }
}

SubsetdfCol <- function(df_stats, include, NoCol) {
  if(include[1] == "all"){
    as.numeric(row.names(df_stats))
  } else{
    na.omit(as.numeric(row.names(df_stats)[df_stats[,NoCol] %in% include]))
  }
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

repDF <- function(df, col){
  if (nrow(df) > 0){
    as.data.frame(table(droplevels(df[,col])))
  }
  else{
    df=df
  }
}

aggregateSum <- function(df, col1, col2, col3, FUN){
  if (nrow(df) > 0){
    if (missing(col3)){
      aggregate(df[,col1]~df[,col2], df, FUN=(FUN))
    } else{
      aggregate(df[,col1]~df[,col2]+df[,col3], df, FUN=(FUN))
    }
  }
  else{
    df
  }
}

func <- function(x,y){merge(x, y, by.x=names(x)[2], by.y=names(y)[2])}

rangeL <- function(df, col){
  if (nrow(df) > 0){
    df <- na.omit(df)
    range(df[,col])
  }
  else{
    0
  }
}

meanL <- function(df, col){
  if (nrow(df) > 0){
    df <- na.omit(df)
    mean(df[,col])
  }
  else{
    NA
  }
}

season <- function(df, colNo){
  if (nrow(df) > 0){
    df <- na.omit(df)
    df[,colNo] <- df[,colNo] - min(df[,colNo])
  }
  else{
    NA
  }
}

summeravg <- function(df, col){
  if (nrow(df) > 0){
    df <- na.omit(df)
    mean(df[df$Y_M %in% c("2015 06","2015 07","2015 08","2016 06","2016 07","2016 08"),][,col])
  }
  else{
    NA
  }
}

ggplot_alt <- function(funct){
  the_data <- data.frame(
    x <- seq(0, 1, length.out = 100),
    y = pbeta(x, 1, 10)
  )
  funct
}

ggplot_distributions <- function(data, variable){
  c <- ggplot(data, aes_string(as.name(variable)))+ 
    geom_bar(fill="darkgreen", colour="darkgreen") + xlab (sprintf("%s", variable)) + ylab ('Number of properties') + 
    theme(axis.title.x = element_text(face="bold", size=11),
          axis.text.x  = element_text(angle=45, vjust=0.5, size=9),
          axis.title.y = element_text(face="bold", size=11),
          axis.text.y  = element_text(angle=90, vjust=0.5, size=9))
}

dist_station_post <- function (post, station){
  
  lat1 <- Stations[Stations$Station.name==station, "Latitude"]
  lon1 <- Stations[Stations$Station.name==station, "Longitude"]
  
  lat2 <- as.numeric(unique(final[final$postcode==post, "latitude" ]))
  lon2 <- as.numeric(unique(final[final$postcode==post, "longitude"]))
  
  distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
}

dist_stations <- function (post1, post2){
  
  lat1 <- as.numeric(unique(final[final$postcode==post1, 9]))
  lon1 <- as.numeric(unique(final[final$postcode==post1, 10]))
  
  lat2 <- as.numeric(unique(final[final$postcode==post2, 9]))
  lon2 <- as.numeric(unique(final[final$postcode==post2, 10]))
  
  distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
}

distPost <- function (station1, station2){
  
  lat1 <- stations[stations$NAME==station1, 4]
  lon1 <- stations[stations$NAME==station1, 5]
  
  lat2 <- stations[stations$NAME==station2, 4]
  lon2 <- stations[stations$NAME==station2, 5]
  
  distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
}

leaks <- function(vectorAbsC){
  repeats <- as.data.frame(table(vectorAbsC))
  noconsrec  <- max(as.numeric(repeats[repeats$vectorAbsC==0,]$Freq),1)
  totalrec   <- sum(repeats$Freq)
  c(noconsrec, totalrec)
}

leakrate <- function(x){
  if (nrow(x)>0){
    cbind(x, "Leakrate" = x$ConsRecords[,1]/x$ConsRecords[,2])  
  }
}

non.leak.months <- function(df.list, threshold){
  df.leaks <- lapply(df.list, function(x) aggregateSum(x,"AbsConsumption", "Year", "Month", FUN=leaks))
  df.leaks <- lapply(df.leaks, setNames, c('Year','Month','ConsRecords'))
  df.leaks <- lapply(df.leaks, leakrate)
  df.leaks <- lapply(df.leaks, function(x) cbind(x, "Leak" = x[,"Leakrate"]<threshold))
  df.leaks <- lapply(df.leaks, function(x) if (nrow(x)>0) {cbind(x, "Not_too_few" = x$ConsRecords[,2]>24,
                                                                 "Y_M" = paste(x$Year,x$Month))})
  non.leaking.months <- lapply(df.leaks, function(x) if (length(x)>0) {x[,"Y_M"][x[,"Leak"]==FALSE & x[,"Not_too_few"]==TRUE]})
  non.leaking.months
}

non.leak.days <- function(df.list, threshold){
  df.leaks <- lapply(df.list, function(x) aggregateSum(x,"AbsConsumption", "Date", FUN=leaks))
  df.leaks <- lapply(df.leaks, setNames, c('Date','ConsRecords'))
  df.leaks <- lapply(df.leaks, leakrate)
  df.leaks <- lapply(df.leaks, function(x) cbind(x, "Leak" = x[,3]>threshold))
  df.leaks <- lapply(df.leaks, function(x) if (nrow(x)>0) {cbind(x, "Not_too_few" = x$ConsRecords[,2]>12)})
  non.leaking.days <- lapply(df.leaks, function(x) if (length(x)>0) {x[,"Date"][x[,"Leak"]==TRUE & x[,"Not_too_few"]==TRUE]})
  non.leaking.days
}

df <- function(df.list, Weekends, Time_d, Season, Gardens, Acorn_groups, Occupants, rateable, Status, Consumers){
  
  timeA     <- timeofday(Time_d)[1]
  timeB     <- timeofday(Time_d)[2]
  
  if (Consumers == "high"){
    Consumers <- High_cons
  }
  
  #  if (area == "YEOVILTON"){postcodes = c("BA22","BA21","TA11","BA20","TA15","TA14","TA12","DT9","BA7")}
  #  if (area == "LYNEHAM"){postcodes = c("SN11","SN15")}
  #  if (area == "BOSCOMBEDOWN"){postcodes = c("SP4","SP1","SP2","SP9")}
  #  if (area == "all"){postcodes = c("BA22","BA21","TA11","BA20","TA15","TA14","TA12","DT9","BA7","SN11","SN15","SP4","SP1","SP2","SP9")}
  
  if (Season == "summer"){
    df.list <- lapply(df.list, function(x) x[x$Month %in% c("06","07","08"),])
  } else if (Season == "autumn"){
    df.list <- lapply(df.list, function(x) x[x$Month %in% c("09","10","11"),])
  } else if (Season == "winter"){
    df.list <- lapply(df.list, function(x) x[x$Month %in% c("12","01","02"),])
  } else if (Season == "spring"){
    df.list <- lapply(df.list, function(x) x[x$Month %in% c("03","04","05"),])
  }
  
  #Day of week (All, Working day, non-working day)
  if (Weekends == FALSE){
    df.list <- lapply(df.list, function(x) x[x$Weekend == Weekends & x$Holiday == Weekends,])
  }
  if (Weekends == TRUE){
    df.list <- lapply(df.list, function(x) x[x$Weekend == Weekends | x$Holiday == Weekends,])
  }
  #Time of day (Daily, morning, afternoon, evening)
  time1 <- chron(times=as.character(timeA))
  time2 <- chron(times=as.character(timeB))
  if (Time_d != "all"){
    df.list <- lapply(df.list, function(x)  subset(x, time2>chron(times=as.character(x$Time)) & time1<=chron(times=as.character(x$Time))))
  }
  #Subset your dataset
  SubsetMetered  <- Subsetdf(final, Status)
  SubsetGardens   <- SubsetdfCol(final, Gardens, 4)
  SubsetAcorn     <- SubsetdfCol(final, Acorn_groups, 15)
  SubsetRate      <- SubsetdfCol(final, rateable,     16)
  SubsetOccupants <- SubsetdfCol(final, Occupants,    17)  
  #  SubsetPostcode  <- Subsetdf(final, postcodes)
  SubsetConsumers <- SubsetdfCol(final, Consumers,  1) 
  
  SubsetFinal <- list(SubsetMetered, SubsetGardens, SubsetAcorn, SubsetRate, SubsetOccupants, SubsetConsumers)
  Subset <-  Reduce(intersect, SubsetFinal)
  df.list <- df.list[Subset]
  df.list <- df.list[sapply(df.list, function(x) dim(x)[1]) > 0]
  rbind.fill(df.list)
}

Weather_df <- function(dataset, weather_station){
  SubsetWeather <- Subsetdf(dataset, weather_station)
  Weather <- dataset[SubsetWeather,]
  Weather
}

kend_r <- function(dfT, w_df, area, x, y){
  
  Days <- length(unique(dfT$ID))
  
  Cor <- Cor(dfT, w_df, area)
  cor.k <- Kendall(Cor[,x], Cor[,y])
  c(Days, nrow(Cor), format(round(as.numeric(cor.k[1]),2), nsmall =2), format(round(as.numeric(cor.k[2]),3), nsmall =3))
}

Cor <- function(dfT, column, distances, w_df, weightsT, final, temporal){
  
  postcodes <- unique(dfT$Postcode)
  
  weightsT <- weightsT[weightsT$Postcodes %in% postcodes,]
  weightsT <- aggregate(Properties~Station, weightsT, sum)
  weightsT$Station <- as.character(weightsT$Station) 
  
  Weather  <- w_df[w_df$Station_Name %in% weightsT$Station, ]
  
  if (nrow(Weather)>0){
    Weather$Weights <- NA
    for (i in 1:nrow(weightsT)){
      if (nrow(Weather[Weather$Station_Name == weightsT$Station[i],])>0){
        Weather[Weather$Station_Name == weightsT$Station[i],]$Weights <- weightsT$Properties[i]   
      }
    }
    
    if (temporal == "perhour"){
      
      TotalCons <- aggregate(formula(paste0(column, "~Date+Time")),dfT,FUN = (mean), na.rm=TRUE)
      Weather   <- 
        ddply(Weather, .(Date), plyr::summarize,  # so by asset class invoke following function
              Radiation = weighted.mean(Radiation, Weights, na.rm = T),
              Wind_Speed = weighted.mean(Wind_Speed, Weights, na.rm = T),
              Air_Temperature = weighted.mean(Air_Temperature, Weights, na.rm = T),
              Humidity = weighted.mean(Humidity, Weights, na.rm = T),
              Snow_depth = weighted.mean(Snow_depth, Weights, na.rm = T),
              Amount_10CM = weighted.mean(Amount_10CM, Weights, na.rm = T),
              Amount_30CM = weighted.mean(Amount_30CM, Weights, na.rm = T),
              Amount_100CM = weighted.mean(Amount_100CM, Weights, na.rm = T),
              Derived_H_Sun_dur = weighted.mean(Derived_H_Sun_dur, Weights, na.rm = T)
        )
      
      Correl <- merge(TotalCons, Weather)
    }else{
      
      TotalCons <- aggregate(formula(paste0(column, "~Date")),dfT,FUN = (mean), na.rm=TRUE)
      Weather   <- 
        ddply(Weather, .(Date), plyr::summarize,  # so by asset class invoke following function
              Sunshine_Dur = weighted.mean(Sunshine_Dur, Weights, na.rm = T),
              Radiation = weighted.mean(Radiation, Weights, na.rm = T),
              Rainfall = weighted.mean(Rainfall, Weights, na.rm = T),
              CS_Sunshine_Dur = weighted.mean(CS_Sunshine_Dur, Weights, na.rm = T),
              Snow_ID = weighted.mean(Snow_ID, Weights, na.rm = T),
              Hail_ID = weighted.mean(Hail_ID, Weights, na.rm = T),
              Thunder_ID = weighted.mean(Thunder_ID, Weights, na.rm = T),
              Gale_ID = weighted.mean(Gale_ID, Weights, na.rm = T),
              WMO_Sunshine_Dur = weighted.mean(WMO_Sunshine_Dur, Weights, na.rm = T),
              Derived_D_Sun_dur = weighted.mean(Derived_D_Sun_dur, Weights, na.rm = T),
              Wind_speed = weighted.mean(Wind_speed, Weights, na.rm = T),
              Air_Temperature = weighted.mean(Air_Temperature, Weights, na.rm = T),
              Humidity = weighted.mean(Humidity, Weights, na.rm = T),
              Amount_10CM = weighted.mean(Amount_10CM, Weights, na.rm = T),
              Amount_30CM = weighted.mean(Amount_30CM, Weights, na.rm = T),
              Amount_100CM = weighted.mean(Amount_100CM, Weights, na.rm = T),
              Max_Air_09_21 = weighted.mean(Max_Air_09_21, Weights, na.rm = T),
              Min_Air_09_21 = weighted.mean(Min_Air_09_21, Weights, na.rm = T),
              Max_Air_21_09 = weighted.mean(Max_Air_21_09, Weights, na.rm = T),
              Min_Air_21_09 = weighted.mean(Min_Air_21_09, Weights, na.rm = T),
              Max_Air_09_09 = weighted.mean(Max_Air_09_09, Weights, na.rm = T),
              Min_Air_09_09 = weighted.mean(Min_Air_09_09, Weights, na.rm = T)
        )
      
      Correl <- merge(TotalCons, Weather)
      Correl$Week  <- lubridate::week(Correl$Date)
      Correl$Month <- lubridate::month(Correl$Date)
      Correl$Year  <- lubridate::year(Correl$Date)
      Correl$Cons  <- Correl[,column]*24
    }
    
    if (temporal == "perweek"){
      
      Correl <- as.data.frame(Correl[,-1] %>% 
                                group_by(Week, Year) %>% 
                                summarise_all(funs(mean(., na.rm = !all(is.na(.))))))      
    }
    
    if (temporal == "permonth"){
      
      Correl <- as.data.frame(Correl[,-1] %>% 
                                group_by(Month, Year) %>% 
                                summarise_all(funs(mean(., na.rm = !all(is.na(.))))))      
    }
    
    Correl$Date <- as.Date(Correl$Date)
    Correl
  } else {NULL}
}

cor_r <- function(dfT, w_df, area, x, y){
  
  Correl <- Cor(dfT, "AbsConsumption", w_df, area)
  
  Days <- length(unique(Correl$Date))
  Properties <- length(unique(dfT$ID))
  
  cor.k <- Kendall(Correl[,x], Correl[,y])
  cor.s <- cor.test(Correl[,x], Correl[,y], method = "spearman")
  
  if ("RelativeHumidity" %in% colnames(Correl)){
    Cor_m <- ddply(Correl, .(Y_M), plyr::summarize, Cons = mean(Cons), RelativeHumidity = mean(RelativeHumidity))
  }else{
    Cor_m <- ddply(Correl, .(Y_M), plyr::summarize, Cons = mean(Cons), MAX_T_C = mean(MAX_T_C), Precipitation_mm = mean(Precipitation_mm))
  }
  cor_m.k <- Kendall (Cor_m[,x], Cor_m[,y])
  cor_m.s <- cor.test(Cor_m[,x], Cor_m[,y], method = "spearman")
  
  c(Properties, Days, format(round(as.numeric(cor.k[1]),2), nsmall =2),
    format(round(as.numeric(cor.s$estimate),2), nsmall =2), format(round(as.numeric(cor.s$p.value),2), nsmall =2),
    format(round(as.numeric(cor_m.k[1]),2), nsmall =2),
    format(round(as.numeric(cor_m.s$estimate),2), nsmall =2), format(round(as.numeric(cor_m.s$p.value),2), nsmall =2))
}

cor_r_d <- function(dfT, w_df, area, x, y){
  
  Correl <- Cor(dfT, w_df, area)
  
  Days <- length(unique(Correl$Date))
  Properties <- length(unique(dfT$ID))
  
  cor.k <- Kendall(Correl[,x], Correl[,y])
  cor.s <- cor.test(Correl[,x], Correl[,y], method = "spearman")
  
  c(Properties, Days, format(round(as.numeric(cor.k[1]),2), nsmall =2),
    format(round(as.numeric(cor.s$estimate),2), nsmall =2), format(round(as.numeric(cor.s$p.value),2), nsmall =2))
}

cor_r_p <- function(dfT, w_df, area, x, y){
  
  Correl <- Cor(dfT, w_df, area)
  
  Days <- length(unique(Correl$Date))
  Properties <- length(unique(dfT$ID))
  
  m <- nrow(Correl)
  
  Correl$Precipitation_1 <- c(0, Correl$Precipitation_mm[1:(m-1)])
  Correl$Precipitation_2 <- c(0, 0, Correl$Precipitation_mm[1:(m-2)])
  Correl$Precipitation_3 <- c(0, 0, 0, Correl$Precipitation_mm[1:(m-3)])
  Correl$Precipitation_4 <- c(0, 0, 0, 0, Correl$Precipitation_mm[1:(m-4)])
  Correl$Precipitation_5 <- c(0, 0, 0, 0, 0, Correl$Precipitation_mm[1:(m-5)])
  Correl$Precipitation_6 <- c(0, 0, 0, 0, 0, 0, Correl$Precipitation_mm[1:(m-6)])
  Correl$Precipitation_7 <- c(0, 0, 0, 0, 0, 0, 0, Correl$Precipitation_mm[1:(m-7)])
  Correl$Precipitation_8 <- c(0, 0, 0, 0, 0, 0, 0, 0, Correl$Precipitation_mm[1:(m-8)])
  Correl$Precipitation_9 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, Correl$Precipitation_mm[1:(m-9)])
  
  Correl$Days_P_1 <- Correl$Precipitation_1
  Correl$Days_P_2 <- Correl$Precipitation_1 + Correl$Precipitation_2
  Correl$Days_P_3 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3
  Correl$Days_P_4 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3 + Correl$Precipitation_4
  Correl$Days_P_5 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3 + Correl$Precipitation_4 + Correl$Precipitation_5
  Correl$Days_P_6 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3 + Correl$Precipitation_4 + Correl$Precipitation_5 + Correl$Precipitation_6
  Correl$Days_P_7 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3 + Correl$Precipitation_4 + Correl$Precipitation_5 + Correl$Precipitation_6 + Correl$Precipitation_7
  Correl$Days_P_8 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3 + Correl$Precipitation_4 + Correl$Precipitation_5 + Correl$Precipitation_6 + Correl$Precipitation_7 + Correl$Precipitation_8
  Correl$Days_P_9 <- Correl$Precipitation_1 + Correl$Precipitation_2 + Correl$Precipitation_3 + Correl$Precipitation_4 + Correl$Precipitation_5 + Correl$Precipitation_6 + Correl$Precipitation_7 + Correl$Precipitation_8 + Correl$Precipitation_9
  
  Correl <- Correl[10:nrow(Correl), c(1,3:6, 8, 10:12, 22:30)]
  
  cor.k <- Kendall(Correl[,x], Correl[,y])
  cor.s <- cor.test(Correl[,x], Correl[,y], method = "spearman")
  
  c(Properties, Days,  format(round(as.numeric(cor.k[1]),2), nsmall =2),
    format(round(as.numeric(cor.s$estimate),2), nsmall =2), format(round(as.numeric(cor.s$p.value),2), nsmall =2))
}

timeofday <- function(timeofday){
  if (timeofday == "all"){
    timeA     <- "00:00:00"
    timeB     <- "23:45:00"
  }else if (timeofday == "morning"){
    timeA     <- "06:00:00"
    timeB     <- "12:00:00"
  }else if (timeofday == "afternoon"){
    timeA     <- "12:00:00"
    timeB     <- "18:00:00"
  }else if (timeofday == "evening"){
    timeA     <- "18:00:00"
    timeB     <- "23:45:00"
  }else if (timeofday == "night"){
    timeA     <- "00:00:00"
    timeB     <- "06:00:00"
  }
  c(timeA,timeB)
}

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

getSeason <- function(month) {
  if (length(month)>0){
    for (i in 1:length(month)){
      if (month[i] == 12 | month[i] == 01 | month[i] == 02){
        month[i] = "Winter"
      }else if (month[i] == 03 | month[i] == 04 | month[i] == 05){
        month[i] = "Spring"
      }else if (month[i] == 06 | month[i] == 07 | month[i] == 08){
        month[i] = "Summer"
      }else if (month[i] == 09 | month[i] == 10 | month[i] == 11){
        month[i] = "Autumn"
      }
    }
    month
  }
}

rmse <- function(error){
  sqrt(mean(error^2))
}

mae <- function(error){
  mean(abs(error))
}

lagged_weather <- function(df, nodays){
  #Consider weather for last n days
  #Choose how many days
  var_SunD <- paste("Sunshine_Dur_", nodays, sep = "")
  var_Radi <- paste("Radiation_", nodays, sep = "")
  var_Rain <- paste("Rainfall_", nodays, sep = "")
  var_AirT <- paste("Air_Temperature_", nodays, sep = "")
  var_Humi <- paste("Humidity_", nodays, sep = "")
  var_10cm <- paste("Amount_10CM_", nodays, sep = "")
  var_Max21_09 <- paste("Max_Air_21_09_", nodays, sep = "")
  var_Max09_21 <- paste("Max_Air_09_21_", nodays, sep = "")
  var_Max09_09 <- paste("Max_Air_09_09_", nodays, sep = "")
  
  #Create your columns
  df[, var_SunD] <- NA
  df[, var_Radi] <- NA
  df[, var_Rain] <- NA
  df[, var_AirT] <- NA
  df[, var_Humi] <- NA
  df[, var_10cm] <- NA
  df[, var_Max21_09] <- NA
  df[, var_Max09_21] <- NA
  df[, var_Max09_09] <- NA
  
  for (i in nodays:nrow(df)){
    
    Sun <- df$Sunshine_Dur[(i-(nodays-1)):i]
    df[,var_SunD][i] <- mean(Sun)  
    
    Radiat <- df$Radiation[(i-(nodays-1)):i]
    df[,var_Radi][i] <- mean(Radiat)  
    
    Rain <- df$Rainfall[(i-(nodays-1)):i]
    df[,var_Rain][i] <- mean(Rain)
    
    AirT <- df$Air_Temperature[(i-(nodays-1)):i]
    df[,var_AirT][i] <- mean(AirT)  
    
    Humi <- df$Humidity[(i-(nodays-1)):i]
    df[,var_Humi][i] <- mean(Humi)  
    
    Am10cm <- df$Amount_10CM[(i-(nodays-1)):i]
    df[,var_10cm][i] <- mean(Am10cm)
    
    MaxT <- df$Max_Air_21_09[(i-(nodays-1)):i]
    df[,var_Max21_09][i] <- mean(MaxT)
    
    MaxT <- df$Max_Air_09_21[(i-(nodays-1)):i]
    df[,var_Max09_21][i] <- mean(MaxT)
    
    MaxT <- df$Max_Air_09_09[(i-(nodays-1)):i]
    df[,var_Max09_09][i] <- mean(MaxT)
    
  }
  df
}

replaceNA<-function(x){
  if(all(is.na(x))){
    rep(0,length(x)) 
  } else x
  
} 

MAPE_hours <- function(n, Correl, dataSet, ...){
  
  dots <- c(...)
  dotsNo <- length(dots)
  
  #  Correl$Hour <- 1:nrow(Correl)
  nCor <- nrow(Correl)
  
  hours_f <- data.frame(matrix(ncol = 24, nrow = nCor))
  colnames(hours_f) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7",
                         "X8","X9","X10","X11","X12","X13","X14","X15","X16",
                         "X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24")
  
  hours_f$X1 <- Correl$Cons
  hours_f$X2 <- c(Correl$Cons[-1], NA)
  
  for (i in 3:24){
    hours_f[ , paste("X", i, sep="")] <- c(Correl$Cons[-c(1:(i-1))], rep(NA, (i-1)))
  }
  
  Hour <- n 
  m <- Hour + 24 
  hours_f[ , paste("X", m, sep="")] <- c(Correl$Cons[-c(1:(m-1))], rep(NA, (m-1)))
  
  if (dotsNo>=1){
    for (i in 1:dotsNo){
      hours_f[,dots[i]] <- c(Correl[,dots[i]][m:nrow(Correl)], rep(NA,(m-1)))
    }
  }
  
  hours_f$Hours <- c(as.factor(Correl$Hours[m:nrow(Correl)]), rep(NA,(m-1)))
  
  #Create calibration and validation datasets    
  hours_f <- na.omit(hours_f)
  k <- nrow(hours_f)
  cal <- na.exclude(hours_f[1:round(k*7/10),])
  val <- na.exclude(hours_f[round(k*7/10):k,])
  
  # Choose your variables
  variables <- names(cal) 
  variables <- variables[!variables %in% c(paste("X", m, sep=""), "Date", dots)]
  variables <- paste(variables, collapse = "+")
  rf.form <- as.formula(paste(paste("X", m, sep=""), variables, sep="~"))
  ###################################################################
  fit <-  randomForest(rf.form, 
                       data=cal,
                       importance = TRUE,
                       ntree = 5000)
  
  #Choose cal/val dataset
  dataSet <- val
  dataSet[ , "Prediction"] <- predict(fit, dataSet)
  dataSet[ , "error"] <-  dataSet[ , "Prediction"] - dataSet[,paste("X", m, sep="")]
  dataSet[ , "error_perc"] <- dataSet[ , "error"]/dataSet[,paste("X", m, sep="")] *100
  
  MAPE(dataSet[ , "Prediction"], dataSet[, paste("X", m, sep="")])
}

MAPE_days <- function(n, Correl, dataSet, ...){
  
  dots <- c(...)
  dotsNo <- length(dots)
  
  ###### For 8 days - rolling daily
  Correl <- cbind(Correl, Weekday = weekdays(as.Date(Correl$Date)))
  
  Correl <- cbind(Correl, W_Y = paste(Correl$Week, Correl$Year))
  Correl$Date <- as.Date(Correl$Date)
  
  start = as.Date(Correl[1,1])
  end = as.Date(Correl[nrow(Correl),1])
  full <- data.frame(Date = seq(start, end, by='1 day'))
  
  Correl <- merge (full, Correl, all = TRUE)
  nCor <- nrow(Correl)
  Correl$Day <- rep_len(1:8, nrow(Correl))
  
  Correl <- cbind(Correl, Weekend = chron::is.weekend(Correl$Date))
  Correl <- cbind(Correl, Holiday = is.holiday((Correl$Date), chron(as.character(holidayLONDON(year = 2008:2017)),format = "y-m-d")))
  Correl <- transform(Correl, Working = ifelse(Weekend == TRUE, Weekend, Holiday))
  
  days_f <- data.frame(matrix(ncol = 7, nrow = nCor))
  colnames(days_f) <- paste0("X",1:7)
  
  days_f$X1 <- Correl$Cons
  days_f$X2 <- c(Correl$Cons[-1], NA)
  
  for (i in 3:7){
    days_f[ , paste("X", i, sep="")] <- c(Correl$Cons[-c(1:(i-1))], rep(NA, (i-1)))
  }
  
  m <- n + 7 
  days_f[ , paste("X", m, sep="")] <- c(Correl$Cons[-c(1:(m-1))], rep(NA, (m-1)))
  
  if (dotsNo>=1){
    for (i in 1:dotsNo){
      days_f[,dots[i]] <- c(as.factor(Correl[,dots[i]][m:nrow(Correl)]), rep(NA,(m-1)))
    }
  }
  days_f$Date <- c(Correl$Date[m:nrow(Correl)], rep(NA,(m-1)))
  
  #Create calibration and validation datasets    
  days_f <- na.omit(days_f)
  k <- nrow(days_f)
  cal <- na.exclude(days_f[1:round(k*8/10),])
  val <- na.exclude(days_f[round(k*8/10):k,])
  
  if(dataSet == "cal"){
    dataS <-  cal  
  }else if(dataSet == "val"){
    dataS <-  val
  }
  
  # Choose your variables
  variables <- names(cal) 
  variables <- variables[!variables %in% c(paste("X", m, sep=""),"Date")]
  variables <- paste(variables, collapse = "+")
  rf.form <- as.formula(paste(paste("X", m, sep=""), variables, sep="~"))
  ###################################################################
  fit <-  randomForest(rf.form, 
                       data=cal,
                       ntree = 1000,
                       nodesize = 5,
                       mtry = 7)
  
  #####Cal/Val data#####
  #Choose cal/val dataset
  dataS[ , "Prediction"] <- predict(fit, dataS)
  dataS[ , "error"] <-  dataS[ , "Prediction"] - dataS[, paste("X", m, sep="")]
  dataS[ , "error_perc"] <- dataS[ , "error"]/dataS[, paste("X", m, sep="")] *100
  
  lm_f <- as.formula(paste(paste("X", m, sep=""), "Prediction", sep="~"))
  lm_pred <- lm(lm_f, data=dataS)
  
  c(MAPE(dataS[ , "Prediction"], dataS[, paste("X", m, sep="")]),
    MAE(dataS[ , "Prediction"], dataS[, paste("X", m, sep="")]),
    NSE (dataS[ , "Prediction"], dataS[, paste("X", m, sep="")]),
    RMSE(dataS[ , "Prediction"], dataS[, paste("X", m, sep="")]),
    summary(lm_pred)$r.squared)
}

Scatter_Correl <- function(segmentation, final, No){
  
  segmentation <- aggregate(formula(paste0("AbsConsumption", "~Date+ID")), segmentation, FUN = (mean))
  x <- table(segmentation$Date)
  
  segmentation <- segmentation[segmentation$Date %in% names(x[x >= 60]), ]
  
  segmentation <- na.omit(segmentation)
  segmentation <- cbind(segmentation, Postcode = final$postcode[match(segmentation$ID, 
                                                                      paste('ID', final$ID, sep = ''))])
  segmentation <- cbind(segmentation, Week  = lubridate::week(segmentation$Date))
  segmentation <- cbind(segmentation, Month = lubridate::month(segmentation$Date))
  segmentation <- cbind(segmentation, Year = lubridate::year(segmentation$Date))
  
  ##Weighted averages
  weightsT <- as.data.frame(table(final$postcode))
  closest <- data.frame(cbind(rownames(distances), colnames(distances)[apply(distances, 1, which.min)]))
  weightsT <- merge(weightsT, closest, by.x = "Var1", by.y = "X1")
  colnames(weightsT) <- c("Postcodes", "Properties", "Station")
  
  testD <- Cor(segmentation, "AbsConsumption", distances, Daily , weightsT, final, "perday")
  testD$type <- No
  testD
}

Cor_Individual <- function(dfT, column, Weather, temporal){
  
  if (nrow(Weather)>0){
    
    TotalCons <- aggregate(formula(paste0(column, "~Date+ID")), dfT, FUN = (mean), na.rm=TRUE)
    
    Correl <- unique(merge(TotalCons, Weather, by = "Date", all=FALSE))
    
    Correl$Week  <- lubridate::week(Correl$Date)
    Correl$Month <- lubridate::month(Correl$Date)
    Correl$Year  <- lubridate::year(Correl$Date)
    
    Correl$Cons  <- Correl[,column]*24
    
    if (temporal == "perweek"){
      
      Correl <- as.data.frame(Correl[,-1] %>% 
                                group_by(Week, Year, ID) %>% 
                                summarise_all(funs(mean(., na.rm = !all(is.na(.))))))      
    }
    
    if (temporal == "permonth"){
      
      Correl <- as.data.frame(Correl[,-1] %>% 
                                group_by(Month, Year, ID) %>% 
                                summarise_all(funs(mean(., na.rm = !all(is.na(.))))))      
    }
    
    Correl <- Correl[!is.na(Correl$Date), ]
    Correl$Season <- getSeason(as.numeric(Correl$Month))
    Correl
  } else {NULL}
}

MAPE_days_Individual <- function(Days_pred, Correl, variables, Notrees, Node_size, NoVar){
  
  #  Split on userid
  Correl_St <- split(Correl, f = Correl$ID )
  
  ###### For 8 days - rolling daily
  Correl_St <- lapply(Correl_St, function(x) cbind(x, Weekday = weekdays(as.Date(x$Date))))
  
  ##############################################
  days_f <- list()
  
  for (kk in 1:length(Correl_St)){
    if (nrow(Correl_St[[kk]])>7){
      
      start <- min(as.Date(Correl_St[[kk]]$Date))
      end   <- max(as.Date(Correl_St[[kk]]$Date))
      full  <- data.frame(Date = seq(start, end, by= "1 day"))
      
      Correl_St[[kk]] <- merge (full, Correl_St[[kk]], all = TRUE)
      
      days_f[[kk]] <- data.frame(matrix(ncol = 7, nrow = nrow(Correl_St[[kk]])))
      colnames(days_f[[kk]]) <- paste0("X", 1:7)
      
      days_f[[kk]]$X1 <- Correl_St[[kk]]$Cons
      days_f[[kk]]$X2 <- c(Correl_St[[kk]]$Cons[-1], NA)
      
      for (i in 3:7){
        days_f[[kk]][ , paste("X", i, sep="")] <- c(Correl_St[[kk]]$Cons[-c(1:(i-1))], rep(NA, (i-1)))
      }
      
      m <- Days_pred + 7
      predDay <- paste("X", m, sep="")
      
      days_f[[kk]][ , predDay] <- c(Correl_St[[kk]]$Cons[-c(1:(m-1))], rep(NA, (m-1)))
      
      days_f[[kk]]$Date <- c(Correl_St[[kk]]$Date[m:nrow(Correl_St[[kk]])], rep(NA,(m-1)))
      days_f[[kk]] <- na.omit(days_f[[kk]])
      
      #Change characters to factors
      days_f[[kk]] <- as.data.frame(unclass(days_f[[kk]]))
      
      Correl_St[[kk]] <- merge(days_f[[kk]], Correl_St[[kk]])
    }
  }
  
  #Join the list
  days_f <- rbind.fill(Correl_St)
  
  #Divide cal and val datasets
  days_f <- days_f[, c(variables, "Date", predDay)]
  days_f <- na.exclude(days_f)
  k <- nrow(days_f)
  
  #Change characters to factors
  days_f <- as.data.frame(unclass(days_f))
  
  HH_cal <- days_f[1:round(k*6/10),]
  HH_val <- days_f[round(k*6/10):k,]
  
  variables <- paste(variables, collapse = "+")
  rf.form   <- as.formula(paste(predDay, variables, sep="~"))
  
  #############
  fit <-  randomForest(rf.form, 
                       data=HH_cal,
                       ntree = NoTrees,
                       nodesize = NodeSize,
                       mtry = MTry)
  #####Cal/Val data#####
  #Choose cal/val dataset
  HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
  HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[, predDay]
  HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, predDay] *100
  
  HH_val[ , "Prediction"] <- predict(fit, HH_val)
  HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, predDay]
  HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, predDay] *100
  
  #########
  c(
    MAPE(unlist(HH_cal[ , "Prediction"]), unlist(HH_cal[, predDay])),
    MAPE(unlist(HH_val[ , "Prediction"]), unlist(HH_val[, predDay]))
  )}

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

predict_HH_df <- function(Correl, Days_pred, dots, splitCol) {
  
  #  Split on userid
  Correl_St <- split(Correl, f = Correl[, splitCol] )
  
  # Variables
  dotsNo <- length(dots)
  socialNo <- length(social)
  
  # 8 days rolling daily
  days_f <- list()
  types <- length(Correl_St)
  
  for (kk in c(1:types)){
    if (nrow(Correl_St[[kk]])>14){
      
      start <- min(as.Date(Correl_St[[kk]]$Date))
      end   <- max(as.Date(Correl_St[[kk]]$Date))
      full  <- data.frame(Date = seq(start, end, by= "1 day"))
      
      Correl_St[[kk]] <- merge (full, Correl_St[[kk]], all = TRUE)
      Correl_St[[kk]]$Day <- rep_len(1:8, nrow(Correl_St[[kk]]))
      
      days_f[[kk]] <- data.frame(matrix(ncol = 7, nrow = nrow(Correl_St[[kk]])))
      colnames(days_f[[kk]]) <- paste0("X", 1:7)
      
      days_f[[kk]]$X1 <- Correl_St[[kk]]$ConsPC
      days_f[[kk]]$X2 <- c(Correl_St[[kk]]$ConsPC[-1], NA)
      
      for (i in 3:7){
        days_f[[kk]][ , paste("X", i, sep="")] <- c(Correl_St[[kk]]$ConsPC[-c(1:(i-1))], rep(NA, (i-1)))
      }
      
      m <- Days_pred + 7 
      
      days_f[[kk]][ , paste("X", m, sep="")] <- c(Correl_St[[kk]]$ConsPC[-c(1:(m-1))], rep(NA, (m-1)))
      
      if (dotsNo>=1){
        for (i in 1:dotsNo){
          days_f[[kk]][,dots[i]] <- c((Correl_St[[kk]][,dots[i]][m:nrow(Correl_St[[kk]])]), rep(NA,(m-1)))
        }
      }
      days_f[[kk]]$Date <- c(Correl_St[[kk]]$Date[m:nrow(Correl_St[[kk]])], rep(NA,(m-1)))
      
      if (socialNo>=1){
        days_f[[kk]][,social] <- Correl_St[[kk]][ , social]
      }
    }
  }
  
  #Join the list
  days_f <- rbind.fill(days_f)
  
  #Create calibration and validation datasets    
  days_f <- na.omit(days_f)
  
  #Change characters to factors
  days_f <- as.data.frame(unclass(days_f))
  
  days_f
}

MAPE_days_J <- function(dataS, variables, splitCol, temporal, Days_pred, NoTrees, NodeSize, MTry){
  
  HH <- prepare_HH(dataS, Days_pred, splitCol, temporal)
  
  m <- Days_pred + 7 
  day_pr <- paste("X", m, sep="")
  
  k <- nrow(HH)
  HH_cal <- HH[1:round(k*7/10),]
  HH_val <- HH[round(k*7/10):k,]
  
  predDay <- paste("X", Days_pred+7, sep ="")
  variables <- paste(variables, collapse = "+")
  rf.form   <- as.formula(paste(predDay, variables, sep="~"))
  
  #############
  fit <-  randomForest(rf.form, 
                       data=HH_cal,
                       ntree = NoTrees,
                       nodesize = NodeSize,
                       mtry = MTry)
  
#  fit <- lm(rf.form, data=HH_cal)
  #####Cal/Val data#####
  #Choose cal/val dataset
  HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
  HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[, day_pr]
  HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, day_pr] *100
  
  HH_val[ , "Prediction"] <- predict(fit, HH_val)
  HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, day_pr]
  HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, day_pr] *100
  
  #########
  c(
    MAPE(HH_cal[ , "Prediction"], HH_cal[, day_pr]),
    MSE (HH_cal[ , "Prediction"], HH_cal[, day_pr]),
    rsquare (HH_cal[ , "Prediction"], HH_cal[, day_pr]),
    RMSE(HH_cal[ , "Prediction"], HH_cal[, day_pr]),
    MAPE(HH_val[ , "Prediction"], HH_val[, day_pr]),
    MSE (HH_val[ , "Prediction"], HH_val[, day_pr]),
    rsquare (HH_val[ , "Prediction"], HH_val[, day_pr]),
    RMSE(HH_val[ , "Prediction"], HH_val[, day_pr]))
}

prepare_df <- function(group_var, social, dots, minHHs, Correl, Weather, removeOutliers){
  
  variab <- paste(c(group_var, social), collapse = "+")
  rf.var <- as.formula(paste("ConsPC", variab, sep="~"))
  
  dataS <- cbind(aggregate(rf.var, Correl, mean, na.rm = T), "length" = aggregate(rf.var, Correl, length)[,"ConsPC"])
  dataS <- dataS[dataS$length>minHHs,]
  
  if ("Month" %in% group_var){
    dataS$Month <- sprintf("%02d", as.numeric(dataS$Month))
  }
  
  Weather_cal <- as.data.frame(Weather[,c(dots, group_var)] %>% 
                                 group_by_(.dots= group_var) %>% 
                                 summarise_all(funs(mean(., na.rm = !all(is.na(.))))), stringsasfactors = FALSE)    
  
  dataS <- merge(dataS, Weather_cal)
  
  if(removeOutliers == TRUE){
    dataS <- remove_outliers(dataS, "ConsPC")
    dataS <- dataS[!is.na(dataS[, "ConsPC"]),]
  }
  
  dataS
}

prepare_HH <- function(dataS, Days_pred, splitCol, temporal){
  
  m <- Days_pred + 7 
  day_pr <- paste("X", m, sep="")
  
  #Create your data frame
  HH <- predict_HH_df(dataS, Days_pred, dots, splitCol)
  
  if (length(temporal)>0){
    for (mm in 1:length(temporal)){
      HH[, temporal[mm]] <- Correl[,temporal[mm]][match(HH$Date, Correl$Date)]
    }
  }
  
  HH <- HH[order(HH[,"Date"]), ]
  HH <- as.data.frame(unclass(HH))
  
  HH
}

small_post <- function(HH, Days_pred, variables, Notree, Nodes, Mtry){
  
  variables <- paste(variables, collapse = "+")
  rf.form   <- as.formula(paste("X8", variables, sep="~"))
  
  k <- nrow(HH)
  HH_cal <- HH[1:round(k*7/10),]
  HH_val <- HH[round(k*7/10):k,]
  
  #############
  fit <-  randomForest(rf.form, 
                       data=HH_cal,
                       ntree = Notree,
                       nodesize = Nodes,
                       mtry = Mtry, 
                       localImp = TRUE)
  
  #####Cal/Val data#####
  #Choose cal/val dataset
  #day_pr <- "ConsPC"
  
  HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
  HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[, day_pr]
  HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, day_pr] *100
  
  HH_val[ , "Prediction"] <- predict(fit, HH_val)
  HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, day_pr]
  HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, day_pr] *100
  HH_val[ , "error_naive"] <- mean(HH_val[ , day_pr]) - HH_val[, day_pr]
  HH_val[ , "error_perc_naive"] <- HH_val[ , "error_naive"]/HH_val[, day_pr] *100
  
  c(MAPE(HH_cal[ , "Prediction"], HH_cal[, "X8"]),
    NSE (HH_cal[ , "Prediction"], HH_cal[, "X8"]),
    MAPE(HH_val[ , "Prediction"], HH_val[, "X8"]),
    NSE (HH_val[ , "Prediction"], HH_val[, "X8"]))
}

prepare_HH_post <- function(FFF, Days_pred){
  
  a <- length(FFF)
  m <- Days_pred + 7 
  
  
  for (ii in 1:a){
    
    m <- Days_pred + 7 
    
    if(!is.null(nrow(FFF[[ii]]))){
      
      if(nrow(FFF[[ii]])>m){
        
        # 8 days rolling daily
        start <- min(as.Date(FFF[[ii]]$Date))
        end   <- max(as.Date(FFF[[ii]]$Date))
        full  <- data.frame(Date = seq(start, end, by= "1 day"))
        
        FFF[[ii]] <- merge (full, FFF[[ii]], all = TRUE)
        
        days_f <- data.frame(matrix(ncol = 7, nrow = nrow(FFF[[ii]])))
        colnames(days_f) <- paste0("X", 1:7)
        
        days_f$X1 <- FFF[[ii]]$Cons
        days_f$X2 <- c(FFF[[ii]]$Cons[-1], NA)
        
        for (i in 3:7){
          days_f[ , paste("X", i, sep="")] <- c(FFF[[ii]]$Cons[-c(1:(i-1))], rep(NA, (i-1)))
        }
        
        days_f[ , paste("X", m, sep="")] <- c(FFF[[ii]]$Cons[-c(1:(m-1))], rep(NA, (m-1)))
        days_f$Date <- c(FFF[[ii]]$Date[-c(1:(m-1))], rep(NA,(m-1)))
        
        days_f <- na.omit(days_f)
        
        FFF[[ii]] <- merge(days_f,FFF[[ii]])
        
      }else {FFF[[ii]] <- NA}
    }
  }
  
  FFF <- FFF[lapply(FFF, length) !=1]
  
  HH <- rbindlist(FFF, fill=TRUE)
  
  HH[,"Month"] <- Correl[,"Month"][match(HH$Date, Correl$Date)]
  HH[,"Year"]  <- Correl[, "Year"][match(HH$Date, Correl$Date)]
  HH[,"Type_of_Day"]  <- Correl[, "Type_of_Day"][match(HH$Date, Correl$Date)]
  HH[,"Season"]  <- Correl[, "Season"][match(HH$Date, Correl$Date)]
  
  HH[,"Sunshine"] <- Correl[,"Sunshine"][match(HH$Date, Correl$Date)]
  HH[,"Air_T"]  <- Correl[, "Air_T"][match(HH$Date, Correl$Date)]
  HH[,"Rainfall"]  <- Correl[, "Rainfall"][match(HH$Date, Correl$Date)]
  HH[,"Humidity"]  <- Correl[, "Humidity"][match(HH$Date, Correl$Date)]
  HH[,"Soil_T"] <- Correl[,"Soil_T"][match(HH$Date, Correl$Date)]
  HH[,"Days_without_Rain"]  <- Correl[, "Days_without_Rain"][match(HH$Date, Correl$Date)]
  HH[,"Wind_Speed"]  <- Correl[, "Wind_speed"][match(HH$Date, Correl$Date)]
  HH
}

MAPE_days_post <- function(FFF, Days_pred, variables, NoTrees, NodeSize, MTry){
  
  predDay <- paste("X", Days_pred+7, sep ="")
  HH <- prepare_HH_post(FFF, Days_pred)
  colums <-  c(variables, "Date", predDay)
  
  HH <- HH[, ..colums]
  HH[is.na(HH)] <- 0
  HH <- as.data.frame(unclass(HH))
  #  HH <- na.omit(HH)
  
  k <- nrow(HH)
  HH <- HH[order(HH$Date),]
  HH_cal <- HH[1:round(k*7/10),]
  HH_val <- HH[round(k*7/10):k,]
  
  variables <- paste(variables, collapse = "+")
  rf.form   <- as.formula(paste(predDay, variables, sep="~"))
  
  #############
  fit <-  randomForest(rf.form, 
                       data=HH_cal,
                       ntree = NoTrees,
                       nodesize = NodeSize,
                       mtry = MTry)
  #####Cal/Val data#####
  #Choose cal/val dataset
  HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
  HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[, predDay]
  HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, predDay] *100
  
  HH_val[ , "Prediction"] <- predict(fit, HH_val)
  HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, predDay]
  HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, predDay] *100
  
  #########
  c(
    MAPE(unlist(HH_cal[ , "Prediction"]), unlist(HH_cal[, predDay])),
    MAPE(unlist(HH_val[ , "Prediction"]), unlist(HH_val[, predDay]))
  )
}

MAPE_days_linear <- function(FFF, Days_pred, variables){
  
  predDay <- paste("X", Days_pred+7, sep ="")
  HH <- prepare_HH_post(FFF, Days_pred)
  colums <-  c(variables, "Date", predDay)
  
  HH <- HH[, ..colums]
  HH[is.na(HH)] <- 0
  HH <- as.data.frame(unclass(HH))
  #  HH <- na.omit(HH)
  
  k <- nrow(HH)
  HH <- HH[order(HH$Date),]
  HH_cal <- HH[1:round(k*7/10),]
  HH_val <- HH[round(k*7/10):k,]
  
  variables <- paste(variables, collapse = "+")
  rf.form   <- as.formula(paste(predDay, variables, sep="~"))
  
  #############
  fit <- lm(rf.form, data=HH_cal)

  #####Cal/Val data#####
  #Choose cal/val dataset
  HH_cal[ , "Prediction"] <- predict(fit, HH_cal)
  HH_cal[ , "error"] <-  HH_cal[ , "Prediction"] - HH_cal[, predDay]
  HH_cal[ , "error_perc"] <- HH_cal[ , "error"]/HH_cal[, predDay] *100
  
  HH_val[ , "Prediction"] <- predict(fit, HH_val)
  HH_val[ , "error"] <-  HH_val[ , "Prediction"] - HH_val[, predDay]
  HH_val[ , "error_perc"] <- HH_val[ , "error"]/HH_val[, predDay] *100
  
  #########
  c(
    MAPE(unlist(HH_cal[ , "Prediction"]), unlist(HH_cal[, predDay])),
    MAPE(unlist(HH_val[ , "Prediction"]), unlist(HH_val[, predDay]))
  )
}

rsquare <- function (x,y) cor(x,y)^2