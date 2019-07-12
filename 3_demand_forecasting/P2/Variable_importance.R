#Divide calibration and validation datasets
k <- nrow(HH)
HH_cal <- HH[1:round(k*7/10),]
HH_val <- HH[round(k*7/10):k,]

#Individual Variable Importance and effect
y = HH_val$X8
X = HH_val[,-c(8, 13, 16)]
mod = Predictor$new(fit, data = X, y = y)

#Plot feature importance - IML
#Compute feature importances as the performance drop in mean absolute error
imp = FeatureImp$new(mod, loss = "mse")
plot(imp)

#ICE with clustering (all variables)
## Create an 'ice' object for the predictor:
bh.ice = ice(object = fit, X = X, y = y, predictor = c("Rainfall"),
             frac_to_build = .1)
## cluster the curves into groups.
clusterICE(bh.ice, nClusters = 2, plot_legend = TRUE, x_quantile = TRUE)

#Create predictor value based on the validation dataset
mod = Predictor$new(fit, data = HH_val[,c(variables)], y = HH_val$X8)

#Plot variable importance
imp = FeatureImp$new(mod, loss = "mse", compare = "ratio", n.repetitions = 5)
plot1 <- imp$plot()

#Save your result
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("Fig5_2B.png", plot1, width = 3.7, height = 3.7, dpi = 800)

#Create pdp or ale plots
eff = FeatureEffect$new(mod, feature = "Metering_Status", method = "pdp")
eff$plot()

eff = FeatureEffect$new(mod, feature = c("Season", "Humidity"), method = "pdp", grid = 8)
plot(eff)

eff = FeatureEffect$new(mod, feature = c("Rateable_Value", "Type_of_Day"), method = "pdp")
eff$plot()

#Partial dependence
plot(eff) +
  # Adds a title
  ggtitle("Partial dependence") +
  # Adds original predictions
  geom_point(data = HH_cal, aes(y = mod$predict(HH_cal)[[1]], x = Humidity),
             color = "pink", size = 0.5)

#Save your result
setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("Month.png", Month, width = 2.5, height = 2, dpi = 800)