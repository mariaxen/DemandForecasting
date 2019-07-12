library(e1071)
library(doParallel)
library(foreach)
library(randomForest)
library(lattice)
library(randomForestExplainer)
library(caret)

#trace(utils:::unpackPkgZip, edit=TRUE)
setwd("U:/Desktop")

### Start with a dataframe called Data that contains your covariates 
### and your output names "out"
head(Correl)

# create cluster for parallel processing
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

# prepare grid for grid search over the hyperparameters
gridControl <-expand.grid(.mtry = c(1:12),
                          .nodesize = round(exp(seq(1.5, 7.5, 0.5))),
                          .w1 = c(1, 1.1, 1.2))

# 5 fold cross-validation
nfolds <- 5
folds <- cut(seq(1,nrow(Data)), breaks=nfolds,labels=FALSE)

# count time
start_time <- Sys.time()

# save outputs of cross validation in metrics
# for each is like a loop but in parralell on your computer
metrics <- foreach(
  i=1:nrow(gridControl),
  .combine=rbind,
  .packages = c("randomForest"),
  .export = c("count")) %dopar% {
    
    #c("count")
    
    # create log file in the working directory with follow up progress 
    writeLines(c(""), "log.txt")  
    sink("log.txt", append=TRUE)  
    cat(paste("Starting iteration",i,"\n"))
    sink()
    
    # select parameters for the loop i
    parameters <- gridControl[i,]
    
    CVmetrics <- NULL
    
    # perform nfolds fold cross validation
    for(j in 1:nfolds){
      
      # segment your data by fold 
      testIndexes <- which(folds==j,arr.ind=TRUE)
      testData <- Data[testIndexes, ]
      trainData <- Data[-testIndexes, ]
      
      # fit your model
      fit <- randomForest(out ~., 
                          data = trainData,
                          ntree = 100,
                          nodesize = parameters$.nodesize,
                          mtry = parameters$.mtry    
#                          C = parameters$.C,
#                          classwt = c("A" = parameters$.w1, 
#                                      "B" = 1)
      )

###################################################################
fit <-  randomForest(rf.form, 
                     data=cal,
                     importance = TRUE,
                     ntree = 100)

      pred <- predict(fit, subset(testData, select = -c(out)))
      obs  <- testData$out
      
      # check the confusion matrix
      table(obs,pred)
      
      cm <- caret::confusionMatrix(obs,pred)
      
      CVmetric <- c(cm$byClass["Balanced Accuracy"], cm$byClass["Specificity"])
      
      CVmetrics <- rbind(CVmetrics, CVmetric)
      
    }     
    
    # get the mean of the CV metrics
    metric <- c(mtry = parameters$.mtry,
                nodesize = parameters$.nodesize,
                ntrees = parameters$.ntrees,
                w1 = parameters$.w1,
                round(colMeans(CVmetrics),2))
    
  }

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)

metrics

objectname <- "myname"

save(metrics, file = paste0(objectname,".Rdata"))
write.csv2(metrics, file = paste0(objectname,".csv"))