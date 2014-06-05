
library(caret)
library(ggplot2)
data <- read.csv("TripAdvisor//tidyData.csv",header=TRUE)

data <- data[1:607,]

#, "festival"
cf<- c( "weekday" ,"month")
#data[,cf]<- lapply(data[,cf], as.factor)

sf <- c("date","sessions","energy.consumption","Holidays","index",
        "nthweek","weekday" , "month")
data <- data[,sf]
subdata <- data[,c(-1)]
#print(head(subdata))
inTrain <- createDataPartition(y=subdata$sessions,p = 0.7,list=FALSE)
training <-  subdata[inTrain,]
testing <-  subdata[-inTrain,]
print (head(training))
trellis.par.set(theme = col.whitebg(), warn = FALSE)
plot(subdata)
featurePlot(x = training[,c("energy.consumption","Holidays","index","nthweek","weekday" , "month")],
            y = training$sessions,
            plot="pairs")

#fit <- train(training$sessions~., method = "lm",data = training) 
#,preProcess = "pca"
#finMod <- fit$finalModel
#pred <- predict(fit,testing)
attach(testing)

qplot(sessions,pred,data = testing)