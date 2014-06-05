library(caret)
data <- read.csv("tidyData.csv",header=TRUE)
#data <-data[-c(1:1000),]
data <- data[data$Visite.gain > -50&data$Visite.gain < 2000,]
FulldataSize <- nrow(data)
#select features


sf <- c("date","Visite.gain","cumGrpTotal","cumGrpRefTotal"
        ,"hour","tavtap","cumGrpRef","cumGrp","nthweek"
        ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek")

data <- data[,sf]
index<-1:FulldataSize
traningSize <- as.integer(0.8 * FulldataSize)
#traningSize <- 1666
subdata <- data[1:FulldataSize,c(-1)]
#print (names(subdata))
traindata <- subdata[1:traningSize,]

subsets <- c(1:5, 10, 15, 20, 25)
set.seed(10)
#xtrain <- preProcess(as.matrix(traindata[,-1]))
#x <- predict(xtrain, traindata[,-1])
#print(str(x))
 x <- traindata[,-1]
 y <-traindata[,1]
 ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)
print(lmProfile)
print(head(lmProfile$resample))
par(c(2,1))
plot(lmProfile, type = c("g", "o"))
plot(lmProfile, type = c("g", "o"), metric = "Rsquared")