library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)

#init
data <- read.csv("tidyData.csv",header=TRUE)
#data <- data[data$Visite.gain > 0,]
FulldataSize <- nrow(data)
data <- data[1:FulldataSize,]

data <- data[sample(FulldataSize, FulldataSize),]


#convert to factors  

cf <- c("TypeWeekDay","length","channel","DAYPART",
        "MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea"
        ,"tav","tavtap","hour","month","dayofweek","festival","nthweek")
data[,cf]<- lapply(data[,cf], as.factor) 

#select features

sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART",
        "MMDAYPART","crea"
        ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","consumption","festival")

#    sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART"
#           ,"MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea","tav","tavtap"
#           ,"budgetbrut","budgetnet","grp","grpref","hour","month","dayofweek","consumption")
data <- data[,sf]


index<-1:FulldataSize
traningSize <- as.integer(0.8 * FulldataSize)
subdata <- data[1:FulldataSize,c(-1)]
print (names(subdata))
traindata <- subdata[1:traningSize,]



# ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)
# grid_rf <- expand.grid(.mtry=c(2,4,8,16))
# m_rf <- train(Visite.gain~.+poly(grpref,5)+poly(grp,5)+poly(budgetbrut,5),data = traindata,method = "rf",
#               metric = "Kappa", trControl = ctrl,
#               tuneGrid = grid_rf)



rt <- rpart(Visite.gain~ .,data =traindata)

rpart.plot(rt,digits = 4,fallen.leaves=TRUE,type= 3,extra = 101)
sink("sink-examp.txt", split=TRUE)

print (summary(rt))
print (mean(data$Visite.gain))
#print (data[data$Visite.gain>3000,])
sink()
#unlink("sink-examp.txt")
# print (anova(lm_model_pow1,lm_model_pow1,test = "Chisq"))
#print (summary(lm_model_pow1))

predictAndPlot(index,subdata,rt,traningSize,FulldataSize)
predictAndPlot<-function(index,subdata,model,traningSize,FulldataSize){
  
  test_data <- subdata[,c(-1)]
  pre <-predict(model,test_data)
  originY <- subdata[,1]
  plot(index,originY,type = 'n',col='red',xlab='date',ylab='sessions')
  points(index,originY,col="red",lwd=2)
  points(index[traningSize:FulldataSize],pre[traningSize:FulldataSize],col="blue",lwd=2)
  points(index[1:traningSize],pre[1:traningSize],col="black",lwd=2)
  #print (pre[traningSize:FulldataSize])
  #print (originY[traningSize:FulldataSize])
  x <- cor(originY[traningSize:FulldataSize],pre[traningSize:FulldataSize]) 
  y <- MAE(originY[traningSize:FulldataSize],pre[traningSize:FulldataSize])
  print(paste("COR",x))
  print(paste("Mean Err",y))
}
MAE <- function (actual, pre){
  mean(abs(actual - pre))
}