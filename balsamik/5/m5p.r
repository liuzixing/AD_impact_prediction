library(caret)
library(RWeka)

m5p_balsamik<- function (iii){
  #init
  data <- read.csv("tidyData.csv",header=TRUE)
  #data <-data[-c(1:1000),]
  #data <- data[data$Visite.gain > -50&data$Visite.gain < 1500,]
  FulldataSize <- nrow(data)
  
  
  
  #set.seed(334)
  #print(tail(data,n = 40))
  #data <- data[sample(FulldataSize, FulldataSize),]
  
  
  #select features
  #,"Holidays"
  #
  #print(data$Visite.gain)
  
  sf <- c("date","Visite.gain","TypeWeekDay","length","channel"
          ,"DAYPART","MMDAYPART","cumGrpTotal","cumGrpRefTotal"
          ,"crea","hour","tavtap","tav","cumGrpRef","cumGrp"
          ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek")
  data <- data[,sf]
  
  index<-1:FulldataSize
  traningSize <- as.integer(0.85 * FulldataSize)
#traningSize <- 1666
  subdata <- data[1:FulldataSize,c(-1)]
  #print (names(subdata))
  traindata <- subdata[1:traningSize,]
  #plot(traindata)
  #traindata <- traindata[sample(traningSize, traningSize),]
  #traindata <- traindata[traindata$Visite.gain > 0& traindata$Visite.gain < 3000,]
  m5p <- M5P(Visite.gain~ .,data =traindata)
  
  print (summary(m5p))
  #imp(m5p)
  y <- predictAndPlot(index,subdata,m5p,traningSize,FulldataSize)
  #return (y)
}

predictAndPlot<-function(index,subdata,model,traningSize,FulldataSize){
  
  test_data <- subdata[,c(-1)]
  pre <-predict(model,test_data)
  originY <- subdata[,1]
  testSize <- traningSize+1
  plot(index,originY,type = 'n',col='red',xlab='date',ylab='sessions')
  points(index,originY,col="red",lwd=2)
  points(index[testSize:FulldataSize],pre[testSize:FulldataSize],col="blue",lwd=2)
  points(index[1:traningSize],pre[1:traningSize],col="black",lwd=2)
  #print (pre[traningSize:FulldataSize])
  #print (originY[traningSize:FulldataSize])
  x <- cor(originY[testSize:FulldataSize],pre[testSize:FulldataSize]) 
  y <- MAE(originY[testSize:FulldataSize],pre[testSize:FulldataSize])
  #print (pre[testSize:FulldataSize])
  write.csv(pre[testSize:FulldataSize], "result.csv", row.names=FALSE)
  print(paste("COR",x))
  print(paste("Mean Err",y))
  print(summary(originY[testSize:FulldataSize]))
  print(summary(pre[testSize:FulldataSize]))
  return (y)
}
MAE <- function (actual, pre){
  mean(abs(actual - pre))
}





xx <-sapply(rep(0,1),m5p_balsamik)
#sink("sink-examp.txt", split=TRUE)
print (paste("mean:",mean(xx)))
print (paste("max:",max(xx)))
print (paste("min:",min(xx)))
#sink()            

