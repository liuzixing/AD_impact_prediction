library(caret)
library(RWeka)

m5p_balsamik<- function (iii){
  #init
  data <- read.csv("tidyData.csv",header=TRUE)
  #data <- data[data$Visite.gain > 0& data$Visite.gain < 3000,]
  FulldataSize <- nrow(data)
  data <- data[1:FulldataSize,]
  #print(tail(data,n = 40))
  #data <- data[sample(FulldataSize, FulldataSize),]
  
  
  #select features
  #,"CntToHolidays","CntAfterHolidays","cumGrpTotal","cumGrpRefTotal"
  

  sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART",
          "crea","visitByhour","tavtap","cumGrpRef","cumGrp"
          ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","consumption","Holidays")
  data <- data[,sf]
  
  index<-1:FulldataSize
  #traningSize <- as.integer(0.8 * FulldataSize)
  traningSize <- 1666
  subdata <- data[1:FulldataSize,c(-1)]
  #print (names(subdata))
  traindata <- subdata[1:traningSize,]
  
  m5p <- M5P(Visite.gain~ .,data =traindata)
  
  #print (m5p)
#imp(m5p)
  y <- predictAndPlot(index,subdata,m5p,traningSize,FulldataSize)
  return (y)
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
  print (pre[testSize:FulldataSize])
  print(paste("COR",x))
  print(paste("Mean Err",y))
  print(summary(originY[traningSize:FulldataSize]))
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

