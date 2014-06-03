
MAE <- function (actual, pre){
  mean(abs(actual - pre))
}
pre_lm_diff<-function(){
  #init
  data <- read.csv("tidyData.csv",header=TRUE)
  #data <- data[data$Visite.gain > 0 & data$Visite.gain < 3000,]
  #data <- data[data$Visite.gain < 3000 ,]
  FulldataSize <- nrow(data)
  data <- data[1:FulldataSize,]
  
  data <- data[sample(FulldataSize, FulldataSize),]
  
  
  #convert to factors  
  
  cf <- c("date","TypeWeekDay","channel","DAYPART","crea","festival","length"
          ,"tav","tavtap","emav","emap","screen",
            "hour","nthweek")
  data[,cf]<- lapply(data[,cf], as.factor) 
  
  #select features
  
  sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART",
          "crea","visitByhour","tavtap"
          ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","consumption","festival")
  
  #    sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART","gta"
  #           ,"MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea","tav","tavtap"
  #           ,"budgetbrut","budgetnet","grp","grpref","hour","month","dayofweek","consumption")
  data <- data[,sf]
  
  
  index<-1:FulldataSize
  traningSize <- as.integer(0.8 * FulldataSize)
  subdata <- data[1:FulldataSize,c(-1)]
  print (names(subdata))
  traindata <- subdata[1:traningSize,]
  
  #lm_model_pow1 <- lm(Visite.gain~.,data =traindata)
  lm_model_pow2 <- lm(Visite.gain~.+poly(grpref,5)+poly(grp,5)+poly(budgetbrut,5)+poly(visitByhour,5),data =traindata)
  
  #sink("sink-examp.txt", split=TRUE)
  
  print (summary(lm_model_pow2))
  #print (mean(data$Visite.gain))
  #print (data[data$Visite.gain>3000,])
  #sink()
  #unlink("sink-examp.txt")
  # print (anova(lm_model_pow1,lm_model_pow1,test = "Chisq"))
  #print (summary(lm_model_pow1))
  
  predictAndPlot(index,subdata,lm_model_pow2,traningSize,FulldataSize)
  return  (data)
}
predictAndPlot<-function(index,subdata,lm_model,traningSize,FulldataSize){
  
  test_data <- subdata[,c(-1)]
  pre <-predict(lm_model,test_data)
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
d<-pre_lm_diff()