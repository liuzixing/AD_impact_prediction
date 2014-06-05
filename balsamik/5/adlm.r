
MAE <- function (actual, pre){
  mean(abs(actual - pre))
}
pre_lm_diff<-function(){
  library(earth)
  set.seed(333)
  #init
  data <- read.csv("tidyData.csv",header=TRUE)
  #data <-data[-c(1:1000),]
  data <- data[data$Visite.gain > -100 & data$Visite.gain < 1500,]
  #data <- data[data$Visite.gain < 3000 ,]
  FulldataSize <- nrow(data)
  data <- data[1:FulldataSize,]
  
  #data <- data[sample(FulldataSize, FulldataSize),]
  
  
  #convert to factors  
  #,"hour","nthweek"
  cf <- c("date","TypeWeekDay","channel","DAYPART","crea","length","MMDAYPART"
          ,"tav","tavtap","emav","emap","screen","month","dayofweek")
  data[,cf]<- lapply(data[,cf], as.factor) 
  
  #select features
  
  sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART","nthweek",
          "crea","tavtap","cumGrpRef","cumGrp","cumGrpTotal","cumGrpRefTotal","budgetbrut"
          ,"budgetnet","grp","grpref","month","dayofweek","hour")
  
 #
#noise
#,"CntToHolidays","CntAfterHolidays","MMDAYPART"
  data <- data[,sf]
#   for (i in sf){
#     plot(data[,i],data$Visite.gain,type = 'n',col='red',xlab=i,ylab='Visite.gain')
#     cat ("Press [enter] to continue");line <- readline()
#   }
  #
  
  index<-1:FulldataSize
  traningSize <- as.integer(0.8 * FulldataSize)
  #traningSize <- 1666
  subdata <- data[1:FulldataSize,c(-1)]
  print (names(subdata))
  traindata <- subdata[1:traningSize,]
  #traindata <- traindata[sample(traningSize, traningSize),]

  adlm <- earth(
    Visite.gain~., # the formular for prediction
    traindata, # the training dataset
    trace=0.5, # provide overview information during model building
    nk=100, # the maximum number of terms
    degree=2, # the maximum number of interaction (degrees of freedom)
    penalty=1, # penality per knot for GCV during pruning
    thresh=0.001, # minimum change in SSR in forward stage
    minspan=1, # minimum distance between knots in the model
    fast.k=0, # disable Fast MARS adding multiple terms per forward step
    fast.beta=0, # aging coefficient used in Fast MARS
    pmethod="forward") # pruning method during backward pass
  
  #sink("sink-examp.txt", split=TRUE)
  
  print (summary(adlm))
  print(evimp(adlm))
  #print (mean(data$Visite.gain))
  #print (data[data$Visite.gain>3000,])
  #sink()
  #unlink("sink-examp.txt")
  # print (anova(lm_model_pow1,lm_model_pow1,test = "Chisq"))
  #print (summary(lm_model_pow1))
  
  predictAndPlot(index,subdata,adlm,traningSize,FulldataSize)
  #plotmo(adlm)
  return  (data)
}

predictAndPlot<-function(index,subdata,model,traningSize,FulldataSize){
  library(earth)
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
  print(paste("COR",x))
  print(paste("Mean Err",y))
  print(summary(originY[testSize:FulldataSize]))
  print(summary(pre[testSize:FulldataSize]))
  return (y)
}
d<-pre_lm_diff()