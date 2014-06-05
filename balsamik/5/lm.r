library(ggplot2)
library(lattice)
library(caret)

  #init
  data <- read.csv("tidyData.csv",header=TRUE)
  #data <- data[data$Visite.gain > -80 & data$Visite.gain < 2000,]
  #data <- data[data$Visite.gain < 3000 ,]
  FulldataSize <- nrow(data)
  data <- data[1:FulldataSize,]
  
  #data <- data[sample(FulldataSize, FulldataSize),]
  
  
  #convert to factors  
  #,"dayofweek"
   cf <- c("date","TypeWeekDay","channel","DAYPART","crea","length"
           ,"tav","tavtap","emav","emap","screen")
   data[,cf]<- lapply(data[,cf], as.factor) 
  
  #select features
#   
#   sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART",
#           "crea","visitByhour","tavtap","Holidays","cumGrpRef","cumGrp","cumGrpTotal","cumGrpRefTotal"
#           ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","consumption")
#,"Holidays","visitByhour","CntToHolidays","CntAfterHolidays"
sf1 <- c("date","Visite.gain","cumGrpRefTotal","hour","DAYPART"
         ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","TypeWeekDay","length",
         "channel","nthweek","tavtap","cumGrpRef","cumGrp","cumGrpTotal")

#,"crea","MMDAYPART"
sf2 <- c("Visite.gain","hour","month","dayofweek","TypeWeekDay","nthweek"
        ,"budgetbrut","budgetnet","MMDAYPART","length","channel","DAYPART"
        ,"visitByhour","tavtap","grp","grpref","cumGrpRef","cumGrp","cumGrpTotal","cumGrpRefTotal")
#,"crea"
  data <- data[,sf1]
  data$index <- 1:FulldataSize
  index<-1:FulldataSize
  traningSize <- as.integer(0.81 * FulldataSize)
  subdata <- data[1:FulldataSize,c(-1)]
  print (names(subdata))
# for (i in sf2){
#   #plot(subdata[,i],subdata$Visite.gain,type = 'p',xlab=i,ylab='Visite.gain')
#   #plot(log(subdata[,i]),log(subdata$Visite.gain),type = 'p',xlab=i,ylab='Visite.gain')
#    print(table(subdata[,i]))
#   cat ("Press [enter] to continue");line <- readline()
# }
#  featurePlot(x = subdata[,c("TypeWeekDay","length","channel")],y = subdata$Visite.gain,plot= "pairs")

  traindata <- subdata[1:traningSize,]
  
  #lm_model_pow1 <- lm(Visite.gain~.,data =traindata)
  lm_model_pow2 <- lm(Visite.gain~poly(budgetbrut,5)+poly(budgetnet,5)+poly(grp,1)+poly(grpref,1)
                      +poly(cumGrpRef,1)
                      +poly(cumGrp,1)+poly(cumGrpTotal,1)+poly(cumGrpRefTotal,1)+poly(hour,1)
                      +poly(dayofweek,1)+cos(cumGrpRef)+sin(cumGrpRefTotal)+
                        (.-budgetbrut-budgetnet-grp-grpref-cumGrpRef
                         -cumGrpTotal-cumGrpRefTotal-hour-cumGrp)
                        ,data =traindata)
  
  #sink("sink-examp.txt", split=TRUE)
  
#   print (summary(lm_model_pow2))
  #print (mean(data$Visite.gain))
  #print (data[data$Visite.gain>3000,])
  #sink()
  #unlink("sink-examp.txt")
  # print (anova(lm_model_pow1,lm_model_pow1,test = "Chisq"))
  #print (summary(lm_model_pow1))d
  
   predictAndPlot(index,subdata,lm_model_pow2,traningSize,FulldataSize)

predictAndPlot<-function(index,subdata,lm_model,traningSize,FulldataSize){
  
  test_data <- subdata[,c(-1)]
  pre <-predict(lm_model,test_data)
  originY <- subdata[,1]
  plot(index,originY,type = 'n',col='red',xlab='index',ylab='sessions')
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
