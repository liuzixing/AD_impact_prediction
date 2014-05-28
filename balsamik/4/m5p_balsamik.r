library(caret)
library(RWeka)
#init
data <- read.csv("tidyData.csv",header=TRUE)
#data <- data[data$Visite.gain > 0,]
FulldataSize <- nrow(data)
data <- data[1:FulldataSize,]

data <- data[sample(FulldataSize, FulldataSize),]


#convert to factors  
#,"hour","month","dayofweek","nthweek","length","festival"
cf <- c("TypeWeekDay","channel","DAYPART",
        "MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea"
        ,"tav","tavtap")
data[,cf]<- lapply(data[,cf], as.factor) 

#select features

sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART",
        "MMDAYPART","crea","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD"
        ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","consumption","festival")

#    sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART"
#           ,"MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea","tav","tavtap"
#           ,"budgetbrut","budgetnet","grp","grpref","hour","month","dayofweek","consumption","nthweek")
data <- data[,sf]


index<-1:FulldataSize
traningSize <- as.integer(0.8 * FulldataSize)
subdata <- data[1:FulldataSize,c(-1)]
print (names(subdata))
traindata <- subdata[1:traningSize,]






m5p <- M5P(Visite.gain~ .,data =traindata)

#rpart.plot(rt,digits = 4,fallen.leaves=TRUE,type= 3,extra = 101)
sink("sink-examp.txt", split=TRUE)

print (summary(m5p))
print (mean(data$Visite.gain))
#print (data[data$Visite.gain>3000,])
sink()
#unlink("sink-examp.txt")
# print (anova(lm_model_pow1,lm_model_pow1,test = "Chisq"))
#print (summary(lm_model_pow1))

predictAndPlot(index,subdata,m5p,traningSize,FulldataSize)
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