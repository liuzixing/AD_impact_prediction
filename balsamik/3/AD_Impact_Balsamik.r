

pre_lm_diff<-function(){
  #init
  data <- read.csv("tidyData.csv",header=TRUE)
  print (head(data))
  FulldataSize <- 2214
  data <- data[1:FulldataSize,]
  data <- data[sample(FulldataSize, FulldataSize),]
  #data$Heure[data$Heure==0]<-24
  
  #convert to factors  
  
  cf <- c("TypeWeekDay","length","channel","DAYPART",
          "MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea"
          ,"tav","tavtap","hour","month","dayofweek")
  data[,cf]<- lapply(data[,cf], as.factor) 

  #select features
  # 
  sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART",
          "MMDAYPART","crea"
         ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek")
#    sf <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART","MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea"
#           ,"budgetbrut","budgetnet","grp","grpref","tav","hour","month","dayofweek","tavtap")
  data <- data[,sf]
  
  
  index<-1:FulldataSize
  traningSize <- as.integer(0.9 * FulldataSize)
  subdata <- data[1:FulldataSize,c(-1)]
  #print (head(subdata))
  traindata <- subdata[1:traningSize,]
  
  #lm_model_pow1 <- lm(Visite.gain~.,data =traindata)
  lm_model_pow2 <- lm(Visite.gain~.^2,data =traindata)

  sink(file = "report//report_pow_test.txt",type = "output") 


  print (summary(lm_model_pow2))

 # print (anova(lm_model_pow1,lm_model_pow1,test = "Chisq"))
  
  #print (summary(lm_model_pow1))
  x<-predictAndPlot(index,subdata,lm_model_pow2,traningSize,FulldataSize)
  #res<-cv.lm(df=traindata, lm_model_pow2, m=5) 
  #summary(res)
  print(x)
 # print(sum(x))  
  sink(NULL,type = "output") 
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
  return (x)
}
getTidyData <- function(){
  data <- read.csv("Balsamik.csv",header=TRUE)
  #"date","TypeWeekDay","length","channel","DAYPART","MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea",
  #"budgetbrut","budgetnet","grp","grpref","tav","tavtap","EmavShort","hour","year","month","dayofweek","Visite.gain"
  s <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART","MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea"
         ,"budgetbrut","budgetnet","grp","grpref","tav","tavtap","EmavShort","hour","month","dayofweek")
  write.csv(data[, s], "tidyData_without_energyx.csv", row.names=FALSE)
  data <-add2features()
  return (data)
}
MAE <- function (actual, pre){
  mean(abs(actual - pre))
}
d<-pre_lm_diff()
#dd <-getTidyData() 

add2features <-function(){
  library(DAAG)
  data <- read.csv("tidyData_without_energyx.csv",header=TRUE)
  fea <- read.csv("energyx.csv",header=FALSE)
  names(fea) <- c("date","energy.consumption","festival")
  D3<-merge(data,fea[,c(-2)],by = "date")
  write.csv(D3, "tidyData.csv", row.names=FALSE)
  return (D3)
}


#ddd<-getTidyData()

