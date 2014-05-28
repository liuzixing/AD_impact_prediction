
MAE <- function (actual, pre){
  mean(abs(actual - pre))
}
pre_lm_diff<-function(){
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
  
  #lm_model_pow1 <- lm(Visite.gain~.,data =traindata)
  lm_model_pow2 <- lm(Visite.gain~.,data =traindata)

  sink("sink-examp.txt", split=TRUE)
  
  print (summary(lm_model_pow2))
  print (mean(data$Visite.gain))
  #print (data[data$Visite.gain>3000,])
  sink()
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
#dd <-getTidyData() 





getTidyData <- function(){
  data <- read.csv("Balsamik_2.csv",header=TRUE)
  #"date","TypeWeekDay","length","channel","DAYPART","MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea",
  #"budgetbrut","budgetnet","grp","grpref","tav","tavtap","EmavShort","hour","year","month","dayofweek","Visite.gain","datetime"
  s <- c("date","Visite.gain","TypeWeekDay","length","channel","DAYPART","MMDAYPART","ChaineEcranWD","ChaineDaypartWD","ChaineMMDaypartWD","crea"
         ,"budgetbrut","budgetnet","grp","grpref","tav","tavtap","EmavShort","hour","month","dayofweek","consumption","nthweek")
  
  library(reshape2)
  data <- data[1182:1666,]
  
  energy <- read.csv("energy_byhours.csv",header=TRUE)
  names(energy) <- gsub("X", "", names(energy))
  names(energy) <- gsub("\\.", ":", names(energy))
  #energy <- melt(energy, date=c("date"))
  newenergy <- reshape(energy, 
                       varying = names(energy)[-1], 
                       v.names = "consumption",
                       timevar = "time", 
                       times = names(energy)[-1], 
                       direction = "long")
  energy <- newenergy[order(newenergy$Date),]
  
  energy$Date <- paste(energy$Date,energy$time)
  energy <- energy[,c(-2,-4)]
  rownames(energy) <- NULL
  
  print(head(energy))
  data$datetime <- paste(data$date,data$datetime)
  #   posb <- as.POSIXlt(data$datetime,format="%Y-%m-%d %H:%M")
  #   x <- (posb$min < 30)
  #   posb$min[x] <- 30
  data <- within(data,{
    posb <- as.POSIXlt(datetime,format="%Y-%m-%d %H:%M")
    
    x <- (posb$min < 30)
    posb$min[x] <- 30
    y <- (posb$min > 30 )
    posb$min[y] <- 0
    posb$hour[y] <- posb$hour[y] + 1
    
    Date <- format(posb, "%Y-%m-%d %H:%M")
    nthweek <- format(as.Date(posb), "%W")
    posb <- NULL  # cleanup
    x <- NULL
    y <- NULL
  })
  #print(head(data))
  
  data <-merge(data,energy,by="Date")
  
  data <- data[,s]
  print(head(data))
  #add festival
  festival <- read.csv("energyx.csv",header=FALSE)
  names(festival) <- c("date","energy.consumption","festival")
  D3<-merge(data,festival[,c(-2)],by = "date")
  
  write.csv(D3, "tidyData.csv", row.names=FALSE)
  return (D3)
}

#ddd<-getTidyData()

