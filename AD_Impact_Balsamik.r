
Pre_Avant_Apres<-function(){
  library(statmod)
  #init
  data <- read.csv("balsamik//tinyData_visit_gain2.csv",header=TRUE)
  data <- data[,-13]
  print (head(data))
  print (names(data))
  #data$Heure <- as.factor(data$Heure)
  FulldataSize <- nrow(data)
  index<-1:FulldataSize
  traningSize <- as.integer(0.8 * FulldataSize)
  subdata_avant <- data[1:FulldataSize,c(-1,-3)]
  subdata_apres <- data[1:FulldataSize,c(-1,-2)]
  traindata_avant <- subdata_avant[1:traningSize,]
  traindata_apres <- subdata_apres[1:traningSize,]
   
  #build model
  glm_model_avant <- glm(Avant.Visites~.^2,data=traindata_avant,family = poisson)
  glm_model_avant_2 <- glm(Avant.Visites~.,data=traindata_avant,family = gaussian)
  glm_model_avant_3 <- glm(Avant.Visites~.,data=traindata_avant,family = quasipoisson )
  glm_model_apres <- glm(Apres.Visites~(.)^2,data=traindata_apres,family = poisson)
  glm_model_apres_2 <- glm(Apres.Visites~.,data=traindata_apres,family = gaussian)
  glm_model_apres_3 <- glm(Apres.Visites~.,data=traindata_apres,family = quasipoisson )
  
   
  #see avant summary
  cat ("Press [enter] to continue");line <- readline()
  print (summary(glm_model_avant))
  print (anova(glm_model_avant_2,glm_model_avant_3,glm_model_avant, test = "Chisq"))
  plot(glm_model_avant)
  #see apres summary
  
  cat ("Press [enter] to continue");line <- readline()
  print (summary(glm_model_apres))
  print (anova(glm_model_apres_2,glm_model_apres_3,glm_model_apres, test = "Chisq"))
  plot(glm_model_apres)
  test_data_avant <- subdata_avant[,c(-1)]
  test_data_apres <- subdata_apres[,c(-1)]
  pre_avant <-predict(glm_model_avant,test_data_avant,type = "response")
  pre_apres <-predict(glm_model_apres,test_data_apres,type = "response")
  cat ("Press [enter] to continue");line <- readline()
  #print (glm.scoretest(pre_avant))
  
  origin_avant <- subdata_avant[,1]
  origin_apres <- subdata_apres[,1]
  originY<- origin_apres - origin_avant
  pre <- pre_apres - pre_avant
  Plot_Avant_Apres(index,pre,originY,traningSize,FulldataSize)
}



Plot_Avant_Apres<-function(index,pre,originY,traningSize,FulldataSize){
 
  plot(index,originY,type = 'n',col='red',xlab='date',ylab='sessions')
  #points(0:25, rep(0.3, 26), pch = 0:25, bg = "grey")
  points(index,originY,col="red",lwd=2)
  points(index[traningSize:FulldataSize],pre[traningSize:FulldataSize],col="blue",lwd=2)
  points(index[1:traningSize],pre[1:traningSize],col="black",lwd=2)
  #   legend("topleft",                       # x-y coordinates for location of the legend  
  #          legend=c("predicted with traning set", "Real Sessions", "predicted with testing set"),      # Legend labels  
  #          col=c("black", "red", "blue"),   # Color of points or lines  
  #          pch=c(19,19,19),                 # Point type  
  #          lty=c(1,1,1),                    # Line type  
  #          lwd=c(1,1,1),                    # Line width  
  #          title="Time series")  
  
}



predictAndPlot<-function(index,subdata,lm_model,traningSize,FulldataSize){
  
  test_data <- subdata[,c(-1)]
  pre <-predict(lm_model,test_data)
  originY <- subdata[,1]
  plot(index,originY,type = 'n',col='red',xlab='date',ylab='sessions')
  points(index,originY,col="red",lwd=2)
  points(index[traningSize:FulldataSize],pre[traningSize:FulldataSize],col="blue",lwd=2)
  points(index[1:traningSize],pre[1:traningSize],col="black",lwd=2)
  
}


add2features <-function(){
  library(DAAG)
  data <- read.csv("balsamik//tinyData_visit_gain.csv",header=TRUE)
  fea <- read.csv("balsamik//energyx.csv",header=FALSE)
  names(fea) <- c("Date","energy.consumption","festival")
  D3<-merge(data,fea,by = "Date")
  write.csv(D3, "balsamik//tinyData_visit_gain2.csv", row.names=FALSE)
  return (D3)
}

getTidyData <- function(){
  data <- read.csv("balsamik//predict_visit.csv",header=TRUE)
#   First we get a list of all unique classes:
#   lname <- c("Chaine","Version","TAv","EmAv","TAp","EmAp")
#   for (i in 1:6){
#     data <- mapString(data,lname[i])
#   }
  data$Date <- as.Date(data$Date)
  weekday <- as.numeric(format(data$Date,'%w'))
  nthweek <- as.numeric(format(data$Date,'%W'))
  month <- as.numeric(format(data$Date,'%m'))
  D3<-data.frame(data,weekday,nthweek,month)
  D3$GRP<- as.numeric(gsub(",",".",D3$GRP))
  D3$BudgetNet<- as.numeric(gsub(",",".",D3$BudgetNet))
  #visits_gain <- D3$Apres.Visites - D3$Avant.Visites
  #D3<-data.frame(D3,visits_gain)
  D3 <- D3[D3$Isole == 1,]
  D3$Heure <- as.factor(D3$Heure)
  write.csv(D3[,c(1,11,12,2,6:9,17:19)], "balsamik//tinyData_visit_gain.csv", row.names=FALSE)
  D3 <-add2features()
  return (D3)
}

mapString<- function(data,s){
  print (head(data[,c(s)]))
  allClasses <- c(unique(data[,c(s)]))
  
  # Now a named vector mapping classes to numeric values:
  numMap <- 1:length(allClasses)
  names(numMap) <- allClasses
  
  # Now we can use numMap to reassign numeric values
  data[,c(s)] <- numMap[data[,c(s)]]
  
  # clean up
  rm(allClasses)
  return (data)
}

Pre_Avant_Apres();
