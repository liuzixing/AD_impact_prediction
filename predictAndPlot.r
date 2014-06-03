predictAndPlot<-function(index,subdata,lm_model,traningSize,FulldataSize){
  
  test_data <- subdata[,c(-1)]
  
  pre <-predict(lm_model,test_data)

  originY <- subdata[,1]
  
  plot(index,originY,type = 'l',col='red',xlab='date',ylab='sessions')
  
  lines(index,originY,col="red",lwd=2)
  lines(index[traningSize:FulldataSize],pre[traningSize:FulldataSize],col="blue",lwd=2)
  lines(index[1:traningSize],pre[1:traningSize],col="black",lwd=2)
#   legend("topleft",                       # x-y coordinates for location of the legend  
#          legend=c("predicted with traning set", "Real Sessions", "predicted with testing set"),      # Legend labels  
#          col=c("black", "red", "blue"),   # Color of points or lines  
#          pch=c(19,19,19),                 # Point type  
#          lty=c(1,1,1),                    # Line type  
#          lwd=c(1,1,1),                    # Line width  
#          title="Time series")  
x <- cor(originY[traningSize:FulldataSize],pre[traningSize:FulldataSize]) 
y <- MAE(originY[traningSize:FulldataSize],pre[traningSize:FulldataSize])
print(paste("COR",x))
print(paste("Mean Err",y))
  return (pre)
}
MAE <- function (actual, pre){
  mean((actual - pre) * (actual - pre))
}

predictAndPlotSpecial<-function(index,subdata,lm_model,traningSize,FulldataSize){
  
  test_data_With_GRP <- subdata[,c(-1)]
  test_data_without_GRP<- subdata[,c(-1)]
  test_data_without_GRP[,c("x5","x6")] <- 0
  
  preWithGRP <-predict(lm_model,test_data_With_GRP)
  
  preWithoutGRP <-predict(lm_model,test_data_without_GRP)
  
  originY <- subdata[,1]
  
  plot(index,originY,type = 'l',col='red',xlab='date',ylab='sessions')
  
  lines(index,originY,col="red",lwd=2)
  
 
  preWithoutGRP[preWithoutGRP <= 0] <- 500
  lines(index[1:FulldataSize],preWithoutGRP[1:FulldataSize],col="mediumturquoise",lwd=2)
  lines(index[1:traningSize],preWithGRP[1:traningSize],col="blue",lwd=2)
  lines(index[traningSize:FulldataSize],preWithGRP[traningSize:FulldataSize],col="black",lwd=2)
  
# 
#   legend("topleft",                       # x-y coordinates for location of the legend  
#          legend=c("Real Sessions", "with GRP","Without GRP"),      # Legend labels  
#          col=c("red", "blue","mediumturquoise"),   # Color of points or lines  
#          pch=c(19,19,19),                 # Point type  
#          lty=c(1,1,1),                    # Line type  
#          lwd=c(1,1,1),                    # Line width  
#          title="Time series")  
  #print(preWithGRP)
}

predictAndPlotSpecial2<-function(index,subdata,lm_model,traningSize,FulldataSize){
  
  test_data_With_GRP <- subdata[,c(-1)]
  test_data_without_GRP<- subdata[,c(-1)]
  test_data_without_GRP[,c("x5","x6")] <- 0
  
  preWithGRP <- exp(predict(lm_model,test_data_With_GRP))
  
  preWithoutGRP <-exp(predict(lm_model,test_data_without_GRP))
  
  originY <- subdata[,1]
  
  plot(index,originY,type = 'l',col='red',xlab='date',ylab='sessions')
  
  lines(index,originY,col="red",lwd=2)
  
  
  preWithoutGRP[preWithoutGRP <= 0] <- 500
  lines(index[1:FulldataSize],preWithoutGRP[1:FulldataSize],col="mediumturquoise",lwd=2)
  lines(index[1:traningSize],preWithGRP[1:traningSize],col="blue",lwd=2)
  lines(index[traningSize:FulldataSize],preWithGRP[traningSize:FulldataSize],col="black",lwd=2)
  
  # 
  #   legend("topleft",                       # x-y coordinates for location of the legend  
  #          legend=c("Real Sessions", "with GRP","Without GRP"),      # Legend labels  
  #          col=c("red", "blue","mediumturquoise"),   # Color of points or lines  
  #          pch=c(19,19,19),                 # Point type  
  #          lty=c(1,1,1),                    # Line type  
  #          lwd=c(1,1,1),                    # Line width  
  #          title="Time series")  
  #print(preWithGRP)
}
