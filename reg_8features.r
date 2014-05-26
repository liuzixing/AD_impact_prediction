library(DAAG)
library(abind)
traningSize <- 32000
FulltrainingSet <- 39446
data <- read.csv("DBfinal.csv",header=FALSE)
names (data) <- c("x1","x2","x3","x4","x5","x6","x7","x8","y")
data<-data[,c(9,1:8)]
traindata <- data[1:traningSize,]
#lm_model <- lm(y~poly(x3,8)+poly(x4,10)+poly(x2,5)+poly(x1,2)+sin(x1)+sin(x2)+sin(x3),data=traindata)

#lm_model <- lm(y~poly(x3,8)+poly(x4,10)+poly(x2,5)+poly(x1,2)/
#               +poly(x5+x7+x6+x8,5)+sin(x1)+sin(x2)+sin(x3)+sin(x4)+sin(x5)+sin(x6),data=traindata)

lm_model <- lm(y~poly(x3,8)+poly(x4,10)+poly(x2,5)+poly(x1,2)/
               +poly(x5+x7+x6+x8,5)+sin(x1)+sin(x2)+sin(x3)+sin(x4)+sin(x5)+sin(x6),data=traindata)

subdata <- data[,2:9]
source('C:/Users/zixing.liu/Desktop/regression/create_one_month_data_8f.r')

subdata <- rbind(subdata,f())

pre <-predict(lm_model,subdata)

sum_Y <- tapply(data$y,data$x1,FUN=sum)
r_pre <- data.frame(Category=factor(subdata$x1), 
                    Frequency=pre)
r_pre<- tapply(r_pre$Frequency, r_pre$Category, FUN=sum)

reshapeSize <- 607
reshapetrain <- 450
index <-1:607
sum_Y<-abind(sum_Y,rep(0,30))
plot(index,sum_Y,type = 'l',col='red',xlab='day',ylab='sessions')
lines(index,sum_Y,col="red")
lines(index[reshapetrain:reshapeSize],r_pre[reshapetrain:reshapeSize],col="blue")
lines(index[1:reshapetrain],r_pre[1:reshapetrain],col="black")

#plot(index,originY,type = 'c',col='red',xlab='foo',ylab='bar')
#lines(index,originY,col="red")
#lines(index[traningSize:FulldataSize],pre[traningSize:FulldataSize],col="blue")
#lines(index[1:traningSize],pre[1:traningSize],col="black")
sum_Y
#plot(lm_model)

#cv.lm(df=traindata, lm_model, m=10) 