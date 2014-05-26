library(DAAG)
traningSize <- 500
FulldataSize <- 632
data <- read.csv("AllDB.csv",header=FALSE)

pdata <-read.csv("DBm.csv",header=FALSE)
names (data) <- c("x1","x2","x3","x4","y")
names (pdata) <- c("x1","x2","x3","x4")
data<-data[,c(5,1:4)]
traindata <- data[1:traningSize,]


lm_model <- lm(y~poly(x3,5)+poly(x4,10)+poly(x2,5)+poly(x1,2),data=traindata)



#lm_model <- lm(y~poly(x1,7)+poly(x2,6)+poly(x3,7)+poly(x4,7)+poly(x4+x1+x2+x3,21),data=traindata)

subdata <- data[,2:5]
subdata <- rbind(subdata,pdata)
pre <-predict(lm_model,subdata)

#zeros <- vector("numeric", length = 31)

presult <-read.csv("theLastMonth2.csv",header=FALSE)
originY <- as.numeric(data[,1])
originY <-c(data[,1],presult[,1])

plot(subdata[,1],originY,type = 'l',col='red',xlab='days',ylab='sessions')
lines(subdata[,1],originY,col="red")
lines(subdata[traningSize:FulldataSize,1],pre[traningSize:FulldataSize],col="blue")
lines(subdata[1:traningSize,1],pre[1:traningSize],col="black")

#plot(lm_model)

#cv.lm(df=traindata, lm_model, m=10) 