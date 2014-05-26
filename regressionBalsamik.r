traningSize <- 150
FulldataSize <- 185
data <- read.csv("balsamik//tinyDatax _sale_one_zero.csv",header=TRUE)
#data <- read.csv("balsamik//tinyDatax_sale_increasing.csv",header=TRUE)
subdata <- data[,c(-1)]
print (names(subdata))
index<-factor(data[,1])
names (subdata) <- c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9")
traindata <- subdata[1:traningSize,]


the_best_model <- lm(y~poly(x1,2)+x2+x3+x4+poly(x5,2)+poly(x7,4)+x8+poly(x6,4)+poly(x9,2),data=traindata, x = TRUE)

linear_model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=traindata, x = TRUE)

testing_model <- lm(y~x3+x4+poly(x5,2)+x6+I(x6^4)+x7,data=traindata)

sin_on_sale_model <- lm(y~poly(x1,2)+x2+x3+x4+poly(x5,2)+poly(x7,4)+x8+poly(x6,4)+sin(x9),data=traindata)

cos_on_sale_model <- lm(y~poly(x1,2)+x2+x3+x4+poly(x5,2)+poly(x7,4)+x8+poly(x6,4)+cos(x9),data=traindata)

exp_on_sale_model <- lm(y~poly(x1,2)+x2+x3+x4+poly(x5,2)+poly(x7,4)+x8+poly(x6,4)+exp(x9),data=traindata)

#poly_on_sale_model <- lm(y~x1+x2+x3+x4+poly(x5,1)+poly(x7,1)+x8+poly(x6,3)+poly(x9,11),data=traindata, x = TRUE)

best_log_on_sale_model <- lm(log(y)~x1+x2+x3+x4+x5+poly(x7,2)+x8+poly(x6,7)+x9,data=traindata, x = TRUE)

#log_on_sale_model <- lm(log(y)~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=traindata, x = TRUE)

#check_model <- lm(y~I(x1^2)+I(x1^3)+I(x2^4)+I(x2^3)+I(x2^5)+I(x3^4)+I(x4^3)+I(x4^5)+I(x5^2)+poly(x7,2)+poly(x6,2)+I(x6^4),data=traindata, x = TRUE)

source('C:/Users/zixing.liu/Desktop/regression/predictAndPlot.r')
#predictAndPlot(index,subdata,the_best_model,traningSize,FulldataSize)
#par(mfrow=c(2,1))
#predictAndPlotSpecial(index,subdata,the_best_model ,traningSize,FulldataSize)
predictAndPlotSpecial2(index,subdata,best_log_on_sale_model,traningSize,FulldataSize)
print(summary(best_log_on_sale_model))

# library(leaps)
# x<-the_best_model$x
# outs <- leaps(x, traindata[,1], int = FALSE, strictly.compatible = FALSE)
# plot(outs$size, outs$Cp, log = "y", xlab = "p", ylab = expression(C[p]))
# lines(outs$size, outs$size)
# ifoo <- outs$Cp == min(outs$Cp)
# ifoo <- outs$which[ifoo, ]
# foo_name <- paste(names(ifoo)[-1],)
# sig.formula <- as.formula(paste("y ~",foo_name))
# 
# #foo <- x[, ifoo]
# best <- lm(y ~ foo+0,data=traindata, x = TRUE)
# #best <- lm(sig.formula,data=traindata)
# print(summary(best))
# #predictAndPlotSpecial(index,subdata,best,traningSize,FulldataSize)




#x <-cv.lm(df=subdata, lm_model, m=4) 


# test_model <- glm(formula=y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=traindata)
# toselect.x <- summary(test_model)$coeff[-1,8] < 0.05
# relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# sig.formula <- as.formula(paste("y ~",relevant.x)) 
# sig.model <- glm(formula=sig.formula,data=traindata)





clearDataToFile<-function(){
    library(DAAG)
    data <- read.csv("balsamik//balsamilk.csv",header=FALSE)
    names(data) <- c("date","sessions")
    NewData <- data.frame(date=factor(data$date), 
                        sessions=as.numeric(data$sessions))
    NewData<- tapply(NewData$sessions, NewData$date, FUN=sum)
    D3<-as.data.frame(NewData,stringsAsFactors=FALSE)
    D3 <- data.frame(as.Date(rownames(D3)),D3)
    
    names(D3) <- c("date","sessions")
    weekday <- as.numeric(format(D3$date,'%w'))
    nthweek <- as.numeric(format(D3$date,'%W'))
    month <- as.numeric(format(D3$date,'%m'))
    index<- 1:203
   # ads <- c(rep(1,83),rep(2,36),rep(3,13),rep(4,71))
    
    GRP <- read.csv("balsamik//GRP.csv",header=FALSE)
    names(GRP) <- c("date","GRP","GRPRef")
    NewGRP<- tapply(GRP$GRP, GRP$date, FUN=sum)
    D4<-as.data.frame(NewGRP,stringsAsFactors=FALSE)
    NewGRPRef<- tapply(GRP$GRPRef, GRP$date, FUN=sum)
    D5<-as.data.frame(NewGRPRef,stringsAsFactors=FALSE)
    #D4 <- data.frame(as.Date(rownames(D4)),D3)
    GRP<-c(rep(0,83),D4[1:36,1],rep(0,13),D4[37:107,1])
    GRPRef<-c(rep(0,83),D5[1:36,1],rep(0,13),D5[37:107,1])
    D3<-data.frame(D3,weekday,nthweek,month,index,GRP,GRPRef)
    write.csv(D3, "balsamik//tinyData.csv", row.names=FALSE)
    return (D3)
}

add2features <-function(){
  library(DAAG)
  data <- read.csv("balsamik//tinyData.csv",header=TRUE)
  fea <- read.csv("balsamik//energy.csv",header=FALSE)
  names(fea) <- c("date","energy.consumption","festival")
  D3<-data.frame(data,fea[,c("energy.consumption","festival")])
  write.csv(D3, "balsamik//tinyData.csv", row.names=FALSE)
  return (D3)
}