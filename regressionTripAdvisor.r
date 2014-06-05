data <- read.csv("TripAdvisor//tidyData.csv",header=TRUE)
FulldataSize <- 607
traningSize <- as.integer(0.8 * FulldataSize)
#, "festival"
cf<- c( "weekday" ,"month")
data[,cf]<- lapply(data[,cf], as.factor)
print(head(data))

sf <- c("date","sessions","energy.consumption","index",
         "nthweek","weekday" , "month")

data <- data[,sf]
#,"Holidays", "Cnt.to.Holidays" ,"Cnt.after.Holidays"
subdata <- data[,c(-1)]
print (names(subdata))
index<-factor(data[,1])
#names (subdata) <- c("y","x1","x2","x3","x4","x5","x6")
traindata <- subdata[1:traningSize,]
#,"Holidays"


#the_best_model <- lm(y~poly(x1,2)+poly(x2,10)+poly(x3,5)+poly(x4,21)+poly(x5,3)+x6,data=traindata, x = TRUE)


linear_model <- lm(sessions~(.-index)^2+poly(nthweek,21)+index,data=traindata)


# library(MASS)
# step <- stepAIC(the_best_model, direction="both")
# step$anova # display results
# step2<- stepAIC(linear_model, direction="both")
# step2$anova # display results

source('C:/Users/zixing.liu/Desktop/AD_impact_prediction/predictAndPlot.r')
print(summary(linear_model))
predictAndPlot(index,subdata,linear_model,traningSize,FulldataSize)
library(DAAG)

#cv.lm(df=traindata, linear_model, m=10) 
# x<-the_best_model$x
# outs <- leaps(x, traindata[,1], int = FALSE, strictly.compatible = FALSE)
# plot(outs$size, outs$Cp, log = "y", xlab = "p", ylab = expression(C[p]))
# lines(outs$size, outs$size)
# ifoo <- outs$Cp == min(outs$Cp)
# ifoo <- outs$which[ifoo, ]
# foo <- x[, ifoo]
# best <- lm(y ~ foo + 0,data=traindata, x = TRUE)
# print(summary(best))








# test_model <- glm(formula=y~x1+x2+x3+x4+x5+x6,data=traindata)
# toselect.x <- summary(test_model)$coeff[-1,4] < 0.05
# relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# sig.formula <- as.formula(paste("y ~",relevant.x)) 
# sig.model <- glm(formula=sig.formula,data=traindata)





