traningSize <- 500
FulldataSize <- 607
data <- read.csv("TripAdvisor//tidyData.csv",header=TRUE)
subdata <- data[,c(-1)]
print (names(subdata))
index<-factor(data[,1])
names (subdata) <- c("y","x1","x2","x3","x4","x5","x6")
traindata <- subdata[1:traningSize,]


the_best_model <- lm(y~poly(x1,2)+poly(x2,10)+poly(x3,5)+poly(x4,21)+poly(x5,3)+x6,data=traindata, x = TRUE)

linear_model <- lm(y~x1+x2+x3+x4+x5+x6,data=traindata)

testing_model <- lm(y~I(x1^2)+I(x2^2)+I(x2^5)+I(x2^7)+I(x2^10)+x3+I(x3^4)+poly(x4,21)+x6,data=traindata)

# library(MASS)
# step <- stepAIC(the_best_model, direction="both")
# step$anova # display results
# step2<- stepAIC(linear_model, direction="both")
# step2$anova # display results

source('C:/Users/zixing.liu/Desktop/regression/predictAndPlot.r')
predictAndPlot(index,subdata,testing_model,traningSize,FulldataSize)
print(summary(the_best_model))

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





