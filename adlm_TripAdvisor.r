library(earth)
data <- read.csv("TripAdvisor//tidyData.csv",header=TRUE)
FulldataSize <- 607
data <- data[1:FulldataSize,]
traningSize <- as.integer(0.8 * FulldataSize)

cf<- c( "weekday" , "festival","month",
        "nthweek")
data[,cf]<- lapply(data[,cf], as.factor)

sf <- c("date","sessions","festival","energy.consumption","Holidays", "Cnt.to.Holidays" 
        ,"nthweek","Cnt.after.Holidays","weekday" , "month")
data <- data[,sf]
#,"index"
subdata <- data[,c(-1)]
print (names(subdata))
index<-factor(data[,1])
#names (subdata) <- c("y","x1","x2","x3","x4","x5","x6")
traindata <- subdata[1:traningSize,]



adlm <- earth(
  sessions~., # the formular for prediction
  traindata, # the training dataset
  trace=1, # provide overview information during model building
  nk=100, # the maximum number of terms
  degree=2, # the maximum number of interaction (degrees of freedom)
  penalty=0, # penality per knot for GCV during pruning
  thresh=0.001, # minimum change in SSR in forward stage
  minspan=0.1, # minimum distance between knots in the model
  fast.k=0, # disable Fast MARS adding multiple terms per forward step
  fast.beta=0, # aging coefficient used in Fast MARS
  pmethod="backward") # pruning method during backward pass

source('C:/Users/zixing.liu/Desktop/AD_impact_prediction/predictAndPlot.r')
print(summary(adlm))
predictAndPlot(index,subdata,adlm,traningSize,FulldataSize)

