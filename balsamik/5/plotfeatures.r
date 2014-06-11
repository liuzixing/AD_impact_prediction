library(ggplot2)
library(lattice)
library(caret)

#init
data <- read.csv("tidyData.csv",header=TRUE)
FulldataSize <- nrow(data)
data <- data[1:FulldataSize,]



#convert to factors  
#,"dayofweek"
cf <- c("date","TypeWeekDay","channel","DAYPART","crea","length"
        ,"tav","tavtap","emav","emap","screen")
data[,cf]<- lapply(data[,cf], as.factor) 

#select features
sf1 <- c("date","Visite.gain","cumGrpRefTotal","hour","DAYPART"
         ,"budgetbrut","budgetnet","grp","grpref","month","dayofweek","TypeWeekDay","length",
         "channel","nthweek","tavtap","cumGrpRef","cumGrp","cumGrpTotal")

sf2 <- c("Visite.gain","hour","month","dayofweek","TypeWeekDay","nthweek"
         ,"budgetbrut","budgetnet","MMDAYPART","length","channel","DAYPART"
         ,"visitByhour","tavtap","grp","grpref","cumGrpRef","cumGrp","cumGrpTotal","cumGrpRefTotal")
#,"crea"
data <- data[,sf1]
data$index <- 1:FulldataSize
index<-1:FulldataSize
traningSize <- as.integer(0.81 * FulldataSize)
subdata <- data[1:FulldataSize,c(-1)]
print (names(subdata))
for (i in sf2){
  plot(subdata[,i],subdata$Visite.gain,type = 'p',xlab=i,ylab='Visite.gain')
  #plot(log(subdata[,i]),log(subdata$Visite.gain),type = 'p',xlab=i,ylab='Visite.gain')
   print(table(subdata[,i]))
  cat ("Press [enter] to continue");line <- readline()
}