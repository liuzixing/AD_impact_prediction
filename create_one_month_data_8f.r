f<- function(){
date <- rep(41730:41759,each = 64)
week <- rep(c(3:7,1:2),each = 64,len = 64*30)
weeknum <- c(rep(14,5*64),rep(15:17,each = 64*7),rep(18,4*64))
month <- rep(4,64*30)
# last_4_f = expand.grid(1:3,1:5,1:2,1:3)
last_4_f <- read.csv("DBsample.csv",header=FALSE)
last_4_f <- last_4_f[1:64,]
last_4_f<-do.call("rbind", replicate(30, last_4_f, simplify = FALSE))

predata <- date
predata <- cbind(predata,week)
predata <- cbind(predata,weeknum) 
predata <- cbind(predata,month) 
predata <- cbind(predata,last_4_f)
names (predata) <- c("x1","x2","x3","x4","x5","x6","x7","x8")
return (predata)
}
#print (f())
