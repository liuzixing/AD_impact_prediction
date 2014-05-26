library(data.table)
getSessions <- function(from,days){
  d<- seq(as.Date(from),by='days',length=days)
  res <- lapply(d,rf)
  write.csv(res, "theLastMonth2.csv")
}

rf <- function (s){
  x<-fread(paste0("data//",s,".tsv//187070.tsv"), sep='\t',header=FALSE)
  return (nrow(x))
}