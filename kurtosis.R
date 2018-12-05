kurtosis<-function(x) {
mean.x<-mean(x)
n<-length(x)
output<-(sum((x-mean.x)^4)/n)/(sum((x-mean.x)^2)/n)^2

return(output) 
}
