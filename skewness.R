skewness<-function(x) {
mean.x<-mean(x)
n<-length(x)
output<-(sum((x-mean.x)^3)/n)/(sum((x-mean.x)^2)/n)^(3/2)

return(output) 
}
