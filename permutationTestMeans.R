permutationTestMeans <- function(x,y,B=1000) {
n1 <- length(x)
n2 <- length(y)
delta <- abs(mean(x) - mean(y))
fullset <- c(x,y)
new.delta <- numeric(B)
for (i in 1:B) {
sample.1 <- sample(fullset,n1,replace=T)
sample.2 <- sample(fullset,n2,replace=T)
new.delta[i]<- abs(mean(sample.1) - mean(sample.2))
}

counts <- ifelse(new.delta >= delta,1,0)
p.value <- sum(counts) / B
return(p.value)
}
