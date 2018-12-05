diceroll<-function(rolls,num.dice) {
outcome<-numeric(rolls*num.dice)
outcome<-matrix(outcome,ncol=num.dice,nrow=rolls)
for (i in 1:rolls) {
for (j in 1:num.dice) {
outcome[i,j] <- runif(1,0.5,6.5)
if (outcome[i,j] < 1.5) {outcome[i,j] <- 1}
if (1.5 <=outcome[i,j] && outcome[i,j] < 2.5) {outcome[i,j] <- 2}
if (2.5 <=outcome[i,j] && outcome[i,j] < 3.5) {outcome[i,j] <- 3}
if (3.5 <=outcome[i,j] && outcome[i,j] < 4.5) {outcome[i,j] <- 4}
if (4.5 <=outcome[i,j] && outcome[i,j] < 5.5) {outcome[i,j] <- 5}
if (5.5 <=outcome[i,j] && outcome[i,j] <= 6.5) {outcome[i,j] <- 6}
}
}
return(outcome)
}