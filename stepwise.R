stepwise <- function(y,x,method="forward") {

## This function will conduct a stepwise regression either based off of
## forward or backward selection.  The selection is based off of the
## F statistic.  It requires the anova.new function provided in class.

## We need to create an anova.new function that is compatible with this
## function.  It is built in here:

## First, we assume that our initial y.intercept is simply the mean of y, and 
## that we have no other variables.  Next, we need to determine the 
## individual capabilities of the different variables, so we will compute
## their individual linear regressions.


max.F <- 0
dims <- dim(x)
size <- dims[2] * 4
positions <- c(1:dims[2])
bestpreds <- matrix(numeric(size),ncol=4)
#best.x <- rep(1,dims[1])

if (method=="forward") { ## For the forward selection of predictors

for (j in 1:dims[2])  { ## Big for loop for all of the variables
m <- dims[2]-(j-1)
max.F <- 0
for (i in 1:m) { ## Begin the for loop for each step

if (j < dims[2]) {testvec <- x[,i]}
if (j == dims[2]) {testvec <- x}
if (j == 1) {
predictor.matrix <- testvec
df.MSR <- 1
df.MSE <- length(predictor.matrix)-2

} else { ## Begin the else statement

predictor.matrix <- cbind(best.x,testvec)
df.MSR <- dim(predictor.matrix)[2]-1
df.MSE <- dims[1]-dim(predictor.matrix)[2]

} ## End the else statement

test.model <- lm(y~predictor.matrix)
F.stat <- summary(test.model)$fstatistic[1]

if (F.stat > max.F) {
best.pred <- positions[i]
best.i <- i
max.F <- F.stat
#p.value <- 1 - pf(F.stat,df.MSR,df.MSE)
MSE <- sum(test.model$resid^2)/df.MSE
r.squared <- summary(test.model)$r.squared

} ## End the if statement

} ## End the for loop for each step

bestpreds[j,1] <- best.pred
bestpreds[j,2] <- round(MSE,3)
bestpreds[j,3] <- round(r.squared,3)
bestpreds[j,4] <- round(max.F,3)
#bestpreds[j,5] <- round(p.value,3)

if (j==1) {
addedx <- x[,best.i]
best.x <- addedx
x <- x[,-best.i]
positions <- positions[-best.i]
}

if (j!=1 && j != dims[2]) {
addedx <- x[,best.i]
best.x <- cbind(best.x,addedx)
x <- x[,-best.i]
positions <- positions[-best.i]
}

if (j==dims[2]) {
addedx <- x
best.x <- cbind(best.x,addedx)
} 

} ## End the big for loop

} ## End the forward selection section

## Now for the backward selection section

if (method=="backward") { ## Begin the backward selection if statement

for (j in 1:dims[2]) { ## Begin the for loop for the x parameters
	m <- dims[2] - (j-1)
	max.F <- 0


	for (i in 1:m) { ## Begin the loop for each individual model
		if (m > 1) { ## If test for predictor matrix
		predictor.matrix <- x[,-i]
		} else { ## Predictor matrix
		predictor.matrix <- x
		} ## End if else

		if (dim(cbind(predictor.matrix,predictor.matrix))[2]==2) {
		df.MSR <- 1
		df.MSE <- dims[1]-2
		} else { ## Else statement for if test of dim or length
		df.MSR <- dim(predictor.matrix)[2]
		df.MSE <- dims[1]-dim(predictor.matrix)[2]
		}

		## We need to remove a column to try to find the best setup

		test.model <- lm(y~predictor.matrix)
		F.stat <- summary(test.model)$fstatistic[1]
		
		if (F.stat > max.F) { ## F.stat if test
		best.pred <- positions[i]
		best.i <- i
		max.F <- F.stat
		MSE <- sum(test.model$resid^2)/df.MSE
		r.squared <- summary(test.model)$r.squared

		} ## F.stat end if

	} ## End the for loop for the individual tests

	if (j == 6) {
	test.model <- lm(y~1)

	df.MSE <- dims[1] - 1
	MSE <- sum(test.model$resid^2)/df.MSE
	}

	bestpreds[j,1] <- best.pred
	bestpreds[j,2] <- round(MSE,3)
	bestpreds[j,3] <- round(r.squared,3)
	bestpreds[j,4] <- round(max.F,3)
	
	if (j < dims[2]) {
	x<- x[,-best.i]
	positions <- positions[-best.i]
	} 

} ## End the for loop for the x parameters

} ## End the backward selection if statement

colnames <- c("Var #","MSE","R-squared","F-stat")
rownames <- bestpreds[,1]
output <- rbind(colnames,bestpreds)
output <- as.data.frame(output,row.names=rownames)
return(output)

} ## End the function

