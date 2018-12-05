con.table <- function(observed,predicted) {
aa <- 0	## I called these double letters because single c is a function in R.
bb <- 0
cc <- 0
dd <- 0

for (i in 1:length(observed)) { ## Lets first create our contingency matrix

	if (observed[i] == 1 && predicted[i] == 1) { # Test for correct yes forecasts
	aa<-aa+1
	}  # End the correct yes forecasts test

	if (observed[i] == 0 && predicted[i] == 1) { # Test for incorrect yes forecasts
	bb<-bb+1
	}  # End the incorrect yes forecasts test

	if (observed[i] == 1 && predicted[i] == 0) { # Test for incorrect no forecasts
	cc<-cc+1
	}  # End the incorrect no forecasts test

	if (observed[i] == 0 && predicted[i] == 0) { # Test for correct no forecasts
	dd<-dd+1
	}  # End the correct no forecasts test

} ## End the for loop

# Now we can simply put this together in a nice contingency matrix.

contingency.matrix <- matrix(c(aa,bb,cc,dd),ncol=2,byrow=T)

# Lets return our contingency matrix.

return(contingency.matrix)

} ## End the function