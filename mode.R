Mode <- function(x) {

# This function will compute the mode of a vector of data

mode.x <- names(sort(-table(x)))[1]
as.double(mode.x)->mode.x
return(mode.x) 

} ## End the function
	
	