predictlm<-function(model,inputdata) {

junk<-cbind(inputdata,inputdata) # Need this step to find the row
					   # dimensionality regardless of if 
					   # it is a vector or matrix.
dims<-dim(junk)


if (dims[2] == 2) {  # Perform the prediction for a single input sample
ones <- rep(1,dim(junk)[1])
inputdata<-cbind(ones,inputdata)
predictions <- inputdata %*% model$coefficients

} else {  # End the prediction if for a single input statement

ones<-rep(1,dim(junk)[1])

inputdata<-cbind(ones,inputdata)

predictions<-inputdata %*% model$coefficients

} # End the if statement for multiple inputs

return(predictions)  # Return the predictions

}
