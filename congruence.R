congruence <- function(cor.matrix, load.matrix)  {  ## Begin the congruence coefficient function

   ## Remember that cor.matrix is the first correlation matrix 
   ## and load.vector is a loading vector.

m <- dim(load.matrix)[1]
n <- dim(load.matrix)[2]

conoutput <- numeric(n)

for (j in 1:n)  { ## Begin computing congruence over all loading columns

load.vector <- load.matrix[,j]
testvalue <- max(abs(load.vector))

bestcolumn <- 0

for (i in 1:m)  { ## Check for the largest absolute loading value

if (abs(load.vector[i]) ==  testvalue) { ## If test for testvalue
bestcolumn <- i
} ## End the if

}  ## End the for


conoutput[j] <- sum(cor.matrix[,bestcolumn] * load.vector)/sqrt(sum(cor.matrix[,bestcolumn]^2) * sum(load.vector^2))

} ## End the big for loop

return(conoutput)

} ## End the total congruence function
