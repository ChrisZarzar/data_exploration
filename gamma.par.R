gamma.par <- function(x)  {
n <- length(x)

D <- log(mean(x)) - (1/length(x)) * sum(log(x))

# The method used is from Greenwood and Durand 1960.

if (D >= 0 && D < 0.5772) {
alpha.hat <- (0.5000876 + 0.1648852 * D - 0.0544274 * D^2)/D
} # End the first if

if (D >= 0.5772) {
alpha.hat <- (8.898919 + 9.059950 * D + 0.977373 * D^2)/(17.79728*D+11.968477 * D^2 + D^3)
} ## End the second if

beta.hat <- mean(x)/alpha.hat

output <- data.frame(alpha.hat,beta.hat)
#product <- alpha.hat * beta.hat
#return(product)

return(output)



}  ## End the function