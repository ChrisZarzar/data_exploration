limits.ci <-function(x,alpha=0.05) {  # This function will create CIs for 
                                      # the mean of x

n <- length(x)# Need this to compute the CIs.

if (n <=100) {

lower.ci <- mean(x) + qt(alpha/2,n-1)*(sd(x)/sqrt(n))
upper.ci <- mean(x) + qt(1-alpha/2,n-1)*(sd(x)/sqrt(n))

} else { # End the if statement for the t-distribution

lower.ci <- mean(x) + qnorm(alpha/2)*(sd(x)/sqrt(n))
upper.ci <- mean(x) + qnorm(1-alpha/2)*(sd(x)/sqrt(n))

}# End the else statement for the z-distribution

output<-data.frame(lower.ci,upper.ci)

return(output)

}
