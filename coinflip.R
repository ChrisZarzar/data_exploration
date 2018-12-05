coinflip<- function (number.of.flips) {  ## Define the function coinflip
heads <- 0 ## Initialize the counter for the number of heads
tails <- 0 ## Initialize the counter for the number of tails
outcome<-numeric(number.of.flips)  ## Initialize the outcome variable
for (i in 1:number.of.flips) {  ## Begin the for loop
   number <- rnorm(1,0,1) ## Generate a single random normal number for testing
if (number > 0){ ## Test if it is a heads
    outcome[i] <- 'H'
    heads <- heads + 1
} else  {  ## Test if it is a tails
    outcome[i] <- 'T'
    tails <- tails + 1
}## End the else statement
}## End the for loop
return(list(outcome,heads,tails))
}## End the function
