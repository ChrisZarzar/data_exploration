## Resampling statistics ##

# A final useful set of statistics are resampling statistics.  They are so named
# as they involve sampling a sample of data repeatedly, or resampling it.  There
# are two primary resampling methods, and any statistics that exist can be applied
# to these resampling methods.  We will discuss each one individually.

# The first resampling method is called a jackknife.  The jackknife is also referred
# to as a leave on out approach.  The jackknife method involves looking at your
# sample and iteratively leaving out the observation you are currently on, computing 
# the statistic on the remaining observations.  There is not a function in R
# for the jackknife itself (and most R resampling code is not good, a major 
# downfall for its use), but we can easily code a jackknife ourselves.  Lets do an
# example.  

# For our example, we will compute the jackknife mean high and low temperature
# for our Ithaca data.  Let's rescan in those data since we haven't used them in
# a while. 

ithaca.data <- matrix(scan("ithaca_data.txt"),ncol=4,byrow=T)

# To do a jackknife mean, we simply need to compute a set of n means using a leave
# one off approach.  We can easily do this in R:

jackknife.mean.low <- numeric(length(ithaca.data[,1]))
jackknife.mean.high <- numeric(length(ithaca.data[,1]))
n <- length(ithaca.data[,1])

for (i in 1:length(ithaca.data[,1])) {
if (i == 1) {
jackknife.mean.low[i] <- mean(ithaca.data[2:n,4])
}
if (i == n) {
jackknife.mean.low[i] <- mean(ithaca.data[1:n-1,4])
}
if (i !=1 && i !=n) {
j <- i-1
k <- i+1
jackknife.mean.low[i] <- mean(c(ithaca.data[1:j,4],ithaca.data[k:n,4]))
}

}


for (i in 1:length(ithaca.data[,1])) {
if (i == 1) {
jackknife.mean.high[i] <- mean(ithaca.data[2:n,3])
}
if (i == n) {
jackknife.mean.high[i] <- mean(ithaca.data[1:n-1,3])
}
if (i !=1 && i !=n) {
j <- i-1
k <- i+1
jackknife.mean.high[i] <- mean(c(ithaca.data[1:j,3],ithaca.data[k:n,3]))
}

}

# We can compare these means with the actual mean of our data vectors:

mean(jackknife.mean.low)
mean(ithaca.data[,4])
mean(jackknife.mean.high)
mean(ithaca.data[,3])

# and see they are the same.  This won't always be the case but our dataset is 
# small and has little variability.  If we change our first low temperature to 
# 100 (for demonstrative purposes)

ithaca.data[1,4]<-100
jackknife.mean.low <- numeric(length(ithaca.data[,1]))
n <- length(ithaca.data[,1])

for (i in 1:length(ithaca.data[,1])) {
if (i == 1) {
jackknife.mean.low[i] <- mean(ithaca.data[2:n,4])
}
if (i == n) {
jackknife.mean.low[i] <- mean(ithaca.data[1:n-1,4])
}
if (i !=1 && i !=n) {
j <- i-1
k <- i+1
jackknife.mean.low[i] <- mean(c(ithaca.data[1:j,4],ithaca.data[k:n,4]))
}

}

# and compare the means:
mean(jackknife.mean.low)
mean(ithaca.data[,4])

# you see that the larger variability associated with the outlier is affecting
# the jackknife.  Outliers do tend to affect the jackknife.  So, someone invented
# the bootstrap.  The bootstrap samples a user-defined number of times, with
# replacement, any statistic the user is interested in.  Once this sampling is
# complete, the user can easily generate confidence intervals by simply
# looking at the quantiles of the bootstrap replicates.  In order to do the bootstrap
# in R, we first have to download and install the boot package.  Remember to modify
# your .First function to add library('boot') into the list of libraries.  We 
# can easily modify a function by simply doing fix(.First) and adding the line.
# Also be sure to type .First() this time so that you have it loaded.  It will then
# load for you from here on.  This function boot requires some manipulation (another
# disadvantage of resampling in R) such that we have to write functions for the
# statistics we are interested in.  Lets do an example of the mean.

mean.boot <- function(x,d) {
return(mean(x[d]))
}

# With this, we can put mean.boot as our function in boot to run a bootstrap
# on the mean.  Lets do two bootstrap means, with 1000 replicates each, for our
# Ithaca high and low temperature data.

ithaca.data <- matrix(scan("ithaca_data.txt"),ncol=4,byrow=T)

boot.low <- boot(ithaca.data[,4],mean.boot,R=1000)
boot.high <- boot(ithaca.data[,3],mean.boot,R=1000)
boot.low
boot.high

# Now that we have these values, we can use the replicates (the 1000 means we generated)
# to create confidence limits around our high and low.  You have 1000 samples, so 
# you can just use positions in your replicates to represent your 
# confidence level.  So you can generate 97.5% and 2.5% quantiles to get a 95%
# confidence level for any statistic you are interested in!  Lets do a histogram
# of the replicates to prove this. (not sure why we have to use $t, it's an R thing)

hist(boot.low$t,bin.size(boot.low$t))
hist(boot.high$t,bin.size(boot.high$t))

# As you can see, they are pretty well normally distributed.  To get a CI about 
# the mean, we can just use quantile on the replicates.  Lets do that.

quantile(boot.low$t,probs=c(0.025,0.5,0.975))
quantile(boot.high$t,probs=c(0.025,0.5,0.975))


# As you can see, the low temperature confidence intervals are slightly wider than
# the high temperature counterparts.  Does this make sense?  

# The bootstrap is very useful as you can use it for about anything.  Lets do a 
# bootstrap correlation of high and low temperature in ithaca, and generate
# confidence limits around it.  We first have to write the bootstrap function.

boot.cor <- function(x,d){
return(cor(x[d,1],x[d,2]))
}

# Now lets do the bootstrap

corboot.ithaca <- boot(ithaca.data[,3:4],boot.cor,R=1000)
# Lets histogram those replicates!

hist(corboot.ithaca$t,bin.size(corboot.ithaca$t))

# Not normally distributed, but thats okay.  We can still just use the position
# in our replicates to get upper and lower confidence limits.  And the median.

quantile(corboot.ithaca$t,probs=c(0.025,0.5,0.975))

# So in reality, the actual correlation between these data can fall anywhere
# between 0.33 and 0.81!  If we just did the correlation, we would get:

cor(ithaca.data[,3],ithaca.data[,4])

# which is near our median, but not very descriptive of the actual behavior
# here.  

# The final method we will consider is called a permutation test.  The permutation
# test is a hypothesis test which makes no underlying assumption about the 
# distribution of the data.  We can use it to determine if the means of two
# datasets are statistically significantly different, regardless of distribution!

# The method work by considering the individual groups, then pooling them together.
# If we had two datasets x and y, we would first take the mean of x and the
# mean of y, then compute the difference between them (mean(x) - mean(y)).  With
# this result, we then pool all of the values of x and y both into a single 
# vector, and randomly sample, with replacement, two vectors out of that 
# pool that have lengths of x and y.  We compute their means, then we compare
# them to the original mean difference.  If the magnitude of the permuted mean
# difference is larger than the original difference, we keep count of that
# permutation.  We then repeat the process a user defined number of times (in 
# the function I provide that value defaults to 2000).  From the number of times
# that the mean difference of the permutations is greater than of the original
# data, we can compute a p-value. The p-value is simply n/N, where n is the
# number of means of the permutations that were larger than the observed mean
# and N is the number of permutations used (i.e. 2000).  The function
# permutationTestMeans will compute this test for you. Lets do an example.  We
# can try a permutation test on the mean of the first half of the month of low 
# temperature data for Ithaca and the second half of the month.  Lets call these
# x and y.

x <- ithaca.data[1:15,4]
y <- ithaca.data[16:31,4]

permutationTestMeans(x,y,B=2000)

# We get a p-value of 0.005, which is highly statistically significant.  That
# means that the low temperatures in the second half of the month are largely
# different than the first half.  Lets see what the actual means were as a point
# of reference.

mean(x)
mean(y)

# It's not surprising that we got this result based on the number of highly
# negative values in y and the number of highly positive values in x.  This is
# even more helpful when you have lots of datapoints to test.  

# This concludes a VERY brief overview of resampling.  Unfortunately, resampling
# could be taught as an entire class, looking at different statistics and how
# they behave when bootstrapped.  These tools are very powerful and very valuable,
# so anyone doing statistics is STRONGLY encouraged to use resampling for their
# research.




