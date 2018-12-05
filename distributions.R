# We are now going to advance our discussion from individual statistics to
# data distributions.  The data distribution is simply a mathematical form
# that describes the frequency of the dataset (i.e. how frequently
# individual values appear in the dataset).  These distributions give a
# shape to the underlying data, which can be helpful in understanding the
# behavior of the dataset.  Two types of distributions exist:

# Discrete - Those in which the there are a finite number of possible data
# points.  These can include class data (those which exist as one of a few
# class types).  Several types of discrete distributions exist, including:

# Binomial - The binomial distribution is one in which there are two possible 
# outcomes, often assigned as a 1 and a 0.  In this distribution, the
# number of interest is the number of occurrences of each of the outcomes
# out of the total number of trials N.  This distribution can be used to
# compute the probability that any particular value of X (the number of
# occurrences of each outcome) occurs, based on two criteria:

# 1) The probability of the event occurring does not change from trial to
# trial

# 2) The outcomes of the N events are mutually exclusive (not dependent on
# each other)

# The most common illustration of the binomial distribution is the coin
# flip.  Lets use our simple function that will randomly flip a coin.

test.coinflips<-coinflip(10)

# You will see that your result is not the same each time.  We could plot a
# histogram of your last iteration:

numeric.solution<-ifelse(test.coinflips=="H",1,0)
hist(numeric.solution,nclass=2)

# We use nclass=2 since it is obvious that there are only two possible
# outcomes here.  In
# general, the two have the same probability of occurring.  If we start with
# the 10 flips, then proceed to 100 flips, we can look at the histogram at
# this point.

test.coinflips <- coinflip(100)
numeric.solution<-ifelse(test.coinflips=="H",1,0)
hist(numeric.solution,nclass=2)

# Still, these two outcomes seem to have a near likelihood of 50.  Anyone
# else have a pretty big difference between the two coin flip outcomes?

# Finally, lets try 10000!

test.coinflips <- coinflip(10000)

numeric.solution<-ifelse(test.coinflips=="H",1,0)
hist(numeric.solution,nclass=2)

# As you can see, as you increase the number of flips, the tendency is to
# approach the same number of each event.  

# The binomial distribution has numerous applications in meteorology.  Many
# problems have one of two possible outcomes.  These problems have their
# own set of special statistics, which we will discuss later.

# Lets look at another discrete distribution, the Poisson distribution.
# This distribution describes the number of discrete events that occur in a
# series, that exhibit a particular type of independence.  Independent
# events are those whose occurrence does not depend on the other events.
# These variables will have a zero correlation as well.

# The poisson distribution has a single variable that can be used to
# describe it, a lowercase mu.  This value specifies the average occurrence
# rate, and has physical dimensions of occurrences per unit time.  The
# Poisson distribution has a probability density function of:

# Pr(X = x) = (mu^x * exp^(-mu))/x!

# The pdf describes the probability of a single value occurring for
# discrete distributions.  Lets look at the example dataset in the
# textbook.

num.tornadoes<-matrix(scan("tornado_data.txt"),ncol=2,byrow=T)
             
# Lets plot a histogram of the tornado data.

hist(num.tornadoes[,2],nclass=bin.size(num.tornadoes[,2]))

# We can use the poisson pdf on these data by first computing a mean value
# for the number of tornadoes.  If we compute this mean:

Mu <- mean(num.tornadoes[,2])
negative.mu <- -Mu
# and use mu = 4.6 in the Poisson pdf.  How close will this look to our
# original histogram?  We can plot up to 12 tornadoes and get their
# probabilities of occurring.

# If we use this Poisson distribution, we can use the pdf function to
# calculate the probability of a certain number of tornadoes for a year,
# based on the data we have used as input.  Lets try one.  How about for 5
# tornadoes in a given year?

n.tornadoes <- 5
probability <- (Mu^n.tornadoes)*exp(-Mu)/factorial(n.tornadoes)

# Which makes sense since there are 5 years with 5 tornadoes, or about
# 1/6th of the total (17%).  Thus, the pdf is describing our dataset well.
# Lets try another one with a very large value for n.tornadoes, say 15.

n.tornadoes <- 15
probability <- (Mu^n.tornadoes)*exp(-Mu)/factorial(n.tornadoes)

# This probability is basically zero.  What about for zero tornadoes?

n.tornadoes <- 0
probability <- (Mu^n.tornadoes)*exp(-Mu)/factorial(n.tornadoes)

# So, even though this didn't occur in our sample dataset, the probability
# of a year with no tornadoes is only about 1%, if the dataset is truly
# following the Poisson distribution.  Obviously this has numerous forecast
# applications!

# Quick question.  If we define our probability density function as the
# probability of a given value of the discrete distribution, what would be
# the result of summing all of the pdf values that are possible?  Lets try
# it with a large number of our tornado Poisson distribution data.  With
# the factorial term, we will keep the value near 50, since this is about
# as high as the factorial will be able to go on a 32-bit desktop machine.

sum.probability <- 0;
for (i in 0:50) {
    probability <- (Mu^i)*exp(-Mu)/factorial(i)
    sum.probability <- sum.probability + probability
}

# We get a value of 1.  Is this what you would expect?
    
# We can shift gears slightly before discussing continuous distributions
# and discuss the concept of expected value.  The expected value is simply
# defined as the population mean.  Most scenarios do not allow for the
# computation of a population mean, so often a sample mean is substituted.
# Provided a large amount of data is used in the computation of the
# expected value, this is typically okay.  Using only 10 datapoints out of
# a distribution of 10000 to compute a population mean can be problematic
# though.  So care must be taken when computing the expected value (mu) of
# a distribution, both discrete and continuous.  For discrete
# distributions, the expected value could be a non-integer solution (i.e.
# the Poisson example with mu = 4.6).  This is just one of the properties
# of the expected value, and must be remembered when computing this value
# on discrete data.

# We can also define the variance as simply the expected value of the
# quantity (x - mu)^2, where mu represents the expected value of the
# population, or the population mean.  Hence, the variance is the
# population variance, and is often defined as a lower case sigma.  

# We can now shift our focus from discussing discrete distributions, and
# instead look at continuous distributions.  As we stated with discrete
# distributions, the probability density function must sum to zero for all
# of the data in the distribution.  For continuous distributions, our pdf
# definition changes only slightly, such that the area under the curve for
# the pdf must sum to one.  That is:

# integral(f(x)dx) = 1  if f(x) is the pdf.

# PDF functions must also always be positive.  It is important to remember
# that the height of the pdf is not important, but instead the area under
# the curve that is important.  

# Analagous to the probability density function is the the cumulative
# density function.  The cumulative density function specifies
# probabilities that the chosen quantity X will not exceed the particular
# value.  This can also be called the quantile function, since it
# represents the amount of probability swept out by the particular point in
# the PDF, (i.e. the particular quantile).  

# The simplest of the data distributions is called the uniform distribution.
# With the uniform distribution, the probability of obtaining any value is
# equal (hence the name uniform).  The uniform distribution is defined over a
# range, and the PDF over the range from a to b is:

# f(x) = (b-a)^-1   for a <= x <= b
# f(x) = 0 everywhere else

# The CDF is a straight line between the points a and b, increasing from
# 0 to 1.  We can use the qunif, punif, and runif commands to determine
# various properties of the uniform distribution.  Lets first generate 100
# random uniform numbers and take a look at the histogram.  We'll take
# a=0 and b=1.

set.seed(50)  # All start at the same place
uniform.example <- runif (100,0,1)

# The histogram of these numbers is:

hist(uniform.example, nclass=bin.size(uniform.example))

# Not much to it.  Lets calculate at what position in our distribution (what
# quantile) our 30th datapoint is.  To do this, we use the punif command.  
# This information is especially important when we look at hypothesis testing
# and confidence intervals.

punif(uniform.example[30],0,1)
uniform.example[30]

# As you can see, the value of uniform.example is exactly equal to the
# result from punif.  Do you understand why?  Will this always be true with
# every uniform distribution?

# The most commonly analyzed continuous distribution is called the
# Gaussian, or Normal distribution.  This distribution follows from the
# powerful theoretical result of the distribution, i.e. the central limit
# theorem.  The central limit theorem states that in the limit, as the
# sample size becomes large, the sum of a set of independent observations
# will have a Gaussian distribution.  

# The equation for the gaussian distribution pdf is:

# f(x) = (1/(sigma*sqrt(2*pi)))*exp(-(x-mu)^2/(2*sigma^2))

# The rnorm function we have used to generate datasets creates random
# variables that follow this distribution.  Lets create a sample dataset
# of uniformly distributed variables and see what they look like.

set.seed(100)
sample.data <- runif(500,0,1)

hist(sample.data,nclass=bin.size(sample.data))

# Now, lets take many samples (how about 100) and calculate their means.
# According to the central limit theorem, these means should be normally
# distributed.

datasamples<-matrix(numeric(500*100),ncol=100)

for (i in 1:100) {

	datasamples[,i]<-runif(500,0,1)

}  # End the for loop

# If we calculate column means, we can plot those on a histogram and see if 
# they are normally distributed.

col.means<-apply(datasamples,2,mean)

hist(col.means,nclass=bin.size(col.means))

# And sure enough they are.  Even if we use uniform distribution data, we
# get normally distributed means.  What if we do the rows instead of the 
# columns?

row.means<-apply(datasamples,1,mean)

hist(row.means)

# Even they tend towards a normal distribution.  We don't even know what
# their distribution was!  The central limit theorem is fundamental in 
# resampling statistics and in hypothesis testing, so its important to 
# properly demonstrate it.

# We can use the cumulative
# distribution function to determine the probability of any given value
# occurring.  Area to the left of our point on the CDF for the given
# distribution tells us what the probability of that point is.  If we 
# want to do this though, we must first convert our dataset into the
# given distribution.  To do this for the normal distribution, we need
# to calculate the z-scores.  

# We can use the population mean and standard deviation to determine the Z
# scores, or the standardized z values.  This is the same as for computing
# anomalies, i.e. 

z.scores <- (sample.data - mean(sample.data))/sd(sample.data);

# The z score data has a mean of 0 and a standard deviation of 1.  Lets
# test to be sure this is correct.

mean(z.scores)
sd(z.scores)

# And these are correct.  Lets use our ithaca high temperature data to
# determine the probability that a given high temperature will occur on a
# given day.  We should first plot the temperatures to see if they follow a
# Gaussian distribution.  The Ithaca data should be available, but if not
# we can run:

ithaca.data<-matrix(scan("ithaca_data.txt"),ncol=4,byrow=T)

# Lets put the high temperatures into a separate vector.

ithaca.highs<-ithaca.data[,3]

# Lets visualize these high temperatures too, so we can see how they look

hist(ithaca.highs,15)

# While not continuous, the data do appear to have some normal distribution
# about them.  In order to determine the probability that a given
# temperature does not exceed a certain value, we first need to convert our
# high temperature data into z.scores, which can be compared with the data
# table in the text or using the normcdf function.  Lets first convert to
# z.scores.

z.score.ithaca <- (ithaca.highs - mean(ithaca.highs))/sd(ithaca.highs);

# Lets test the probability that the air temperature does not exceed 21.4
# F, as an example (this value is arbitrary).  In this case, we first need
# to convert the 21.4 to a z.score value, based on the data we have:

sample.score <- (21.4 - mean(ithaca.highs))/sd(ithaca.highs)

# This score value is -1.09.  If we want to determine the probability that
# our high temperatures for a given winter will be colder than this value,
# on average, then we can use our normcdf function:

pnorm(sample.score)

# And this says that the temperatures will only be colder than this value,
# on average, about 20% of the time.  If we consider a much warmer value,
# say 33 F (above freezing), then the probability that the temperatures
# will be colder than this value is simply computed the same way.

sample.score <- (33 - mean(ithaca.highs))/sd(ithaca.highs)
pnorm(sample.score)

# In this case, January will be colder than freezing 65% of the time in
# Ithaca.  How often will it be warmer than freezing?

# We can compute the probability that the temperature will fall between two
# values with only slightly more work.  We just need to compute the
# probabilities for each and take the difference.  Lets see what the
# probability is that our average temperature will fall between 22F and
# 32F.

sample.score1 <- (32 - mean(ithaca.highs))/sd(ithaca.highs)
sample.score2 <- (22 - mean(ithaca.highs))/sd(ithaca.highs)
prob1 <- pnorm(sample.score1)
prob2 <- pnorm(sample.score2)

total.prob <- prob1 - prob2

# So there is a 40% chance that the average high temperature for a given
# January will fall in the range of 22F and 32F.  

# An additional important data distribution is the Gamma distribution.  The
# gamma distribution has the following PDF:

# f(x) = ((x/beta)^(alpha-1)*exp(-x/beta))/(beta*gamma(alpha))

# where gamma is the gamma function, defined by:

# gamma(k) = integral(0 to INF) t^(k-1) e^-t  dt

# A numeric solver for gamma is in R, simply the gamma function.  

gamma(-100)
gamma(100)

# We can thus compute the PDF depending on our alpha and beta for our
# given data distribution.  We call beta our scale parameter and alpha 
# our shape parameter.  The challenge for using the gamma distribution
# on a particular set of data is finding these parameters.  Typically,
# the data are scaled by the distribution's value of beta, such that 
# a new variable, xi, is defined as:

# xi = x/beta

# and this will have a beta value of 1.  Lets experiment with these parameters 
# some to see what they do.

set.seed(100)

# We can use the rgamma command to provide random gamma distribution 
# numbers.  Lets generate 1000 random numbers with a beta of 1 and alpha
# of 4.

sample.gamma <- rgamma(1000,4,1,1)

# As you can see, most of the weight in the gamma distribution
# is in the left side of the distribution.  Wonder what the skewness
# is on this dataset?

skewness(sample.gamma)

# The first one represents the rate parameter, which is simply 1/scale.
# This tells us that our dataset has a long right tail, which we see in
# the histogram.  

hist(sample.gamma,nclass=bin.size(sample.gamma))

# Lets try a different shape parameter, say alpha = 10

sample.gamma <- rgamma(1000,10,1,1)
hist(sample.gamma,nclass=bin.size(sample.gamma))

# In this case, the distribution appears more like a normal distribution,
# since more weight was added into the left tail.  Lets compute the 
# skewness here.

skewness(sample.gamma)

# The value closer to zero is a good sign that it is nearer to the normal
# distribution.  What do you think will happen when we set alpha = 100?

sample.gamma <- rgamma(1000,100,1,1)
hist(sample.gamma,nclass=bin.size(sample.gamma))

# Now the distribution is nearly symmetric.  Our skewness should be 
# pretty small in this case too.  Lets see if it is.

skewness(sample.gamma)

# What happens now if we set alpha = 1?

sample.gamma <- rgamma(1000,1,1,1)
hist(sample.gamma,nclass=bin.size(sample.gamma))

# This distribution is HEAVILY skewed, with a very long right tail.  
# This should provide you with an idea of how changing alpha changes
# the distribution.  

skewness(sample.gamma)

# What about beta.  Lets leave alpha = 100, and change beta to 2.

sample.gamma<-rgamma(1000,100,0.5,2)
hist(sample.gamma,nclass=bin.size(sample.gamma))

# As you can see, the distribution has been changed a little, but still
# is not that different.  What about beta = 100?

sample.gamma<-rgamma(1000,100,0.01,100)
hist(sample.gamma,nclass=bin.size(sample.gamma))

# The appearance is the same, but the values are much larger.  Hence, 
# increasing beta increases the scale of the data.  So we can set our
# size of our dataset and our shape using these parameters.  This distribution
# is especially useful in rainfall studies.  Why?

# There is a purely objective way to compute a "first guess" value for alpha
# and beta, using the data in the distribution.  To do this, we first need
# to compute a sample statistic D:

# D = log(mean(x)) - (1/length(x)) * (sum(log(x)))

# This statistic will then be applied to get guess values for alpha and
# beta; call them alpha.hat and beta.hat

D <- log(mean(sample.gamma)) - (1/length(sample.gamma)) * sum(log(sample.gamma))

alpha.hat <- (1+sqrt(1+(4*D/3)))/(4*D)
beta.hat <- mean(sample.gamma)/alpha.hat

alpha.hat
beta.hat

# The values for these are close to 100, which was our actual numbers from
# our data distribution.  This is a pretty good result!  I have provided
# a function that requires a data input and will provide you a guess
# alpha and beta for your distribution.  The function is based off of the
# second set of equations in Wilks (4.43a and b) and is called gamma.par.

gamma.par(sample.gamma)

# One final distribution of interest is the lognormal distribution.  This 
# distribution has a PDF of the form:

# f(x) = (1/(x*sd(y)*sqrt(2*pi)))exp[-(ln(x)-mean(y))^2/(2*var(y))]

# The normal distribution can be easily transformed into this distribution
# by simply using the samples from the regular normal distribution.  That is:

# mean(x) = exp(mean(y) + var(y)/2)
# var(x) = exp([var(y)]-1)*exp(2*mean(y)+var(y))

# Lets try an example to see if this really works.  Our transformed variable
# in the lognormal distribution is y = ln(x).  If we perform this
# transformation, we can simply use standard Gaussian methods to use the
# distribution.  Lets sample 500 random normal numbers.

set.seed(100)
sample.data<-rnorm(500,0,1)
sample.log<-log(sample.data)

# Right off the bat we run into trouble.  The lognormal distribution
# does not exist for negative normal distribution numbers.  Lets change
# our input distribution to have a mean of 100 and standard deviation of 
# 10.

set.seed(100)
sample.data<-rnorm(500,100,10)
sample.log<-log(sample.data)
hist(sample.log,nclass=bin.size(sample.log))

# Now, if we try to transform our sample.log mean and variance back
# into the original mean and variance of the original data, we simply 
# can use:

mean.orig <- exp(mean(sample.log)+(var(sample.log)/2))

# As you can see, our data value is almost 100!  This is what we
# expected, so this is a good result.  What about the variance?

var.orig <- (exp(var(sample.log)) - 1) * exp(2*mean(sample.log)+var(sample.log))

# And this variance is near 100, so that the standard deviation is near 10.
# Hence, we were able to recreate our original mean and sample statistics.
# What could we do to recreate our original data?

# The lognormal dataset is a commonly assumed data transformation when 
# trying to test different options for your data.  

# Wilks provides other examples of data distributions, including the exponential
# distribution, which is a special case the gamma distribution with 
# of alpha = 1, 1/beta intersects the x axis, which would be
# worthwhile to read through, but we will not discuss them in class.  These
# include the beta distribution, the Weibull distribution, the lognormal
# distribution, and the Gumbel distribution.  It is also possible to create
# multivariate (more than one variable) distributions, but we will not
# discuss this further.  If you require additional information on these
# topics, see Wilks.