## Now that we have studied the different possible probability
## distributions, we can look at the idea of hypothesis testing.  The
## hypothesis test proceeds via a set of 5 steps:

## 1)  A test statistic that is appropriate to the data and question at
## hand.  This is the quantity computed from the data values that will be
## the subject of the test.  This is often a test statistic from a sample
## for parametric testing, and for nonparametric testing, there is no limit
## to what could be used.

## 2)  A null hypothesis must be defined, typically defined as Ho.  This
## hypothesis constitutes a logical frame of referencce against which to
## judge the observed test statistic.  The goal of hypothesis testing is to
## reject the null hypothesis in most situations.

## 3)  Define an alternative hypothesis Ha.  This is often assumed the
## simple solution that Ho is not true.  

## 4)  Obtain the null distribution, which is the sampling distribution of
## the test statistic provided that the null hypothesis is true.  This
## sampling distribution can be parametric, one that is near parametric, or
## purely empirical.  

## 5)  Compare the test statistic to the null distribution. If the test
## statistic falls in a reasonably improbable region of the null
## distribution, Ho is rejected.  If the statistic falls within a reasonable
## area, it may not be possible to reject Ho, but it still may not be true.

## The sufficiently improbable region of the null distribution is called the
## rejection level, or simple the level, of the test.  The null hypothesis
## is then rejected provided that the probability of the test statistic and all
## other results that are farther from the distribution center (denoted as 
## the p-value)  are less than
## or equal to an arbitrary test level.  The test level, often chosen in
## advance by the user, is usually anywhere between 10%, 5%, and 1%, giving
## a 90%, 95%, and 99% confidence that the null hypothesis should be
## rejected.  Typically in meteorology, the 95% level is used.

## There are two types of errors associated with hypothesis testing.
## Consider type I errors, which are falsely rejecting the null hypothesis and
## are given a probability of alpha.  Type II errors do not reject the null
## hypothesis when it should have been rejected, and are denoted as beta.
## In the textbook, a sample problem with both a null distribution and
## alternative distribution is presented.  The overlap region in these
## represents the areas of beta and alpha.  It is apparent that it is not
## possible to reduce both error types at the same time.  These
## probabilities can be set by defining a value for alpha, but it is not
## possible to define a value for beta.  Decreasing alpha too much increases
## beta, and reduces the reliability of the test.  Thus, these errors must
## be carefully considered when doing hypothesis testing.

## Two types of hypothesis tests exists, those that are one sided and two
## sided.  The one-sided test is relevant if prior knowledge of the dataset
## indicates that violations of the null hypothesis will occur on a specific
## side, or tail, of the distribution.  In contrast, a two-tailed test is
## one in which both sides of the null distribution are unfavorable, i.e.
## you don't want values that are too high or too low.  We will look at
## examples of both types of tests below.

## One final useful property of hypothesis testing is observed by inverting
## a hypothesis test.  By inverting hypothesis testing, we can create
## confidence intervals around sample statistics.  These intervals contain a
## specified amount of probability of a population quantity.  For example,
## one can use a specified amount of the population distribution to
## determine the confidence interval.  In essence, the confidence intervals
## are computed by assuming a hypothesis test with a null hypothesis that
## the two distributions of data are identical.  Then, we can determine the
## range of the sample statistic from the population distribution in which
## you could not reject Ho.

## Lets look at some hypothesis tests and implement these ideas in examples.

## Lets first create a population and a sample to do some hypothesis testing
## on.

set.seed(100)

mu <- 0

sample <- rnorm(50,0,1)

n <- length(sample)

## Several hypothesis testing methods exist.  The first test we will 
## consider is the z-test.  The z-test uses the following test statistic:

z.statistic <- (mean(sample) - mu)/(sd(population)/sqrt(n))

## This statistic can be used to test if the sample distribution is a
## strictly normal distribution with a mean of mu.  That is, we can test if
## our sample mean is within the 95% confidence limit of mu, such that we
## can make the assumption that our sample data came from a population with 
## a mean of mu.  We can compute confidence intervals based on a given alpha level that
## we choose.  Commonly in statistics, we want a confidence of 95%, which
## implies an alpha level of 0.05.  In that case, we want the positions in
## the sample distribution that are within the 2.5 %ile and the 97.5%ile of
## the population distribution.  We need certain positions in the z
## distribution based on our alpha value.  R does not have a command to execute
## a z-test explicitly, so we will have to do it the brute force way.  Lets 
## do a simple z-test that the sample above came from the normal distribution
## population.  To do this, we first need to compute our z-score statistic,
## which we have already done:

z.statistic 

## Now, we need to look at what the position of our z.statistic is on the 
## standard normal distribution.  We can do this using the pnorm command:

pnorm(z.statistic)

## This value is probability swept out between -Inf and our z.statistic
## on the standard normal distribution.  In order to determine whether or not
## to reject the null hypothesis, we can approach it in two different ways.
## First, if our alpha level is defined as 0.05, we can check our p-value and
## determine if it is greater than 0.975 (the upper limit of the 95% confidence
## level) or less than 0.025 (the lower limit).  We can also compute the
## quantile value of our z.statistic

qnorm(z.statistic)

## and determine if it falls between the quantiles for 0.975 and 0.025, which
## are 1.96 and -1.96, respectively.  In both instances, our z.statistic
## does not meet the criteria for rejection, so we cannot reject Ho.

## What about testing if the gamma distribution with alpha = 1 and beta = 1
## has the same mu as the standard normal distribution?  Lets generate 
## a sample of gamma distribution numbers.

set.seed(50)
sample.gamma<-rgamma(50,1,1,1)

hist(sample.gamma,bin.size(sample.gamma))
n<-length(sample.gamma)

## What is the sample mean here?

mean(sample.gamma)

## This mean is not very near 1.  What about the sample standard deviation?

sd(sample.gamma)

## This value is smaller than 1!  These data don't seem to fit the 
## null hypothesis of mu = 0.  Lets compute the z.statistic and 
## try it.

z.statistic<-(mean(sample.gamma)-mu)/(sd(population)/sqrt(n))
z.statistic

## This z statistic is MUCH larger!  Lets do the pnorm command on it to see 
## what we get.

pnorm(z.statistic)

## This probability is basically 1, so it is outside the level of 0.975.  Also,
## the value of the z-statistic is larger than the 97.5th percentile of 1.96.  
## That means the sample mean is significantly different and the null hypothesis
## should be rejected.  This makes sense based on what we saw.

## Lets finally test a set of sample grades from a previous class.  
## Lets assume we want to test whether or not our set of grades is derived from
## a population of grades with an average of 75, for the purpose of curving.  

mu <- 75

## Lets see if the following set of grades follows the normal distribution.

test.grades<-c(75,76,100,92,79,45,62,80,91,77,69,72,75,78,88,61,51,85,77,72,69,66,85,0,99,77,86,79,91,89,85,57,52,90,61,58,77,49,95,39)
n <- length(test.grades)

## We can test these data using a z-test.  Lets compute our z statistic.

z.statistic<-(mean(test.grades) - mu)/(sd(grades)/sqrt(n))

## Lets do the pnorm command on our z statistic.

pnorm(z.statistic)

## which means our p-value is 0.020, which is slightly outside of the
## 95% rejection level.  If we set our rejection level to 99%, we would 
## not reject the null!  So, more subjectivity enters into our work.  This is one
## of the many "gray" areas in statistics, but it is often more common to
## use the 95% rejection level.  

## This test is useful for the normal distribution, but when we have a small
## sized data sample (typically less than 30 datapoints), we may have extra
## variance in the tails.  As such, the shape of the distribution will be modified
## somewhat.  To account for this, the Student's t-test was created.  
## Lets look at the t-test next.

## The t-test examines the null hypothesis that a sample mean was
## drawn from a population characterized by some population mean mu, the same as
## with the z-test.  Provided the number of data values is large enough 
## (greater than 30), the sampling distribution is essentially Gaussian 
## (via the central limit theorem).  The test statistic is simply the same 
## as for the z-statistic:

n <- length(sample)
mu <- 0
t.stat <- (mean(sample) - mu)/(sd(sample) / sqrt(n))

## We can define another parameter, called the degrees of freedom.  Fewer
## degrees of freedom in a test statistic indicate larger amounts of
## probability in the tails and a more narrow distribution.  The degrees of
## freedom for the above statistic is simply v = n - 1.  Larger values of v
## indicate smaller variance and tendency of the t-distribution to head
## towards a normal distribution.  For our test dataset:

mu <- 0
set.seed(100)

sample <- rnorm(25,0,1)

n <- length(sample)

t.stat <- (mean(sample) - mu)/(sqrt(sd(sample)^2 / n))


## Our t.statistic must be compared to the quantiles based off of the t-distribution.
## The t-distribution quantiles are computed via the qt command.

qt(0.975,24) #Here, 24 = v the number of degrees of freedom
qt(0.025,24)

## We can use the pt command to get the p-value:

p.value <-pt(t.stat,24)

## We use 24 since it is the number of degrees of freedom (n-1).  Both our 
## probability limits and our quantiles include our t-stat, so we cannot
## reject the null hypothesis.

##  Lets try an example with a non-normal distribution.

sample <- c(1:20)
n <- length(sample)

t.stat <- (mean(sample) - mu)/(sd(sample) / sqrt(n))

## Here, our t statistic is huge!  Obviously, a sequence of numbers from 1
## to 20 is not Gaussian, and the mean of this is not near zero, but instead
## near 10! How can we fix this?  Lets try a sample where the mean is zero.

sample<-numeric(21)

for (i in 1:21) {
    
sample[i]  = ((i-1) - 10)/10

}

n<-length(sample)

## What do you suppose will happen here?

t.stat <- (mean(sample) - mu)/(sd(sample)/sqrt(n))

## Here, the value for t is slightly negative.  Our p-value in this case would
## be:

p.value<- pt(t.stat,n-1)

## Our p-value is large enough that we cannot reject the notion that this 
## sample came from a population with a mean of
## 0.  This is good, since the mean is zero in this sample.  However, this
## tells us nothing about the underlying data distribution, which could be
## important!  What can we use to plot our data distributions in summary form?
population <- rnorm(1000000,0,1)
boxplot(sample,population)

## There are obviously some differences between the two boxplots.  R has a 
## command to implement a t.test called, strangely enough, t.test.

set.seed(100)

sample<-rnorm(50,0,1)

n <- length(sample)

## Now the t-test.

t.test(sample,alternative="g") ### Alternative =g is a one-sided test where
                               ### we're trying to see if our test statistic
                               ### is greater than our 0.95 limit for alpha=0.05

## Here, our t-test tells us that our p-value is 0.2425, which is much
## larger than our 0.05 p-value needed to reject to a 95% confidence level.
## Hence, as was concluded previously, we cannot reject the null
## hypothesis that the two are the same, which is a good result.

## We can also use the t-test for a one-sided test instead of a two-sided test.
## Try to think of a physical situation in meteorology arises where it is
## thought that most of the null hypothesis violations will occur in one
## tail of the null distribution.  In math, consider an example where a test
## statistic involves a squared difference between distributions. If we test
## the idea that the null distribution's mean is zero, the left side of the
## distribution will be highly supportive of this idea, while large
## differences will lead to larger values in the right hand side of the
## distribution.  Thus, only right tailed probabilities would be of
## interest.  Lets try this as an example.

set.seed(100)

sample1 <- rnorm(25,0,1)
sample2 <- rnorm(25,10,12)

squared.diff <- (sample1 - sample2) ^ 2

## We can do a one-sided t-test to see if the means are the same using the
## following version of the t.test function:

t.test(squared.diff,alternative="g")

## This outcome says that our sample mean computed from squaring differences
## is not the same as our normal distribution.  This is correct of course.
## What happens if we make the test two-sided?

t.test(squared.diff,alternative="t")

## Still the same answer.  The two-sided test p-value is double what the
## one sided t-test p-value is.  Do you understand why?  

## Another commonly used hypothesis test is called the F-test.  The F-test
## tests the probabilty that variances from two populations that originate
## from the normal distribution are indeed the same.  The F statistic is
## simply computed as:

## F <- (var(sample1)/var(population1))/(var(sample2)/var(population2))

## Where sample1 and sample2 are pulled from their corresponding population
## distributions.  We can adapt this equation to a simpler version, testing
## whether or not the ratio var(population1)/var(population2) = 1 falls
## outside the 1 - alpha confidence interval (i.e. if the lower confidence
## interval is greater than 1 or the upper is less than 1).  We can conduct
## this test using an F statistic of:

## F <- var(sample1) / var(sample2)

## and reject the null hypothesis that the two have the same variance if the
## value of the F statistic falls outside this confidence level.  Our F 
## distribution is a function of the degrees of freedom of the two underlying
## populations.  Hence, we need to compute these, and can do so the same way
## as with the t-test (i.e. n-1 for each).  Lets create a couple of samples 
## and do an F-test for the alpha=0.05 confidence level.

sample1 <- rnorm(500,0,1)

sample2 <- rnorm(500,0,1)

v1 <- 500 - 1        ## Degrees of freedom in sample 1
v2 <- 500 - 1        ## Degrees of freedom in sample 2

## The corresponding F statistic is:

F <- var(sample1) / var(sample2)

## We can use the pf function to determine the probability associated 
## with our test F statistic. 

p.value <- pf(F,v1,v2)

## Our p-value of 0.3 is larger than our rejection threshold of alpha=0.05, so
## we cannot reject the null hypothesis that the variances are the same.

## We can also use quantiles by simply doing:

qf(0.025,v1,v2)
qf(0.975,v1,v2)
F

## and comparing whether F falls between the two quantiles.  This is analagous to
## what was done for t and z tests.

## Lets try to increase the variance in the second sample to see what happens
## to our F-test.

set.seed(10)
sample1 <- rnorm(500,0,1)

sample2 <- rnorm(500,0,1.5)

## we can try to recompute our F statistic.

F <- var(sample1) / var(sample2)

p.value <- pf(F,v1,v2)

## Our p-value is near zero, so we should reject Ho.  
## The changes are large enough here to allow us to reject the null.
## We can write a simple function to do an F test.  Lets call it f.test.
## Lets also test out a situation where our variance is only slightly
## different between distributions.

sample1<-rnorm(500,0,1)
sample2<-rnorm(500,0,1.1)
f.test(sample1,sample2,0.05)

## We get a final answer of false, which means we do NOT reject the null
## hypothesis.  This will be a useful test for our future analysis.  

## Another important hypothesis test that can be utilized to compare the
## distributions of two datasets is called the chi-squared test.  The chi-squared
## test is a goodness of fit test that tests whether the data you provide
## came from the hypothesized null distribution.  The test is most widely used
## with discrete data, because it requires the breaking of datasets into
## bins which is easily accomplished with discrete inforamtion.  However, this
## breaking up of the data can be done with continuous data as well, risking
## losing important information between the bins.  The statistic is basically:

## chi.squared = sum((#observed in a bin - n * probability of the bin) ^2 / (n*probability of the bin))

## If we apply the null hypothesis that the data were drawn from the fitted
## distribution, the sampling distribution of the data is simply the chi-squared
## distribution with parameter v = #classes - #parameters - 1 degrees of freedom. 
## This test is always one sided because the test statistic involves the squaring
## of the numerator.  Small values of the test statistic support Ho.  We can 
## get the quantiles for the chi-squared distribution via the qchisq and pchisq
## commands.  Lets do an example from the textbook using our Ithaca data.

## We need to read in a new dataset to do this test.  It is called ithaca_precip.txt.

ithaca.precip <- matrix(scan("ithaca_precip.txt"),ncol=2,byrow=T)

## We are going to compare how well the gamma distribution or the 
## gaussian distribution fits this dataset.  To do the gamma distribution,
## we must first get hypothetical values for alpha and beta using the gamma.par 
## function.

gammapars <- gamma.par(ithaca.precip[,2])

## We need the sample mean and sample standard deviation to apply it to the
## Gaussian distribution.  These would be:

ithaca.mean <- mean(ithaca.precip[,2])
ithaca.sd <- sd(ithaca.precip[,2])
n <- length(ithaca.precip[,2])
## Lets plot histograms of all three, to see what we think!

hist(ithaca.precip[,2],bin.size(ithaca.precip[,2]))
hist(rgamma(100000,shape=3.76,rate=1/0.52),bin.size(ithaca.precip[,2]))
hist(rnorm(100000,ithaca.mean,ithaca.sd))

## Visual inspection says that the gamma distribution is best, but lets
## be sure.  We can divide our precip data into six classes (the number
## is arbitrary, we could have done 49 classes, but that would not have
## been interesting.  These six will be between 1 and 3 inches over half
## inch ranges.  We can write a lengthy for loop to count these for us.

count.class1 <-0
count.class2 <-0
count.class3 <-0
count.class4 <-0
count.class5 <-0
count.class6 <-0
for (i in 1:length(ithaca.precip[,2]))  { ### Begin the for loop

if (ithaca.precip[i,2] < 1) { #If test for class 1
count.class1 <- count.class1 + 1
} ## End if

if (ithaca.precip[i,2] >= 1 && ithaca.precip[i,2] < 1.5) { #If test for class 2
count.class2 <- count.class2 + 1
} ## End if

if (ithaca.precip[i,2] >= 1.5 && ithaca.precip[i,2] < 2) { #If test for class 3
count.class3 <- count.class3 + 1
} ## End if

if (ithaca.precip[i,2] >= 2 && ithaca.precip[i,2] < 2.5) { #If test for class 4
count.class4 <- count.class4 + 1
} ## End if

if (ithaca.precip[i,2] >= 2.5 && ithaca.precip[i,2] < 3) { #If test for class 5
count.class5 <- count.class5 + 1
} ## End if

if (ithaca.precip[i,2] >= 3) { #If test for class 6
count.class6 <- count.class6 + 1
} ## End if

} ## End the for loop

count.class1
count.class2
count.class3
count.class4
count.class5
count.class6

## These should sum to 50, which they do.  We now need to figure out
## how much probability ineach range is swept out by each distribution.  We
## can then get an expected value for each range and look at how well they
## fit.  Lets start with the gammas.

## Gamma probs

gamma.probs1 <- pgamma(1,3.76,1/0.52)
gamma.probs2 <- pgamma(1.5,3.76,1/0.52)-gamma.probs1
gamma.probs3 <- pgamma(2,3.76,1/0.52) - gamma.probs1 - gamma.probs2
gamma.probs4 <- pgamma(2.5,3.76,1/0.52) - gamma.probs1 - gamma.probs2 - gamma.probs3
gamma.probs5 <- pgamma(3,3.76,1/0.52) - gamma.probs1 - gamma.probs2 - gamma.probs3 - gamma.probs4
gamma.probs6 <- 1 - pgamma(3,3.76,1/0.52) # Why is this 1-pgamma?

## We can compute our expected number from our gamma distribution by simply
## multiplying n by the probabilities.

gamma.expect1 <- n*gamma.probs1
gamma.expect2 <- n*gamma.probs2
gamma.expect3 <- n*gamma.probs3
gamma.expect4 <- n*gamma.probs4
gamma.expect5 <- n*gamma.probs5
gamma.expect6 <- n*gamma.probs6

## Lets do the same thing for the Gaussian distribution.

gauss.probs1 <- pnorm(1,ithaca.mean,ithaca.sd)
gauss.probs2 <- pnorm(1.5,ithaca.mean,ithaca.sd)-gauss.probs1
gauss.probs3 <- pnorm(2,ithaca.mean,ithaca.sd) - gauss.probs1 - gauss.probs2
gauss.probs4 <- pnorm(2.5,ithaca.mean,ithaca.sd) - gauss.probs1 - gauss.probs2 - gauss.probs3
gauss.probs5 <- pnorm(3,ithaca.mean,ithaca.sd) - gauss.probs1 - gauss.probs2 - gauss.probs3 - gauss.probs4
gauss.probs6 <- 1 - pnorm(3,ithaca.mean,ithaca.sd) # Why is this 1-pgauss?

## We can compute our expected number from our gauss distribution by simply
## multiplying n by the probabilities.

gauss.expect1 <- n*gauss.probs1
gauss.expect2 <- n*gauss.probs2
gauss.expect3 <- n*gauss.probs3
gauss.expect4 <- n*gauss.probs4
gauss.expect5 <- n*gauss.probs5
gauss.expect6 <- n*gauss.probs6

## Question, if we sum all our expected values for both distributions, what
## will we get?

sum(gauss.expect1,gauss.expect2,gauss.expect3,gauss.expect4,gauss.expect5,gauss.expect6)

## We can try to put all of this in a table to make it easier to visualize.

gamma.probs <- round(c(gamma.probs1,gamma.probs2,gamma.probs3,gamma.probs4,gamma.probs5,gamma.probs6),3)
gauss.probs <- round(c(gauss.probs1,gauss.probs2,gauss.probs3,gauss.probs4,gauss.probs5,gauss.probs6),3)
gamma.expect <- round(c(gamma.expect1,gamma.expect2,gamma.expect3,gamma.expect4,gamma.expect5,gamma.expect6),3)
gauss.expect <- round(c(gauss.expect1,gauss.expect2,gauss.expect3,gauss.expect4,gauss.expect5,gauss.expect6),3)

obs <- c(count.class1,count.class2,count.class3,count.class4,count.class5,count.class6)
data.frame(rbind(obs,gamma.probs,gamma.expect,gauss.probs,gauss.expect))

## Visually inspecting the numbers, it looks like the gamma distribution is
## better.  Lets do the hypothesis test now that we have all of our
## information.  We now need to compute our chi-squared values for both
## gamma and gaussian results.

chi.square.gamma <- sum((obs-gamma.expect)^2/gamma.expect)
chi.square.gauss <- sum((obs-gauss.expect)^2/gauss.expect)

## Now then, we can use the chi-squared distribution to determine if we should
## reject Ho.  The Ho's we are considering are whether or not the data we 
## have has a gamma or a gaussian distribution.  Lets do gamma first.

## We need to compute our degrees of freedom v. Recall that:

## v = #classes - #parameters -1
## So,

v <- 6 - 2 - 1 ## The 2 represents fitting two parameters; alpha and beta

## Lets get our threshold values for a 0.05 alpha level.

qchisq(0.025,v)
qchisq(0.975,v)

## We can look at these and our values of chi.square.gamma and chi.square.gauss
## and determine that indeed, the Ho for the gamma distribution is not rejected,
## but Ho for the gaussian distribution should be!

## CONFIDENCE INTERVALS!! ##

## It was mentioned above that it is possible to invert hypothesis tests as
## well, obtaining what are known as confidence intervals.  These are 
## extremely important when doing comparative statistics, as they allow
## the researcher to obtain an idea of whether or not their result is
## statistically significant.  It follows from the central limit theorem
## that the mean values of any distribution are normally distributed.  This
## is very useful information, because we can construct a confidence limit
## around the mean based on this information.  If we want to find the confidence
## limit, we simply need to do:

## CI(lower) = mean(x) - qnorm(alpha/2)*(sigma(x)/sqrt(n))
## CI(upper) = mean(x) + qnorm(1-alpha/2) * (sigma(x)/sqrt(n))

## This gives us the confidence limits for the expected value for our
## population mean mu, regardless of the underlying distribution of x.  
## Typically, the most common alpha levels are 0.1,0.05,and 0.01, representing
## 90%, 95%, and 99% confidence levels.  The most common of these is the 95%
## confidence level.  Lets create an example using the lognormal distribution,
## and see if we can get a reasonable confidence interval around its expected
## value.  We will specify that value in advance, so we know what we will get.

set.seed(101)
sample.ci <- rlnorm(10000,2,1)

## In this command, the mean and standard deviation are on the log scale
## (i.e. mean of 2 means a mean of e^2, etc.)

## Lets calculate our mean and see if we are close to what we should get.

log(mean(sample.ci))

## As you can see, we are off slightly, but pretty close.  Now, we can view
## the histogram:

hist(sample.ci,bin.size(sample.ci))

## Clearly this is not normally distributed.  However, lets now try to
## get a 95% confidence interval around the mean.  Remember, we only need
## to use the normal distribution, no matter what the underlying distribution
## is, because of the central limit theorem.

lower.ci <- mean(sample.ci) + (qnorm(0.025)*(sd(sample.ci)/sqrt(length(sample.ci))))
upper.ci <- mean(sample.ci) + (qnorm(1-0.025) * (sd(sample.ci)/sqrt(length(sample.ci))))

## So, our confidence limits are:

lower.ci
upper.ci

## Which means the expected value of our distribution lies somewhere in
## that range.  For the rlnorm distribution, the expected value is
## not just the mean, but is instead represented by:

## E(x) = exp(mu+0.5*sigma^2)

## meaning that for our answer, our E(x) would be:

expected.value <- exp(2 + 0.5*(1^2))

## Lets see if this is correct.

lower.ci
expected.value
upper.ci

## As you can see, our confidence limits do encompass our expected value,
## so we are finished.  It is highly beneficial to construct confidence limits
## on almost everything you do, especially if you are comparing different
## results with each other.  I have written a simple function that will
## compute the confidence limits you desire, given the input dataset.
## This function is called limits.ci.

limits.ci(sample.ci,alpha=0.05)

## We can also do confidence limits on the t-distribution, since it is 
## approximately the normal distribution with different weighting.  If we 
## assume that our population variance is unknown (likely the case) and
## we do not have a very big sample (less than about 30), our sampling
## variability will be high, so we need to use the t-distribution
## to account for this.  Then, we can replace our sigma with our
## standard deviation.  The new confidence limits can then be approximated
## by:

## lower.ci <- mean(x) + qt(alpha/2,n-1)*(sd(x)/sqrt(n))
## upper.ci <- mean(x) + qt(1-alpha/2,n-1)*(sd(x)/sqrt(n))

## Lets create a small sample based on the uniform distribution.
set.seed(101)
small.sample <- runif(10,0,1)

## We have this small sample and want to create a confidence interval around
## the expected value for this distribution, which is obviously (a+b)/2

## Our confidence limits at a 95% confidence level are:
alpha <- 0.05
n<-length(small.sample)
lower.ci <- mean(small.sample) + qt(alpha/2,n-1)*(sd(small.sample)/sqrt(n))
upper.ci <- mean(small.sample) + qt(1-alpha/2,n-1)*(sd(small.sample)/sqrt(n))

## As you can see, our confidence limit does include our expected value, which
## is a good result. The limits.ci function will do the t-test too, based on
## sample size (for sample sizes less than 100, the t-test will execute instead
## of the z-test). 

limits.ci(small.sample,alpha=0.05)

## This is a useful function for your work, so I would strongly encourage you
## to use it!