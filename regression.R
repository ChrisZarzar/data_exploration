## We have looked at some of the basics of statistics.  Now we can turn our 
## focus to an important application of statistics in meteorology, which is
## through modeling.  Modeling via statistics is accomplished through different
## statistical formulas, which are obtained via a method known as regression.
## Several regression techniques exist, and we will consider each one and its
## unique applications.  

## We can investigate statistical modeling by first describing different 
## regression types.  The most commonly utilized regression type is purely 
## linear regression.  Linear regression is often called a least squares approach.
## It is so named because the method seeks to minimize the error of the
## regression line by fitting a line which has the smallest total sum of
## squared residuals (the line which is nearest to the inputdata.  This method
## can be utilized in nonlinear regressions as well, which we will see a little
## later on.
## With linear regression, we simply use a linear
## equation (i.e. y = mx + b) to describe the relationship between two
## datasets.  The equation for linear regression is typically given to be:

## y = a + bx

## where b is defined as:

## b = n(sum(x*y) - sum(x)sum(y)) / n(sum(x^2)) - sum(x)^2

## and a, the y intercept, can be defined as:

## a = mean.y - b * mean.x

## We can create an R script to compute this intercept parameter and
## the values for a, or we can use the R function lm.  Lets do it
## the brute force way first.  We can do this experiment with our Ithaca
## data, and test the linear relationship between high temperature and
## low temperature.  Thus, we can set our high temperature data to x and our
## rainfall data to y.

ithaca.data<-matrix(scan("ithaca_data.txt"),ncol=4,byrow=T)
           
## Lets assign the data their corresponding variable names.  We can call x
## the predictor, since we are hoping to predict low temperature with high
## temperature, and y the predictand, or the low temperature.
 
 predictor <- ithaca.data[,3]
 predictand <- ithaca.data[,4]
 
## Now, lets compute our slope for our line.
 
 n <- length(predictor)
 
 slope.num <- (n*(sum(predictor*predictand)) - sum(predictor)*sum(predictand))
 slope.denom <- (n*(sum(predictor^2)) - sum(predictor)^2)
 
 slope <- slope.num/slope.denom
 
## Lets also compute our y.intercept.
 
 y.intercept <- mean(predictand) - slope * mean(predictor)
 
## Now then, we have a line that is defined by a slope and y-intercept that
## describes the linear relationship between high temperature and low temperature.
## We can plot the datapoints in a scatterplot, and they overlay
## our line to see how well the line describes the fit between the data (if
## there is a fit).  We can use the abline function to plot our line.
 
plot(predictor,predictand)
abline(y.intercept,slope)

## This fit seems okay, but most of the data don't follow a pure linear 
## regression.  Now that we have our values for a and b, we can write a 
## prediction equation:

y.intercept
slope

## Prediction equation:  y.hat <- -24.85 + 1.267 * x

## With this prediction equation, we can get predicted values based off of 
## a new set of data.  Lets try some Ithaca January data from 2009!


 test.highs <- c(19,18,24,31,12,10,8,11,23,22)
 test.lows <- c(3,3,5,13,3,-4,-2,-9,8,1)
 
## We can test both our original line and our new line.

 model.lows <- test.highs * slope + y.intercept
 
## This year's winter in Ithaca was exceptionally cool, so there were some
## pretty big errors in our low temperature forecasts.   We can actually look
## at the two next to each other to see how they look:

 data.frame(test.lows,model.lows)

## As you can see, a few of the really cold days were way off in the cold direction.
## Our model forecasted way too cold of temperatures!! 

## There are objective error measurements for a regression, based off of the
## difference between the observed and predicted values for y.  This
## difference is called the residuals, and can be written as:

## resid = y - y.hat
 
## The y.hat term represents predicted values of y, while y represents the
## observed values.  The distribution of the residuals is often a telling 
## feature of your regression.  The current approach to regression can make
## an assumption that the mean of these residuals is zero, based on the
## definition of the least squares simple linear regression.  Therefore, it
## makes more sense to describe the residuals based on their variance 
## around the mean.  Thus, we should instead define our standard error based
## on the variance of these residuals.  This is typically written as:

## standard.error <- (1/(n-2)) * sum(residual^2)

## The n-2 exists because we estimated two parameters in our linear regression.

## For our example above:
 
 resid <- test.lows - model.lows

 hist(resid)

## The histogram shows us that our model predicted lows that were too 
## cold in most circumstances.  If we have positive residuals, the value
## for y.hat is too small!  Of our data, we had 7 out of 10 residuals that
## were positive, so our model has some bias.  
 
## We can look at many different error statistics that are associated with
## a linear regression method.  The first was described previously, and is known
## as standard error.  We can also look at the sum of squares of the residuals, which is the
## sum of squares error (SSE). This error provides information about how large the residuals are.  Larger residual
## values will increase the SSE.  The mean square error (MSE) is simply the SSE
## divided by the number of degrees of freedom of the residuals, which is n-2, since
## two parameters are predicted by the regression (a and b).    

## Another possible measure of the performance of regression is called the
## regression sum of squares (SSR).  The SSR is given as:

## SSR = (slope^2) * sum(x - mean.x)^2
## This equation is based off of the difference between y.hat and the 
## mean of y, which indicates that a regression line which deviates very
## little from the samle mean of y values will have a very small slope and 
## small differences between mean.y and y.hat, thus a small SSR.  This can
## help provide information about how closely the residuals are spread about
## the mean, or how good the mean is at predicting.  If you have
## small SSR, it tells you the behavior of your regression will always be near
## the mean!  This is not necessarily desirable, since you want x to provide the
## ability to predict, not the mean of y!  Small SSR is not a desirable property
## of a regression.  You can derive the SSR from sum(y.hat - mean.y)^2 to get it
## in terms of x, which is what was done above and is typically done to compute
## the SSR.  If we divide the SSR by the number of degrees of freedom (the
## number of x's, for univariate regression this value is always 1, then 
## we get the mean regression sum of squares.  This is not different from the
## SSR in the case of a univariate regression.

## One final quantity to consider is the total sum of squares (SST).  This is simply
## the equivalent of the difference between the mean of y and the y values.

## SST = sum(y-mean.y)^2

## Small values of the SST tell you that your initial input data are very 
## close to the mean and/or have very low variability.  This information will help
## you understand how good your regression is.  Smaller SST values tell you
## that a regression line fit using the mean of y (which is how we get the
## y intercept remember) is very descriptive of the behavior of the actual
## y data since they are close to the mean.  The mean total sum of squares
## (MST) is rarely computed, since it is just SST/(n-1), since there are n-1
## degrees of freedom in a dataset by definition, and this quantity is equivalent
## to the y variance, which has likely already been computed.

## We can summarize all of these error measurements in what is known as an
## analysis of variance (ANOVA).  This tool is used in 
## regression analysis.  We want to understand what is involved in an ANOVA
## table.  The typical ANOVA table includes:

## ========================================================================
##  Source      df         SS              MS
## 
## Total         n - 1      SST         nothing here
## Regression    1          SSR         MSR = SSR / 1        (F = MSR/MSE)
## Residual      n - 2      SSE         MSE = Se^2

## The ANOVA table often includes an F-statistic, which is just the quotient
## of the MSR and MSE values.  Smaller F values are indicative of a weaker
## regression, because small MSE values and large MSR values are indicative
## of a strong relationship between x and y.  So, the bigger the F, the better
## the regression!
 
## We can create an ANOVA table for these datasets using the anova.new 
## command which has been written specifically for a univariate regression.

## Lets check out the results for our regression using anova:
 
 anova.new(predictor,predictand)
 
## Our regression has large values of SSR and SSE.  However, the ratio 
## between MSR and MSE, the F statistic, is also large, which is a good
## result and tells us our regression is strong.  Obviously, we can predict
## low temperature with high temperature with some confidence!  Remember
## what F is, a measure of the ratio of sample variances.  For ANOVA, the sample
## variances are for the MSR and the MSE, and we want to know if they are 
## equal.  If they are, that says that x does not do a good job of predicting
## y, because the amount of error created by the regression is equal to the 
## difference in y.hat and its mean.  It instead says the mean of y has the strongest
## relationship, and tells us we have a weak regression!
 
## What do we get if we use two datasets that are equal to each other (i.e.
## x = y)?
 
 predictand <- predictor
 
## How does this relationship look?
 
 plot(predictor,predictand)
  
 anova.new(predictor,predictand)
 
## Here, our F statistic is infinity, so it is massive.  Also, our SSE
## is zero, which is expected since our data are perfectly linear.  Our MSE
## is zero as well, which we expect.  This is the extreme case where the
## data are perfectly linear.  It is unlikely that in your research you
## will see such a perfectly linear dataset...
 
## R of course has a function that will do our regression for us.  
## The function is the lm command (lm standing for linear model).  
## Lets look at the lm command.  

 predictor <- ithaca.data[,3]
 predictand <- ithaca.data[,4]

 test.model<-lm(predictand~predictor)
 
## We must write the equation we want to predict.  Here we are trying to
## predict y as a function of x, so we write y~x (the ~ is in place of an 
## equal sign, but is required by R syntax).  When we see what we got in 
## test.model:

 test.model

## As you see, the function computes a slope (labeled as the variable
## name predictor) and a y-intercept.  These are the same as what we
## came up with above.  Lets rerun that model to be sure.
 
## Lets compute our slope for our line.
 
 n <- length(predictor)
 
 slope.num <- (n*(sum(predictor*predictand)) - sum(predictor)*sum(predictand))
 slope.denom <- (n*(sum(predictor^2)) - sum(predictor)^2)
 
 slope <- slope.num/slope.denom
 
## Lets also compute our y.intercept.
 
 y.intercept <- mean(predictand) - slope * mean(predictor)
 
slope
y.intercept

## R has a function anova that can be used for regression objects.  We 
## can use that function to get our anova table for our R created model.

anova(test.model)

## You'll see it is slightly different than the other version, but the
## answers are the same.  It does include a p-value, which tells us the
## likelihood that x is a better predictor than the mean of y for predicting
## y.  This is useful, telling us if we have a statistically significant 
## relationship between x and y (we want our p value to be less than our
## desired alpha, typically 0.05).  Here our p-value is near zero!

anova.new(predictor,predictand)

## With the lm function, we can return different parts of interest.  If we 
## want our coefficients, we can simply type:

test.model$coefficients

## If we want the residuals, we simply use:

test.model$residuals

## These residuals are from the input data (i.e. the predicted y's from our
## initial data, not the new 2009 data!).

## We can also use the summary command to give us a nice summary of the
## properties of our regression equation.

summary(test.model)

## This gives us the 5 number summary of our residuals, the intercept and 
## slope for our line, as well as their associated errors, the residual
## standard error (the standard.error we discussed above), the F 
## statistic, and the R-squared value. The R-squared value represents the 
## proportion of the variance of the predictand that is described by the 
## predictor(s), so R-squared values near 1 are ideal.  Our R-squared value
## is near 1, so this is a good result!  R-squared is simply:

## R.squared <- (SSR/SST) = (1 - SSE/SST)

## We can also use the coefficients to test new data with our model.  

predictions<-test.model$coefficients[1] + test.model$coefficients[2]*test.highs

## or by doing a matrix multiply.  To do this, we first need to create a vector
## of 1s so we can include the y intercept (since it is multiplied by 1).

ones<-c(rep(1,length(test.highs)))
test.highs<-cbind(ones,test.highs) ## This binds the vector of 1s and highs together

predictions<-t(test.model$coefficients) %*% t(test.highs)

## These can be written into a function to get predictions.  I called the
## function predictlm.
test.highs <- test.highs [,-1]

predictlm(test.model,test.highs)

## Linear regression is the most common type of regression used in the
## geosciences.  However, it suffers from being the simplest.  Rarely are
## relationships between physical features purely linear.  It is possible
## to transform your data to become nonlinear by applying mathematical 
## functions throughout (i.e. instead of using pressure use the log of 
## pressure since pressure decreases in a logarithmic fashion).  
## However, the resulting prediction equation is still linear.  Linear regression
## is a good baseline when doing a statistical study, but additional regression
## types should be considered when doing more in-depth statistical modeling.

## In addition to a simple linear regression with one predictand and one
## predictor, it is possible to use many predictors in combination with a 
## predictand to get an answer (and this is much more commonly done).  This
## method is called multivariate (multi-variable) regression.  The equation
## is simply a multi-linear equation:

## y.hat <- y.intercept + slope.1 * x1 + slope.2 * x2 + ... + slope.n * xn

## The command in R for multivariate regression is the same as for simple linear regression
## only instead of a vector for x, you use a matrix with your predictors in
## your columns.  Lets try now to predict precipitation in Ithaca using the
## high temperatures and low temperatures in both Ithaca and Canadaigua.
## First, we need to put all of this data into our predictor matrix.  
canadaigua.data <- matrix(scan("canadaigua_data.txt"),ncol=4,byrow=T)

predictor.matrix <- cbind(ithaca.data[,3],ithaca.data[,4],canadaigua.data[,3],canadaigua.data[,4])
predictand <- ithaca.data[,2]

## Lets take a look at our predictor matrix.

predictor.matrix

## We can look at a multi-panel scatterplot of our data, similar to a correlation
## matrix, to look at individual relationships between the different
## predictors and the predictand.

pairs(cbind(predictand,predictor.matrix))

## Now, lets try to use R to get a linear model using all of these predictors.

multi.model <- lm(predictand~predictor.matrix)

## Lets do anova on our multi.model.

anova(multi.model)

## Notice in this anova table that the predictor matrix degrees of freedom (the
## degrees of freedom associated with SSR) is now 4, which is the number of
## x variables we have.  Our MSR would then be the SSR / 4, as opposed to
## just being equal to SSR as it was for univariate regression!  We also predict
## 5 coefficients now, the 4 slopes and the y intercept, so our df for residuals
## is n-5 = 26!  These numbers change depending on the properties of the regression,
## so be sure to pay attenteion to them.  

## Our f statistic is considerably lower this time, and if we were using 95% as
## a statistical significance level, we would not say our regression was 
## statistically significant.  However, if we used 90%, we could say that!  

## Lets look at the summary as well.

summary(multi.model)

## As you can see, our R-squared value is only 0.25, which is not a very
## good regression (only 25% of the variance in y is described by x)

## How do you suppose we could improve our regression model?  Perhaps we should
## add precipitation in Canadaigua to predict precipitation in Ithaca.

predictor<-cbind(predictor.matrix,canadaigua.data[,2])

## Lets try this model.

new.model<-lm(predictand~predictor)  

## Lets see a summary of our model.

summary(new.model)

## Obviously this made things much better!  This makes sense when we look at
## the correlation matrix of our predictand/predictor combination:

cor(cbind(predictand,predictor))

## There is a smarter way to tell which
## variables work well and which do not when we look at multivariate linear
## regression.  This method is called stepwise regression.  Stepwise regression
## typically uses F-tests to determine objectively (remember what that means?)
## which predictors are good and which are not good for the given predictand.
## THIS ONLY WORKS FOR LINEAR REGRESSION!!  What could be another reason to
## remove predictors from a dataset (besides they predict badly)?

## Three methods of stepwise regression are possible (forward selection, 
## backward selection, and both).  For forward selection, the process begins by
## creating a model that only includes an intercept, and computing the 
## results of that model.  The algorithm then iteratively examines all the
## combinations of predictors to obtain those which produce the best results.
## This is done via the use of the F-test.  If no testing criteria (stopping
## criteria) was used, the forward selection would keep going until all 
## variables were used, and the backward selection would keep going until
## all were removed.  The criteria used are related to the number of predictors
## kept and the F test value associated with those predictors that are kept.  
## Higher F test values mean a larger relationship between the particular
## predictor and predictand.

## Unfortunately, the forward selection method for stepwise regression
## suffers from a problem of an increasing "rejection" F-statistic value
## depending on how many predictors you have.  This increased value leads
## to keeping more predictors than necessary (over-fitting), which is a 
## problem.  One way around this is the use of the mean square error as 
## a stopping criteria.  Once the MSE does not change significantly, it is
## good to stop removing or adding predictors.

## I have provided a function that will do this method for you.  This function is
## called stepwise.  Feel free to look it over, but know it is in depth.  The function
## allows for both forward and backward stepwise regression, defaulting to forward
## stepwise regression.  Lets redo our regression using stepwise methods instead.
## Remember we are regressing rainfall in Ithaca against high and low in Ithaca
## and Canadaigua and rainfall in Canadaigua.  

predictor.matrix<-cbind(predictor.matrix,canadaigua.data[,2])
forward.preds <- stepwise(predictand,predictor.matrix)
backward.preds <- stepwise(predictand,predictor.matrix,method="backward")

## Which one is better?  They seem to be the same, just in the opposite order.
## This is not normally the case, but there may be a problem with backward
## stepwise part of the function.  Lets stick with the forward stepwise regression
## for now.  We can create our regression based off of that stepwise method.  
## In our results, it looks like precip in canadaigua is the best variable
## by far, with only slight improvement seen when including low temperature
## in canadaigua as well.  Lets include those two variables.

for.model <- lm(predictand~predictor.matrix[,5]+predictor.matrix[,4])

## How does this model look?

anova(for.model)
summary(for.model)

## This is a good result, and demonstrates which variables are good predictors
## of high temperatures in canadaigua.  Stepwise regression is frowned upon in
## many statistical circles, as it requires the use of an F test which is not
## really designed for this type of application.  It is still a useful tool to
## know about!

## Lets now shift our focus slightly to nonlinear regression.  Nonlinear
## regression does not limit itself to only using a linear response.  We 
## can specify additional responses based on what we want.  For example,
## if we assume some of our data has a quadratic response, we could
## create a "linear" model with the log of y as our response, instead of just
## y.  We can do this using the glm command.  Lets try this for our 
## nice predictor, predictand combination.

nonlinear.test.model<-glm(sqrt(predictand)~predictor.matrix)

## If we look at the summary of this model, we will notice that only the fifth
## predictor should not be rejected.  We can try to redo our model based on
## this logic:

summary(nonlinear.test.model)
nonlinear.test.model.new<-glm(sqrt(predictand)~predictor.matrix[,5])

## Now, we can try to predict an outcome from our
## log model.  Remember, our log model is basically of the form:

## sqrt(y) = a + bx

## so that our log model could written as:

## y = (a+bx)^2 = a^2 + 2abx + bx^2

## For our particular model, a = 0.067 and b = 1.412.  Lets try to 
## predict with this model.  We can simply pull off the first five rows
## of our datasets and create a testing dataset.

test.set <- canadaigua.data[1:5,2]
test.set

y.hat <- (nonlinear.test.model.new$coef[1])^2 + 2 *nonlinear.test.model.new$coef[1]*nonlinear.test.model.new$coef[2]*test.set + (nonlinear.test.model.new$coef[2]*test.set)^2

## Now, we can see our predictions versus the observations

y.hat
predictand[1:5]

## and our residuals are just:

resids <- predictand[1:5]-y.hat
 
## This isn't a horrible result, considering we tried to fit a quadratic 
## function to a dataset that appears more linear.  Lets try to do a 
## scatterplot using the log(y) and see how this relationship looks.

plot(ithaca.data[,3],sqrt(predictand))

## If this plot were to look like a straight line, it would tell us that
## the logarithmic look to the data is correct.  This adds a nonlinear response
## to a linear function, hence making it nonlinear.  

## There is a workaround regression type that accounts for small nonlinearities
## in a dataset.  This type of regression is called local regression, often
## denoted as either LOWESS or LOESS regression.  This method works by using
## small subsets of data and doing small regressions on those, in order to 
## build up a larger function.  In other words, the method uses a small subset
## of the data, does a least squares linear regression on that subset, and 
## proceeds to the next subset, and when complete, connects the small regressions
## into a final prediction function.  This method only requires a smoothing
## parameter for your function, as well as a degree for the polynomial you
## wish to fit (i.e. 1 would be linear, 2 would be quadratic, etc.).  Some 
## disadvantages of LOESS regression include its computational expense and 
## the result of not getting a nice, transferrable, robust mathematical function. 
## Instead, you get a relationship based on those data, which may work well for
## those data, but may perform poorly for new data or under a new enviornment.

## Lets try a loess regression on our ithaca data.  We will try to regress
## the high temperature in Canadaigua versus high temperature in Ithaca.  This
## will allow us to visualize the regression as well.

predictand <- canadaigua.data[,3]
predictors <- ithaca.data[,3]

## To do a loess regression, you simply need to run the loess command.

sample.loess <- loess(predictand ~ predictors,span=10)

## Changing the span increases or decreases the smoothness of the loess
## functions that are created from this method.  

## If we try to predict based on our original data, we can simply do:

y.hat<-predict(sample.loess)

## and if we plot sample.loess on our original scatterplot:

plot(predictors,y.hat,type="l")
points(predictors,predictand)

## Many other types of nonlinear regressions exist, including artificial
## intelligence learning methods such as neural networks and support 
## vector regression.  


##   CLASSIFICATION ##  

## We will now shift our focus off of regression and 
## onto classification, which is a specific type of regression.  If we 
## assume that we have a binary response to our data (i.e. we can have 
## only one of two possible outcomes), we can use nonlinear classification
## methods to obtain statistics on classification models we create.  This method
## is advantageous, because we can create probabilities based on these
## binary classifiers.  For example, if we have our set of rainfall
## data for Ithaca:

ithaca.data[,2]

## we can turn this into a binary dataset with a 0 assigned to the variable
## if we have no rainfall, and a 1 if we do have rainfall.  Lets do
## this.

binary.ithaca.data<-ifelse(ithaca.data[,2]>0,1,0)

## Lets try to plot these data versus ithaca high temperature and ithaca low
## temperature in a scatterplot to try to demonstrate the separability.

plotdata<-cbind(binary.ithaca.data,ithaca.data[,3:4])

first.y <- plotdata[1,1]
plot(plotdata[1,2],plotdata[1,3],xlim=c(-10,50),ylim=c(-10,50))
for (i in 2:length(binary.ithaca.data)) {

if (plotdata[i,1] == 0) {
points(plotdata[i,2],plotdata[i,3])
}

if (plotdata[i,1] == 1) {
points(plotdata[i,2],plotdata[i,3],pch=6)
}

}## End the for loop

## This little script allows us to view what a scatterplot of ithaca high
## and low temperature looks like, based on what class the data are in (either
## yes precip (triangles) or no precip (circles)).  This shows us if our data
## can be separated easily by a line, hence the idea behind classification.

## Now that we have this information, we can create probabilities based on
## this setup.  The most common (and oldest) of these methods is called
## a regression estimation of event probabilities (REEP) analysis.  In
## this method, we simply use our binary data as a response and our 
## input data as they are in a linear regression model.  In doing so, we 
## end up getting some value (likely between 0 and 1) for our y.hat, which
## gives us the probability of the event 1 occurring.  However, with a simple
## linear regression it is difficult to be certain that the responses will
## fall within the range 0 to 1, which can be problematic.  Lets try
## a REEP for our ithaca precipitation data, using highs and lows of 
## the both cities as our predictors.

predictand<- binary.ithaca.data
predictors <- cbind(ithaca.data[,3:4],canadaigua.data[,3:4])
reep.model <- lm(predictand~predictors)

summary(reep.model)

## In this case, the R-squared value is not that high.  We can predict a 
## value from our coefficients for a given temperature setup.  Lets assume
## on a given day the high in Ithaca was 32, the low was 15, and the high
## in Canadaigua was 29 and the low was 16.  

test.data<-c(32,15,29,16)
test.data

## We can use the predictlm command we made.
y.hat<-predictlm(reep.model,t(test.data))
y.hat

## Our y.hat value is only slightly greater than zero.  What does this mean?
## It means, theoretically, that given the 4 temperatures that I presented
## the model, there is about a 50% chance of rain!  This method is similar
## to how MOS works in model output, although this simple regression is not
## used.  Also, the residuals are not Gaussian, they are clearly Bernoulli (
## binomial, one of two possible outcomes).  

## A better technique which is bounded by the 0-1 range and accounts for a
## binary response is called logistic regression.  Logistic regression uses the
## prediction equation:

## p = exp(b0 + b1x1 + ... + bnxn)/(1+exp(b0+b1x1+...+bnxn))
## This function is not easy to visualize, but it will provide the binary
## response and will be bounded by the range 0 to 1.  This method can
## be thought of as a linear regression with respect to the logit, which 
## is an odds ratio (p/(1-p)).  This p value is a probability, just like
## with the REEP, but it is more analytically sound and can be assigned
## based on the results.  Lets try our example using logistic regression.

log.regress <- glm(binary.ithaca.data~predictors[,1]+predictors[,2]+predictors[,3]+predictors[,4],family=binomial(link="logit"))

## It is usually safer, although not required in all cases, to write out the function 
## when you do a regression using glm.  We can now take our output data and 
## apply the logit calculation equation to determine the probabilities associated
## with logistic regression, and hence the result for new data.  Lets first
## chop off the first 10 data points from our predictor matrix to use as
## a "independent" set of data points.

newdata<-predictors[1:10,]

## Now then, remember that our prediction equation is given as:

## p = exp(b0+b1x1+...bnxn)/(1+exp(b0+b1x1+...+bnxn))

## So, we can get our predicted probabilities by first using our
## predictlm command to get the values for the exponents, then
## simply plugging them into this equation.

exponents<-predictlm(log.regress,newdata)
probs <- exp(exponents) / (1 + exp(exponents))

## Now that we have probabilities, we can simply assign those probabilities
## greater than 0.5 as having an outcome of 1 (yes) and those that are
## less having an outcome of 0 (no).  

classes <- t(ifelse(probs>0.5,1,0))

## We have to do the transpose to be sure we get a row vector.  So our final 
## classes are:

classes

## Lets try the sample data day that we created previously to see
## what the probability of precip is on that day.

test.data<-c(32,15,29,16)
test.data

## Lets predict on these data.

exponents <- predictlm(log.regress,test.data)
probs <- exp(exponents)/(1+exp(exponents))

## So, we know that our probability of precip on this day based on these
## data is 0.6, which means we would classify this as a rain day in our 
## binary classification.  

## How can we assess what the performance is of a classification model? It 
## is not as straightforward as doing MSE or SSE calculation, since we have
## a binary response.  Instead, we can use what are known as contingency
## statistics.  Contingency statistics are a unique set of statistics that
## can be used only for binary classification results.  Contingency statistics
## can help describe the properties of your classification.  In order to 
## compute these contingency statistics, we require the creation of what
## is known as a contingency table, or a confusion matrix.  This matrix
## is always 2x2, where the rows in the matrix represent the forecasted data
## and the columns represent observed data.  The positions in the matrix
## are typically identified by the letters a-d.

## For example:

##  					Observed(yes)			Observed(no)
## 
##  Forecasted (yes)         a                    b
##  Forecasted (no)          c                    d

## In the contingency table, the first position (position a) represents a 
## correctly classified yes forecast (1 forecast).  The value of a is simply
## the sum total of all of these correct yes forecasts.  For our example
## ithaca dataset, where we have the 10 test days:


y.hat <- classes

## and our original 10 observations:

y <- binary.ithaca.data[1:10]

## we can cbind the two and compare them.

compare <- cbind(y,y.hat)

## By visualizing the data, we can see that the number of times that we
## get a correct 1 forecast (1 is in both columns) is 5.  So, a in our
## contingency table would be equal to 5.

## The position d represents all points where we created a correct no (0) 
## forecast.  Thus, in our example, this would be where both columns have 
## a value equal to 0.  This occurs 4 times, so d = 4.

## Positions b and c require a bit more thought.  For position b, we are 
## considering situations where we forecasted a yes (1) but the actual
## observed result was a no (0).  In this case, since our data are in the
## order of observed,predicted in our compare matrix, we know that
## the value of b = 1.  Position c is simply the opposite of b; a situation
## where we predicted a no (0) and actually observed a yes (1).  Our example
## here does not have any points which meet this criteria, so c = 0.  So,
## our contingency table should be:

contingency.table <- matrix(c(5,1,0,4),ncol=2,byrow=T)
contingency.table

## I wrote a function in R that will create a contingency table for us
## to use in our contingency statistics calculations.  It also calculates
## most of the common contingency statistics.  I called it con.table.

## Lets see what con.table gives us for our y and y.hat data.

contin<-con.table(y,y.hat)
contin

## Lets assign the contin matrix to the corresponding letters for the 
## remainder of the contingency statistics discussion.

aa <- contin[1,1]
bb <- contin[1,2]
cc <- contin[2,1]
dd <- contin[2,2]

## Now lets look at some example contingency statistics.  These statistics
## give many different measures about the skill, accuracy, and inaccuracy
## of a classification model.  Our first two statistics provide a good
## measure of the accuracy of the model.  The first of these is called the percent
## correct (PC).  The equation for PC is simply:

## PC = (a+d) / n

## Does it make sense why this would be the equation for the percent correct?
## Higher values of the PC indicate that the model is classifying with more
## accuracy.  However, the PC does not include ANY information about misclassification.
## This is important as well, so we need to consider other measures besides the 
## PC.  What is the PC for our sample data?

PC.sample <- (aa + dd)/length(y)

## So our PC is 0.9, which is quite good!  Of course, only using 10 data points
## may have artificially inflated our results.  Another measure which does
## include error information but does not include any information on the
## correct no forecasts is called the threat score, or the critical success
## index (CSI).  The CSI is written as:

## CSI = a/(a+b+c)

## and the CSI represents the proportion of correct yes forecasts after removing
## the no forecasts from the calculation.  Values of 1 are best and values of 0
## are worst.  The CSI is commonly used when the yes event is something comparitively
## rarer than the no event (i.e. tornado days in MS; there are fewer yes days
## than no days).  What is the CSI for our dataset?

CSI.sample <- aa/(aa+bb+cc)

## This is a very high value as well!

## Lets now look at a bias parameter.  The bias term tells us the number of
## yes forecasts versus the number of yes observations.  This number will
## allow us to know if our model is tending to predict more 1s than 0s.  Values
## of the bias greater than 1 indicate a bias towards 1, those less than 1 indicate
## a bias towards 0s.  The equation for bias is:

## BIAS = (a+b)/(a+c)

## The bias for our simple example would be:

BIAS.example <- (aa+bb)/(aa+cc)

## The result of 1.2 says we are predicting more 1s than 0s.  Since we have 
## only 10 data points, we probably shouldn't worry.  Bias is important as
## your datasets increase in size though.

## We can also assess the reliability of a forecast by using the false alarm
## ratio.  The false alarm ratio (FAR) provides a measure of the number of
## times an incorrect yes is forecast versus the total number of times a 
## yes is forecast.  This is very helpful in severe weather studies, since
## more false alarms lead to apathy by the general public.  The FAR ranges
## from 0 to 1, and values nearer to 0 are ideal, since we want as few 
## false alarms as possible.  The equation for FAR is:

## FAR = b/(a+b)

## So for our dataset, our FAR is:

FAR.example <- bb/(aa+bb)

## Which is pretty small!  Of course, 10 data points helps.  

## In addition to the above properties of the classification model, we can
## look at its discrimination capability.  This can be assessed by
## a pair of statistics.  The first of these is known as the hit rate or 
## the probability of detection (POD).  The POD represents the number of times
## that a yes is forecasted correctly out of the total number of times the
## yes is observed.  This gives the user an idea as to how many times the
## model is correctly identifying a yes forecast.  This value ranges from 0
## to 1, with 1 being perfect and 0 having no discrimination ability.  The
## equation is:

## POD = a/(a+c)

## For our example, our POD is:

POD.example <- aa/(aa+cc)

## Since we had no misforecast nos (c = 0 in our example), our POD is simply
## 1, meaning we had a perfect forecast of yes values!  This will rarely happen
## unless a dataset is very small, such as our example.

## We can also identify the rate of false yes values over the entire time that
## a no forecast is given.  In other words, we are identifying the number
## of times our model classifies an event as a yes when the event has occurred
## as a no.  This is known as the probability of false detection or the false
## alarm rate.  This quantity is different from the false alarm ratio because
## the false alarm ratio looks at all yes forecasts (points a and b) and the 
## POFD only considers incorrect yes forecasts.  The equation for POFD is:

## POFD = b/(b+d)

## For our example:

POFD.example <- bb/(bb+dd)

## As you can see, this value is different from the FAR value.  In this class
## we will use FAR as false alarm ratio and POFD for the probability of false
## detection, to help avoid confusion between the FAR and false alarm rate.

##  Finally, we can look at the skill of the model using contingency statistics.
##  There are many (dozens) of skill scores that have been derived from 
##  contingency matrices, and we will touch on the most common ones here.  One 
##  the most widely utilized skill scores is called the Heidke skil score.  This
##  score is based on a reference accuracy that the forecast achieves a certain
##  proportion correct based on random chance.  This score is a measure of accuracy
##  such that perfect forecasts receive a score of 1 and completely incorrect
##  forecasts received a score of zero.  The equation for the Heidke skill score
##  (HSS) is:

##  HSS = 2*(ad-bc)/((a+c)(c+d)+(a+b)(b+d))

##  For our example, the HSS could be:

HSS.example <- (2*(aa*dd - bb*cc))/((aa+cc)*(cc+dd)+(aa+bb)*(bb+dd))
  
##  which gives us a nice value of 0.8, a very good score!  

##  Another widely used skill score is called the true skill score (TSS) or the
##  Peirce skill score (PSS).  This score is almost identical to the HSS in 
##  its formulation, except that it assumes that the underlying random forecasts
##  that are used as a reference accuracy have the same distribution as the
##  climatology of the variable (i.e. it follows the same distribution).  
##  The PSS is:

##  PSS = (ad-bc)/((a+c)(b+d))

##  For our example, the PSS is:

PSS.example <- (aa*dd - bb*cc)/((aa+cc)*(bb+dd))

##  We get the same result here as we did for HSS, but this is rarely true.
##  Often the HSS is thought of more highly than the PSS since it makes no
##  assumptions about the underlying random distribution, so for your 
##  work I would advise using HSS as a skill measure.  Other skill scores
##  (i.e. the CSS, the GSS) are listed in Wilks, but will not be covered
##  in your notes.  I have provided a function called table.stats which will
##  compute all of these variables for you based on a given set of input
##  observations and predictions.

contingency.results <- table.stats(y,y.hat)


##  To finalize discussion on regression, we need to discuss the idea of
##  cross-validation.  Many times in research you will obtain a dataset which
##  contains predictands and predictors, such as what we worked with earlier.
##  How to use these data to get a good model is not straightforward.  If you
##  create a model based on all of your data, then test your model with the
##  same data, you aren't really checking to see if your model is general.  
##  You need a new dataset of points which were not used in the creation of the
##  model.  Hence the development of cross-validation.

##  In cross-validation, you can take an existing dataset, and use some of the
##  data for training the model (creating the model), and the remaining data for
##  testing the model.  This way, data points which were not used for the
##  training of the model can be used for testing, and you are in a sense testing
##  independent observations.  Exact ratios of training to testing data is an
##  area of ongoing research, but I find that using about 80-85% of a large
##  dataset for training and the remaining 15-20% for testing is ideal.  It is
##  important when choosing these data to do so in a random fashion.  Otherwise 
##  you may bias your model.  You need to have training and testing data that
##  will span the possible outcomes of your model so you don't give your model
##  something it hasn't seen or so your testing truly encompasses all the possible
##  scenarios.    Lets do a simple linear regression on our ithaca data with
##  cross-validation.  Lets use the first 25 points for training and the final
##  6 for testing, just as an example.  This is not typically a good practice, 
##  you should instead mix the data values, but this will be demonstrative.

training.data<-predictors[1:25,2:4]
testing.data <- predictors[26:31,2:4]

training.y <- predictors[1:25,1] ##  Here we are trying to predict ithaca highs
testing.y <- predictors[26:31,1] ##  based on everything else up to this point.

trained.model <- lm(training.y~training.data)

##  Now lets test on our model using the remaining data and see how we
##  did.

y.hat <- predictlm(trained.model,testing.data)
resid <- testing.y - y.hat

##  We had a major problem with that first datapoint, probably because we don't
##  have enough data in there to account for that.  Otherwise, our results
##  weren't horrible for such a simple regression.  Good training and testing
##  practices are vital when doing regression in research, so be sure to 
##  take care when conducting cross-validations in your work.

