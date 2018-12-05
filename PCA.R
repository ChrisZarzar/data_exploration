## Principal Component Analysis ##

## We will now shift our focus away from regression and on to principal
## component analysis (PCA).  PCA is a technique which reduces a dataset
## containing a large number of variables to a smaller sized dataset.  The
## new variables which result from a PCA are linear combinations of the old
## variables.  In the case of significantly high correlations between the data
## variables (often the case), the reduction is significant.  The new
## variables are called principal components (PCs).  PCA is a subset of the
## idea of factor analysis, which uses various statistical methods to reduce
## dimensionality of data and/or provide more information using less data.

## PCs are usually calculated as linear combinations of the anomalies
## x' = (x-x_mean)/sx.  The first PC corresponds to the linear combination of
## x' with the largest variance, and subsequent PCs represent smaller and
## smaller variance.  All PCs are uncorrelated as one of the default conditions.

## In order to accomplish this feat, we need to return to the concept of the 
## eigenvalues and eigenvectors.  Eigenvectors, by definition, are orthogonal
## (what is orthogonal?), so that if we project our anomaly data x' onto the
## eigenvectors (which result from the x' covariance matrix), 
## the data will all be uncorrelated.  The prediction equation for PCA which
## accomplishes this feat is:

## Z = F * t(A)

## where Z represents the standardized data, F represents the PCs, and A is a
## transformation matrix which allows for the transition from the normal 
## coordinate system to one in which the eigenvectors are the axes.  

## So how does the eigenvector/eigenvalue system enter into all of this?  First,
## it is necessary to compute either a correlation or a covariance matrix.  
## Often times the correlation matrix is best, because the units of a dataset
## are not often uniform (i.e. mixing geopotential height at 100 mb with precipitation
## gives a bit of a different result).  The correlation matrix, as you recall,
## is simply a matrix with ones along the diagonal and correlations between
## the remaining variables for all of the off-diagonal points.  

## By definition, the correlation matrix can be written as a combination of
## the eigenvalues and eigenvectors of that correlation matrix, based on
## the definition of the eigenvector and eigenvalue.  In equation form, if
## R represents a correlation matrix, then the eigen-decomposition of the
## correlation matrix is just:

## R = V %*% D %*% V^-1

## where V is the eigenvector matrix and D is a matrix with the eigenvalues
## along the diagonal.  One uses the eigen command in R to perform this
## decomposition.  This is often a very computationally intensive process for
## large datasets, so keep that in mind!  As an example, during my PhD 
## research I was asked to create an eigenanalysis of a 50000 X 50 matrix 
## along the 50000 dimension.  In order to get a 50000 X 50000 correlation
## matrix, before even doing the eigenanalysis, requires:

(50000 *50000 * 8) / (1024*1024*1024)

## 18.6 GB of RAM on a computer, which is obviously only available when you
## do parallel supercomputing.  

## Once we have the eigenvalues and eigenvectors for our dataset, we can
## compute the transformation matrix which allows us to transition from
## the original coordinate axis system to one in which the axes are the
## eigenvectors.  This matrix, defined as A in the prediction equation, is 
## known as the loading matrix.  We can calculate the PC loading matrix
## via:

## A = V %*% D^0.5

## There are other methods for computing A which some agree are better than
## others, but this method creates a transformation which assumes the 
## lengths of the eigenvectors are unity.  Once the loading matrix is
## computed, we can use the transformation loading matrix in combination
## with the original anomaly data to get a matrix of the principal components,
## often referred to (and from here on out referred to) as the PC scores.  

## F = Z %*% A^-1

## Unfortunately, inverting the loading matrix can sometimes be 
## problematic because it is not always symmetric or even square.  In
## order to avoid this problem, we instead use the following approach,
## called the least squares approach:

## F = Z %*% A %*% (t(A)%*%A)^-1

## The PC scores represent the transformation of the original data
## onto the new coordinate system with the eigenvectors as axes.  These
## axes are better, because we know they are uncorrelated, so we can use
## this as a baseline for reducing our data dimensionality.  
## Since they are uncorrelated, the correlation matrix of the
## the u.ms is simply the identity matrix!  This means that we can say that
## the covariance matrix is simply a diagonal matrix with the eigenvalues along
## the diagonal.  We can then say, by definition, that the amount of
## variance explained by each PC is proportional to the PC's eigenvalue, such
## that:

## var.explained <- lambda.m/(sum(lambda.k)) * 100%


## If we want to think of what is going on geometrically, by definition
## (because we set the scaling to a constant) the first eigenvector points
## in the direction in which all of the data vectors jointly exhibit the
## greatest variability.  This first eigenvector is also associated with the
## largest eigenvalue lambda.1.  Subsequent eigenvectors will have less 
## of the variability than previous vectors, such that eigenvector 2 will
## include less variability of the data vectors than eigenvector 1, but will
## contain more than eigenvector 3.  All of these eigenvectors will be 
## orthogonal to each other as well.  Another way to think about this is that
## we are projecting data onto a new coordinate system in which the eigenvectors
## are the coordinate axes.  

## These concepts are very arbitrary at this point, so lets try to demonstrate
## PCA using a simple example from the text.  We will use our Ithaca and
## Canadaigua minimum temperature data.  Lets capture these data.

min.ithaca <- ithaca.data[,4]
min.canada <- canadaigua.data[,4]

## Now then, to do a PCA, we first need to compute the anomalies of these
## data.  This is easily accomplished using the scale command in R.

ithaca.anomaly <- scale(min.ithaca)
canada.anomaly <- scale(min.canada)

## Lets plot those anomalies on a scatterplot to recall what we were working
## with.

plot(ithaca.anomaly,canada.anomaly)

## To conduct a PCA, we first need to compute the correlation matrix
## of our data.  We can choose which dimension we wish to compute the
## correlation matrix on, based on what we are interested in.  If we want
## to reduce our number of data vectors from 2, we should compute the 
## correlation matrix on the column dimension.  What would we be reducing
## if we computed the correlation matrix on the row dimension?

anomaly.data <- cbind(ithaca.anomaly,canada.anomaly)

cor.anomaly <- cor(anomaly.data)

## Lets check the correlation matrix.

cor.anomaly

## It is symmetric, which is what we expect.  Our next step is to do an
## eigenanalysis on this correlation matrix, which we can do using
## the eigen command.

eigen.anomaly <- eigen(cor.anomaly)
eigen.anomaly

## Here we see what our eigenvectors and eigenvalues are.  We have one
## larger eigenvalue and a smaller eigenvalue.  What is the variance
## explained by each eigenvalue?

var.explained.1 <- eigen.anomaly$values[1] / (sum(eigen.anomaly$values))
var.explained.2 <- eigen.anomaly$values[2] / (sum(eigen.anomaly$values))

var.explained.1
var.explained.2

## These should add to 1, and it appears by inspection that they do.  So,
## a huge percentage of the variance of the data is described by the 
## first eigenvector.  Lets plot these eigenvectors on our scatterplot
## to visualize what this is saying.

abline(0,(eigen.anomaly$vectors[2,1]/eigen.anomaly$vectors[1,1]))
abline(0,(eigen.anomaly$vectors[2,2]/eigen.anomaly$vectors[1,2]))

## These should be orthogonal, and even this rough plot shows them to be
## nearly orthogonal.  If we stretch the plot downward so that the scales
## are identical on the x and y axes, the lines become orthogonal.
## As you can see, most of the variability of the data
## lies along the first eigenvector, which is where most of the datapoints 
## seem to cluster around.  The variability in the other direction 
## includes the rest of the variation in the data.  

## Now that we have our eigenvalues and eigenvectors, we need to compute
## our transformation matrix, called the loading matrix.  Lets do that
## next.

loading.mat <- eigen.anomaly$vectors %*% sqrt(diag(eigen.anomaly$values))

## What should the dimensionality of our loading matrix be?

dim(loading.mat)

loading.mat

## Okay, now that we have our loading matrix, we can compute the scores, which
## correspond to our actual PCs of interest.  The PCs are computed via the
## least squares approximation of the inverse of A:

score.mat <- anomaly.data %*% loading.mat %*% solve(t(loading.mat) %*% loading.mat)

## Now, if we plot our score.mat instead, we will get our same dataset, but 
## they have been transformed to a coordinate system where the x axis
## is the first eigenvector and the y axis is the second.

plot(score.mat[,1],score.mat[,2])

## So now we know what the data look like along these dimensions.  If we look at
## the variance explained, it says that the more highly variable dimension
## is the e1 dimension, which is our x direction.  Since we used correlation
## instead of covariance, it is difficult to see this in the plot, but it is
## definitely there.  

## Well, we've considered a very simple example of PCA involving a 2-dimensional
## matrix.  The point of all of this was to get data reduction, right?  Well, 
## since the eigenvectors are all uncorrelated and all are orthogonal, each
## eigenvector describes a certain percentage of the variance.  If that is true, 
## we can truncate the number of eigenvectors that we keep and retain a smaller
## subset of the variance than the full 100%.  In the case we just presented,
## we got a result where 95% of the variance lay in the first PC and the remaining
## 5% in the second PC.  In this case, it is safe to throw out the 2nd PC and 
## only retain the first.  We will still have 95% of the data variability, and
## we have only a single vector instead of a matrix to do calculations on.  

## Most examples aren't this simple.  We will often have many vectors and many 
## large eigenvalues.  There needs to be a method for determining a truncation
## "point" for eigenvectors.  Currently in most meteorological research, this
## is determined by using a scree plot.  The scree, in geology, is basically
## the accumulation of debris at the base of a cliff that is no longer a part
## of the cliff.  In other words, it is stuff that is not useful.  In a typical
## scree plot in PCA, we get a plot of numerous eigenvalues (the same number
## as there are data values; columns in our example above).  At some point,
## the eigenvalues level off, indicating the end of the "cliff", and the subsequent
## information is the scree, or the stuff you don't want to keep.  The scree
## test subjectively looks at the first 20 or so eigenvalues and chooses a good
## rejection threshold based on the position of the scree.  Lets try an example.
## Assume we have the following set of eigenvalues:

sample.eigenvalues <- c(1000,100,10,2,1,0.5,0.25,0.05,0.04,0.03,0.02,0.01)

## If we plot these against a sequence of 12 numbers (or leave off the sequence,
## it will do it by default)

plot(seq(1:12),sample.eigenvalues)

## One would say to keep the first two eigenvalues and truncate the rest.  
## So, the remaining eigenvalues would be the scree.  

## This test, while the most widely used, is one which has a great deal of 
## subjectivity associated with it.  What if our eigenvalues were instead:

sample.eigenvalues <- c(1000,750,500,400,300,200,100,50,25,10,5,1)
plot(seq(1:12),sample.eigenvalues)

## Where is the logical truncation point?  Around eigenvalue 7, or 9?
## What about just stopping at 6?  Each truncation point leads to a 
## different answer!  So while this is the most widely used, it is not
## the best since it requires subjectivity.  Two methods of eigenvalue
## truncation exist which are objective, called North's test and the 
## congruence coefficient.  

## The North's test is based on the sampling error of the eigenvalues.
## The test involves creating confidence intervals based on the normal
## distribution and the confidence level, as well as the standard error,
## which North estimates as equal to:  

## se <- lambda * (2/n)^0.5

## The North's test states that if the eigenvalue CIs do NOT overlap, they
## contain pertinent independent information and should not be removed.  
## When overlap in the CIs begins, you should then truncate your eigenvectors
## prior to that point.  So, if we had a set of four eigenvalues which were
## given as:

test.eigenvalues <- c(14.02,12.61,10.67,10.43)

## which were the top four eigenvalues from a set of n=300 eigenvalues, we can
## construct confidence limits around those.  Those CIs, at a 95% level, would
## simply be:

CI.1 <- test.eigenvalues[1]*(2/300)^0.5 * qnorm(0.975)
## We only need to compute one since they are symmetric due to the normal distribution.

CI.2 <- test.eigenvalues[2]*(2/300)^0.5 * qnorm(0.975)
CI.3 <- test.eigenvalues[3]*(2/300)^0.5 * qnorm(0.975)
CI.4 <- test.eigenvalues[4]*(2/300)^0.5 * qnorm(0.975)


## If we look at the value for lambda.1 and the upper CI for lambda.2:

test.eigenvalues[1] 
test.eigenvalues[2] + CI.2

## These have overlap.  This means that our eigenvectors will be random mixtures 
## of their population counterparts.  If we increase the sample size to 1000:

CI.1 <- test.eigenvalues[1]*(2/1000)^0.5 * qnorm(0.975)
CI.2 <- test.eigenvalues[2]*(2/1000)^0.5 * qnorm(0.975)
CI.3 <- test.eigenvalues[3]*(2/1000)^0.5 * qnorm(0.975)
CI.4 <- test.eigenvalues[4]*(2/1000)^0.5 * qnorm(0.975)

## Then try our test:

test.eigenvalues[1] 
test.eigenvalues[2] + CI.2

## The eigenvalues are no longer in the CI of the next eigenvalue.  Lets try
## the second versus the third:

test.eigenvalues[2] 
test.eigenvalues[3] + CI.3

## Still, they are outside of the CI.  What about the 4th versus the 3rd?

test.eigenvalues[3] 
test.eigenvalues[4] + CI.4

## Now there is some overlap.  This tells us that we are able to determine
## two unique eigenvector patterns from our first two eigenvalues before the
## data begin to overlap each other.  As we increase our sample size, the 
## value of these CIs will decrease, so that we have more distinct patterns. 
## More evidence that more data is better!!

## An even better approach to the truncation problem is called the congruence
## coefficient.  The congruence coefficient assigns values based on the
## equation:

## eta = sum(x*y)/(sum(x^2)*sum(y^2))^0.5

## where x represents a vector of the original correlation matrix R, with the
## largest absolute magnitude of a loading in the loading vector Y.  In other
## words, for our example above, our loading matrix was:

loading.mat

## In our matrix, it is not possible to pick out a maximum absolute magnitude
## for each loading vector (column), so we can just pick the first one.  If
## we then take the first column of our correlation matrix as x, our
## congruence coefficient eta is just:

eta.1 <- sum(loading.mat[,1]*cor.anomaly[,1])/(sum(cor.anomaly[,1]^2)*sum(loading.mat[,1]^2))^0.5

## This coefficient represents the first PC loading.  Values over 0.81 (determined
## experimentally) indicate good congruence such that there should not be rejection
## of the PC.  The second eta for the second PC is:

eta.2 <- sum(loading.mat[,2]*cor.anomaly[,2])/(sum(cor.anomaly[,2]^2)*sum(loading.mat[,2]^2))^0.5

## which is tiny.  This suggests we should only keep the first PC, which
## we concluded anyway.  You have been provided a function, congruence.

## Once a truncated set of eigenvectors is created, this is used instead
## of the full eigenvector matrix when computing the loadings.  This often
## results in a matrix that is not symmetric, requiring the least squares
## estimation of the inverse to get the PCs themselves.  To use the congruence
## coefficient in this process, you must first choose some number of PCs based
## on a scree plot, likely a number larger than you actually think, then
## compute the loading matrix based on this set.  With this loading matrix you
## can then calculate the congruence coefficients, and if any have an absolute
## value less than 0.81, you can throw out their corresponding eigenvector.  
## You must do this sequentially.  For example, if you have seven loadings you
## are testing, and your fifth loading is 0.5, you must throw out loadings 5, 6
## and 7, since they describe less variance as you increase the eigenvalue number
## based on the definition of PCA.  You must then rerun the congruence test
## based on this smaller number of loadings to be sure everything is okay with
## eigenvectors 1-4.  

## There are many different "modes" that PCA can be conducted in, based on the
## axis that the correlation matrix is taken and the properties of the data.  
## The 6 modes, as defined in a paper by Richman (1986), which is now on
## mycourses and you should all read.  In his paper, he discusses the idea
## of the rotation of PCs (below) and the different modes of eigenanalysis.
## When you conduct an eigenanalysis, there are three possible things that
## can vary; time, space, and the variables.  Based on this idea, the 
## brief table below, from Richman (1986), will show you the different
## modes and how they relate to these three properties of the data.

## Mode	Cor. mat. taken		Cor.mat not taken			Fixed
## O-mode		time			      parameter			station
## P-mode		parameter			  time			station
## Q-mode		parameter			station			  time
## R-mode		stations			parameters			  time
## S-mode		stations			  time			parameters
## T-mode		time				stations			parameters

## Determining what you want to do for your PCA helps you to know which
## mode is best for your work.  The most common types you will encounter
## in meteorology, and those which we will do for class, are the S-mode
## and the T-mode analyses.

## There is another technique that can be used to improve the results of PCA
## even further.  When we have the orthogonality constraint on our data, 
## the 2nd and higher eigenmodes are constrained to being orthogonal
## to the first eigenmode, which is along the direction of highest variance.
## When you are looking for physical interpretation of the dataset, 
## as opposed to compression of the data into a smaller subset, it may be
## helpful to further rotate the eigenvectors to a new set of coordinate
## vectors.  Usually, the truncated set of eigenvectors is rotated.  The
## advantages to rotation are that they can help provide a better physical
## meaning (i.e. maybe some physical features are lost in the variance in the
## unrotated eigenmode that will be gained by rotating the coordinate system), 
## but rotation will remove the variance constraint (i.e. first eigenvector
## points to the region of greatest variance, and so on) and may remove the
## orthogonality constraint as well.  You will see an example of a 
## rotation in our big example below.

## How can we best demonstrate the usefulness of PCA?  PCA on anomaly patterns
## can help identify regions of significant difference, or provide information
## on the underlying spatial structure of data.  This can be useful for both
## meteorology and GIS (possibly for geology?)  We can look at a PCA of some
## simple daily weather data for the United States.  Please scan the hgtdata.txt
## file into your R session.  It's a big file, so it may take a few moments.

hgtdata <- matrix(scan("hgtdata.txt"),ncol=493,byrow=T)

## This dataset consists of 500 mb height data over the United States between 
## 1980 and 1989.  Go ahead and grab the plot.weathermap function as well.  
## Run it.  Now that you have a plot.weathermap function, you will need to
## run the following commands.

.First <- function() {
library(maps)
library(mapdata)
library(mapproj)
}

## This function will run each time you start R, and will load these
## libraries, which are not loaded by default, for you.  It will only
## work in your current working directory, so if your map command doesn't
## work, you will need to run this code again in that directory.  If your
## machine doesn't have these libraries,let me know.

## Before we begin the example PCA, we are going to be plotting height
## contour maps using R.  We need to go through a brief tutorial of how
## to do this first.  I have created a function called plot.weathermap which
## will plot anywhere in the world (it will give you an error if there are 
## no land boundaries in your plot, so keep that in mind when you use this
## over the oceans) for a single variable.  If you want to add variables, you 
## can use the contour command.  The contour command requires at least three
## inputs, which are a vector of the x coordinates, a vector of the y coordinates,
## and a 2-d matrix of the data oriented the same way as the grid (i.e. positions
## in the matrix farther to the right (larger column values) represent larger
## x positions).  If you were to print out the matrix and contour it, you would
## get the map in R, depending on the order of your x and y coordinates.

## Lets do a simple contouring example to demonstrate how it works.  You 
## have loaded your maps library so you can plot the world maps.  Lets do
## that first.

map('usa')
map('world')
map('state')

## Now, we can overlay a contour map on our states plot.  We have to watch
## our coordinates so we don't plot things in the wrong place.  The best
## way to get a good world map (including canada/mexico) with the states
## is to draw two maps based on the size of our contour data.  The hgtdata
## matrix I gave you has latitudes that span from 20N to 60N over 2.5 degree
## intervals.  The longitudes span from -130 to -60 over 2.5 degree intervals.
## Lets make these into a pair of vectors.

lons <- seq(-130,-60,by=2.5)
lats <- seq(20,60,by=2.5)

## Lets see what is in lats and lons.

lons
lats

## Okay, now we need to pull a vector of data from our hgtdata vector.  The
## data are arranged in such a way that the first 493 (29 lons X 17 lats) elements in the
## vector represent the grid from the first temporal day.  If we want to make
## a grid that has the days as rows and gridpoints as columns, we can do:

## Now, lets contour our hgtdata.  To do this, we need to convert hgtmat
## into a second matrix that has the longitudes on the x-axis and the
## latitudes on the y-axis.  The data are arranged such that we can simply
## say

hgtmatnew <- matrix(hgtdata[1,],ncol=29,byrow=T)

## Lets see what hgtmatnew looks like.

hgtmatnew

## As you can see, the data at the top are larger than on the bottom.  This
## is consistent with latitude increasing in latitude data vector, right
## meteorologists?  

## Okay, now lets plot these data using the contour command.

contour(lons,lats,t(hgtmatnew),axes=FALSE)

## We have to do the transpose since we have 29 lons and they are along
## our columns and 17 lats and they are on our rows.

## Now lets add a US map underneath our contour map.

map('world',xlim=range(lons),ylim=range(lats),add=TRUE)
map('state',xlim=range(lons),ylim=range(lats),add=TRUE)
box()

## And now you see, we have a height contoured weather map!  We can
## add a title to our map by using title:

title("500mb heights")

## You can now export this graphic by clicking on it and going to the file menu
## and selecting the format you want.  You can also use the postscript() command.
## This command will turn on a postscript file which will keep track of 
## everything you plot in the graphics window.  Use dev.off() to turn
## off postscript().  This is useful for linux machines more than windows, but
## you can use ghostview and ghostscript, which are open source softwares, to view
## postscript files (or possibly Adobe if you have the full version).  Lets
## just export it to a jpeg for now.  

## I have written a function called plot.weathermap that will create this
## type of a plot for you.  Lets now look at that function.

dev.off()  ## Turn off the graphics device

plot.weathermap

## In this function, you add a data matrix in the format I specified, as well
## as latitudes and longitudes, with latitude increasing.  You have the 
## option to create a shaded map, but these don't work as well right now (I'm
## still working on it).  If you don't do anything else, it will create a 
## contour map just like the one we just made.  If you change nlevels to
## a different number, you can modify the contour interval to show more or
## fewer contours.

plot.weathermap(lons,lats,hgtmatnew,nlevels=10)
plot.weathermap(lons,lats,hgtmatnew,nlevels=20)
box()
## You can add a title to your plot by adding an identifier for title.

plot.weathermap(lons,lats,hgtmatnew,nlevels=20)
titlenew <- "500 mb heights"
title(titlenew)
box()

## 

## Okay, we can get some basic weather map contour plots now.  These will be
## useful when we do our PCA so we can see what is happening with the spatial 
## structure of our data.  Lets now begin our PCA.

## To begin our PCA, we first need to think about what we want to analyze.
## I am interested in seeing if there are a few primary height patterns that
## exist over the United States, or at least that did exist between 1980 and
## 1989.  As a result, we need to think about what type of analysis we want.
## If we do a S-mode analysis, we consider how highly correlated the gridpoints
## are with each other, over the 10 year span we are considering.  This will
## mean that our PC loadings will have dimensionality of 493 X # of PCs, so they
## will be what we can plot.  This is an S-mode analysis.  To do a T-mode
## analysis, we would instead correlate over the different days, such that
## we will end up with PCs that have a dimensionality of the 493 X # of PCs, 
## and can be plotted.  

## To begin our PCA, we first need to compute the correlation matrix.  The
## correlation matrix will be computed over the spatial dimension, such that
## our correlation matrix will be 493 X 493.  Lets compute this first.

hgtdata.orig <- hgtdata
mean.hgtdata <- apply(hgtdata.orig,2,mean)
sd.hgtdata <- apply(hgtdata.orig,2,sd)
hgtdata <- scale(hgtdata.orig)
cor.smode <- cor(hgtdata)
dim(cor.smode)

## We now have a a 493 X 493 correlation matrix.  We can plot one row of the
## correlation matrix, just to see what it will look like.  The first row 
## represents the gridpoint at 20N, -130W, so we can see how highly correlated
## that gridpoint is with other surrounding gridpoints.

plotcor <- cor.smode[,1] ## Could be [1,].  Why?
plotcor <- matrix(plotcor,ncol=29,byrow=T)

plot.weathermap(lons,lats,plotcor,title="500mb correlation")

## Does this map make sense with what you expect?  What about if we plot
## the 493rd row/column?

plotcor <- cor.smode[493,]
plotcor <- matrix(plotcor,ncol=29,byrow=T)

plot.weathermap(lons,lats,plotcor)
titlenew<-"500mb correlation"
title(titlenew)
box()

## Does this make sense?

## Okay, we now should eigenanalyze these data to get D and V.  

eigen.smode <- eigen(cor.smode)

## This is relatively slow because it requires inverting a big, positive
## definite and non-singular, matrix.  We can now look at our two methods
## of eigenvector truncation, the scree plot and the congruence coefficient.
## First, the scree plot over the first 15 eigenvalues.

plot(seq(1:15),eigen.smode$values[1:15])

## This is an interesting result.  We appear to have one big eigenvalue,
## but eigenvalues 2 and 3 are also somewhat large.  Lets try to retain
## the first three and compute our loading matrix.

load.smode <- eigen.smode$vectors[,1:3] %*% sqrt(diag(eigen.smode$values[1:3]))

## Based on this set of three loadings, we can check our result using the
## congruence coefficient.  Remember how to use the congruence coefficient?

congruence(cor.smode,load.smode)

## Here, we see one value has an absolute magnitude of 0.99, but the 
## second and third have values of 0.32 and 0.28.  This says that we should
## only retain the first PC.  However, the matrix multiplication does
## not work very well with only 1 PC, so lets keep the first two and 
## see how the second one looks.  

load.smode <- eigen.smode$vectors[,1:2] %*% sqrt(diag(eigen.smode$values[1:2]))

## Okay, now we can use our loading matrix to compute our PC score matrix. 
## Our PC score matrix will have dimensionality equal to the rows.  Our
## PC loading matrix will be a matrix of gridpoints, and the value of the
## scores represents how highly related the loading plot is to the given
## day.  Lets compute our scores.

scores.smode <- hgtdata %*% load.smode %*% solve(t(load.smode) %*% load.smode)

## Dimensions	3953x493    493x2               2x493              493x2
## Inner dimensions all cancel such that we get a 3963 x 2 matrix.

dim(scores.smode)

## Now then, we have a pair of loading vectors that we can plot.  Lets plot
## the first loading vector to see how the pattern looks.

load.mat <- matrix(load.smode[,1],ncol=29,byrow=T)
plot.weathermap(lons,lats,load.mat,title="Loading1 500mb heights")

## This map is a little uninteresting.  What does it mean?  It is better
## to retransform back to the original units.  How to do this?

## Remember, multiply by the standard deviation and add the mean to get
## back to the original units.

loading1 <- load.smode[,1]*sd.hgtdata + mean.hgtdata
load.mat <- matrix(loading1,ncol=29,byrow=T)
plot.weathermap(lons,lats,load.mat,title="500 mb Heights Loading 1")
titlenew<-"500mb Heights Loading 1"
title(titlenew)
box()

## This pattern looks fairly consistent with a typical weather pattern
## over the United States, with a ridge over the west and a trough over the
## east.  This looks vastly different than our anomaly pattern, so the mean
## pattern must be dominant here.  Lets plot the mean pattern to see
## how it looks comparatively.

mean1 <- matrix(mean.hgtdata,ncol=29,byrow=T)
plot.weathermap(lons,lats,mean1)
titlenew<-"Mean 500 mb heights"
title(titlenew)
box()

## So clearly, our observed values are basically the mean pattern.  This
## means that over a 10 year span, climatology isn't a bad diagnostic for
## what occurs.  Lets try to plot the second PC and see what we get.


load.mat <- matrix(load.smode[,2],ncol=29,byrow=T)
plot.weathermap(lons,lats,load.mat)
titlenew <-"Loading2 500mb heights"
box()
title(titlenew)

## This map is much more interesting!  The anomalies are negative over 
## Florida and positive over the Pacific Northwest!  Maybe we're seeing
## a teleconnection here...

## Remember, multiply by the standard deviation and add the mean to get
## back to the original units.

loading2 <- load.smode[,2]*sd.hgtdata + mean.hgtdata
load.mat <- matrix(loading2,ncol=29,byrow=T)
plot.weathermap(lons,lats,load.mat)
titlenew <-"500mb heights PC2"
box()
title(titlenew)

## The pattern still is consistent with the mean, but now the trough and
## ridge is amplified because of the anomalies near the
## trough and ridge centers.  Lets plot the original loadings to see
## this amplification.


loading1 <- load.smode[,1]*sd.hgtdata + mean.hgtdata
load.mat <- matrix(loading1,ncol=29,byrow=T)
plot.weathermap(lons,lats,load.mat)
titlenew <-"500mb heights PC1"
box()
title(titlenew)

## It's subtle, but it's definitely there.  Maybe this has some significance?
## Thats outside the scope of this class, but it certainly is interesting.

## Lets look at the PC scores now.  What do they tell us?  If we find the PC
## score with the largest positive magnitude:

for (i in 1:length(scores.smode[,1])) {


testvalue <- max(scores.smode[,1])
if (scores.smode[i,1] == testvalue) {
position <- i
break
}
}

## After running this code, we see that the maximum score for the first
## PC occurs at position 49.  Lets plot the actual values at position 49 and see
## how well the PCA represents that map, since thats waht our PC scores tell us.

test.case <- hgtdata.orig[49,]

testmat <- matrix(test.case,ncol=29,byrow=T)
plot.weathermap(lons,lats,testmat,title="Example case")
titlenew <-"Example case"
box()
title(titlenew)

## This case clearly has that trough/ridge system well defined, even more so
## than is the case with the mean pattern.  Lets choose one with the smallest
## magnitude PC score.  

for (i in 1:length(scores.smode[,1])) {


testvalue <- min(abs(scores.smode[,1]))
if (scores.smode[i,1] == testvalue) {
position <- i
break
}
}

position

## This time it is case 3408.  Lets plot that case.

test.case <- hgtdata.orig[3408,]

testmat <- matrix(test.case,ncol=29,byrow=T)
plot.weathermap(lons,lats,testmat,title="Example case")
titlenew <-"Example case"
box()
title(titlenew)

## This one is not very close to what we had previously.  There is a 
## trough over the west and central US and a ridge over the east.  What
## do you think we will get with the absolutely smallest (most highly negative)
## PC score?


for (i in 1:length(scores.smode[,1])) {


testvalue <- min(scores.smode[,1])
if (scores.smode[i,1] == testvalue) {
position <- i
break
}
}

position

## This time it is case 3493.  Lets plot that case.

test.case <- hgtdata.orig[3493,]

testmat <- matrix(test.case,ncol=29,byrow=T)
plot.weathermap(lons,lats,testmat,title="Example case")
titlenew <-"Example case"
box()
title(titlenew)

## This is just a ridge everywhere, and is not even remotely close to
## our mean pattern.  Obviously, absolute magnitude of the PC scores is not
## important!  Only the relative magnitude is.  This concludes our discussion
## on PCA.  You will be given a homework assignment asking you to visualize
## the El Nino oscillation over the Pacific using SST data over 10 years.  
## Hopefully it will be a bit more interesting than this result!

