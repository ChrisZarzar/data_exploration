## Cluster analysis

## The final analysis type we will consider also works with dividing data
## into different groups.  However, no prior knowledge of the identities of the
## clusters (unlike DA) is necessary.  This tool does not even require a prior
## knowledge of how many clusters; all of this information is inferred from
## the dataset and the method.

## Cluster analysis is an exploratory tool, rather than an inferential tool.  
## Cluster analysis does not work with applying new data to the clustering 
## method; instead it assigns group memberships at varying levels depending on
## the distance method considered.  

## Since cluster analysis is based on a distance method, it is important to 
## consider several different measures of distance that can be utilized in a 
## cluster analysis.  The most common of these measures is the weighted 
## Euclidean distance which is defined by:

## d[i,j] = sum(w[k]*(x[i,k]-x[j,k])^2)^0.5

## where w is a weight assigned to the different clusters.  Different weights
## yield different results; for example, if the weights are defined as 
## the reciprocal of the variance, you get the Karl Pearson distance.  You could
## also use weights that are reciprocals of the data if you have large ranges in
## your datasets.  We can write this distance formula in a more general form, 
## known as the Minkowski metric:

## d[i,j] = sum(w[k]*(x[i,k]-x[j,k])^(lambda))^1/lambda

## which reduces to Euclidean distance for lambda = 2.  Once you choose a measure
## of distance, you can compute all distances between all in a matrix form with zeros
## along the diagonal.  This can be useful from an organizational standpoint.

## Once the distance matrix has been computed, the data are formed using a 
## hierarchial approach; that is, one in which a hierarchy of groups is computed, 
## each of which is formed by merging a pair from a collection of previously
## defined groups.  This is done by first finding the two datapoints with the
## closest grouping, and merging them together.  This yields n-1 groups.  
## Then, the group with the next closest grouping is merged together, etc.  
## One key to hierarchial cluster analysis is that the datapoints do not get
## moved from a group once they are put there.  This method continues until
## all data are put into pairs.  However, the individual groups are not informative or helpful.  
## It is thought that by analyzing the structure of the large group n, or looking at
## an intermediate stage in the cluster analysis, that nice groupings G of the
## data may be revealed.

## Once the distances between pairs of points are defined, unambiguously using
## some form of Euclidean distance, there must be a method to define how
## the distance between the groups changes.  There are several common methods,
## known as linkages, which are used in this process.  

## One linkage is called single linkage.  This method simply finds the smallest
## distance between two points in the two vectors of interest and uses that
## for clustering.  A second method, called complete linkage, uses the largest
## distance between two points in the corresponding vectors to do the clustering.
## A more widely used method is called average linkage, 
## which basically uses the average distance for all possible pairs of data
## between the two vectors.  Another method is called centroid clustering, which
## is based on the difference in the means of the two vectors.  Obviously, these 
## different methods will give you different results, and it is difficult to 
## discern which is best.  Typically, the average linkage method is the best
## when doing a hierarchial cluster analysis.  

## Another cluster analysis method that is not based on distance, but is instead 
## based on minimum variance, is called Ward's method.  This method still groups
## data in the same type of order, but the groupings are instead based on the
## sum of squared differences between the points and the centroids of their
## respective squared groups, summed over the resulting groups.  This method 
## minimizes the sum, over the dimensionality of x, of within-group variances,
## which is an alternative method for grouping and sometimes is better than
## a typical distance clustering.  

## Clustering inherently is difficult to understand, but it is even more difficult
## to visualize.  The most commonly utilized tool for visualizing a cluster analysis
## is called a dendrogram.  In order to use a dendrogram though, we need to have
## a set of data to visualize.  Lets read in our precipitation and high temperature
## data that we used in our discriminant analysis discussion.


region.weather <- matrix(scan("region_weather.csv",sep=',',what="c"),ncol=5,byrow=T)
region.temps <- as.real(region.weather[,3])
region.precip <- as.real(region.weather[,4])
groups <- as.integer(region.weather[,5])

## We can use a cluster analysis to try and see if we can identify the same
## groupings as what are present in the data.  We will have x values that have
## two numbers associated with them; temperature and precipitation.  These
## values do not have the same magnitude because they have physically different
## units, so we should probabily consider doing a Karl-Pearson distance instead
## of a strictly Euclidean distance.  I wrote a function, kpdist, that will
## compute the Karl-Pearson distance for us.  We can then do a cluster analysis
## on the kp distance.  

## We can use the hclust function to do a hierarchial cluster analysis.  We 
## must specify if we do not want a complete linkage, which we do not.  Lets
## do an average linkage.  

x.values <- cbind(region.temps,region.precip)
distances <- kpdist(x.values)
clust.analysis <- hclust(distances,method="average")

## Now that we have completed the cluster analysis and have a cluster analysis
## R object, we can put that into the plclust function to plot the dendrogram,
## which is a visualization method that is utilized in cluster analysis.  Lets
## take a look at the dendrogram and try to understand what it shows.

labs <- cbind(region.weather[,1],groups)

plclust(clust.analysis,labels=region.weather[,1])

## Now then, we have a grouping of our data based on city name.  As you can
## see, the cities seemed to cluster pretty well together.  Lets look at the
## group numbers associated with each city instead to see what they look
## like:

plclust(clust.analysis,labels=groups)

## Looking at the dendrogram, the group numbers are near each other in the 
## figure.  This figure is a little difficult to interpret.  Basically, the height
## of the junction of each observation represents the distance between those
## two datapoints.  The lengths of the lines for each datapoint are strictly
## based on the visualization scheme used in R.  The important point is the junctions.
## You can look at the junctions and identify groupings based on those
## junctions.  In this figure, I right off the bat see a good separation between
## groups 1&2 and group 3.  This is good; it identifies that the Northeast
## groupings are much different than either the Plains or the Southeast.  However,
## without the numbers (which you won't have when you do this), it is not
## as easy to discern groups 1 and 2 from each other.  A series of 4 of the 
## cities; Jacksonville, Gainesville, Pensacola, and Savannah, all cluster
## together, which is expected due to their proximity in the Southeast.  Some
## Southeastern cities cluster in with Midwestern cities, such as Kansas City and
## Topeka mixed in with Atlanta, Athens, Huntsville, etc., which is a little
## surprising, but not shocking.  You can also see a pretty good separation 
## between that group of Southeast cities and the upper Midwestern cities on
## the far right.  Based on this, I would probably identify 4 clusters of these
## data; maybe 3.  Of those 4, you can easily see which cities fit into each
## cluster, and this is informative as to how the clustering works.  We can 
## try a scatterplot of these data, to see how the groups are separated
## in a scatterplot.  Lets assign different symbols for each of the 4
## groups and go from there.  First, we need to identify the rows associated
## with each group.

plclust(clust.analysis)
rows.1 <- c(6,4,9,10)
rows.2 <- c(23,20,22,26,27,26,21,24,28)
rows.3 <- c(8,3,7,14,18,2,1,5)
rows.4 <- c(19,12,15,13,11,16,17)
dataset.1 <- matrix(numeric(length(rows.1)*2),ncol=2,byrow=T)
dataset.2 <- matrix(numeric(length(rows.2)*2),ncol=2,byrow=T)
dataset.3 <- matrix(numeric(length(rows.3)*2),ncol=2,byrow=T)
dataset.4 <- matrix(numeric(length(rows.4)*2),ncol=2,byrow=T)

for (i in 1:length(rows.1)) {
dataset.1[i,] <- x.values[rows.1[i],]
}

for (i in 1:length(rows.2)) {
dataset.2[i,] <- x.values[rows.2[i],]
}

for (i in 1:length(rows.3)) {
dataset.3[i,] <- x.values[rows.3[i],]
}

for (i in 1:length(rows.4)) {
dataset.4[i,] <- x.values[rows.4[i],]
}

## Lets now do the plot.

plot(dataset.1[,1],dataset.1[,2],pch=1,xlim=c(50,85),ylim=c(2,8))
points(dataset.2[,1],dataset.2[,2],pch=2)
points(dataset.3[,1],dataset.3[,2],pch=3)
points(dataset.4[,1],dataset.4[,2],pch=4)

## As you can see, it is easy to identify that the cluster analysis separated the
## data into 4 distinct groups with no overlap.  We also will get different results based on different
## linkages.  Average linkage is a good one to use; you can try others as well if
## you are interested!

## In addition to a hierarchial clustering method, a non-hierarchial method, known
## as kmeans, exists.  This method, unlike the hierarchial methods, allows you to
## move datapoints from one cluster to another if the data present themselves
## in that way.  In many ways, this method is superior!  When we say 
## K-means, we mean K number of groups of the data, not K in the data
## dimension.  K-means requires some prior definition of the number of 
## groups in the data, whether it is a random guess of the number, a specified
## guess set/logical division, or the groups from a hierarchial cluster analysis.
## Once you have a guess set, the method proceeds as:

## 1)  Compute the centroids for each cluster (the mean of the x vector in
## each cluster).

## 2)  Compute the distance between the current vector of interest x[i] and 
## each of the centroids.  Usually either Karl-Pearson or Euclidean distance
## is used.

## 3)  If x[i] is already in the group where its membership is closest to
## the centroid, leave it alone.  Otherwise, move it into the new group.  
## This method iterates until each x[i] is closest to its group mean.  It is
## often useful to redo this analysis several times for different group 
## sizes to find the optimal one, since K-means requires an initial group
## specification; a significant disadvantage.  Lets do a K-means analysis on
## our data, assuming the 3 groups we have initially, and see how it does.
## We can use the kmeans function in R to do this analysis for us.

kmeans.test <- kmeans(x.values,3,iter.max=100,nstart=25)

## We can compare the results of this test to our original groups:

rbind(kmeans.test$cluster,groups) -> results
results

## The group numbers are arbitrary, but the points where the group numbers
## are different than their surrounding members are areas where the data
## are misclassified.  In the kmeans command, we specified 3 as the number
## of groups, iter.max = 100 is the maximum number of iterations when creating
## groups, and nstart=25 tells us to test 25 different groupings of the
## data.  Our results show us that a few of the cities:

t(rbind(region.weather[,1],results))

## such as Athens, Atlanta, Huntsville, and Wichita are not classified 
## into their correct groups.  Obviously this method is not perfect, but in
## many cases it is superior to a hierarchial method since that method is
## limited to the groups once the data are inserted into a group.  