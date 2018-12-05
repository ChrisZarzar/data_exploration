#  R intro


#  The most important command you will use in R is the ?.  If you enter:

?mean

# it will pull up the help information for the mean.  This information
# will contain anything you may need for the given function, and probably
# a lot of information you won't need.  Use it often!

#  You can define variables with an <- sign.  Arrows can point either direction, but
#  some conflicts can arise when pointing the arrow -> instead of <-.  

a<-2
b<-a*2
c<-a/2
d<-a+2

# Remember it is important to have DESCRIPTIVE VARIABLE NAMES!!  These will prevent
# headaches in the future.  It's much easter to remember what input.vector means
# than a or b.  

## R PROGRAMMING ##

# R is a programming language, much like C or Fortran.  R is considerably 
# less complex than true programming languages though, as many of the things you would
# want to do are already created for you.  For example, in fortran, if you were to 
# want to compute the mean of a variable, you would need to write a separate program
# for the mean.  It may look something like:

# PROGRAM mean

# Implicit None

# REAL :: sum.variable,mean
# REAL, DIMENSION(1:n.vars) :: input.variable

# sum.variable = 0

# DO i = 1, n.vars

# sum.variable = input.variable(i) + sum.variable

# END DO

# mean = sum.variable/n.vars

# PRINT *, mean

# END PROGRAM

# Obviously, this is more complicated than simply typing:

# mean(input.variable)

# to obtain a mean value.  That being said, much of the logic
# used in programming in R is similar to that in computer
# programming.  

# There are several types of variables in R, just as there are in 
# programming.  These include:

# REAL:  Those variables which are real numbers, and can include decimals
# COMPLEX:  Those which include both a real part and a non-real part
# INTEGER: Those which only can contain a value that is a whole number.  
# LOGICAL:  Those which can only contain a result of true or false

# You can also use scientific notation in R.  You can represent 1000 mb in 
# pascals (100 000 Pa) as either:

pressure <- 100000
pressure2 <- 10E+5

pressure - pressure2

# As you can see, these are equal to each other.  Now that we have looked at some of the 
# variable types, lets understand the logic of building a R program.  There are a 
# few keywords which are important and analagous to their Fortran/C counterparts.  These
# include if/else and for.  

# The if/else command is a logical test that a programmer can use to tell R to
# do something if certain conditions are met (hence conditional).  This is very useful
# when trying to find numbers within a certain range, test for errors, etc., and will
# be useful in the course.  For example, lets try a simple if  test.  We'll start
# with a simple example.

sample.dataset <- c(2,4,6,8,10)

if (mean(sample.dataset) <=10) {
	show('The test is true!')
	} else  # Here we MUST put the else statement on the same line as the closed brace
{      show('The test is false!')
}

# As you can imagine, this will have numerous applications in what we will do in the 
# class, as well as in program writing in general.  In addition to the if/else statements,
# we can use a for loop.  The for loop will allow you to specify a certain number of times
# to do an action in R.  The notation is as follows:

big.counter <- numeric(100)
for (counter in 1:100) {
	big.counter[counter] <- counter
}

# If we look at what we put into big.counter, we see

big.counter

# So we essentially counted to 100.  This will have applications throughout our work 
# as well.  Your first homework (assigned today) will allow you to experiment with
# these words.  This is crucial to understanding R!

# Lets see how R handles a complex number too.  

complex_number <- sqrt(-4)

# In other words, R doesn't handle complex numbers very well.  This can be 
# overcome, but is not of interest for this course.  


#  You can create vectors in R using c().  Use of a
#  : will allow you to create a sequence of numbers without typing them all
#  out.

sample.vector <- c(1,2,3,4,5,6,7,8,9,10)
sample.vector2 = c(1:10)

#  You can do simple operations to vectors, such as
#  add, subtract, etc.

added.vector <- sample.vector + sample.vector
subtract.vector <- sample.vector - sample.vector

#  Multiplication simply multiplies the individual components of the vector, and 
#  Division is more complex, will cover that later

multiplied.vector <- t(sample.vector) %*% sample.vector

#  Why does this multiplication result in a single number?

#  We will also need to look at how to make matrices in R.  To do this, we require
#  an input vector we wish to convert to a matrix.  If we don't give it one, it will
#  instead create a matrix of NAs over our specified size.  Lets try one.

vector<-c(1,0,1,2,1,0)
sample.matrix<-matrix(vector,nrow=2,ncol=3,byrow=T)

#  The byrow tag tells R to read the vector row by row instead of column by
#  column.  This is very useful when reading in data files, as you will see
#  later in the class.  If we do not give R a data vector:

sample.matrix2<-matrix(nrow=2,ncol=3,byrow=T)

#  Then you see we get a matrix of NAs, which is not helpful.

#  If we consider our sample.matrix, we can look at individual components by:

matrix.component <- sample.matrix[2,1]

#  As you saw above, you can compute the transpose of the matrix
#  with a function.  Function notation is done as     function.name(options).  Often, *.m files have
#  the various function options at their beginning, and should be analyzed before using the functions.
#  The transpose can also be computed by adding a ' symbol at the end of the variable name.


t.sample.matrix <- t(sample.matrix)

#  USING THE APOSTROPHE DOES NOT WORK IN R FOR THE TRANSPOSE!!!
#  You can do matrix operations, the same as vector operations.  To matrix multiply, you must be sure
#  the dimensions are correct or R will give you an error message.

multiplied.matrix <- sample.matrix %*% t.sample.matrix

#  What  is special about this matrix?

#  You can also invert square matrices using the inv() function

inverse.matrix <- solve(multiplied.matrix)

#  Just to test that the inversion is correct:

identity.matrix <- multiplied.matrix %*% inverse.matrix

#  Is this the identity matrix?

#  Creating an identity matrix is not as straightforward in R as it was in Matlab.
#  We need to use the diag command.

big.identity.matrix<-diag(x=1,20)

#  You can also generate random matrices based off of either the normal distribution or the uniform distribution.  More on these
#  later.  To generate the uniform distribution, use:

uniform.vector <- runif(100,0,1)

#  which provides a 100 X 1 matrix, or a 100 piece vector

#  And the normal distribution

normal.vector <- rnorm(100,0,1)

#  The semicolon  at the end of the previous two code lines tells R to not output the entire result of the expression.  This
#  is useful to avoid lots of data scrolling while you work, and is faster.  

#  Lets look at how these matrices appear.  We will plot a histogram of each vector.  To view a histogram, use the hist( ) command.

hist(uniform.vector)

#  Why does this histogram look differently on your screen than on mine?

#  Lets try the normal distribution

hist(normal.vector)

#  Since this is the normal distribution, the largest number of data points should cluster near the 0 mean, which they do.  This
#  has the typical bell-shaped curve that is expected from the normal distribution.


#  You can compute simple statistics on vectors and matrices.  Many statistics are possible, including:

mean.vector <- mean(normal.vector)
variance.vector <- var(normal.vector)
standard.deviation.vector <- sd(normal.vector)
maximum.value <- max(normal.vector)
minimum.value <- min(normal.vector)
median.value <- median(normal.vector)
mode.value <- mode(normal.vector)


#  Is the normal vector you created biased in any way?  




