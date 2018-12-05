#This script will be used to run the F-test between the variables for zip codes with and without cases
#Rather than use HW 7, I am going to do this using the built in var.test() function because it is simpler and provides
#more organized output compared to the output we produced in class.


#Now I will test whether the variance of the noCase zipcodes and case zipcodes are equal.
#I will need to use the f test for this
#Ho: The variance of mean values in the noCase dataset and the mean values in the case dataset are equal
#Ha: The variance of mean values in the noCase dataset and the mean values in the case dataset are not equal
#I may just bring this all into one script with the F test. 

print("f-test for bird slopes in 2002")
#set these first two variables to changes what datasets will be calculated
noCase.data <- slope.02bnoCase.data
case.data <- slope.02bcase.data
var.test(noCase.data[,8],case.data[,8])


print("f-test for bird slopes in 2003")
noCase.data <- slope.03bnoCase.data
case.data <- slope.03bcase.data
var.test(noCase.data[,8],case.data[,8])



print("f-test for bird aspect in 2002")
noCase.data <- aspect.02bnoCase.data
case.data <- aspect.02bcase.data
var.test(noCase.data[,8],case.data[,8])



print("f-test for bird aspect in 2003")
noCase.data <- aspect.03bnoCase.data
case.data <- aspect.03bcase.data
var.test(noCase.data[,8],case.data[,8])



print("f-test for bird elev in 2002")
noCase.data <- elev.02bnoCase.data
case.data <- elev.02bcase.data
var.test(noCase.data[,8],case.data[,8])



print("f-test for bird elev in 2003")
noCase.data <- elev.03bnoCase.data
case.data <- elev.03bcase.data
var.test(noCase.data[,8],case.data[,8])


print("f-test for human slopes in 2002")
noCase.data <- slope.02hnoCase.data
case.data <- slope.02hcase.data
var.test(noCase.data[,8],case.data[,8])


print("f-test for huamn slopes in 2003")
noCase.data <- slope.03hnoCase.data
case.data <- slope.03hcase.data
var.test(noCase.data[,8],case.data[,8])


print("f-test for human aspect in 2002")
noCase.data <- aspect.02hnoCase.data
case.data <- aspect.02hcase.data
var.test(noCase.data[,8],case.data[,8])


print("f-test for human aspect in 2003")
noCase.data <- aspect.03hnoCase.data
case.data <- aspect.03hcase.data
var.test(noCase.data[,8],case.data[,8])


print("f-test for human elev in 2002")
noCase.data <- elev.02hnoCase.data
case.data <- elev.02hcase.data
var.test(noCase.data[,8],case.data[,8])



print("f-test for human elev in 2003")
noCase.data <- elev.03hnoCase.data
case.data <- elev.03hcase.data
var.test(noCase.data[,8],case.data[,8])
