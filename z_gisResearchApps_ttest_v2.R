#This script will be used to run the t-test between the variables for zip codes with and without cases
#This script is now using the built in T-test just because it is simpler than the one we used in class and provides organized output. This runs the
#classic t-test of whether means are equal. I want to read more into the
#difference of a t-test and a z-test because I may also want to do a Z test like
#in HW 8. I could also look at the confidence intervals using the information
#from HW 9.

#**I NEED TO EDIT ALL OF THIS TO MATCH THE OUTPUT SARA AND COREY PROVIDE ME 
#no case are the zipcodes associated with no West Nile virus cases
#b or h before the noCase or case corresponds to bird or human cases
slope.02bnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_bird_neg.csv", header = TRUE)
slope.02bcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_bird_poz_02.csv", header = TRUE)
aspect.02bnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_bird_neg.csv", header = TRUE)
aspect.02bcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_bird_poz_02.csv", header = TRUE)
elev.02bnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_bird_neg.csv", header = TRUE)
elev.02bcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_bird_poz_02.csv", header = TRUE)


slope.03bnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_bird_neg.csv", header = TRUE)
slope.03bcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_bird_poz_03.csv", header = TRUE)
aspect.03bnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_bird_neg.csv", header = TRUE)
aspect.03bcase.data <-  read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_bird_poz_03.csv", header = TRUE)
elev.03bnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_bird_neg.csv", header = TRUE)
elev.03bcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_bird_poz_03.csv", header = TRUE)
# roadDen.03bnoCase.data <- matrix(scan("[INSERT LOCATION]"), ncol = 4, byrow=T)
# roadDen.03bcase.data <- matrix(scan("[INSERT LOCATION]"), ncol = 4, byrow=T)

slope.02hnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_hum_neg.csv", header = TRUE)
slope.02hcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_hum_poz_02.csv", header = TRUE)
aspect.02hnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_hum_neg.csv", header = TRUE)
aspect.02hcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_hum_poz_02.csv", header = TRUE)
elev.02hnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_hum_neg.csv", header = TRUE)
elev.02hcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_hum_poz_02.csv", header = TRUE)


slope.03hnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_hum_neg.csv", header = TRUE)
slope.03hcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/slope_hum_poz_03.csv", header = TRUE)
aspect.03hnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_hum_neg.csv", header = TRUE)
aspect.03hcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/aspect_hum_poz_03.csv", header = TRUE)
elev.03hnoCase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_hum_neg.csv", header = TRUE)
elev.03hcase.data <- read.csv("C:/Users/Chris/Desktop/RWorkspace/gisappData/txt_files/elev_hum_poz_03.csv", header = TRUE)




#Now I will use a t-test to test whether the nocase and case zipcodes for h or b for 2002 or 2003 averages are equal
#Ho: The average of the nocase and case zip code for the mean of each variable are  equal
#Ha: The average of the nocase and case zip code for the mean of each variable are not equal
print("t-test for bird slopes in 2002")
#set these first two variables to changes what datasets will be calculated
noCase.data <- slope.02bnoCase.data
case.data <- slope.02bcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for bird slopes in 2002")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)


print("t-test for bird slopes in 2003")
noCase.data <- slope.03bnoCase.data
case.data <- slope.03bcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for bird slopes in 2003")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)


print("t-test for bird aspect in 2002")
noCase.data <- aspect.02bnoCase.data
case.data <- aspect.02bcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for bird aspect in 2002")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)


print("t-test for bird aspect in 2003")
noCase.data <- aspect.03bnoCase.data
case.data <- aspect.03bcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for bird aspect in 2003")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)



print("t-test for bird elev in 2002")
noCase.data <- elev.02bnoCase.data
case.data <- elev.02bcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for bird elev in 2002")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)



print("t-test for bird elev in 2003")
noCase.data <- elev.03bnoCase.data
case.data <- elev.03bcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for bird elev in 2003")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)



print("t-test for human slopes in 2002")
noCase.data <- slope.02hnoCase.data
case.data <- slope.02hcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for human slopes in 2002")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)



print("t-test for huamn slopes in 2003")
noCase.data <- slope.03hnoCase.data
case.data <- slope.03hcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for huamn slopes in 2003")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)




print("t-test for human aspect in 2002")
noCase.data <- aspect.02hnoCase.data
case.data <- aspect.02hcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for human aspect in 2002")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)



print("t-test for human aspect in 2003")
noCase.data <- aspect.03hnoCase.data
case.data <- aspect.03hcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for human aspect in 2003")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)



print("t-test for human elev in 2002")
noCase.data <- elev.02hnoCase.data
case.data <- elev.02hcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for human elev in 2002")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)




print("t-test for human elev in 2003")
noCase.data <- elev.03hnoCase.data
case.data <- elev.03hcase.data
t.test(noCase.data[,8],case.data[,8])
print("bootstrapped t-test for human elev in 2003")
boot.noCase <- boot(noCase.data[,8], mean.boot,R=1000)
boot.case <- boot(case.data[,8], mean.boot,R=1000)
t.test(boot.noCase$t,boot.case$t)


