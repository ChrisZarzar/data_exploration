## Script to determine nocturnal tornado outbreaks
## For thesis for Alex Elmore

## This variable allows the user to change the number of hours that must go by before
## a nighttime tornado may be counted as a nocturnal outbreak.  For days with daytime
## tornadoes that also have nocturnal tornadoes later.

delay <- 6 ## This changes the criteria by which daytime tornadoes are handled.  Larger numbers are stricter criteria (more time from sunrise/sunset)

## As of now, this script will work using the following approach
##
## Step 1 - Read in data and identify lat/lons for all tornadoes post 1979 and all points east of Denver (105W longitude)
## Step 2 - Formulate a searchable time database for dates and to use to determine the time between tornado reports
## Step 3 - Formulate the sunrise and sunset time for each tornado, based on its DOY, latitude, and longitude, and convert tornado times to decimal hours
## Step 4 - Determine the amount of time between tornadoes as a vector, one time for each tornado
## Step 5 - Using the sunrise/sunset time, determine if each tornado is nocturnal or not.  Store that in a vector (-1 for sunrise nocturnal, 0 for daytime, 1 for sunset nocturnal)
## Step 6 - Loop through each tornado individually to determine if it meets the criteria for membership in a nocturnal outbreak, based on specific criteria in the comments in 
##          this step



############
## Step 1 ##
############

## Read in the data
tor.data <- matrix(scan('1950-2014_torn.csv',what='c',sep=','),ncol=28,byrow=T)
## Remove everything pre 1979 (since we are using NARR)
tor.data <- tor.data[-c(1:(which(tor.data[,2]=="1979")[1]-1)),]

## Now create starting lats and lons information
## Pull starting lat/lon for each tornado
slat <- as.double(tor.data[,16])
slon <- as.double(tor.data[,17])
## Cull all tornadoes west of Denver and any tornadoes with a 0 longitude
cull.pts <- which(slon < -105 | slon==0)
slon <- slon[-cull.pts]
slat <- slat[-cull.pts]
tor.data <- tor.data[-cull.pts,] 
## Final database of 38,568 tornadoes by 28 columns

############
## Step 2 ##
############

## Create a date database to make the search easier
#Create date database
d.31 <- c(1:31)
d.30 <- c(1:30)
d.29 <- c(1:29)
d.28 <- c(1:28)
n.obs <- 1
days.ly <- c(rep(d.31,each=n.obs),rep(d.29,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs))
days.ly <- ifelse(days.ly<10,paste("0",days.ly,sep=""),days.ly)
days.nly <- c(rep(d.31,each=n.obs),rep(d.28,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs),rep(d.30,each=n.obs),rep(d.31,each=n.obs))
days.nly <- ifelse(days.nly<10,paste("0",days.nly,sep=""),days.nly)
months.ly <- c(rep(1,31*n.obs),rep(2,29*n.obs),rep(3,31*n.obs),rep(4,30*n.obs),rep(5,31*n.obs),rep(6,30*n.obs),rep(7,31*n.obs),rep(8,31*n.obs),rep(9,30*n.obs),rep(10,31*n.obs),rep(11,30*n.obs),rep(12,31*n.obs))
months.ly <- ifelse(months.ly<10,paste("0",months.ly,sep=""),months.ly)
months.nly <- c(rep(1,31*n.obs),rep(2,28*n.obs),rep(3,31*n.obs),rep(4,30*n.obs),rep(5,31*n.obs),rep(6,30*n.obs),rep(7,31*n.obs),rep(8,31*n.obs),rep(9,30*n.obs),rep(10,31*n.obs),rep(11,30*n.obs),rep(12,31*n.obs))
months.nly <- ifelse(months.nly<10,paste("0",months.nly,sep=""),months.nly)
date.ly <- cbind(months.ly,days.ly)
date.nly <- cbind(months.nly,days.nly)

## We will also calculate an accumulated time variable to use in determining the time between tornado reports.
date.database <- numeric(3)
years <- as.integer(unique(tor.data[,2]))
for (k in 1:length(years)) {
     if( years[k] %% 4 == 0) { ## leap year
        datemat <- cbind(rep(years[k],times=366),date.ly)
        date.database <- rbind(date.database,datemat)
     } else {
        datemat <- cbind(rep(years[k],times=365),date.nly)
        date.database <- rbind(date.database,datemat)
     }
} ## 13149 total days through 2014
acc.time <- seq(0,(dim(date.database)[1]-1)*24,by=24)  ## This vector gives the number of hours since the first time that have accumulated by this day
date.database <- cbind(date.database,acc.time)
date.database <- matrix(as.double(date.database),ncol=4,byrow=F) ## This object tells us numbers of hours accumulated up to a given day at 0000 UTC.  Additional
date.database <- date.database[-1,]                                                 ## time can be added based on timevec
############
## Step 3 ##
############

## Next, need to solve for sunrise/sunset time for each tornado based on
## its DOY, latitude, and longitude.  Will get one vector of 
## sunrise/sunset times for each tornado in the list (38,568 sunrise/sunset times)

## Now compute a vector of solar declination angles for each tornado date
sda <- numeric(0)
for (i in 1:dim(tor.data)[1]) { ## Loop over all tornadoes
	if (as.integer(tor.data[i,2])%%4==0) {
		ly.flag <- 1
	} else {
		ly.flag <- 0
	}
	month <- ifelse(as.integer(tor.data[i,3])<10,paste("0",tor.data[i,3],sep=""),tor.data[i,3])
	day <- ifelse(as.integer(tor.data[i,4])<10,paste("0",tor.data[i,4],sep=""),tor.data[i,4])
	if (ly.flag==1) {
		julian.day <- which(date.ly[,1]==month & date.ly[,2]==day)
	} else {
		julian.day <- which(date.nly[,1]==month & date.nly[,2]==day)
	}
	sda.tmp <- 23.5*cos((pi/180)*360*(julian.day-173)/365.25)
      sda <- c(sda,sda.tmp)
}  ## The object sda has the solar declination angles for each tornado in it

## Now compute sunrise and sunset times from solar declination angles and lats/lons

sun.set <- (24/360)*((-slon)-(180/pi)*acos(tan((pi/180)*slat)*tan((pi/180)*sda)))
sun.set <- ifelse(sun.set < 0, 24+sun.set,sun.set)
sun.set <- sun.set-6 ## Convert to CST time
sun.set <- ifelse(sun.set < 0, 24+sun.set,sun.set)

sun.rise <- (24/360)*((-slon)+(180/pi)*acos(tan((pi/180)*slat)*tan((pi/180)*sda)))
sun.rise <- ifelse(sun.rise < 0, 24+sun.rise,sun.rise)
sun.rise <- sun.rise-6 ## Convert to CST time
sun.rise <- ifelse(sun.rise < 0, 24+sun.rise,sun.rise)

write(sun.set,'sun_set.txt',ncol=1)
write(sun.rise,'sun_rise.txt',ncol=1)

time.mat <- matrix(as.double(unlist(strsplit(tor.data[,6],split=":"))),ncol=3,byrow=T)[,1:2]
time.vec <- time.mat[,1]+time.mat[,2]/60

## Now we need to convert the sunrise/sunset times for each tornado, and the valid time for each tornado,
## as a continuous variable beginning at the original time of the dataset.  We will use the acc.time variable
sunrise.acc <- numeric(0)
sunset.acc <- numeric(0)
timevec.acc <- numeric(0)

for (i in 1:dim(tor.data)[1]) { ## Loop over all tornadoes
     
     working.year <- as.integer(tor.data[i,2])
     working.month <- as.integer(tor.data[i,3])
     working.day <- as.integer(tor.data[i,4])
     working.row <- which(working.year==date.database[,1] & working.month==date.database[,2] & working.day==date.database[,3])

     sunrise.acc <- c(sunrise.acc,sun.rise[i]+(date.database[working.row,4]-24))
     sunset.acc <- c(sunset.acc,(sun.set[i]+date.database[working.row,4]-24))
     timevec.acc <- c(timevec.acc,(time.vec[i]+date.database[working.row,4]-24))

} ## End for loop for all tornadoes

############
## Step 4 ##
############

## NOTE:  This step takes some time.  You can run it once and comment it out for future runs of the script, only reading in the 
## generated text file, time_between.txt, as part of your script instead.  I've left it commented out.  Uncomment everything to the write
## statement if it needs to be rerun.

## Let's also create a column based on the time between tornadoes.  Obviously the first one is 0, but we need to set the second one, etc.

## Uncomment below here to the write statement to run step 4

time.between <- 0 ## set first time to 0

for (k in 2:dim(tor.data)[1]) { ## Determine time between current tornado and previous tornado
     working.day <- as.integer(tor.data[k,4])
     working.month <- as.integer(tor.data[k,3])
     working.year <- as.integer(tor.data[k,2])
     prev.day <- as.integer(tor.data[(k-1),4])
     prev.month <- as.integer(tor.data[(k-1),3])
     prev.year <- as.integer(tor.data[(k-1),2])
   
     day.lab <- which(date.database[,1]==working.year & date.database[,2]==working.month & date.database[,3]==working.day) 
     hours.accum <- date.database[day.lab,4] + time.vec[k]
     prev.lab <- which(date.database[,1]==prev.year & date.database[,2]==prev.month & date.database[,3]==prev.day)
     prev.accum <- date.database[prev.lab,4]+time.vec[k-1]
     time.between <- c(time.between,(hours.accum-prev.accum))

} ## Write out this object for future use
write(time.between,'time_between.txt',ncol=1)

time.between <- scan('time_between.txt')

############
## Step 5 ##
############

## Now, the object time.vec contains the tornado valid time, 
## we know the tornado date information from columns 2-5 of the tor.data object,
## and sun.set and sun.rise contain the sunrise/sunset info for each tornado

## Now we need to identify each tornado as nocturnal or not nocturnal.  We will define it in this way:
## Not noctural - 0
## Nocturnal (before sunrise) - -1
## Nocturnal (after sunset) - 1

is.nocturnal <- ifelse(time.vec < sun.rise,-1,ifelse(time.vec > sun.set,1,0))

############
## Step 6 ##
############

## This will require a series of checks for each tornado.  Begin a for loop for all tornadoes:
nocturnal.mat <- matrix(numeric(dim(tor.data)[2]+1),nrow=1) ## This object will store all tornado information, +1 for outbreak number

outbreak.num <- 0
is.night <- F
is.morn <- F
tor.num <- 2

while(tor.num <= dim(tor.data)[1]) { 
     working.day <- tor.data[tor.num,4]
     working.month <- tor.data[tor.num,3]
     working.year <- tor.data[tor.num,2]
     working.row <- which(working.year==date.database[,1] & working.month==date.database[,2] & working.day==date.database[,3])
     is.night <- F
     is.morn <- F

     ## Now we need to see if we've found the start of an outbreak
     if ((is.nocturnal[tor.num]==1 && timevec.acc[tor.num-1]<sunset.acc[tor.num]-delay) || (is.nocturnal[tor.num]==-1)) { ## Found the starting point, start while loop
         outbreak.num <- outbreak.num + 1
         nocturnal.mat <- rbind(nocturnal.mat,c(tor.data[tor.num,],outbreak.num))
 
         if(is.nocturnal[tor.num]==1) { is.night <- T }  ## is.night==T, first tornado is a pre-midnight tornado
         if(is.nocturnal[tor.num]==-1) { is.morn <- T }  ## is.morn==T, first tornado is post-midnight

         tor.num <- tor.num + 1 ## iterate forward once you found an outbreak.  Now start a while loop to see if any other tornadoes belong to this outbreak

         while((is.nocturnal[tor.num]==1 || is.nocturnal[tor.num]==-1) && time.between[tor.num] < 24) { ## Tornado occurs within 24 hours of outbreak tornado, so we can consider it

             if (is.nocturnal[tor.num]==1 && tor.data[tor.num,4]==working.day && tor.data[tor.num,3]==working.month && tor.data[tor.num,2]==working.year) { ## nighttime, same day
                 nocturnal.mat <- rbind(nocturnal.mat,c(tor.data[tor.num,],outbreak.num))
             } else if (is.nocturnal[tor.num]==-1 && is.morn) { ## Consider ongoing morning outbreak
                 ref.time <- sun.rise[tor.num]+(date.database[working.row,4]-24)
                 delay.time <- ref.time + delay
                 if (timevec.acc[tor.num]<ref.time && (timevec.acc[tor.num+1]<ref.time || (is.nocturnal[tor.num+1]==0 && timevec.acc[tor.num+1]>delay.time) || is.nocturnal[tor.num+1]==1)) {
                     nocturnal.mat <- rbind(nocturnal.mat,c(tor.data[tor.num,],outbreak.num))    
                 }
             } else if (is.nocturnal[tor.num]==-1 && is.night && time.between[tor.num]<24) {
                 nocturnal.mat <- rbind(nocturnal.mat,c(tor.data[tor.num,],outbreak.num))    
             }
             tor.num <- tor.num + 1
           
         } ## End while loop

     } ## End if statemetn when we found a tornado

tor.num <- tor.num + 1
} ## End for loop for tor.num
nocturnal.mat <- nocturnal.mat[-1,]
write.table(nocturnal.mat,'nocturnal_data.csv',sep=',',col.names=F,row.names=F)

##Find events with 6 or greater tornadoes
noct.data <- matrix(scan('nocturnal_data.csv',what='c',sep=','),ncol=29,byrow=T)
nocturnal_outbreaks <- c(4,42,64,93,136,139,199,213,221,224,263,303,307,351,382,384,414,422,448,495,517,528,565,576,590,651,670,687,720,724,731,763,765,775,776,805,812,844,845,861,866,871,885,891,893,894,895,916,931,954,963,965,976,977,979,981,986,1000,1005,1019,1036,1040,1045,1048,1079,1080,1082,1084,1085,1092,1095,1099,1101,1119,1121,1126,1156,1157,1174,1179,1181,1184,1188,1190,1216,1223,1256,1280,1290,1306,1308,1309,1311) 
noct_ob <- noct.data[which(noct.data[,29] %in% nocturnal_outbreaks),]
write.table(noct_ob,'nocturnal_outbreaks.csv',sep=',',col.names=F,row.names=F)