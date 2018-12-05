#####################################
# Incorporate SST data from netCDF
# David March
# damamo82@gmail.com
#####################################

rm(list=ls(all=TRUE)) #reset data

#####################################
# load libraries
library(ncdf)
library(raster)
library(stringr)

#####################################
# list all netCDF files
files<-dir("c:/temp/myocean/",pattern=".nc$",recursive=TRUE)

#####################################
# Extract SST at Palma Bay for each file
data=NULL
for (i in 1:length(files)){
  
  print(paste("file",i,"from",length(files)))
  
  # open netcdf
  nc<-paste("c:/temp/myocean/",files[i],sep="") #set raster path
  sst <- raster(nc,varname="analysed_sst") #read raster
  sst<-sst-273.15 #convert kelvin to celsius
  
  # extract value at Palma Bay
  xy <- cbind(2.6, 39.45) #define lon/lat
  temp<-extract(sst, xy) #extract sst
  
  #extract date from netcdf
  grid.nc <- open.ncdf(nc)
  sec=get.var.ncdf(grid.nc,'time')
  d=as.POSIXct(sec, origin="1981-01-01 00:00:00",tz="UTC") #set time reference
  close.ncdf(grid.nc)
  
  # fill data.frame with date (d) and sst (temp)
  data<-rbind(data,data.frame(d,temp))
}

# save results
write.table(data,"sst.csv",sep=";",dec=",",row.names=F)
#####################################
