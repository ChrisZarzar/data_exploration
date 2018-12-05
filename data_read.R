#Author: Christopher Zarzar
#Created: 4-Dec-2018

#NOTES: The purpose of this script is to offer examples for loading different types of data

##Loading a CSV formatted file
csv.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined06%_MicaSense.csv", header = TRUE)
##standard stats functions will struggle with this formatting, so you can fix this by converting the column of interest into a data.matrix
x1<-data.matrix(csv.data$Avg_Rad_Band1)
x2<-data.matrix(csv.data$Avg_Rad_Band2)
x3<-data.matrix(csv.data$Avg_Rad_Band3)
x4<-data.matrix(csv.data$Avg_Rad_Band4)
x5<-data.matrix(csv.data$Avg_Rad_Band5)
x<-cbind(x1,x2,x3,x4,x5)

boxplot.matrix(x)

##Loading a simple text file with various possible formats 
text.data <- matrix(scan("G:/RWorkspace/datasets/wind_outbreaks.txt"), ncol = 5, byrow=T)