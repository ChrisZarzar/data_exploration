#Author: Christopher Zarzar
#Created: 4-Dec-2018

#NOTES: The purpose of this script is to investiate using boxplots to the North Farm data

panel06.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined06%_MicaSense.csv", header = TRUE)
panel22.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined22%_MicaSense.csv", header = TRUE)
panel44.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined44%_MicaSense.csv", header = TRUE)

#Create graphic output
pdf(file = "C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/data_exploration/MicaSense_NF_DataExplore_boxplot.pdf", width = 14, height = 14)
par(mfrow=c(5,3),mar=c(1,2,1,1),oma=c(4,4,4,1)) #(bottom, left, top, right)

# The analysis for all altitudes and panel
set.seed(7)
loopa <- 0
loopb <- 0
band <- c('Blue', 'Green', 'Red', 'rEdge','NIR')
band.col <- c(3:7)
tarp <- c('06', '22', '44')
alt <- c('30','100','200','300','400','500','600','700','800')
for (k in 1:length(band.col)){
  loopa<-loopa+1
  for(j in 1:length(tarp)){
    loopb<-loopb+1
    plotmat <- numeric(3)
    for (i in 1:length(alt)){
      working.cmd <- paste('working.data <- panel',tarp[j],'.data',sep='')
      eval(parse(text=working.cmd))
      level <- as.double(alt[i])
      panel.tmp <- ifelse(working.data[,2]==level,working.data[,band.col[k]],NA)
      panel.tmp <- panel.tmp[!is.na(panel.tmp)]
      
      ## Add each altitude to a variable data matrix
      var.cmd <- paste('var.',tarp[j],'_',alt[i],'.',band[k],'<- data.matrix(panel.tmp)',sep='')
      eval(parse(text=var.cmd))
      
      ## Create and bind together the confidence intervals from each altitude
      var2.cmd <- paste('var.tmp <- var.',tarp[j],'_',alt[i],'.',band[k], sep='')
      eval(parse(text=var2.cmd))
      ci.cmd <- paste('ci.',tarp[j],'_',alt[i],'.',band[k],'<- quantile(var.tmp,probs=c(0.025,0.5,0.975))',sep='')
      eval(parse(text=ci.cmd))
      
      plotmat.cmd <- paste('plotmat <- cbind(plotmat,','ci.',tarp[j],'_',alt[i],'.',band[k],')',sep='')
      eval(parse(text=plotmat.cmd))
    }  
    ##Create CI boxplots

    plotmat <- plotmat[,-1]
    xrange <- c(seq(4,244,by=30)) 
    #xrange <- c(30,(seq(100,800,by=100)))
    barwidth=0.2
    plot.title <- paste(band[k],' Band ',tarp[j], '% Panel', sep='')
    boxplot.matrix(plotmat, ylab="", xlab="", xaxt='n',cex.axis=1.5)
    #if (loopb %in% c(1,4,7,10,13)){
    #  axis(2, at=seq(0,0.15,by=0.05), cex.axis=2, las=2)
    #} 
    if (loopb %in% c(13,14,15)){
      axis(1, at=1:9, labels=xrange, cex.axis=1.5, las=2) #change at= to 1:9 when doing 30 - 800 ft
    } else{
    }
    mtext(plot.title, side=3, cex=1, adj=0.5,0.5)
    box(col="grey60")
    #abline(h=plotmat[2,1], col="blue")
    #abline(h=plotmat[1,1],lty= 2, col="red")
    #abline(h=plotmat[3,1],lty= 2, col="red")
    #dev.off()
  }
  
}
#text(5,2, "DN", cex=1)
#text(1,7, "Altitude (meters)", cex=1)
#mtext("DN", side=2, cex=1.5, adj=0.5,0.5)
#mtext("Altitude (meters)", side=1, cex=1.5, adj=0.5,0.5)
## COMMET THE BELOW BACK IN IF YOU WANT IT IN AN ENTIRE SINGLE FILE. 
dev.off()
closeAllConnections()