#Author: Christopher Zarzar
#Created: 8-Feb-2019

#NOTES: The purpose of this script is to plot bootstrapped median lines with 95% confidence halo for data from each waveband along each flight 
# altitude onto a signle plot for comparison between bands and analysis of atmospheric effects. 

# Execute and load libraries and functions that I will need

library("boot")

mean.boot <- function(x,d){
  return(mean(x[d]))
}


source('C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/mercer_rcodes/plot.ci.R')


##Start script for the MICASENSE CAMERA
# Read in the data
panel06.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined06%_MicaSense.csv", header = TRUE)
panel22.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined22%_MicaSense.csv", header = TRUE)
panel44.data <- read.csv("C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/datasets/AltitudesCombined44%_MicaSense.csv", header = TRUE)


par(mfrow=c(1,3),mar=c(1,2,1,1),oma=c(4,4,4,1)) #(bottom, left, top, right)

## Organize the data by altitude
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
      if (i==1){
        panel.sfc <- panel.tmp
      }
      
      # Run bootstrap confidence interval 
      
      boot.tmp <- boot(panel.tmp,mean.boot,R=1000)
      ci.cmd <- paste('ci.',tarp[j],'_',alt[i],'.',band[k],'<- quantile(boot.tmp$t,probs=c(0.025,0.5,0.975))',sep='')
      eval(parse(text=ci.cmd))
      
      #Now run a permutation test to determine whether the two dataset samples come from the same population
      #Running a two sided permutation test to determine whether mean of the datasets are the same (alpha = 0.05: 2 sided)
      
      plotmat.cmd <- paste('plotmat <- cbind(plotmat,','ci.',tarp[j],'_',alt[i],'.',band[k],')',sep='')
      eval(parse(text=plotmat.cmd))
      
    }
    # cbind the confidence intervals together and make plots of altitude CI'S
    
    #Plot the CIs
    #png(file =paste('C:/Users/zarzarc/OneDrive/Desktop/Research/scripts/RWorkspace/NorthFarm_output_2018/NorthFarmPlots_Canon_',band[k],'_',tarp[j], '.png', sep=''))
    plotmat <- plotmat[,-1]
    xrange <- c(seq(4,244,by=30)) #xrange <- c(30,(seq(100,800,by=100)))
    #yrange <- c(seq(0,250,by=50),255)
    barwidth=0.2
    plot.title <- paste(band[k],' Band ',tarp[j], '% Panel', sep='')
    n.plots <- length(plotmat[1,])
    plot(plotmat[2,], axes=FALSE,ylim=c(0,255), xlab="",ylab="", cex.main=2)
    mtext(plot.title, side=3, cex=1.5, adj=0.5,0.5)
    #mtext(letters[loopb], side=1, line=-1, adj=0, cex=1, col="grey40")
    if (loopb %in% c(1,4,7,10,13)){
      axis(2, at=seq(0,0.2,by=0.05), cex.axis=2, las=2)
    } 
    if (loopb %in% c(13,14,15)){
      axis(1, at=1:9, labels=xrange, cex.axis=2, las=2) #change at= to 1:9 when doing 30 - 800 ft
    } else{
    }
    box(col="grey60")
    for (i in 1:n.plots) {
      lines(c(i-barwidth,i+barwidth),c(plotmat[1,i],plotmat[1,i]))
      lines(c(i-barwidth,i+barwidth),c(plotmat[3,i],plotmat[3,i]))
      lines(c(i,i),c(plotmat[1,i],plotmat[3,i]))
      
    }
    abline(h=plotmat[2,1], col="blue")
    abline(h=plotmat[1,1],lty= 2, col="red")
    abline(h=plotmat[3,1],lty= 2, col="red")
    #dev.off()
  }
  
}
## COMMENT THE BELOW BACK IN IF YOU WANT IT IN AN ENTIRE SINGLE FILE. 
dev.off()
closeAllConnections()
#END
