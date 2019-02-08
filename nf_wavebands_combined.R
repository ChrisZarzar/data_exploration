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
looptest <- 0
tarp <- c('06', '22', '44')
band <- c('Blue', 'Green', 'Red', 'rEdge','NIR')
band.col <- c(3:7)
alt <- c('30','100','200','300','400','500','600','700','800')
for (j in 1:length(tarp)){
  loopa<-loopa+1
  for(k in 1:length(band.col)){
    if(k==1){
      lncol<-"blue1"
    }
    if(k==2){
      lncol<-"darkgreen"
    }
    if(k==3){
      lncol<-"red"
    }
    if(k==4){
      lncol<-"violetred3"
    }
    if(k==5){
      lncol<-"tan4"
    }    
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
    barwidth=0.2
    plot.title <- paste(tarp[j], '% Panel', sep='')
    n.plots <- length(plotmat[1,])
    if (looptest != loopa){
    plot.new()
    plot.window(xlim=c(1,length(xrange)),ylim=c(0,0.2), xlab="",ylab="", cex.main=1.2)
    mtext(plot.title, side=3, cex=1.5, adj=0.5,0.5)
    #par(bg=NA) #make transparent background 
    #mtext(letters[loopb], side=1, line=-1, adj=0, cex=1, col="grey40")
    lines(plotmat[2,], lty=k, lwd=2, col=lncol) #plot additional waveband median values
    if (loopb %in% c(1,7,13)){
      axis(2, at=seq(0,0.2,by=0.05), cex.axis=2, las=2)
    } 
    if (loopb %in% c(1,7,13)){
      axis(1, at=1:9, labels=xrange, cex.axis=2, las=2) #change at= to 1:9 when doing 30 - 800 ft
    }
    box(col="grey60")
    if (looptest==0){
      legend("topleft", legend=c('Blue', 'Green', 'Red', 'rEdge','NIR'), col=c("blue1","darkgreen","red","violetred3","tan4"),lty=c(1,2,3,4,5), box.lty=0, bg="transparent")
    }
    looptest<-loopa
    }else{
    mtext(plot.title, side=3, cex=1.5, adj=0.5,0.5)
    #par(bg=NA) #make transparent background 
    #mtext(letters[loopb], side=1, line=-1, adj=0, cex=1, col="grey40")
    if (loopb %in% c(1)){
      axis(2, at=seq(0,0.2,by=0.05), cex.axis=2, las=2)
    } 
    if (loopb %in% c(1,7,13)){
      axis(1, at=1:9, labels=xrange, cex.axis=2, las=2) #change at= to 1:9 when doing 30 - 800 ft
    }
    box(col="grey60")
    for (i in 1:n.plots) {
      lines(plotmat[2,], lty=k,lwd=2, col=lncol) #plot additional waveband median values
      
    }
    }
  }
}

## COMMENT THE BELOW BACK IN IF YOU WANT IT IN AN ENTIRE SINGLE FILE. 
#dev.off()
#closeAllConnections()
#END
