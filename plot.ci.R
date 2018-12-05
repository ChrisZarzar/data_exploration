plot.ci <- function(x,barwidth=0.1,xaxis=NULL, yaxis=NULL, min.x=NULL,max.x=NULL,min.y=NULL,max.y=NULL,xlabel=NULL,ylabel=NULL,txtsz=NULL) {
## This function expects a matrix with 3 rows and a number
## of columns corresponding to the number of values you're comparing
## The function assumes the first row is the lower limit, the middle
## row is the median, and the top row is the upper limit
if (barwidth > 0.5) {
warning("Warning:  Error bars will overlap unless you reduce barwidth")
}

n.plots <- length(x[1,])

## Determine maximum and minimum values of x for the plotting routine.
## The function figures this out if it is not specified.

if (is.null(min.y) && is.null(max.y)) {
    min.y <- min(x)-2*sd(x[2,])
    max.y <- max(x)+2*sd(x[2,])
}

plot(x[2,],xaxt=xaxis, yaxt=yaxis, xlab=xlabel,ylab=ylabel,xlim=c(min.x,max.x),ylim=c(min.y,max.y),cex.axis=txtsz,cex.lab=txtsz,pch=16)

for (i in 1:n.plots) {
     lines(c(i-barwidth,i+barwidth),c(x[1,i],x[1,i]))
     lines(c(i-barwidth,i+barwidth),c(x[3,i],x[3,i]))
     lines(c(i,i),c(x[1,i],x[3,i]))

}

} ## End function plot.ci