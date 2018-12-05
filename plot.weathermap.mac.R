plot.weathermap <- function(x,y,dataset,filled=FALSE,interval=100)  {

## This simple function will plot weather data on a map
## of the world. It will zoom to the particular region 
## you desire and plot the weather data there.  You can
## only plot a single variable in this version, although
## it is possible to plot additional weather data manually.

## Data must be a matrix that has the same structure as the 
## grid of the x,y data, with the longitudes as columns.  
## For example, if you have a 15 longitude by
## 10 latitude grid, you would need a 10X15 data matrix to use as
## input.

## levels is a vector of desired contour levels that the user can input.  
## Without it specified, the function will use the default values.

## Check to be sure the dimensions are correct.
error=FALSE
if (length(x) != dim(dataset)[2]) {error = TRUE}
if (length(y) != dim(dataset)[1]) {error = TRUE}

if (error) { ## Check for errors in the data dimensions.
    print("The dimensions are incorrect.  You may want to try the transpose.")
} else {


    key.axis <- pretty(range(dataset),round((max(dataset)-min(dataset))/interval))
 #   key.axis <- key.axis[-length(key.axis)] 
  #  key.axis <- key.axis[-1]

    ## Set up window margins
    marg.1 <- (range(y)[2]-range(y)[1])/9
    marg.2 <- (range(x)[2]-range(x)[1])/14.75
    quartz(title="",marg.2,marg.1)
    par(mai=c(1.1,1.1,1.1,1.1))

    if (filled==TRUE) { ## shaded is true
        rgb.palette <- colorRampPalette(c("blue", "green", "orange","red"),space = "rgb")
        filled.contour(x,y,t(dataset),plot.axes={axis(1);axis(2);map('world',add=T)})
        try(map('state',projection='rectangular',parameters=0,add=TRUE),silent=T)

    } else { ## end if shaded is true
        rgb.palette <- colorRampPalette(c("blue", "green","orange", "red"),space = "rgb")
        contour(x,y,t(dataset),levels=key.axis,labcex=0.75,col=rgb.palette(length(key.axis)),xlab="Longitude (E)",ylab="Latitude (N)",)
        map(database='world',xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),add=TRUE)
        try(map(database='state',xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),add=TRUE),silent=T)
    } ## End the else statement

} ## End the error checking else statement



} ## End the function
