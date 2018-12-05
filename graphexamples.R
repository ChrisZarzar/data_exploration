## basic example
png(file="mygraphic.png",width=400,height=350)
plot(x=rnorm(10),y=rnorm(10),main="example")
dev.off()

## Reduced margins at top and left
set.seed(21721)
png(file="notitle.png",width=400, height=350)
par(mar=c(5,3,2,2)+0.1)
hist(rnorm(100),ylab=NULL,main=NULL)
dev.off()

## Same as above with default margins, for comparison
set.seed(21721)
png(file="notitle-default.png",width=400, height=350)
hist(rnorm(100),ylab=NULL,main=NULL)
dev.off()

png(file="animals45.png",width=400,height=350,res=45)
plot(Animals, log="xy", type="n", main="Animal brain/body size")
text(Animals, lab=row.names(Animals))
dev.off()

png(file="animals72.png",width=400,height=350,res=72)
plot(Animals, log="xy", type="n", main="Animal brain/body size")
text(Animals, lab=row.names(Animals))
dev.off()



require(vcd)
data(CoalMiners) 
l <- oddsratio(CoalMiners)
g <- seq(25, 60, by = 5) 

## graph, with default anti-aliasing (on, on my system)
png(file="coal-antialias-on.png", width=500, height=350)
plot(l, 
     xlab = "Age Group",
     main = "Breathelessness and Wheeze in Coal Miners")
m <- lm(l ~ g + I(g^2)) 
lines(fitted(m), col = "red") 
dev.off()

## this should turn antialiasing off, but doesn't work for me
png(file="coal-antialias-off.png", width=400, height=350, 
  antialias="none")
plot(l, 
     xlab = "Age Group",
     main = "Breathelessness and Wheeze in Coal Miners")
m <- lm(l ~ g + I(g^2)) 
lines(fitted(m), col = "red") 
dev.off()