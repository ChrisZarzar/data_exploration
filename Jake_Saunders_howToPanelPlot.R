library(datasets)
DNase

pdf(file = "NameOfMy.pdf", width = 8.5, height = 14)
par(mfrow=c(5,3),mar=c(2,2,1,1),oma=c(3,3,0,0))

#row 1----
plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

#row 2----
plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

#row 3----
plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)
#row 4----
plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)
#row 5----
plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

plot(DNase$density ~ DNase$conc, 
     main="This is the Title of my Plot",cex.main=1,
     xlab="",ylab=""
)

# common axis labels
mtext(text="A common x-axis label",side=1,line=0,outer=TRUE)
mtext(text="A common y-axis label",side=2,line=0,outer=TRUE)

#don't forget this if making a pdf
dev.off()