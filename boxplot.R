# set multi-panel
par(mfrow=c(1,2))

#Week 1 ----
C1 <- c(11,9.5,6,9.5,11,5)
A1 <- c(12,12,11.5,7.5,11.5,10,11.5,9.5,8.5,9.5)

boxplot(100*c(C1,A1)/12, 
        ylim=c(40,100),
        main="Quiz Grades",
        names = "1",
        ylab="% correct",xlab="Week 1",frame=F,notch=T,col="gold")
stripchart(100*C1/12,col = "blue",pch=19,method = "jitter",vertical = T,add=T,cex=1.5)
stripchart(100*A1/12,col = "darkgreen",pch=19,method = "jitter",vertical = T,add=T,cex=1.5)
points(mean(100*C1/12),col="blue",pch=10,cex=2)
points(mean(100*A1/12),col="darkgreen",pch=10,cex=2)
points(mean(100*c(A1,C1)/12),col="black",pch=10,cex=2)
legend("topleft",title = "Section",legend = c("C","A"),pch=19,col = c("blue","darkgreen"))
abline(h = c(90,80,70,60,50),lty=3,col="lightgray")


#Week 2 ----
C2 <- c(9.5,9,9.5,9.5,7,7.5)
A2 <- c(10.75,8.5,7.5,10.25,8,10.5,9,9.5, 6.5)

boxplot(100*c(C2,A2)/12,ylim=c(40,100),
        #main="Weekly Quiz Grade",
        names = "1",
        ylab="% correct",xlab="Week 2",frame=F,notch=T, col="gold")
stripchart(100*C2/12,col = "blue",pch=19,method = "jitter",vertical = T,add=T,cex=1.5)
stripchart(100*A2/12,col = "darkgreen",pch=19,method = "jitter",vertical = T,add=T,cex=1.5)
points(mean(100*C2/12),col="blue",pch=10,cex=2)
points(mean(100*A2/12),col="darkgreen",pch=10,cex=2)
points(mean(100*c(A2,C2)/12),col="black",pch=10,cex=2)
abline(h = c(90,80,70,60,50),lty=3,col="lightgray")
