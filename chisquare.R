chi.square <- function(dataset,dataset2=NULL,dist.name="norm",n.bins,dist.params=c(0,1)) {

      vec.breaks <- seq(min(dataset),max(dataset),by=(max(dataset)-min(dataset))/n.bins)   
      vec.breaks2 <- vec.breaks
      vec.counts <- hist(dataset,breaks=vec.breaks,plot=F)$counts
      vec.probs <- vec.counts/length(dataset)
      len.data <- length(dataset)
      dist.param.list <- NULL

      ## If comparing a dataset to a particular distribution specified by dist.name
      if(is.null(dataset2)) {

         for (k in 1:length(dist.params))  {
              if (k==1) {
                  dist.param.list <- paste(dist.params[k],sep='')
              } else {
                  dist.param.list <- paste(dist.param.list,",",dist.params[k],sep='')
              }
         }
         random.cmd <- paste('comparison <- r',dist.name,'(10000,',dist.param.list,')',sep='')
         eval(parse(text=random.cmd))

         if (min(comparison) < min(dataset))  ## Fix the end points of the breaks vector
             vec.breaks2[1] <- min(comparison)

         if (max(comparison) > max(dataset))  ## Fix the end points of the breaks vector
             vec.breaks2[n.bins+1] <- max(comparison)

         vec.counts2 <- hist(comparison,breaks=vec.breaks2,plot=F)$counts
         vec.probs2 <- vec.counts2/10000

         chi.square.stat <- sum(((vec.counts-(len.data*vec.probs2))^2)/(len.data*vec.probs2))
         if (is.na(chi.square.stat)) {
             stop("Check your distribution parameters or n.bins, these definitely do not match")
         }
         n.dist.params <- length(dist.params)

     } else {  ## End if for is.null(dataset2)

         ## This section allows you to compare one dataset to another directly, regardless of their distribution

         if (min(dataset2) < min(dataset))
             vec.breaks2[1] <- min(dataset2)
         if (max(dataset2) > max(dataset))
             vec.breaks2[n.bins+1] <- max(dataset2)

         vec.counts2 <- hist(dataset2,breaks=vec.breaks2,plot=F)$counts
         vec.probs2 <- vec.counts2/length(dataset2)

         chi.square.stat <- sum(((vec.counts)-len.data*(vec.probs2)^2)/(len.data*vec.probs2))
 
         if (chi.square.stat==Inf) {
             stop("You have zero probabilities in dataset2.  Your chi-square test won't work on these data.")
         }

         if (is.na(chi.square.stat)) {
             stop("Check your datasets or n.bins; these definitely do not match")
         }
         n.dist.params <- 0

    } ## End else statement for if you have two datasets to compare

    df.val <- n.bins - n.dist.params - 1
    p.value <- pchisq(chi.square.stat,df.val)     
    return(p.value)    


} ## End chi.square test