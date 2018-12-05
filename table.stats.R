table.stats <- function(contable) {  ## This function requires observations and
                                    ## predictions, using 1s and 0s as your binary
								 ## operators.

#contable <- con.table(y,y.hat)  ## This is a separate function required by table.stats

aa <- contable[1,1]
bb <- contable[1,2]
cc <- contable[2,1]
dd <- contable[2,2]

## We should create a vector of labels so we know what we are viewing.

labels <- c("PC","CSI","BIAS","FAR","POD","POFD","HSS","TSS")

## Now, lets compute each of these contingency stats.

PC <- (aa + dd) / sum(aa,bb,cc,dd)  ## Compute the percent correct
CSI <- (aa) / (aa+bb+cc)     ## Compute the critical success index
BIAS <- (aa + bb)/(aa+cc)	   ## Compute the bias of the data
FAR <- bb/(aa+bb)			   ## Compute the false alarm ratio
POD <- aa/(aa+cc)            ## Compute the probability of detection
POFD <- bb/(bb+dd) 	     ## Compute the probability of false detection
HSS <- (2*(aa*dd-bb*cc))/((aa+cc)*(cc+dd)+(aa+bb)*(bb+dd))
TSS <- (aa*dd-bb*cc)/((aa+cc)*(bb+dd))

## We can combine all of these into two separate vectors and then output 
## our results.

contin.stats <- c(PC,CSI,BIAS,FAR,POD,POFD,HSS,TSS)
output <- data.frame(cbind(labels,contin.stats))

return(output)

} ## End the function