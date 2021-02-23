rm(list=ls())
library(matrixStats)
library(data.table)
library(stringr)

source("functions/estimationFunctions2018.R")
load("model1_fitted.Rdata")
load("Data/woaData_USEA.rdata")
rwin <- RWIN2 #military defeats are censored
fullData <- cbind.data.frame(ccode, year,caseList, y, RWIN2,X)
fullData <- as.data.table(fullData)  


X[,14] <- 1-X[,14] #territory is 1
XR <- XG <- X[,c(1:8,10:11,  14)] #SOS and Incomp might well have been colinear
ZG <- matrix(1, ncol=1, nrow=nrow(XR))
llik <- function(x0){
  lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
}


par <- output$par


#############
set.seed(1)
bootIT <- 500
bootMat <- matrix(0, nrow=bootIT, ncol=length(par)+1)   
colnames(bootMat) <- c(names(par), "conv")

for(i in 1:bootIT){
  
  subsample <- sample(1:nrow(XR), replace=T)
  
  llik2 <- function(x0){
    lik(x0, XR=XR[subsample,], XG=XG[subsample,], Z=ZG[subsample,,drop=F],
        y=y[subsample,,drop=F], rwin=rwin[subsample,,drop=F])
  }
  out <- optim(par, llik2, control=list(maxit=50000))
  out <- c(out$par, out$convergence)
  
  bootMat[i,] <- out

}

# Table 1 model 1
cat("Table I: Model 1\n")
print(round(cbind(par[str_detect(colnames(bootMat),"gamma")],
                  colQuantiles(bootMat[bootMat[,"conv"]==0,
                                       str_detect(colnames(bootMat),"gamma")], 
                               probs=c(.025, .975))),2))
round(output$llik,2)
nrow(XR) # N


# Table D.1
cat("Table D.1: Model 1\n")
print(round(cbind(par[str_detect(colnames(bootMat),"lambda")],
                  colQuantiles(bootMat[bootMat[,"conv"]==0,
                                       str_detect(colnames(bootMat),"lambda")], 
                               probs=c(.025, .975))),2))

round(output$llik,2)
nrow(XR) # N

cat("a one tailed test on if zG > zR (reported in text)\n")
1-mean((bootMat[bootMat[,"conv"]==0, 24] - bootMat[bootMat[,"conv"]==0, 23]) >0)
save(bootMat, file="model1_bootstrap.rdata")
