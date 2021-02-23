rm(list=ls())
library(DEoptim)
library(matrixStats)
library(doParallel)
library(doRNG)
library(data.table)
library(stringr)

source("functions/estimationFunctions2018.R")
load("model2_fitted.Rdata")
load("Data/woaData_USEA.rdata")
rwin <- RWIN2 #military defeats are censored
fullData <- cbind.data.frame(ccode, year,caseList, y, RWIN2,X)
fullData <- as.data.table(fullData)  


X[,14] <- 1-X[,14] #territory is 1
XR <- XG <- X[,c(1:8,10:11,  14)] 
ZG <- cbind(1, X[, c("polity2", "rgdpna", "Incomp" ,"lmtnest", "ethfrac", "relfrac")])
llik <- function(x0){
  lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
}


upper <- rep(10, ncol(XR)*2+2*ncol(ZG))
lower <- -upper

par <- output$par
pop <- output$pop

#############
set.seed(1)
bootIT <- 500
bootMat <- matrix(0, nrow=bootIT, ncol=length(par)+2)   
colnames(bootMat) <- c(names(par), "conv", "logLik")
for(i in 1:bootIT){

  subsample <- sample(1:nrow(XR), replace=T)

  llik2 <- function(x0){
    lik(x0, XR=XR[subsample,], XG=XG[subsample,], Z=ZG[subsample,,drop=F],
        y=y[subsample,,drop=F], rwin=rwin[subsample,,drop=F])
  }



  
  out <- optim(par, llik2, control=list(maxit=25000))
  out <- c(out$par, out$convergence, out$value)
  bootMat[i,] <- out
}

# Table 1 model 2
cat("Table I: Model 2\n")

print(round(cbind(par[str_detect(colnames(bootMat),"gamma")],
                  colQuantiles(bootMat[bootMat[,"conv"]==0,
                                       str_detect(colnames(bootMat),"gamma")], 
                               probs=c(.025, .975))),2))
round(output$llik,2)
nrow(XR) # N


save(bootMat, file="model2_bootstrap.rdata")
