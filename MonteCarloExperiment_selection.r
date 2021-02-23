rm(list=ls())

source("functions/estimationFunctions2018.R")
library(matrixStats)
library(doParallel)
library(dfoptim)
library(doRNG)


n <- c(250, 500, 1000, 2500,5000)
bG <- c(-2.7, -.5)
bR <- c( -3.0, .5)
gG <- c(1.5, -.5); gR <- c(1, .5)
truth <- c(bR, bG, (c(gR, gG)))
# out <- matrix(0, nrow=5, ncol=6)
MCout <- list()
for(j in 1:length(n)){
  N <- n[j]
  workers <- makeCluster(32)
  registerDoParallel(workers)
  
  set.seed(10)
  MCout[[j]] <-  foreach(i = 1:250, .combine='rbind', .packages="dfoptim")%dorng%{
    
    XG <- cbind(rep(1, N), runif(N))
    XR <- XG
    lG <- exp(XG %*% bG)
    lR <- exp(XR %*% bR)
    ZG <- cbind(rep(1, N), XG[,2])
    zG <- plogis(ZG %*% gG)
    zR <- plogis(ZG %*% gR)
    dat <- genData(XR, XG, bR, bG, zR, zG, N)
    
    
    
    delta1 <- dat$delta1
    y <- dat$y
    rwin <- dat$rwin
    crazy <- dat$crazy
    summary(cbind(delta1, rwin, y))
    
    q <- quantile(y, prob=1-mean(zG*zR))
    rwin[y > q] <- 2
    y[y > q] <- q
    summary(cbind(delta1, rwin, y)); table(rwin); table(y==0)
    
    unselected <- rbinom(sum(y==0), size=1, p=.5)
    y[y==0][as.logical(unselected)] <- NA
    data <- cbind(XR,XG, ZG, y, rwin)
    data <- na.omit(data)
    XR <- data[,1:2]
    XG <- data[,3:4]
    ZG <- data[,5:6]
    y <- data[,7]
    rwin <- data[,8]
    summary(cbind(rwin, y)); table(rwin); table(y==0)
    
    
    b <- truth
    llik <- function(x0){
      lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
    }
    
    time2 <- system.time(m2 <- optim(b, llik, control=list(maxit=1500)))
    
    if(m2$conv==0){
      doubleZ2 <- c(m2$par, m2$value, time2[3])
    }else{
      doubleZ2 <- rep(NA, 10)
    }
    c(doubleZ2)
  }
  stopCluster(workers)
  cat("done with ",j," of ", length(n), "\n")

}


save(list="MCout", file="MonteCarloResults_selection.rdata")

