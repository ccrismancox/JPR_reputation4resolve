library(matrixStats)
library(ggplot2)
library(reshape2)
library(stringr)

rm(list=ls())

n <- c(250, 500, 1000, 2500, 5000)
bG <- c(-2.7, -.5)
bR <- c( -3.0, .5)
gG <- c(1.5, -.5); gR <- c(1, .5)
truth <- c(bR, bG, (c(gR, gG)))
k <- length(truth)

load("MonteCarloResults_selection.rdata")
MCout <- lapply(MCout, function(x){x[abs(x) > 10] <- NA; return(x[,1:k])})
MCout <- lapply(MCout, na.omit)
bias  <- (abs(do.call(cbind, lapply(MCout, colMeans, na.rm=T))[1:k,]) - abs(truth))
sterr <- (do.call(cbind, lapply(MCout, colSds, na.rm=T))[1:k,] )
MSE <- colMeans((do.call(cbind, lapply(MCout, colMeans, na.rm=T))[1:k,] - truth)^2  + do.call(cbind, lapply(MCout, colVars, na.rm=T))[1:k,] )
out <- data.frame(MSE=MSE, N=n)

print(cbind(truth,do.call(cbind, lapply(MCout, colMeans, na.rm=T))[1:k,] ))
#' Betas are pretty spot on with missing y=0 cases
#' the constants and gamma are a little off, but not bad
#' the covariate gammas are attenuated
print(MSE) 
#' But things look not bad on the MSE front!

