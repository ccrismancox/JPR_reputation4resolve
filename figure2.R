library(ggplot2)
library(gridExtra)
library(matrixStats)
library(data.table)
library(stringr)
rm(list=ls())
source("functions/avdata.r")
source("functions/estimationFunctions2018.R")


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



load("model2_fitted.Rdata") 
load("model2_bootstrap.rdata")
par <- output$par
bootMat <- bootMat[bootMat[,"conv"]==0,]
bootMat <- bootMat[,1:length(par)]
rebBS <- bootMat[, str_detect(names(par), "lambdaR")]
govBS <- bootMat[, str_detect(names(par), "lambdaG")]
gammaR <- bootMat[,str_detect(names(par), "gammaR")]
gammaG <- bootMat[,str_detect(names(par), "gammaG")]
Z <- cbind(1, X[, c("polity2", "rgdpna", "Incomp" ,"lmtnest", "ethfrac", "relfrac")])
set.seed(1)



X.tilde <- av.data(XR, x1="polity2", length.out=25)
Z.tilde <- av.data(Z, x1="polity2", length.out=25)
lambdaR <- t(exp(X.tilde %*% t(rebBS)))
lambdaG <- t(exp(X.tilde %*% t(govBS)))

zG <- t(plogis(Z.tilde %*% t(gammaG)))
zR <- t(plogis(Z.tilde %*% t(gammaR)))

delta <-( ( log(zR)/lambdaR ) >= ( log(zG)/lambdaG))
yG <- yR <- yHat <- rwin <- Gwin <-  matrix(0, nrow=nrow(bootMat), ncol=25)
for(i in 1:nrow(bootMat)){
  for(j in 1:25){
    
    
    R2G1 <- rg2(10000,lambdaR[i,j])
    R2G1[R2G1 < 0 ] <- 0
    R1G2 <- rg1(10000, l1=lambdaR[i,j], l2=lambdaG[i,j], z1=zR[i,j], z2=zG[i,j])
    R1G2[R1G2 < 0 ] <- 0
    G2R1 <- rg2(10000, l2=lambdaG[i,j])
    G2R1[G2R1 < 0] <- 0
    G1R2 <- rg1(10000, l2=lambdaR[i,j], l1=lambdaG[i,j], z2=zR[i,j], z1=zG[i,j])
    G1R2[G1R2 < 0] <- 0
    
    
    yG[i,j] <- ifelse(delta[i,j], #if gov is PFQ
                      median(G1R2),
                      median(G2R1))
    yR[i,j] <- ifelse(delta[i,j], #if gov is PFQ
                      median(R2G1),
                      median(R1G2))
    yHat[i,j] <- ifelse(delta[i,j],#if gov is PFQ
                        median(rowMins(cbind(G1R2, R2G1))),
                        median(rowMins(cbind(G2R1, R1G2))))
    
    rwin[i,j] <- ifelse(delta[i,j],
                        mean( (rowMins(cbind(G1R2, R2G1)) == G1R2)  & (rowMins(cbind(G1R2, R2G1)) < Inf) ),
                        mean( (rowMins(cbind(G2R1, R1G2)) == G2R1)  & (rowMins(cbind(G2R1, R1G2)) < Inf) ))
    Gwin[i,j] <- ifelse(delta[i,j],
                        mean( (rowMins(cbind(G1R2, R2G1)) == R2G1)  & (rowMins(cbind(G1R2, R2G1)) < Inf) ),
                        mean( (rowMins(cbind(G2R1, R1G2)) == R1G2)  & (rowMins(cbind(G2R1, R1G2)) < Inf) ))
    
    
    
  }
}

plot.df<- rbind.data.frame(colQuantiles(yG, probs=c(0.05,.5,.95)), 
                           colQuantiles(yR, probs=c(0.05,.5,.95)))
plot.df2 <- data.frame(colQuantiles(yHat, probs=c(0.05,.5,.95)))
plot.dfRW <-  data.frame(colQuantiles(rwin, probs=c(0.05,.5,.95)))
plot.dfGW <-  data.frame(colQuantiles(Gwin, probs=c(0.05,.5,.95)))


colnames(plot.df) <- colnames(plot.df2) <- colnames(plot.dfRW) <-  colnames(plot.dfGW) <-c("lo", "fit",  "hi")
plot.df$polity2 <- plot.df2$polity2 <- plot.dfRW$polity2 <- X.tilde[,"polity2"]
plot.df$effect <- rep(c("Gov't", "Rebel"), each=25)



gout <- ggplot(plot.dfRW)+
  geom_ribbon(aes(x=polity2, ymin=lo, ymax=hi), alpha=.2)+
  geom_line(aes(x=polity2, y=fit))+
  theme_bw(22)+
  xlab("Democracy (polity)")+
  ylab("Pr. of concessions")
ggsave("figure2.pdf", plot=gout, width=8, height=6)
