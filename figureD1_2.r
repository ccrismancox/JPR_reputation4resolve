library(ggplot2)
library(gridExtra)
library(grid)
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



load("model1_fitted.Rdata") 
load("model1_bootstrap.rdata") 
par <- output$par
colnames(bootMat) <- c(names(par), "conv")
bootMat <- bootMat[bootMat[,"conv"]==0,]
bootMat <- bootMat[,1:length(par)]
rebBS <- bootMat[, str_detect(names(par), "lambdaR")]
govBS <- bootMat[, str_detect(names(par), "lambdaG")]
gammaR <- bootMat[,str_detect(names(par), "gammaR")]
gammaG <- bootMat[,str_detect(names(par), "gammaG")]
Z <- matrix(1, nrow=400, ncol=1)
set.seed(1)




#########Z################

X.tilde <- av.data(XR,  length.out=1)
lambdaR <- drop(exp(X.tilde %*% colMeans(rebBS)))
lambdaG <- drop(exp(X.tilde %*% colMeans(govBS)))


zseq <- seq(.90, .99, length=25)
Znew <- expand.grid(zseq, zseq) #[zR, zG]

draws <- 500000

delta <- ( (1/lambdaR * log(Znew[,1])) >= (1/lambdaG  * log(Znew[,2])))
yG <- yR <- yHat <- yHatCrazy <- rwin <- matrix(0, nrow=25, ncol=25)
for(i in 1:625){
  
  
  
  R2G1 <- rg2(draws,lambdaR)
  R2G1[R2G1 < 0 ] <- 0
  R1G2 <- rg1(draws, l1=lambdaR, l2=lambdaG, z1=Znew[i,1], z2=Znew[i,2])
  R1G2[R1G2 < 0 ] <- 0
  G2R1 <- rg2(draws, l2=lambdaG)
  G2R1[G2R1 < 0] <- 0
  G1R2 <- rg1(draws, l2=lambdaR, l1=lambdaG, z2=Znew[i,1], z1=Znew[i,2])
  G1R2[G1R2 < 0] <- 0
  
  yG[i] <- ifelse(delta[i], #if gov is PFQ
                  median(G1R2),
                  median(G2R1))
  yR[i] <- ifelse(delta[i], #if gov is PFQ
                  median(R2G1),
                  median(R1G2))
  yHat[i] <- ifelse(delta[i],#if gov is PFQ
                    median(rowMins(cbind(G1R2, R2G1))),
                    median(rowMins(cbind(G2R1, R1G2))))
  
  rwin[i] <- ifelse(delta[i],
                    mean( (rowMins(cbind(G1R2, R2G1)) == G1R2)  ),
                    mean( (rowMins(cbind(G2R1, R1G2)) == G2R1) ))
  
  
}







#### Figure D2 
####

#fix zR; Z= [zR, zG]; each column in Y holds zG constant
fixZR <- data.frame(yHat = c(yHat[1,],
                             yHat[25,]),
                    rwin=c(rwin[1,],
                           rwin[25,]),
                    zG = c(Znew[Znew[,1]==zseq[1],2],
                           Znew[Znew[,1]==zseq[25],2]),
                    zR = factor(rep(c("Low (0.90)",  "High (0.99)"), each=25),
                                levels=c("Low (0.90)",  "High (0.99)")))






fixZG <- data.frame(yG = c(yG[,1],
                           yG[,25]),
                    yR = c(yR[,10],
                           yR[,25]),
                    yHat = c(yHat[,1],
                             yHat[,25]),
                    rwin=c(rwin[,1],
                           rwin[,25]),
                    zR = c(Znew[Znew[,2]==zseq[1],1],
                           Znew[Znew[,2]==zseq[25],1]),
                    zG =factor(rep(c("Low (0.90)",  "High (0.99)"), each=25),
                               levels=c("Low (0.90)",  "High (0.99)")))            
rwinFixZr <- ggplot()+
  geom_line(data=fixZR, aes(y=rwin, x=zG, linetype=zR, color=zR), size=2)+
  theme_bw(25)+
  xlab("Government Reputation")+
  ylab("Pr. Rebel Victory")+
  theme(legend.position="bottom")+
  labs(color="Rebel Reputation", linetype="Rebel Reputation")+
  guides(color = guide_legend(keywidth=2, override.aes = list(size=2)))
print(rwinFixZr)
ggsave("figureD2.pdf", plot=rwinFixZr, width=8, height=6)


#### pi (Figure D1)####
#Define the CDF fu nctions
F2 <- function(t, lambda2){ 
  return(ifelse(t>=0,1-exp(-lambda2 * t),0))
}

F1 <- function(t, lambda2, lambda1, z2, z1){
  bound <-  1/lambda1 * log(z2 ^(-lambda1/lambda2) * z1)
  return( ifelse(t>=bound, 1- z1 * z2^(-lambda1/lambda2) * exp(-lambda1 * t), 0))
}

X.tilde <- av.data(XR,  length.out=1)
lambdaR <- drop(exp(X.tilde %*% colMeans(rebBS)))
lambdaG <- drop(exp(X.tilde %*% colMeans(govBS)))
zR <- plogis(mean(gammaR))
zG <- plogis(mean(gammaG))

delta <- ( (log(zR)/lambdaR ) >= (log(zG)/lambdaG)) #Is the gov't the PFQ

if(delta){  #rebels are player 2
  piR <- zR/(1-ifelse(F2(seq(0, 12, length=25), drop(lambdaR)) < 1-zR,
                      F2(seq(0, 12, length=25), drop(lambdaR)),
                      1-zR))
  piG<- zG/(1-ifelse(F1(seq(0, 12, length=25),lambda1 =  drop(lambdaG), lambda2=drop(lambdaR), z1=zG, z2=zR) < 1-zG,
                     F1(seq(0, 12, length=25),lambda1 =  drop(lambdaG), lambda2=drop(lambdaR), z1=zG, z2=zR),
                     1-zG))
}else{
  piG <- zG/(1-ifelse(F2(seq(0, 12, length=25), drop(lambdaG)) < 1-zG,
                      F2(seq(0, 12, length=25), drop(lambdaG)),
                      1-zG))
  piR<- zR/(1-ifelse(F1(seq(0, 12, length=25),lambda2 =  drop(lambdaG), lambda1=drop(lambdaR), z2=zG, z1=zR) < 1-zR,
                     F1(seq(0, 12, length=25),lambda2 =  drop(lambdaG), lambda1=drop(lambdaR), z2=zG, z1=zR),
                     1-zR))
}


priors <- data.frame(pi=c(zR, zG),
                     Actor=c("Rebel", "Gov't"),
                     Years = c(0,0)
)



plotData <- data.frame(pi= c(piR, piG),
                       Actor = rep(c("Rebel", "Gov't"), each=25),
                       Years = seq(0, 12, length=25))
plotData <- rbind(priors, plotData)

piplot <- ggplot(plotData)+
  geom_line(aes(x=Years, y=pi, color=Actor, linetype=Actor), size=3)+
  theme_bw(25)+
  xlab("Months")+
  ylab("Reputation")+
  theme(legend.position="bottom")+
  guides(color = guide_legend(keywidth=2, override.aes = list(size=2)))
print(piplot)
ggsave("figureD1.pdf", plot=piplot, width=12, height=6)




