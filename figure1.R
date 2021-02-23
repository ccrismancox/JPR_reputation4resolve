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

par <- output$par

X.tilde <- rbind(colMeans(XR[XR[,"polity2"] == -6,]), colMeans(XR[XR[,"polity2"] == 6,]))
Z.tilde <- rbind(colMeans(ZG[ZG[,"polity2"] == -6,]), colMeans(ZG[ZG[,"polity2"] == 6,]))


X.tilde <- av.data(XR, "polity2", length.out = 2)
Z.tilde <- av.data(ZG, "polity2", length.out = 2)
X.tilde[,"polity2"] <- c(-6, 6)
Z.tilde[,"polity2"] <- c(-6, 6)

zR <- plogis(Z.tilde %*% par[str_detect(names(par), "gammaR")])
zG <- plogis(Z.tilde %*% par[str_detect(names(par), "gammaG")])
lambdaR <- exp(X.tilde %*% par[str_detect(names(par), "lambdaR")])
lambdaG <- exp(X.tilde %*% par[str_detect(names(par), "lambdaG")])
delta <-( ( log(zR)/lambdaR ) >= ( log(zG)/lambdaG ))

nMonths <- 4
piR <- piG <- matrix(0, ncol=2, nrow=nMonths+1)

for(i in 1:2){
  if(!delta[i]){ #if the rebels are possible first quitter
    piR[,i] <- zR[i]/(1-(1-zR[i]*zG[i]^(-lambdaR[i]/lambdaG[i])*exp(-lambdaR[i] * 0:nMonths)))
    piG[,i] <- zG[i]/(1-(1-exp(-lambdaG[i] *0:nMonths)))
  }else{
    piR[,i] <- zR[i]/(1-(1-exp(-lambdaR[i] *0:nMonths)))
    piG[,i] <- zG[i]/(1-(1-zG[i]*zR[i]^(-lambdaG[i]/lambdaR[i])*exp(-lambdaG[i] * 0:nMonths)))
  }
}
piR[piR>1] <- 1
piG[piG>1] <- 1
priors <- data.frame(pi=c(zG, zR),
                     Actor=c("Government", "Government", "Rebels", "Rebels"),
                     Regime = c("Autocrats (Polity -6)", "Democrats (Polity +6)",
                                "Autocrats (Polity -6)", "Democrats (Polity +6)"),
                     Months = c(0,0,0,0)
                )

plotData <- data.frame(pi= c(c(piG),piR),
                       Actor = rep(c("Government",
                                     "Rebels"), each=(nMonths+1)*2),
                       Regime = rep(c("Autocrats (Polity -6)",
                                      "Democrats (Polity +6)"), each=(nMonths+1)),
                       Months = rep(0:nMonths, 2))
plotData <- rbind(priors,plotData)



piplot2 <- ggplot(plotData[plotData$Actor=="Government",])+
  geom_line(aes(x=Months, y=pi, color=Regime, linetype=Regime), size=3)+
  theme_bw(25)+
  xlab("Months")+
  ylab("Reputation")+
  theme(legend.position="bottom")+
  labs(color="Regime",
       linetype="Regime")+
  guides(color = guide_legend(keywidth=2, override.aes = list(size=2)))
print(piplot2)
ggsave("figure1.pdf", plot=piplot2, width=12, height=6)






