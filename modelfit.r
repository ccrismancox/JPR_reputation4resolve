library(data.table)
library(zoo)
library(chron)
library(foreign)
library(survival)
library(pracma)
library(numDeriv)
library(matrixStats)
library(xtable)


rm(list=ls())
source("functions/estimationFunctions2018.R")
num2str <- function(x){formatC(x, digits=2, format='f')}
set.seed(1)

load("Data/woaData_USEA.rdata")
rwin <- RWIN2 #military defeats are censored
fullData <- cbind.data.frame(ccode, year,caseList, y, RWIN2,X)
fullData <- as.data.table(fullData)  


# Null setup
load("nullModel.Rdata")
nullMod <- output
XR <- XG <- matrix(1, ncol=1, nrow=nrow(fullData))
ZG <- matrix(1, ncol=1, nrow=nrow(fullData))
llik <- function(x0){
  lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
}

LLnull <- -llik(nullMod$par)

#main setup
load("model2_fitted.Rdata")
mainMod <- output
X[,14] <- 1-X[,14] #territory is 1
XR <- XG <- X[,c(1:8,10:11,  14)] #SOS and Incomp might well have been colinear
ZG <- cbind(1, X[, c("polity2", "rgdpna", "Incomp" ,"lmtnest", "ethfrac", "relfrac")])
llik <- function(x0){
  lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
}
LLmain <- -llik(mainMod$par)



# fearon <- fread("Data/fearon.csv")
OS <- fread("Data/fearonOutOfSample.csv")
nmc <- fread("Data/NMC_v4_0.csv")
nmc <- nmc[,list(ccode, year, milper)]
nmc[milper==-9, milper := NA]
nmc[, milper := log(milper)]
fl <- data.table(read.dta("Data/FearonLaitin.dta"))
fl <- fl[, list(ccode, 
                Oil,
                relfrac)]
fl[, Oil := median(Oil), by=ccode]
fl  <- unique(fl)


elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

OS[, startMon := as.yearmon(startMon, "%m/%d/%Y")]
OS[, endMon  := as.yearmon(endMon, "%m/%d/%Y")]
OS[, duration := elapsed_months(endMon, startMon)]
OS[, rgdpna := log(exp(lgdpen)*100/exp(lpop))]
OS[, startYear:= as.integer(as.character(years(startMon)))]
setnames(nmc, 'year', 'startYear')
OS <- merge(OS, nmc, by=c("ccode", 'startYear'), all.x=T, all.y=F)
OS[is.infinite(milper), milper:=NA]
OS <- merge(OS, fl, by=c("ccode"), all.x=T, all.y = F)
setnames(OS, "lpop", "tpop")

####Models####
#Run weibull model on my data
m1 <- survreg(formula = Surv(durationMons+1,I(VicSide3!=2)) ~ rgdpna + polity2 + tpop+milper+relfrac
              +ethfrac+lmtnest+coup+Oil+Incomp, data = fullData, dist="weibull", 
              control=survreg.control(maxiter=50))


m2 <- coxph(formula = Surv(durationMons+1, I(VicSide3!=2)) ~ rgdpna + polity2 + tpop+milper+relfrac
            +ethfrac+lmtnest+coup+Oil+Incomp, data = fullData,ties = 'breslow')




####Predictions####

#Out-of-sample on my model

OS <- na.omit(OS)
XG.OS <- XR.OS <- with(OS, cbind(1, rgdpna, polity2, tpop, milper, relfrac, ethfrac, lmtnest,  coup, Oil,Incomp))
Z.OS <- with(OS, cbind(1, polity2, rgdpna, Incomp, lmtnest, ethfrac, relfrac))
par <- mainMod$par

lambdaR <- exp(XR.OS %*% par[1:ncol(XR)])
lambdaG <- exp(XG.OS %*% par[(ncol(XR)+1):(ncol(XR)*2)])
zR=plogis(Z.OS %*% par[(ncol(XR.OS)*2 + 1):(ncol(XR.OS)*2 + ncol(Z.OS))])
zG=plogis(Z.OS %*% par[(ncol(XR.OS)*2 + ncol(Z.OS)+1):(ncol(XR.OS)*2 + 2*ncol(Z.OS))])
delta <-( (1/lambdaR * log(zR)) >= (1/lambdaG)  * log(zG))
yG <- yR <- yHat <- RWIN <- matrix(0, nrow=nrow(XG.OS), ncol=1)


for(i in 1:nrow(XG.OS)){
  
  
  
  R2G1 <- rg2(10000,lambdaR[i])
  R2G1[R2G1 < 0 ] <- 0
  R1G2 <- rg1(10000, l1=lambdaR[i], l2=lambdaG[i], z1=zR[i], z2=zG[i])
  R1G2[R1G2 < 0 ] <- 0
  G2R1 <- rg2(10000, l2=lambdaG[i])
  G2R1[G2R1 < 0] <- 0
  G1R2 <- rg1(10000, l2=lambdaR[i], l1=lambdaG[i], z2=zR[i], z1=zG[i])
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
  
  RWIN[i] <- ifelse(delta[i],
                    mean( (rowMins(cbind(G1R2, R2G1)) == G1R2)  & (rowMins(cbind(G1R2, R2G1)) < Inf) ),
                    mean( (rowMins(cbind(G2R1, R1G2)) == G2R1)  & (rowMins(cbind(G2R1, R1G2)) < Inf) ))
  
  
  
}

#Out-of sample my weibull
weiOS <- predict(m1, newdata=OS, type="response")
coxOS <- summary(survfit(m2, newdata=OS))$table[,'median']

RMSE.sd.OS <- sqrt(mean((yHat - OS$duration)^2, na.rm=T))
RMSE.wei.OS <- sqrt(mean((weiOS - OS$duration-1)^2, na.rm=T))
RMSE.cox.OS <- sqrt(mean((coxOS - OS$duration-1)^2, na.rm=T))




####Comparisons####
##R squared on main model (Cox and Snell and McFadden)
rsqCSmain <- 1-exp( 2*(LLnull-LLmain)/nrow(X)  )
maxCSmain <- 1-exp( 2*(LLnull)/nrow(X)) 

##R squared on Weibull
rsqCSweibull <- 1-exp( 2*(m1$loglik[1]-m1$loglik[2])/nrow(X)  )
maxCSweibull <- 1-exp( 2*(m1$loglik[1])/nrow(X)) 

##R squared on Cox
rsqCScox <- 1-exp( 2*(m2$loglik[1]-m2$loglik[2])/nrow(X)  )
maxCScox <- 1-exp( 2*(m2$loglik[1])/nrow(X)) 




XG <- XR <- with(fullData, cbind(1, rgdpna, polity2, tpop, milper, relfrac, ethfrac, lmtnest,  coup, Oil,Incomp))

lambdaR <- exp(XR %*% par[1:ncol(XR)])
lambdaG <- exp(XG %*% par[(ncol(XR)+1):(ncol(XR)*2)])
zR=plogis(ZG %*% par[(ncol(XR.OS)*2 + 1):(ncol(XR.OS)*2 + ncol(Z.OS))])
zG=plogis(ZG %*% par[(ncol(XR.OS)*2 + ncol(Z.OS)+1):(ncol(XR.OS)*2 + 2*ncol(Z.OS))])
delta <-( (1/lambdaR * log(zR)) >= (1/lambdaG)  * log(zG))
yG <- yR <- yHat <- RWIN <- matrix(0, nrow=nrow(XG), ncol=1)


for(i in 1:nrow(XG)){
  R2G1 <- rg2(10000,lambdaR[i])
  R2G1[R2G1 < 0 ] <- 0
  R1G2 <- rg1(10000, l1=lambdaR[i], l2=lambdaG[i], z1=zR[i], z2=zG[i])
  R1G2[R1G2 < 0 ] <- 0
  G2R1 <- rg2(10000, l2=lambdaG[i])
  G2R1[G2R1 < 0] <- 0
  G1R2 <- rg1(10000, l2=lambdaR[i], l1=lambdaG[i], z2=zR[i], z1=zG[i])
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
  
  RWIN[i] <- ifelse(delta[i],
                    mean( (rowMins(cbind(G1R2, R2G1)) == G1R2)  & (rowMins(cbind(G1R2, R2G1)) < Inf) ),
                    mean( (rowMins(cbind(G2R1, R1G2)) == G2R1)  & (rowMins(cbind(G2R1, R1G2)) < Inf) ))
  
  
  
  
}



#victory

rwinLogit <- glm(VicSide3 ~ rgdpna + polity2 + tpop+milper+
                   relfrac+ethfrac+lmtnest+sos+coup+Oil+Incomp, data = fullData, 
                 subset=VicSide3!=2, family=binomial)
PR <- predict(rwinLogit, newdata = fullData,type="response")

probs <- data.frame(PR=PR, RWIN=RWIN, VicSide3=fullData$VicSide3)
probs <- na.omit(probs)


sd.probs <- with(probs, (sum(RWIN[VicSide3==1]) + sum((1-RWIN)[VicSide3==0]))/nrow(probs))
logit.probs <- with(probs, (sum(PR[VicSide3==1]) + sum((1-PR)[VicSide3==0]))/nrow(probs))


PWei <- predict(m1, type="response") #in sample weibull
coxPred <- summary(survfit(m2, newdata=fullData))$table[,'median'] #in sample cox



RMSE.sd <- sqrt(mean((yHat[fullData$VicSide3!=2] - fullData$durationMons[fullData$VicSide3!=2])^2, na.rm=T))
RMSE.wei <- sqrt(mean((PWei[fullData$VicSide3!=2]  - fullData$durationMons[fullData$VicSide3!=2]-1)^2, na.rm=T))
RMSE.cox <- sqrt(mean((coxPred[fullData$VicSide3!=2]  - fullData$durationMons[fullData$VicSide3!=2]-1)^2, na.rm=T))






fitOut <- rbind(c(rsqCSmain, rsqCSweibull, rsqCScox, NA),
                c(RMSE.sd, RMSE.wei, RMSE.cox, NA),
                c(RMSE.sd.OS, RMSE.wei.OS, RMSE.cox.OS, NA),
                c(sd.probs, NA, NA, logit.probs))



rownames(fitOut) <- c("Cox-Snell $R^2$",
                      "RMSE: In-sample",
                      "RMSE: Out-of-sample",
                      "Expectation: % Correctly Predicted")

colnames(fitOut) <- c("Strategic Duration",
                      "Standard Weibull",
                      "Standard Cox",
                      "Standard Logit")
fitOut <- apply(fitOut, 2,num2str)
fitOut[fitOut==" NA"] <- "--"
cat("Table F.1\n")
print(fitOut)





## Constraint on T 
cat("What percentage is satisfied at 1 x 10^{-5}?\n")
print(
mean(abs(cbind((1-delta)*(-1/lambdaG * log(zG)) + (delta)*((-1/lambdaG * log(zG/(1-(1-zG*zR^(-lambdaG/lambdaR))))))-
                 (delta)*(-1/lambdaR * log(zR)) + (1-delta)*((-1/lambdaR * log(zR/(1-(1-zR*zG^(-lambdaR/lambdaG))))))
)) < 1e-5)
)
cat("What is the average violation constraint violation?\n")
print(
mean(cbind((1-delta)*(-1/lambdaG * log(zG)) + (delta)*((-1/lambdaG * log(zG/(1-(1-zG*zR^(-lambdaG/lambdaR))))))-
               (delta)*(-1/lambdaR * log(zR)) + (1-delta)*((-1/lambdaR * log(zR/(1-(1-zR*zG^(-lambdaR/lambdaG))))))
))
)
