rm(list=ls())
library(DEoptim)
library(doParallel)
library(doRNG)
library(data.table)
library(matrixStats)

source("../functions/estimationFunctions2018.R")
source("../functions/deoptimWrap.r")
load("../Data/woaData_USEA_pastDispute.rdata")
rwin <- RWIN2 #military defeats are censored
fullData <- cbind.data.frame(ccode, year,caseList, y, RWIN2,X)
fullData <- as.data.table(fullData)  


X[,14] <- 1-X[,14] #territory is 1
XR <- XG <- X[,c(1:8,10:11,  14)] 
ZG <- cbind(1, X[, c("polity2", "rgdpna" ,"Incomp", "lmtnest", "ethfrac", "relfrac", "pastConcession")])
llik <- function(x0){
  lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
}

upper <- rep(15, ncol(XR)*2+2*ncol(ZG))
lower <- -upper
set.seed(1)

outerit <- 1
deTest <- DEoptim(llik, lower, upper,
                  control=DEoptim.control(parallelType=1,
                                          itermax=500000,
                                          trace=FALSE,
                                          parVar=c("lik", 
                                                   "XR",
                                                   "XG",
                                                   "y", 
                                                   "ZG",
                                                   "rwin",
                                                   "F1.given",
                                                   "F2.given",
                                                   "f1.given",
                                                   "f2.given"),
                                          steptol=100000))
outerit <- outerit+ 1
save(list="deTest", file="model7start_currentiteration.rdata") 

#Additional iterations as needed
while(nrow(deTest$member$bestmemit)==500000){
  deTest <- DEoptim(llik, lower, upper,
                    control=DEoptim.control(parallelType=1,
                                            itermax=500000,
                                            trace=FALSE,
                                            parVar=c("lik", 
                                                     "XR",
                                                     "XG",
                                                     "y", 
                                                     "ZG",
                                                     "rwin",
                                                     "F1.given",
                                                     "F2.given",
                                                     "f1.given",
                                                     "f2.given"),
                                            initialpop=deTest$member$pop,
                                            steptol=100000))
  outerit <- outerit+ 1
  save(list="deTest", file="model7start_currentiteration.rdata") 
}

# save.image("model7_startvalues.rdata")  #only if regenerating starting values

