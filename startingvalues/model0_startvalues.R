rm(list=ls())
library(DEoptim)
library(doParallel)
library(doRNG)
library(data.table)
library(matrixStats)

source("../functions/estimationFunctions2018.R")
source("../functions/deoptimWrap.r")
load("../Data/woaData_USEA.rdata")
rwin <- RWIN2 #military defeats are censored
fullData <- cbind.data.frame(ccode, year,caseList, y, RWIN2,X)
fullData <- as.data.table(fullData)  

XR <- XG <- matrix(1, ncol=1, nrow=nrow(fullData))
ZG <- matrix(1, ncol=1, nrow=nrow(fullData))
llik <- function(x0){
  lik(x0, XR=XR, XG=XG, Z=ZG, y=y, rwin=rwin)
}

upper <- rep(10, ncol(XR)*2+2*ncol(ZG))
lower <- -upper
set.seed(1)

outerit <- 1
deTest <- DEoptim(llik, lower, upper,
                  control=DEoptim.control(parallelType=1,
                                          itermax=500000,
                                          trace=F,
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
save(list="deTest", file="model0_currentIT.rdata") 

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
  save(list="deTest", file="model0_currentIT.rdata") 
}
# save.image("model0_startvalues.rdata") #only if regenerating starting values




