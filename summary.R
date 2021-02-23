rm(list=ls())
#This is the main model
library(DEoptim)
library(doParallel)
library(doRNG)
library(data.table)
library(matrixStats)

load("Data/woaData_USEA.rdata")
rwin <- RWIN2 #military defeats are censored
fullData <- cbind.data.frame(ccode, year,caseList, y, RWIN2,X)
fullData <- as.data.table(fullData)  

X[,14] <- 1-X[,14] #territory is 1
XR <- XG <- X[,c(1:8,10:11,  14)] 



results <- matrix("0", ncol=5, nrow=ncol(XR))
XR[,1]  <- y

num2str <- function(x){formatC(x, digits=2, format='f')}


meandian <- function(x,na.rm=FALSE){return(ifelse(all(x %in% c(0,1)),
                                                  mean(x, na.rm=na.rm), 
                                                  median(x, na.rm = na.rm)))}

results[,2]<-num2str(apply(XR, 2, quantile, probs=.25, na.rm=TRUE))
results[,3]<-num2str(apply(XR, 2, meandian, na.rm=TRUE))
results[,4]<-num2str(apply(XR, 2, quantile, probs=.75, na.rm=TRUE))

results[,1]<-c("Duration (Months)",
               "GDP per capita (logged)",
               "Polity 2",
               "Population (logged)",
               "Mil. Per. per capita (logged)",
               "Religious Frac.",            
               "Ethnic Frac.",
               "Mountainous terrain (logged)",
               "Coup",
                "Oil",
               "Territorial")

results[, 5] <- c("UCDP",
                  "Penn World Table",
                  "Polity IV",
                  "COW-NMC",
                  "COW-NMC",
                  "Fearon and Laitin (2003)",
                  "Fearon and Laitin (2003)",
                  "Fearon and Laitin (2003)",
                  "Cunningham (2006)",
                  "Fearon and Laitin (2003)",
                  "UCDP")


colnames(results) <- c("{Variable}",
                       "{1st Quartile}", 
                       "{Average}",
                       "{3rd Quartile}",
                       "{Source/Measurement}")
print(results)
