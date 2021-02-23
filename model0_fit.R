rm(list=ls())
load("startingvalues/model0_startvalues.rdata")
colnames(ZG)[1] <- "const"
start <- deTest$optim$bestmem
names(start) <- c(paste("lambdaR:",colnames(XR), sep=""),
                  paste("lambdaG:",colnames(XG), sep=""),
                  paste("gammaR:",colnames(ZG), sep=""),
                  paste("gammaG:",colnames(ZG), sep=""))

model <- optim(start, llik, control=list(maxit=5000)) 
print(model$convergence)
print(model$value)
output <- list(par=model$par,
               pop=deTest$member$pop,
               llik=model$value)
save(output, file="nullModel.Rdata")
