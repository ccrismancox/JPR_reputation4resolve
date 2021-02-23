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

load("MonteCarloResults.rdata")
MCout <- lapply(MCout, function(x){x[abs(x) > 10] <- NA; return(x[,1:k])})
MCout <- lapply(MCout, na.omit)
bias  <- (abs(do.call(cbind, lapply(MCout, colMeans, na.rm=T))[1:k,]) - abs(truth))
sterr <- (do.call(cbind, lapply(MCout, colSds, na.rm=T))[1:k,] )
MSE <- colMeans((do.call(cbind, lapply(MCout, colMeans, na.rm=T))[1:k,] - truth)^2  + do.call(cbind, lapply(MCout, colVars, na.rm=T))[1:k,] )
out <- data.frame(MSE=MSE, N=n)

print(cbind(truth,do.call(cbind, lapply(MCout, colMeans, na.rm=T))[1:k,] ))
#' Everything looks pretty good

MCplot <- ggplot(out)+
  geom_line(aes(y=MSE, x=N), size=1)+
  geom_point(aes(y=MSE, x=N), size=5)+
  theme_bw(20)+
  ggtitle("Monte Carlo Results")+
  ylab("Mean Squared Error")+
  theme(legend.position="bottom",
        legend.title = element_text(size=18, 
                                    face="bold"),
        legend.text = element_text(size = 18))
print(MCplot)

ggsave(file="MCresult_main.pdf", MCplot, height=6, width=10)

