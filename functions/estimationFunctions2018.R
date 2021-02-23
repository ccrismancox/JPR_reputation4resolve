#Conditional distributions
f2.given <- function(t, lambda2, z2){
  bound <- 0
  return(ifelse(t>=bound,lambda2*exp(-lambda2 * t)/(1-z2),0))
} 


f1.given <-function(t, lambda2, lambda1, z2, z1){
  bound <-  1/lambda1 * log(z2 ^(-lambda1/lambda2) * z1)
  return( ifelse(t>=bound, lambda1*z1 * z2^(-lambda1/lambda2) * exp(-lambda1 * t), 0)/(1-z1))
}



F2.given <- function(t, lambda2, z2){
  bound <- 0
  return(ifelse(t>=bound,1-exp(-lambda2 * t),0)/(1-z2))
} 


F1.given <-function(t, lambda2, lambda1, z2, z1){
  bound <-  1/lambda1 * log(z2 ^(-lambda1/lambda2) * z1)
  return(ifelse(t>=bound, 1- z1 * z2^(-lambda1/lambda2) * exp(-lambda1 * t), 0)/(1-z1))
}


lik <- function(b, XR, XG, Z, y, rwin){
  bR <- b[1:ncol(XR)]
  bG <- b[(ncol(XR)+1):(ncol(XR)+ncol(XG))]
  
  zR <- plogis(Z %*%  b[(ncol(XR)+ncol(XG)+1):(ncol(XR) + ncol(XG) + ncol(Z))]) #transform into [0,1]
  zG <- plogis(Z %*%  b[(ncol(XR)+ncol(XG)+ncol(Z) + 1):(ncol(XR) + ncol(XG) + 2*ncol(Z))]) #transform into [0,1]
  
  #transform into hazard rate
  lR <- exp(XR %*% bR)
  lG <- exp(XG %*% bG)
  
  delta1 <- ( (log(zR)/lR) >= (log(zG)/lG) ) #is the government the PFQ?
  delta2 <- rwin
  
  # Use info where possible
  delta1[(delta2==0) & (y==0)] <-0 #rebels loses at 0 -- must be PFQ
  delta1[(delta2==1) & (y==0)] <-1 #government loses at 0 -- must be PFQ
  

  reb <- ifelse((delta1==1) & (delta2 >= 1), #Government is PFQ (1) and Rebels don't lose
                (1-zR)*(1-F2.given(y,lR,zR)) + zR,
                ifelse((delta1==0) & (delta2 >= 1), #Rebels are PFG (1) and don't lose
                       (1-zR)*(1-F1.given(y, lG, lR, zG, zR))+zR,
                       ifelse((delta1==1) & (delta2 == 0),
                              (1-zR)*f2.given(y, lR, zR), #Government is PFQ and rebels lose
                              ifelse((delta1==0) & (delta2==0) & (y > 0), #Rebels are PFG and lose at y > 0
                                     (1-zR)*f1.given(y, lG, lR, zG,zR),
                                     (1-zR)*F1.given(y, lG, lR, zG,zR))))) # y = 0

  reb[reb<=0] <- .Machine$double.eps
  reb[is.na(reb)] <- .Machine$double.eps
  reb <- log(reb)

  gov <-  ifelse((delta1==0) & (delta2 != 1),
                (1-zG)* (1-F2.given(y,lG,zG)) +zG,
                 ifelse((delta1==1) & (delta2 != 1),
                        (1-zG)*(1-F1.given(y, lR, lG, zR, zG)) +zG,
                        ifelse((delta1==0) & (delta2 == 1),
                               (1-zG)*f2.given(y, lG,zG),
                               ifelse((delta1==1) & (delta2==1) & (y > 0),
                                      (1-zG)*f1.given(y, lR, lG, zR, zG),
                                      (1-zG)*F1.given(y, lR, lG, zR, zG))))) # y = 0

  gov[gov<=0] <- .Machine$double.eps
  gov[is.na(gov)] <- .Machine$double.eps
  gov <- log(gov)
  LL <- -sum(reb + gov)
  
  return(LL)
}







## Generating data from the inverse of the joints
rg2 <- function(n, l2){
  U <- runif(n)
  r <- -log(1-U)/l2
  return(r)
}

rg1 <- function(n, l2, l1, z2, z1){
  U <- runif(n)
  Z <- z1 * z2^(-l1/l2)
  r <- -log((1-U)/Z)/l1
  return(r)
}




genData <- function(XR, XG, bR, bG, zR, zG, N){
  lR <- exp(XR %*% bR)
  lG <- exp(XG %*% bG)
  delta1 <- ( (log(zR)/lR) >= (log(zG)/lG) ) #is the government the PFQ?

  
  rebCrazy <- rbinom(N, 1, zR)
  govCrazy <- rbinom(N, 1, zG)




  # Draw from the joint distributions  
  reb <- ifelse(delta1,  # is the government player 1?
                    rg2(N, lR),  #if so, the draw from player 2
                       rg1(N, l1=lR, l2=lG, z1=zR, z2=zG)) #otherwise, player1
  gov <- ifelse(delta1, #Is the government player 1
                       rg1(N, l1=lG, l2=lR, z1=zG, z2=zR),
                       rg2(N, lG))



  reb[reb <= 0] <- 0
  gov[gov <= 0] <- 0
  
  y <- apply(cbind(reb, gov), 1, min)  
  rwin <- as.numeric(y == gov)
  
  
  return(list(y=y, rwin=rwin, delta1=delta1))
}




    