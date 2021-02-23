
av.data<-function(x, x1=NULL, length.out=25){ ##New function to set columns in
     ##a matrix or data.frame at their mean (for continuous variables)
     ##or median (dummies)
     ##ARGUMENTS:
     ##x=Data
     ##x1= variable that should vary (if any)
     ##length.out=nrow of output
     
     
     if(class(x)=="data.frame"){
          x<-as.matrix(x) 
     }##convert data.frames into matrices (I just like them better)
     
     newdata<-x ##want dim(newdata)==dim(x)
     for(i in 1:ncol(x)){
          if(all(x[,i] %in% c(0,1))){ ##find dummies
               newdata[,i]<-median(x[,i]) ##set to median
          }else{ ##not a dummy?
               newdata[,i]<-mean(x[,i]) ##Set to mean
          }     
     }
     
     newdata<-newdata[1:length.out,] ##Trim to desired length
     
     if(!is.null(x1)){ ##if user inputs an x1, then
          X1<-seq(min(x[,x1]),  ##Create a sequence from min
                  max(x[,x1]),  ##to max
                  length.out=length.out) ##with desired length
          newdata[,x1]<-X1 ##And put it in the right column
     }
     
     
     
     return(newdata)
}
