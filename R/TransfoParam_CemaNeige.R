TransfoParam_CemaNeige <- function(ParamIn,Direction){

  NParam <- 2
  Bool <- is.matrix(ParamIn)
  if(Bool==FALSE){ ParamIn <- rbind(ParamIn) }
  if(ncol(ParamIn)!=NParam){ stop(paste("the CemaNeige module requires ",NParam," parameters \n",sep="")); return(NULL) }  

  if(Direction=="TR"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- (ParamIn[,1]+9.99)/19.98            ### CemaNeige X1 (weighting coefficient for snow pack thermal state) 
    ParamOut[,2] <- exp(ParamIn[, 2]) / 200             ### CemaNeige X2 (degree-day melt coefficient)     
  }	
  if(Direction=="RT"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- ParamIn[,1]*19.98-9.99              ### CemaNeige X1 (weighting coefficient for snow pack thermal state) 
    ParamOut[,2] <- log(ParamIn[,2] * 200)              ### CemaNeige X2 (degree-day melt coefficient)                      
  }	

  if(Bool==FALSE){ ParamOut <- ParamOut[1,] }
  return(ParamOut)

}

