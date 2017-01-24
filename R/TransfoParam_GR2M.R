TransfoParam_GR2M <- function(ParamIn,Direction){

  NParam <- 2;
  Bool <- is.matrix(ParamIn);
  if(Bool==FALSE){ ParamIn <- rbind(ParamIn); }
  if(ncol(ParamIn)!=NParam){ stop(paste("the GR2M model requires ",NParam," parameters \n",sep="")); return(NULL); }  

  if(Direction=="TR"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- exp(ParamIn[,1]);
    ParamOut[,2] <- ParamIn[, 2] / 4 + 2.5;               
  }	
  if(Direction=="RT"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- log(ParamIn[,1]);
    ParamOut[,2] <- (ParamIn[,2] - 2.5) * 4;  
  }	

  if(Bool==FALSE){ ParamOut <- ParamOut[1,]; }
  return(ParamOut);

}

