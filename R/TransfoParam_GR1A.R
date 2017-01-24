TransfoParam_GR1A <- function(ParamIn,Direction){

  NParam <- 1;
  Bool <- is.matrix(ParamIn);
  if(Bool==FALSE){ ParamIn <- rbind(ParamIn); }
  if(ncol(ParamIn)!=NParam){ stop(paste("the GR1A model requires ",NParam," parameters \n",sep="")); return(NULL); }  

  if(Direction=="TR"){
    ParamOut     <-  (ParamIn+10.0)/8;
  }	
  if(Direction=="RT"){
    ParamOut     <-  ParamIn*8-10.0;
  }	

  if(Bool==FALSE){ ParamOut <- ParamOut[1,]; }
  return(ParamOut);

}

