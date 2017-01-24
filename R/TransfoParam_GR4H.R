TransfoParam_GR4H <- function(ParamIn,Direction){

  NParam <- 4;
  Bool <- is.matrix(ParamIn);
  if(Bool==FALSE){ ParamIn <- rbind(ParamIn); }
  if(ncol(ParamIn)!=NParam){ stop(paste("the GR4H model requires ",NParam," parameters \n",sep="")); return(NULL); }  

  if(Direction=="TR"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- exp(ParamIn[,1]);                    ### GR4H X1 (production store capacity)    
    ParamOut[,2] <- sinh(ParamIn[, 2] / 3);              ### GR4H X2 (groundwater exchange coefficient)      
    ParamOut[,3] <- exp(ParamIn[,3]);                    ### GR4H X3 (routing store capacity)       
    ParamOut[,4] <- 480 + (480 - 0.5) * (ParamIn[,4] - 9.99) / 19.98;    ### GR4H X4 (unit hydrograph time constant)
  }	
  if(Direction=="RT"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- log(ParamIn[,1]);                    ### GR4H X1 (production store capacity)    
    ParamOut[,2] <- 3 * asinh(ParamIn[,2]);              ### GR4H X2 (groundwater exchange coefficient)      
    ParamOut[,3] <- log(ParamIn[,3]);                    ### GR4H X3 (routing store capacity)       
    ParamOut[,4] <- (ParamIn[,4] - 480) * 19.98 / (480 - 0.5) + 9.99;    ### GR4H X4 (unit hydrograph time constant)
  }	

  if(Bool==FALSE){ ParamOut <- ParamOut[1,]; }
  return(ParamOut);

}

