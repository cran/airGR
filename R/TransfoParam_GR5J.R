TransfoParam_GR5J <- function(ParamIn,Direction){

  NParam <- 5;
  Bool <- is.matrix(ParamIn);
  if(Bool==FALSE){ ParamIn <- rbind(ParamIn); }
  if(ncol(ParamIn)!=NParam){ stop(paste("the GR5J model requires ",NParam," parameters \n",sep="")); return(NULL); }  

  if(Direction=="TR"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- exp(ParamIn[,1]);                    ### GR5J X1 (production store capacity)
    ParamOut[,2] <- sinh(ParamIn[,2]);                   ### GR5J X2 (groundwater exchange coefficient 1)
    ParamOut[,3] <- exp(ParamIn[,3]);                    ### GR5J X3 (routing store capacity)
    ParamOut[,4] <- 20+19.5*(ParamIn[,4]-9.99)/19.98;    ### GR5J X4 (unit hydrograph time constant)
    ParamOut[,5] <- (ParamIn[,5] + 9.99) / 19.98;        ### GR5J X5 (groundwater exchange coefficient 2)
  }	
  if(Direction=="RT"){
    ParamOut     <-  ParamIn;
    ParamOut[,1] <- log(ParamIn[,1]);                    ### GR5J X1 (production store capacity)
    ParamOut[,2] <- asinh(ParamIn[,2]);                  ### GR5J X2 (groundwater exchange coefficient 1)
    ParamOut[,3] <- log(ParamIn[,3]);                    ### GR5J X3 (routing store capacity)
    ParamOut[,4] <- 9.99+19.98*(ParamIn[,4]-20)/19.5;    ### GR5J X4 (unit hydrograph time constant)
    ParamOut[,5] <- ParamIn[,5] * 19.98 - 9.99;          ### GR5J X5 (groundwater exchange coefficient 2)
  }	

  if(Bool==FALSE){ ParamOut <- ParamOut[1,]; }
  return(ParamOut);

}

