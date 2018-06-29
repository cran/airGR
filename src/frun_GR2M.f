

      SUBROUTINE frun_GR2M(
                                 !inputs
     &                             LInputs      , ! [integer] length of input and output series
     &                             InputsPrecip , ! [double]  input series of total precipitation [mm/month]
     &                             InputsPE     , ! [double]  input series of potential evapotranspiration (PE) [mm/month]
     &                             NParam       , ! [integer] number of model parameters
     &                             Param        , ! [double]  parameter set
     &                             NStates      , ! [integer] number of state variables
     &                             StateStart   , ! [double]  state variables used when the model run starts (store levels [mm])
     &                             NOutputs     , ! [integer] number of output series
     &                             IndOutputs   , ! [integer] indices of output series
                                 !outputs
     &                             Outputs      , ! [double]  output series
     &                             StateEnd     ) ! [double]  state variables at the end of the model run (store levels [mm])


      !DEC$ ATTRIBUTES DLLEXPORT :: frun_GR2M


      Implicit None
      !### input and output variables
      integer, intent(in) :: LInputs,NParam,NStates,NOutputs
      doubleprecision, dimension(LInputs)  :: InputsPrecip
      doubleprecision, dimension(LInputs)  :: InputsPE
      doubleprecision, dimension(NParam)   :: Param
      doubleprecision, dimension(NStates)  :: StateStart
      doubleprecision, dimension(NStates)  :: StateEnd
      integer, dimension(NOutputs) :: IndOutputs
      doubleprecision, dimension(LInputs,NOutputs) :: Outputs

      !parameters, internal states and variables
      integer NMISC
      parameter (NMISC=30)
      doubleprecision St(2)
      doubleprecision MISC(NMISC)
      doubleprecision P,E,Q
      integer I,K

      !--------------------------------------------------------------
      !Initializations
      !--------------------------------------------------------------

      !initialization of model states to zero
      St=0.

      !initilisation of model states using StateStart
      St(1)=StateStart(1)
      St(2)=StateStart(2)

      !parameter values
      !Param(1) : production store capacity [mm]
      !Param(2) : groundwater exchange coefficient [-]

      !initialization of model outputs
      Q = -999.999
      MISC = -999.999
c      StateEnd = -999.999 !initialization made in R
c      Outputs = -999.999  !initialization made in R



      !--------------------------------------------------------------
      !Time loop
      !--------------------------------------------------------------
      DO k=1,LInputs
        P =InputsPrecip(k)
        E =InputsPE(k)
c        Q = -999.999
c        MISC = -999.999
        !model run on one time step
        CALL MOD_GR2M(St,Param,P,E,Q,MISC)
        !storage of outputs
        DO I=1,NOutputs
        Outputs(k,I)=MISC(IndOutputs(I))
        ENDDO
      ENDDO
      !model states at the end of the run
      StateEnd(1)=St(1)
      StateEnd(2)=St(2)

      RETURN

      ENDSUBROUTINE





c################################################################################################################################




C**********************************************************************
      SUBROUTINE MOD_GR2M(St,Param,P,E,Q,MISC)
C Calculation of streamflow on a single time step (month) with the GR2M model
C Inputs:
C       St     Vector of model states at the beginning of the time step [mm]
C       Param  Vector of model parameters (Param(1) [mm]; Param(2) [-])
C       P      Value of rainfall during the time step [mm/month]
C       E      Value of potential evapotranspiration during the time step [mm/month]
C Outputs:
C       St     Vector of model states at the end of the time step [mm]
C       Q      Value of simulated flow at the catchment outlet for the time step [mm/month]
C       MISC   Vector of model outputs for the time step [mm]
C**********************************************************************
      Implicit None
      INTEGER NMISC,NParam
      PARAMETER (NMISC=30)
      PARAMETER (NParam=2)
      DOUBLEPRECISION St(2)
      DOUBLEPRECISION Param(NParam)
      DOUBLEPRECISION MISC(NMISC)
      DOUBLEPRECISION P,E,Q
      DOUBLEPRECISION WS,tanHyp,S1,S2
      DOUBLEPRECISION P1,P2,P3,R1,R2,AE,EXCH

      DOUBLEPRECISION TWS, Sr, Rr ! speed-up
	  
C Production store
      WS=P/Param(1)  
      IF(WS.GT.13.)WS=13.
	  
 	  ! speed-up
	  TWS = tanHyp(WS)
	  S1=(St(1)+Param(1)*TWS)/(1.+St(1)/Param(1)*TWS)                 
	  ! S1=(X(1)+Param(1)*tanHyp(WS))/(1.+X(1)/Param(1)*tanHyp(WS))                 
  	  ! fin speed-up

	  P1=P+St(1)-S1                  
      WS=E/Param(1)         
      IF(WS.GT.13.)WS=13.
	  
 	  ! speed-up
	  TWS = tanHyp(WS)
      S2=S1*(1.-TWS)/(1.+(1.-S1/Param(1))*TWS)  
      ! S2=S1*(1.-tanHyp(WS))/(1.+(1.-S1/Param(1))*tanHyp(WS))  
 	  ! fin speed-up
	  AE = S1 - S2
                
C Percolation
 	  ! speed-up
	  Sr = S2/Param(1)
	  Sr = Sr * Sr * Sr + 1.
      St(1)=S2/Sr**(1./3.)
      ! X(1)=S2/(1+(S2/Param(1))**3.)**(1./3.)         
 	  ! fin speed-up

      P2=S2-St(1)  
      P3=P1+P2

C QR calculation (routing store)
      R1=St(2)+P3

C Water exchange
      R2=Param(2)*R1
	  EXCH = R2 - R1

C Total runoff
      Q=R2*R2/(R2+60.)

C Updating store level
      St(2)=R2-Q


C Variables storage
      MISC( 1)=E             ! PE     ! [numeric] observed potential evapotranspiration [mm/month]
      MISC( 2)=P             ! Precip ! [numeric] observed total precipitation  [mm/month]
      MISC( 3)=AE            ! AE     ! [numeric] actual evapotranspiration [mm/month]
      MISC( 4)=P1            ! Pn     ! [numeric] net rainfall (P1) [mm/month]
      MISC( 5)=P2            ! Perc   ! [numeric] percolation (P2) [mm/month]
      MISC( 6)=P3            ! PR     ! [numeric] P3=P1+P2 [mm/month]
      MISC( 7)=EXCH          ! EXCH   ! [numeric] groundwater exchange (EXCH) [mm/month]
      MISC( 8)=St(1)         ! Prod   ! [numeric] production store level (St(1)) [mm]
      MISC( 9)=St(2)         ! Rout   ! [numeric] routing store level (St(2)) [mm]
      MISC(10)=Q             ! Qsim   ! [numeric] simulated outflow at catchment outlet [mm/month]


      ENDSUBROUTINE


