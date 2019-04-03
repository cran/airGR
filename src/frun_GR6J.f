

      SUBROUTINE frun_GR6J(
                                 !inputs
     &                             LInputs      , ! [integer] length of input and output series
     &                             InputsPrecip , ! [double]  input series of total precipitation [mm/day]
     &                             InputsPE     , ! [double]  input series of potential evapotranspiration (PE) [mm/day]
     &                             NParam       , ! [integer] number of model parameters
     &                             Param        , ! [double]  parameter set
     &                             NStates      , ! [integer] number of state variables
     &                             StateStart   , ! [double]  state variables used when the model run starts (store levels [mm] and Unit Hydrograph (UH) storages [mm])
     &                             NOutputs     , ! [integer] number of output series
     &                             IndOutputs   , ! [integer] indices of output series
                                 !outputs
     &                             Outputs      , ! [double]  output series
     &                             StateEnd     ) ! [double]  state variables at the end of the model run (store levels [mm] and Unit Hydrograph (UH) storages [mm])


      !DEC$ ATTRIBUTES DLLEXPORT :: frun_GR6J


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
      integer NH,NMISC
      parameter (NH=20,NMISC=30)
      doubleprecision St(3), StUH1(NH), StUH2(2*NH)
      doubleprecision OrdUH1(NH), OrdUH2(2*NH)
      doubleprecision MISC(NMISC)
      doubleprecision D
      doubleprecision P1,E,Q
      integer I,K

      !--------------------------------------------------------------
      !Initializations
      !--------------------------------------------------------------

      !initilisation of model states to zero
      St=0.
      StUH1=0.
      StUH2=0.

      !initialization of model states using StateStart
      St(1) = StateStart(1)
      St(2) = StateStart(2)
      St(3) = StateStart(3)
      DO I=1,NH
        StUH1(I)=StateStart(7+I)
      ENDDO
      DO I=1,2*NH
        StUH2(I)=StateStart(7+I+NH)
      ENDDO

      !parameter values
      !Param(1) : production store capacity (X1 - PROD) [mm]
      !Param(2) : intercatchment exchange coefficient (X2 - CES1) [mm/day]
      !Param(3) : routing store capacity (X3 - ROUT) [mm]
      !Param(4) : time constant of unit hydrograph (X4 - TB) [day]
      !Param(5) : intercatchment exchange threshold (X5 - CES2) [-]
      !Param(6) : time constant of exponential store (X6 - EXP) [day]

      !computation of HU ordinates
      OrdUH1 = 0.
      OrdUH2 = 0.
      
      D=2.5
      CALL UH1(OrdUH1,Param(4),D)
      CALL UH2(OrdUH2,Param(4),D)
      
      !initialization of model outputs
      Q = -999.999
      MISC = -999.999
c      StateEnd = -999.999 !initialization made in R
c      Outputs = -999.999  !initialization made in R



      !--------------------------------------------------------------
      !Time loop
      !--------------------------------------------------------------
      DO k=1,LInputs
        P1=InputsPrecip(k)
        E =InputsPE(k)
c        Q = -999.999
c        MISC = -999.999
        !model run on one time step
        CALL MOD_GR6J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,P1,E,Q,MISC)
        !storage of outputs
        DO I=1,NOutputs
          Outputs(k,I)=MISC(IndOutputs(I))
        ENDDO
      ENDDO
      !model states at the end of the run
      StateEnd(1) = St(1)
      StateEnd(2) = St(2)
      StateEnd(3) = St(3)
      DO K=1,NH
        StateEnd(7+K)=StUH1(K)
      ENDDO
      DO K=1,2*NH
        StateEnd(7+NH+K)=StUH2(K)
      ENDDO

      RETURN

      ENDSUBROUTINE





c################################################################################################################################




C**********************************************************************
      SUBROUTINE MOD_GR6J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,P1,E,Q,
     &MISC)
C Run on a single time step with the GR6J model
C Inputs:
C       St     Vector of model states in stores at the beginning of the time step [mm]
C       StUH1  Vector of model states in Unit Hydrograph 1 at the beginning of the time step [mm]
C       StUH2  Vector of model states in Unit Hydrograph 2 at the beginning of the time step [mm]
C       OrdUH1 Vector of ordinates in UH1 [-]
C       OrdUH2 Vector of ordinates in UH2 [-]
C       Param  Vector of model parameters [various units]
C       P1     Value of rainfall during the time step [mm]
C       E      Value of potential evapotranspiration during the time step [mm]
C Outputs:
C       St     Vector of model states in stores at the end of the time step [mm]
C       StUH1  Vector of model states in Unit Hydrograph 1 at the end of the time step [mm]
C       StUH2  Vector of model states in Unit Hydrograph 2 at the end of the time step [mm]
C       Q      Value of simulated flow at the catchment outlet for the time step [mm/day]
C       MISC   Vector of model outputs for the time step [mm or mm/day]
C**********************************************************************
      Implicit None
      INTEGER NH,NMISC,NParam
      PARAMETER (NH=20,NMISC=30)
      PARAMETER (NParam=6)
      DOUBLEPRECISION St(3)
      DOUBLEPRECISION StUH1(NH),StUH2(2*NH),OrdUH1(NH),OrdUH2(2*NH)
      DOUBLEPRECISION Param(NParam)
      DOUBLEPRECISION MISC(NMISC)
      DOUBLEPRECISION P1,E,Q
      DOUBLEPRECISION A,B,C,EN,ER,PN,PR,PS,WS,tanHyp,AR
      DOUBLEPRECISION PERC,PRUH1,PRUH2,EXCH,QR,QD,QRExp
      DOUBLEPRECISION AE,AEXCH1,AEXCH2
      INTEGER K

      DATA B/0.9/
      DATA C/0.4/
      DOUBLEPRECISION TWS, Sr, Rr ! speed-up

      A=Param(1)


C Production store
      IF(P1.LE.E) THEN
        EN=E-P1
        PN=0.
        WS=EN/A
        IF(WS.GT.13)WS=13.
        
        ! speed-up
        TWS = tanHyp(WS)
        Sr = St(1)/A
        ER=St(1)*(2.-Sr)*TWS/(1.+(1.-Sr)*TWS)
        ! ER=X(2)*(2.-X(2)/A)*tanHyp(WS)/(1.+(1.-X(2)/A)*tanHyp(WS))
        ! fin speed-up  
        
        AE=ER+P1
        St(1)=St(1)-ER
        PS=0.
        PR=0.
      ELSE
        EN=0.
        AE=E
        PN=P1-E
        WS=PN/A
        IF(WS.GT.13)WS=13.
        
        ! speed-up
        TWS = tanHyp(WS)
        Sr = St(1)/A
        PS=A*(1.-Sr*Sr)*TWS/(1.+Sr*TWS)
        ! PS=A*(1.-(X(2)/A)**2.)*tanHyp(WS)/(1.+X(2)/A*tanHyp(WS))
        ! fin speed-up  
        
        PR=PN-PS
        St(1)=St(1)+PS
      ENDIF

C Percolation from production store
      IF(St(1).LT.0.)St(1)=0.
      ! speed-up
      ! (9/4)**4 = 25.62890625
      Sr = St(1)/Param(1)
      Sr = Sr * Sr
      Sr = Sr * Sr
      PERC=St(1)*(1.-1./SQRT(SQRT(1.+Sr/25.62890625)))
      ! PERC=X(2)*(1.-(1.+(X(2)/(9./4.*Param(1)))**4.)**(-0.25))
      ! fin speed-up  

      St(1)=St(1)-PERC

      PR=PR+PERC

C Split of effective rainfall into the two routing components
      PRUH1=PR*B
      PRUH2=PR*(1.-B)

C Convolution of unit hydrograph UH1
      DO K=1,MAX(1,MIN(NH-1,INT(Param(4)+1.)))
      StUH1(K)=StUH1(K+1)+OrdUH1(K)*PRUH1
      ENDDO
      StUH1(NH)=OrdUH1(NH)*PRUH1

C Convolution of unit hydrograph UH2
      DO K=1,MAX(1,MIN(2*NH-1,2*INT(Param(4)+1.)))
      StUH2(K)=StUH2(K+1)+OrdUH2(K)*PRUH2
      ENDDO
      StUH2(2*NH)=OrdUH2(2*NH)*PRUH2

C Potential intercatchment semi-exchange
      EXCH=Param(2)*(St(2)/Param(3)-Param(5))

C Routing store
      AEXCH1=EXCH
      IF((St(2)+(1-C)*StUH1(1)+EXCH).LT.0) AEXCH1=-St(2)-(1-C)*StUH1(1)
      St(2)=St(2)+(1-C)*StUH1(1)+EXCH
      IF(St(2).LT.0.)St(2)=0.
      
      ! speed-up
      Rr = St(2)/Param(3)
      Rr = Rr * Rr
      Rr = Rr * Rr
      QR=St(2)*(1.-1./SQRT(SQRT(1.+Rr)))
      ! QR=X(1)*(1.-(1.+(X(1)/Param(3))**4.)**(-1./4.))
      ! fin speed-up
      
      St(2)=St(2)-QR

C Update of exponential store
      St(3)=St(3)+C*StUH1(1)+EXCH
      AR=St(3)/Param(6)
      IF(AR.GT.33.)AR=33.
      IF(AR.LT.-33.)AR=-33.

      IF(AR.GT.7.)THEN
      QRExp=St(3)+Param(6)/EXP(AR)
      GOTO 3
      ENDIF

      IF(AR.LT.-7.)THEN
      QRExp=Param(6)*EXP(AR)
      GOTO 3
      ENDIF

      QRExp=Param(6)*LOG(EXP(AR)+1.)
    3 CONTINUE

      St(3)=St(3)-QRExp

C Runoff from direct branch QD
      AEXCH2=EXCH
      IF((StUH2(1)+EXCH).LT.0) AEXCH2=-StUH2(1)
      QD=MAX(0.d0,StUH2(1)+EXCH)

C Total runoff
      Q=QR+QD+QRExp
      IF(Q.LT.0.) Q=0.

C Variables storage
      MISC( 1)=E                  ! PE     ! observed potential evapotranspiration [mm/day]
      MISC( 2)=P1                 ! Precip ! observed total precipitation [mm/day]
      MISC( 3)=St(1)              ! Prod   ! production store level (St(1)) [mm]
      MISC( 4)=PN                 ! Pn     ! net rainfall [mm/day]
      MISC( 5)=PS                 ! Ps     ! part of Ps filling the production store [mm/day]
      MISC( 6)=AE                 ! AE     ! actual evapotranspiration [mm/day]
      MISC( 7)=PERC               ! Perc   ! percolation (PERC) [mm/day]
      MISC( 8)=PR                 ! PR     ! PR=PN-PS+PERC [mm/day]
      MISC( 9)=StUH1(1)           ! Q9     ! outflow from UH1 (Q9) [mm/day]
      MISC(10)=StUH2(1)           ! Q1     ! outflow from UH2 (Q1) [mm/day]
      MISC(11)=St(2)              ! Rout   ! routing store level (St(2)) [mm]
      MISC(12)=EXCH               ! Exch   ! potential third-exchange between catchments (EXCH) [mm/day]
      MISC(13)=AEXCH1             ! AExch1 ! actual exchange between catchments from routing store (AEXCH1) [mm/day]
      MISC(14)=AEXCH2             ! AExch2 ! actual exchange between catchments from direct branch (after UH2) (AEXCH2) [mm/day]
      MISC(15)=AEXCH1+AEXCH2+EXCH ! AExch  ! actual total exchange between catchments (AEXCH1+AEXCH2+EXCH) [mm/day]
      MISC(16)=QR                 ! QR     ! outflow from routing store (QR) [mm/day]
      MISC(17)=QRExp              ! QRExp  ! outflow from exponential store (QRExp) [mm/day]
      MISC(18)=St(3)              ! Exp    ! exponential store level (St(3)) (negative) [mm]
      MISC(19)=QD                 ! QD     ! outflow from UH2 branch after exchange (QD) [mm/day]
      MISC(20)=Q                  ! Qsim   ! simulated outflow at catchment outlet [mm/day]


      ENDSUBROUTINE


