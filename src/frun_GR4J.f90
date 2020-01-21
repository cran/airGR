!------------------------------------------------------------------------------
!    Subroutines relative to the annual GR4J model
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : frun_GR4J.f
!------------------------------------------------------------------------------
! AUTHORS
! Original code: C. Perrin
! Cleaning and formatting for airGR: L. Coron
! Further cleaning: G. Thirel
!------------------------------------------------------------------------------
! Creation date: 2000
! Last modified: 25/11/2019
!------------------------------------------------------------------------------
! REFERENCES
! Perrin, C., C. Michel and V. Andréassian (2003). Improvement of a 
! parsimonious model for streamflow simulation. Journal of Hydrology, 
! 279(1-4), 275-289. doi:10.1016/S0022-1694(03)00225-7.
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. frun_gr4j
!         2. MOD_GR4J
!------------------------------------------------------------------------------


      SUBROUTINE frun_gr4j(LInputs,InputsPrecip,InputsPE,NParam,Param, &
                           NStates,StateStart,NOutputs,IndOutputs, &
                           Outputs,StateEnd)
! Subroutine that initializes GR4J, get its parameters, performs the call 
! to the MOD_GR4J subroutine at each time step, and stores the final states
! Inputs
!       LInputs      ! Integer, length of input and output series
!       InputsPrecip ! Vector of real, input series of total precipitation [mm/day]
!       InputsPE     ! Vector of real, input series of potential evapotranspiration (PE) [mm/day]
!       NParam       ! Integer, number of model parameters
!       Param        ! Vector of real, parameter set
!       NStates      ! Integer, number of state variables
!       StateStart   ! Vector of real, state variables used when the model run starts (store levels [mm] and Unit Hydrograph (UH) storages [mm])
!       NOutputs     ! Integer, number of output series
!       IndOutputs   ! Vector of integer, indices of output series
! Outputs      
!       Outputs      ! Vector of real, output series
!       StateEnd     ! Vector of real, state variables at the end of the model run (store levels [mm] and Unit Hydrograph (UH) storages [mm])


      !DEC$ ATTRIBUTES DLLEXPORT :: frun_gr4j


      Implicit None

      !! dummies
      ! in
      integer, intent(in) :: LInputs,NParam,NStates,NOutputs
      doubleprecision, dimension(LInputs), intent(in) :: InputsPrecip
      doubleprecision, dimension(LInputs), intent(in) :: InputsPE
      doubleprecision, dimension(NParam),  intent(in) :: Param
      doubleprecision, dimension(NStates), intent(in) :: StateStart
      integer, dimension(NOutputs),        intent(in) :: IndOutputs
      ! out
      doubleprecision, dimension(NStates), intent(out) :: StateEnd
      doubleprecision, dimension(LInputs,NOutputs), intent(out) :: Outputs
      
      !! locals
      integer :: I,K
      integer, parameter :: NH=20,NMISC=30
      doubleprecision, dimension(2)     :: St
      doubleprecision, dimension(NH)    :: StUH1, OrdUH1
      doubleprecision, dimension(2*NH)  :: StUH2, OrdUH2
      doubleprecision, dimension(NMISC) :: MISC
      doubleprecision :: D,P1,E,Q

      !--------------------------------------------------------------
      ! Initializations
      !--------------------------------------------------------------

      ! initialization of model states to zero
      St=0.
      StUH1=0.
      StUH2=0.

      ! initialization of model states using StateStart
      St(1) = StateStart(1)
      St(2) = StateStart(2)
      DO I=1,NH
        StUH1(I)=StateStart(7+I)
      ENDDO
      DO I=1,2*NH
        StUH2(I)=StateStart(7+I+NH)
      ENDDO

      ! parameter values
      ! Param(1) : production store capacity (X1 - PROD) [mm]
      ! Param(2) : intercatchment exchange coefficient (X2 - CES) [mm/day]
      ! Param(3) : routing store capacity (X3 - ROUT) [mm]
      ! Param(4) : time constant of unit hydrograph (X4 - TB) [day]

      ! computation of UH ordinates
      OrdUH1 = 0.
      OrdUH2 = 0.
      
      D=2.5
      CALL UH1(OrdUH1,Param(4),D)
      CALL UH2(OrdUH2,Param(4),D)

      ! initialization of model outputs
      Q = -999.999
      MISC = -999.999
!      StateEnd = -999.999 !initialization made in R
!      Outputs = -999.999  !initialization made in R



      !--------------------------------------------------------------
      ! Time loop
      !--------------------------------------------------------------
      DO k=1,LInputs
        P1=InputsPrecip(k)
        E =InputsPE(k)
!        Q = -999.999
!        MISC = -999.999
        ! model run on one time step
        CALL MOD_GR4J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,P1,E,Q,MISC)
        ! storage of outputs
        DO I=1,NOutputs
          Outputs(k,I)=MISC(IndOutputs(I))
        ENDDO
      ENDDO
      ! model states at the end of the run
      StateEnd(1) = St(1)
      StateEnd(2) = St(2)
      DO K=1,NH
        StateEnd(7+K)=StUH1(K)
      ENDDO
      DO K=1,2*NH
        StateEnd(7+NH+K)=StUH2(K)
      ENDDO

      RETURN

      ENDSUBROUTINE





!################################################################################################################################




!**********************************************************************
      SUBROUTINE MOD_GR4J(St,StUH1,StUH2,OrdUH1,OrdUH2,Param,P1,E,Q,MISC)
! Calculation of streamflow on a single time step (day) with the GR4J model
! Inputs:
!       St     Vector of real, model states in stores at the beginning of the time step [mm]
!       StUH1  Vector of real, model states in Unit Hydrograph 1 at the beginning of the time step [mm]
!       StUH2  Vector of real, model states in Unit Hydrograph 2 at the beginning of the time step [mm]
!       OrdUH1 Vector of real, ordinates in UH1 [-]
!       OrdUH2 Vector of real, ordinates in UH2 [-]
!       Param  Vector of real, model parameters [various units]
!       P1     Real, value of rainfall during the time step [mm/day]
!       E      Real, value of potential evapotranspiration during the time step [mm/day]
! Outputs:
!       St     Vector of real, model states in stores at the end of the time step [mm]
!       StUH1  Vector of real, model states in Unit Hydrograph 1 at the end of the time step [mm]
!       StUH2  Vector of real, model states in Unit Hydrograph 2 at the end of the time step [mm]
!       Q      Real, value of simulated flow at the catchment outlet for the time step [mm/day]
!       MISC   Vector of real, model outputs for the time step [mm/day]
!**********************************************************************
      Implicit None

      !! locals
      integer, parameter :: NParam=4,NMISC=30,NH=20
      doubleprecision :: A,EN,ER,PN,PR,PS,WS
      doubleprecision :: PERC,PRHU1,PRHU2,EXCH,QR,QD
      doubleprecision :: AE,AEXCH1,AEXCH2
      integer :: K
      doubleprecision, parameter :: B=0.9
      doubleprecision, parameter :: stored_val=25.62890625
      doubleprecision :: expWS, TWS, Sr, Rr ! speed-up

      !! dummies
      ! in
      doubleprecision, dimension(NParam), intent(in)  :: Param
      doubleprecision, intent(in) :: P1,E
      doubleprecision, dimension(NH), intent(inout)   :: OrdUH1
      doubleprecision, dimension(2*NH), intent(inout) :: OrdUH2
      ! inout
      doubleprecision, dimension(2), intent(inout)    :: St
      doubleprecision, dimension(NH), intent(inout)   :: StUH1
      doubleprecision, dimension(2*NH), intent(inout) :: StUH2
      ! out
      doubleprecision, intent(out) :: Q
      doubleprecision, dimension(NMISC), intent(out)  :: MISC

      A=Param(1)


! Interception and production store
      IF(P1.LE.E) THEN
        EN=E-P1
        PN=0.
        WS=EN/A
        IF(WS.GT.13.) WS=13.
        ! speed-up
        expWS = exp(2.*WS)
        TWS = (expWS - 1.)/(expWS + 1.)
        Sr = St(1)/A
        ER=St(1)*(2.-Sr)*TWS/(1.+(1.-Sr)*TWS)
        ! ER=X(2)*(2.-X(2)/A)*tanHyp(WS)/(1.+(1.-X(2)/A)*tanHyp(WS))
        ! end speed-up  
        AE=ER+P1
        St(1)=St(1)-ER
        PS=0.
        PR=0.
      ELSE
        EN=0.
        AE=E
        PN=P1-E
        WS=PN/A
        IF(WS.GT.13.) WS=13.
        ! speed-up
        expWS = exp(2.*WS)
        TWS = (expWS - 1.)/(expWS + 1.)
        Sr = St(1)/A
        PS=A*(1.-Sr*Sr)*TWS/(1.+Sr*TWS)
        ! PS=A*(1.-(X(2)/A)**2.)*tanHyp(WS)/(1.+X(2)/A*tanHyp(WS))
        ! end speed-up
        PR=PN-PS
        St(1)=St(1)+PS
      ENDIF

! Percolation from production store
      IF(St(1).LT.0.) St(1)=0.
      ! speed-up
      ! (9/4)**4 = 25.62890625 = stored_val
      Sr = St(1)/Param(1)
      Sr = Sr * Sr
      Sr = Sr * Sr
      PERC=St(1)*(1.-1./SQRT(SQRT(1.+Sr/stored_val)))
      ! PERC=X(2)*(1.-(1.+(X(2)/(9./4.*Param(1)))**4.)**(-0.25))
      ! end speed-up
      St(1)=St(1)-PERC

      PR=PR+PERC

! Split of effective rainfall into the two routing components
      PRHU1=PR*B
      PRHU2=PR*(1.-B)

! Convolution of unit hydrograph UH1
      DO K=1,MAX(1,MIN(NH-1,INT(Param(4)+1.)))
        StUH1(K)=StUH1(K+1)+OrdUH1(K)*PRHU1
      ENDDO
      StUH1(NH)=OrdUH1(NH)*PRHU1

! Convolution of unit hydrograph UH2
      DO K=1,MAX(1,MIN(2*NH-1,2*INT(Param(4)+1.)))
        StUH2(K)=StUH2(K+1)+OrdUH2(K)*PRHU2
      ENDDO
      StUH2(2*NH)=OrdUH2(2*NH)*PRHU2

! Potential intercatchment semi-exchange
      ! speed-up
      Rr = St(2)/Param(3)
      EXCH=Param(2)*Rr*Rr*Rr*SQRT(Rr)
      ! EXCH=Param(2)*(X(1)/Param(3))**3.5
      ! end speed-up

! Routing store
      AEXCH1=EXCH
      IF((St(2)+StUH1(1)+EXCH).LT.0.) AEXCH1=-St(2)-StUH1(1)
      St(2)=St(2)+StUH1(1)+EXCH
      IF(St(2).LT.0.) St(2)=0.
      ! speed-up
      Rr = St(2)/Param(3)
      Rr = Rr * Rr
      Rr = Rr * Rr
      QR=St(2)*(1.-1./SQRT(SQRT(1.+Rr)))
      ! QR=X(1)*(1.-(1.+(X(1)/Param(3))**4.)**(-1./4.))
      ! end speed-up
      St(2)=St(2)-QR

! Runoff from direct branch QD
      AEXCH2=EXCH
      IF((StUH2(1)+EXCH).LT.0.) AEXCH2=-StUH2(1)
      QD=MAX(0.d0,StUH2(1)+EXCH)

! Total runoff
      Q=QR+QD
      IF(Q.LT.0.) Q=0.

! Variables storage
      MISC( 1)=E             ! PE     ! observed potential evapotranspiration [mm/day]
      MISC( 2)=P1            ! Precip ! observed total precipitation [mm/day]
      MISC( 3)=St(1)         ! Prod   ! production store level (St(1)) [mm]
      MISC( 4)=PN            ! Pn     ! net rainfall [mm/day]
      MISC( 5)=PS            ! Ps     ! part of Ps filling the production store [mm/day]
      MISC( 6)=AE            ! AE     ! actual evapotranspiration [mm/day]
      MISC( 7)=PERC          ! Perc   ! percolation (PERC) [mm/day]
      MISC( 8)=PR            ! PR     ! PR=PN-PS+PERC [mm/day]
      MISC( 9)=StUH1(1)      ! Q9     ! outflow from UH1 (Q9) [mm/day]
      MISC(10)=StUH2(1)      ! Q1     ! outflow from UH2 (Q1) [mm/day]
      MISC(11)=St(2)         ! Rout   ! routing store level (St(2)) [mm]
      MISC(12)=EXCH          ! Exch   ! potential semi-exchange between catchments (EXCH) [mm/day]
      MISC(13)=AEXCH1        ! AExch1 ! actual exchange between catchments from branch 1 (AEXCH1) [mm/day]
      MISC(14)=AEXCH2        ! AExch2 ! actual exchange between catchments from branch 2 (AEXCH2) [mm/day]
      MISC(15)=AEXCH1+AEXCH2 ! AExch  ! actual total exchange between catchments (AEXCH1+AEXCH2) [mm/day]
      MISC(16)=QR            ! QR     ! outflow from routing store (QR) [mm/day]
      MISC(17)=QD            ! QD     ! outflow from UH2 branch after exchange (QD) [mm/day]
      MISC(18)=Q             ! Qsim   ! simulated outflow at catchment outlet [mm/day]




      END SUBROUTINE


