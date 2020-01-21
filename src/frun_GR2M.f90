!------------------------------------------------------------------------------
!    Subroutines relative to the annual GR2M model
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : frun_GR2M.f
!------------------------------------------------------------------------------
! AUTHORS
! Original code: S. Mouelhi
! Cleaning and formatting for airGR: L. Coron
! Further cleaning: G. Thirel
!------------------------------------------------------------------------------
! Creation date: 2003
! Last modified: 25/11/2019
!------------------------------------------------------------------------------
! REFERENCES
! Mouelhi S. (2003). Vers une chaîne cohérente de modèles pluie-débit 
! conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et 
! journalier. PhD thesis (in French), ENGREF, Cemagref Antony, France.
!
! Mouelhi, S., C. Michel, C. Perrin and V. Andréassian (2006). Stepwise 
! development of a two-parameter monthly water balance model. Journal of 
! Hydrology, 318(1-4), 200-214. doi:10.1016/j.jhydrol.2005.06.014.
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. frun_gr2m
!         2. MOD_GR2M
!------------------------------------------------------------------------------


      SUBROUTINE frun_gr2m(LInputs,InputsPrecip,InputsPE,NParam,Param, &
                           NStates,StateStart,NOutputs,IndOutputs, &
                           Outputs,StateEnd)
! Subroutine that initializes GR2M, get its parameters, performs the call 
! to the MOD_GR2M subroutine at each time step, and stores the final states
! Inputs
!       LInputs      ! Integer, length of input and output series
!       InputsPrecip ! Vector of real, input series of total precipitation [mm/month]
!       InputsPE     ! Vector of real, input series of potential evapotranspiration (PE) [mm/month]
!       NParam       ! Integer, number of model parameters
!       Param        ! Vector of real, parameter set
!       NStates      ! Integer, number of state variables
!       StateStart   ! Vector of real, state variables used when the model run starts (store levels [mm])
!       NOutputs     ! Integer, number of output series
!       IndOutputs   ! Vector of integer, indices of output series
! Outputs      
!       Outputs      ! Vector of real, output series
!       StateEnd     ! Vector of real, state variables at the end of the model run (store levels [mm])


      !DEC$ ATTRIBUTES DLLEXPORT :: frun_gr2m


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
      integer, parameter :: NMISC=30
      doubleprecision, dimension(2) :: St
      doubleprecision, dimension(NMISC) :: MISC
      doubleprecision :: P,E,Q

      !--------------------------------------------------------------
      ! Initializations
      !--------------------------------------------------------------

      ! initialization of model states to zero
      St=0.

      ! initialization of model states using StateStart
      St(1)=StateStart(1)
      St(2)=StateStart(2)

      ! parameter values
      ! Param(1) : production store capacity [mm]
      ! Param(2) : groundwater exchange coefficient [-]

      ! initialization of model outputs
      Q = -999.999
      MISC = -999.999
!      StateEnd = -999.999 ! initialization made in R
!      Outputs = -999.999  ! initialization made in R



      !--------------------------------------------------------------
      ! Time loop
      !--------------------------------------------------------------
      DO k=1,LInputs
        P =InputsPrecip(k)
        E =InputsPE(k)
!        Q = -999.999
!        MISC = -999.999
        !model run on one time step
        CALL MOD_GR2M(St,Param,P,E,Q,MISC)
        !storage of outputs
        DO I=1,NOutputs
        Outputs(k,I)=MISC(IndOutputs(I))
        ENDDO
      ENDDO
      ! model states at the end of the run
      StateEnd(1)=St(1)
      StateEnd(2)=St(2)

      RETURN

      ENDSUBROUTINE





!################################################################################################################################




!**********************************************************************
      SUBROUTINE MOD_GR2M(St,Param,P,E,Q,MISC)
! Calculation of streamflow on a single time step (month) with the GR2M model
! Inputs:
!       St     Vector of real, model states at the beginning of the time step [mm]
!       Param  Vector of real, model parameters (Param(1) [mm]; Param(2) [-])
!       P      Real, value of rainfall during the time step [mm/month]
!       E      Real, value of potential evapotranspiration during the time step [mm/month]
! Outputs:
!       St     Vector of real, model states at the end of the time step [mm]
!       Q      Real, value of simulated flow at the catchment outlet for the time step [mm/month]
!       MISC   Vector of real, model outputs for the time step [mm/month]
!**********************************************************************
      Implicit None

      !! locals
      integer, parameter :: NParam=2,NMISC=30
      doubleprecision :: WS,S1,S2
      doubleprecision :: P1,P2,P3,R1,R2,AE,EXCH
      doubleprecision :: expWS, TWS, Sr ! speed-up

      !! dummies
      ! in
      doubleprecision, dimension(NParam), intent(in) :: Param
      doubleprecision, intent(in) :: P,E
      ! inout
      doubleprecision, dimension(2), intent(inout) :: St
      ! out
      doubleprecision, intent(out) :: Q
      doubleprecision, dimension(NMISC), intent(out) :: MISC
      
! Production store
      WS=P/Param(1)  
      IF(WS.GT.13.) WS=13.

      ! speed-up
      expWS = exp(2.*WS)
      TWS = (expWS - 1.)/(expWS + 1.)
      S1=(St(1)+Param(1)*TWS)/(1.+St(1)/Param(1)*TWS)                 
      ! S1=(X(1)+Param(1)*tanHyp(WS))/(1.+X(1)/Param(1)*tanHyp(WS))                 
      ! end speed-up

      P1=P+St(1)-S1                  
      WS=E/Param(1)         
      IF(WS.GT.13.) WS=13.

      ! speed-up
      expWS = exp(2.*WS)
      TWS = (expWS - 1.)/(expWS + 1.)
      S2=S1*(1.-TWS)/(1.+(1.-S1/Param(1))*TWS)  
      ! S2=S1*(1.-tanHyp(WS))/(1.+(1.-S1/Param(1))*tanHyp(WS))  
      ! end speed-up
      AE = S1 - S2
                
! Percolation
      ! speed-up
      Sr = S2/Param(1)
      Sr = Sr * Sr * Sr + 1.
      St(1)=S2/Sr**(1./3.)
      ! X(1)=S2/(1+(S2/Param(1))**3.)**(1./3.)         
      ! end speed-up

      P2=S2-St(1)  
      P3=P1+P2

! QR calculation (routing store)
      R1=St(2)+P3

! Water exchange
      R2=Param(2)*R1
      EXCH = R2 - R1

! Total runoff
      Q=R2*R2/(R2+60.)

! Updating store level
      St(2)=R2-Q


! Variables storage
      MISC( 1)=E             ! PE     ! [numeric] observed potential evapotranspiration [mm/month]
      MISC( 2)=P             ! Precip ! [numeric] observed total precipitation  [mm/month]
      MISC( 3)=St(1)         ! Prod   ! [numeric] production store level (St(1)) [mm]
      MISC( 4)=P1            ! Pn     ! [numeric] net rainfall (P1) [mm/month]
      MISC( 5)=AE            ! AE     ! [numeric] actual evapotranspiration [mm/month]
      MISC( 6)=P2            ! Perc   ! [numeric] percolation (P2) [mm/month]
      MISC( 7)=P3            ! PR     ! [numeric] P3=P1+P2 [mm/month]
      MISC( 8)=St(2)         ! Rout   ! [numeric] routing store level (St(2)) [mm]
      MISC( 9)=EXCH          ! EXCH   ! [numeric] groundwater exchange (EXCH) [mm/month]
      MISC(10)=Q             ! Qsim   ! [numeric] simulated outflow at catchment outlet [mm/month]


      END SUBROUTINE


