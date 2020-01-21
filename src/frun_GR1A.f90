!------------------------------------------------------------------------------
!    Subroutines relative to the annual GR1A model
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : frun_GR1A.f
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
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. frun_gr1a
!         2. MOD_GR1A
!------------------------------------------------------------------------------


      SUBROUTINE frun_gr1a(LInputs,InputsPrecip,InputsPE,NParam,Param, &
                           NStates,StateStart,NOutputs,IndOutputs, &
                           Outputs,StateEnd)
! Subroutine that initializes GR1A, get its parameters, performs the call 
! to the MOD_GR1A subroutine at each time step, and stores the final states
! Inputs
!       LInputs      ! Integer, length of input and output series
!       InputsPrecip ! Vector of real, input series of total precipitation [mm/year]
!       InputsPE     ! Vector of real, input series of potential evapotranspiration (PE) [mm/year]
!       NParam       ! Integer, number of model parameters
!       Param        ! Vector of real, parameter set
!       NStates      ! Integer, number of state variables
!       StateStart   ! Vector of real, state variables used when the model run starts (none here)
!       NOutputs     ! Integer, number of output series
!       IndOutputs   ! Vector of integer, indices of output series
! Outputs      
!       Outputs      ! Vector of real, output series
!       StateEnd     ! Vector of real, state variables at the end of the model run (none here)


      !DEC$ ATTRIBUTES DLLEXPORT :: frun_gr1a


      Implicit None

      !! dummies
      ! in
      integer, intent(in) :: LInputs,NParam,NStates,NOutputs
      doubleprecision, dimension(LInputs), intent(in) :: InputsPrecip
      doubleprecision, dimension(LInputs), intent(in) :: InputsPE
      doubleprecision, dimension(NParam) , intent(in) :: Param
      doubleprecision, dimension(NStates), intent(in) :: StateStart
      integer, dimension(NOutputs),        intent(in) :: IndOutputs
      ! out
      doubleprecision, dimension(NStates), intent(out) :: StateEnd
      doubleprecision, dimension(LInputs,NOutputs), intent(out) :: Outputs
      
      !! locals
      integer :: I,K
      integer, parameter :: NMISC=3
      doubleprecision, dimension(NMISC) :: MISC
      doubleprecision P0,P1,E1,Q

      !--------------------------------------------------------------
      ! Initializations
      !--------------------------------------------------------------
            
      ! parameter values
      ! Param(1) : PE adjustment factor [-]

      ! initialization of model outputs
      Q = -999.999
      MISC = -999.999
!      Outputs = -999.999  ! initialization made in R

!      StateStart = -999.999  ! CRAN-compatibility updates
      StateEnd = StateStart  ! CRAN-compatibility updates


      !--------------------------------------------------------------
      ! Time loop
      !--------------------------------------------------------------
      DO k=2,LInputs
        P0=InputsPrecip(k-1)
        P1=InputsPrecip(k)
        E1=InputsPE(k)
!        Q = -999.999
!        MISC = -999.999
        ! model run on one time step
        CALL MOD_GR1A(Param,P0,P1,E1,Q,MISC)
        ! storage of outputs
        DO I=1,NOutputs
          Outputs(k,I)=MISC(IndOutputs(I))
        ENDDO
      ENDDO

      RETURN

      ENDSUBROUTINE





!################################################################################################################################




!**********************************************************************
      SUBROUTINE MOD_GR1A(Param,P0,P1,E1,Q,MISC)
! Calculation of streamflow on a single time step (year) with the GR1A model
! Inputs
!       Param ! Vector of real, model parameters (Param(1) [-])
!       P0    ! Real, value of rainfall during the previous time step [mm/year]
!       P1    ! Real, value of rainfall during the current time step [mm/year]
!       E1    ! Real, value of potential evapotranspiration during the current time step [mm/year]
! Outputs
!       Q     ! Real, value of simulated flow at the catchment outlet for the current time step [mm/year]
!       MISC  ! Vector of real, model outputs for the time step [mm/year]
!**********************************************************************
      Implicit None

      !! locals
      integer, parameter :: NMISC=3
      integer, parameter :: NParam=1
      doubleprecision :: tt ! speed-up
      
      !! dummies
      ! in
      doubleprecision, dimension(NParam), intent(in) :: Param
      doubleprecision, intent(in) :: P0,P1,E1
      ! out
      doubleprecision, dimension(NMISC), intent(out) :: MISC
      doubleprecision, intent(out) :: Q
      

! Runoff
      ! speed-up
      tt = (0.7*P1+0.3*P0)/Param(1)/E1
      Q=P1*(1.-1./SQRT(1.+tt*tt))
      ! Q=P1*(1.-1./(1.+((0.7*P1+0.3*P0)/Param(1)/E1)**2.)**0.5)
      ! end speed-up

! Variables storage
      MISC( 1)=E1            ! PE     ! [numeric] observed potential evapotranspiration [mm/year]
      MISC( 2)=P1            ! Precip ! [numeric] observed total precipitation [mm/year]
      MISC( 3)=Q             ! Qsim   ! [numeric] simulated outflow at catchment outlet [mm/year]


      END SUBROUTINE


