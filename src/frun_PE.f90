!------------------------------------------------------------------------------
!    Subroutines relative to the Oudin potential evapotranspiration (PE) formula
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : frun_PE.f90
!------------------------------------------------------------------------------
! AUTHORS
! Original code: Oudin, L.
! Cleaning and formatting for airGR: Bourgin, F.
! Further cleaning: Delaigue, O., Thirel, G.
!------------------------------------------------------------------------------
! Creation date: 2004
! Last modified: 20/10/2020
!------------------------------------------------------------------------------
! REFERENCES
! Oudin, L., Hervieu, F., Michel, C., Perrin, C., Andréassian, V.,
! Anctil, F. and Loumagne, C. (2005). Which potential evapotranspiration
! input for a lumped rainfall-runoff model? Part 2 - Towards a simple and
! efficient potential evapotranspiration model for rainfall-runoff modelling.
! Journal of Hydrology, 303(1-4), 290-306, doi: 10.1016/j.jhydrol.2004.08.026.
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. frun_pe_oudin
!         2. PE_OUDIN
!------------------------------------------------------------------------------


!*******************************************************************************
      SUBROUTINE frun_pe_oudin(LInputs,InputsLAT,InputsTemp,InputsJJ,OutputsPE)
!*******************************************************************************
! Subroutine that performs the call to the PE_OUDIN subroutine at each time step,
! and stores the final values
! Inputs
!       LInputs      ! Integer, length of input and output series
!       InputsLAT    ! Vector of real, input series of latitude [rad]
!       InputsTemp   ! Vector of real, input series of air mean temperature [degC]
!       InputsJJ     ! Vector of real, input series of Julian day [-]
! Outputs
!       OutputsPE    ! Vector of real, output series of potential evapotranspiration (PE) [mm/time step]



      

      Implicit None

      !! dummies
      ! in
      integer, intent(in) :: LInputs
      doubleprecision, dimension(LInputs), intent(in) :: InputsLAT
      doubleprecision, dimension(LInputs), intent(in) :: InputsTemp
      doubleprecision, dimension(LInputs), intent(in) :: InputsJJ

      ! out
      doubleprecision, dimension(LInputs), intent(out) :: OutputsPE

      !! locals
      integer         :: k
      doubleprecision :: FI, tt, jj, PEoud

      !--------------------------------------------------------------
      ! Time loop
      !--------------------------------------------------------------
      DO k = 1, LInputs
        tt = InputsTemp(k)
        jj = InputsJJ(k)
        FI = InputsLAT(k)!
        !model run on one time step
        CALL PE_OUDIN(FI, tt, jj, PEoud)
        !storage of outputs
        OutputsPE(k) = PEoud
      ENDDO

      RETURN

      ENDSUBROUTINE





!################################################################################################################################




!*******************************************************************************
      SUBROUTINE PE_OUDIN(FI,DT,JD,DPE)
!*******************************************************************************
!     Calculation of potential evapotranspiration (DPE) on a single time step
!     using air temperature and daily extra-atmospheric global radiation
!     (that depends only on Julian day)
!
!     The PE formula is described in:
!     Oudin, L., Hervieu, F., Michel, C., Perrin, C., Andréassian, V.,
!     Anctil, F. and Loumagne, C., 2005. Which potential evapotranspiration
!     input for a rainfall-runoff model? Part 2 - Towards a simple and
!     efficient PE model for rainfall-runoff modelling. Journal of Hydrology
!     303(1-4), 290-306.
!
!     For the calculation of extra-atmospheric global radiation, see Appendix C of
!     the article by Morton, F.I., 1983. Operational estimates of areal
!     evapotranspiration and their significance to the science and practice
!     of hydrology. Journal of Hydrology 66 (1/4), 1-76.
!
!***************************************************************
! Inputs
!       FI           ! Latitude [rad]
!       DT           ! Air Temperature [degC]
!       JD           ! Julian day [-]
!
! Outputs
!       DPE          ! Potential evapotranspiration [mm/time step]
!***************************************************************
      IMPLICIT NONE

      !! dummies
      ! in
      doubleprecision, intent(in) :: FI, DT, JD
      ! out
      doubleprecision, intent(out) :: DPE
      !! locals
      doubleprecision :: COSFI, TETA, COSTETA, COSGZ, GZ, COSGZ2
      doubleprecision :: SINGZ, COSOM, COSOM2, SINOM, COSPZ, OM, GE
      doubleprecision :: ETA

! Calculation of extra-atmospheric global radiation (Appendix C in Morton
! (1983), Eq. C-6 to C-11, p.60-61)
      COSFI=COS(FI)

! TETA: Declination of the sun in radians
      TETA=0.4093*SIN(JD/58.1-1.405)
      COSTETA=COS(TETA)
      COSGZ=MAX(0.001d0,COS(FI-TETA))

! GZ: Noon angular zenith distance of the sun
      GZ=ACOS(COSGZ)
      COSGZ2=COSGZ*COSGZ

      IF(COSGZ2.GE.1.) THEN
        SINGZ=0.
      ELSE
        SINGZ=SQRT(1.-COSGZ2)
      ENDIF

      COSOM=1.-COSGZ/COSFI/COSTETA
      IF(COSOM.LT.-1.) COSOM=-1.
      IF(COSOM.GT.1.) COSOM=1.
      COSOM2=COSOM*COSOM
      IF(COSOM2.GE.1.) THEN
        SINOM=0.
      ELSE
        SINOM=SQRT(1.-COSOM2)
      ENDIF

      OM=ACOS(COSOM)
! PZ: Average angular zenith distance of the sun
      COSPZ=COSGZ+COSFI*COSTETA*(SINOM/OM-1.)
      IF(COSPZ.LT.0.001) COSPZ=0.001
! ETA: Radius vector of the sun
      ETA=1.+COS(JD/58.1)/30.
! GE: extra-atmospheric global radiation
      GE=446.*OM*COSPZ*ETA

! Daily PE by Oudin et al. (2006) formula:
      DPE=MAX(0.d0,GE*(DT+5.)/100./28.5)

      RETURN

      END SUBROUTINE PE_OUDIN
!*******************************************************************************
