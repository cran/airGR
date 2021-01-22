!------------------------------------------------------------------------------
!    Subroutines relative to the daily unit hydrographs and hyperbolic
!    tangent calculation
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : utils_D.f
!------------------------------------------------------------------------------
! AUTHORS
! Original code: Unknown soldier
! Cleaning and formatting for airGR: Coron, L.
! Further cleaning: Thirel, G.
!------------------------------------------------------------------------------
! Creation date: 2000
! Last modified: 21/11/2019
!------------------------------------------------------------------------------
! REFERENCES
!
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. UH1
!         2. UH2
!         3. SS1
!         4. SS2
!         5. tanHyp
!------------------------------------------------------------------------------


!**********************************************************************
      SUBROUTINE UH1(OrdUH1,C,D)
! Subroutine that computes the ordinates of the daily GR unit hydrograph 
! UH1 using successive differences on the S curve SS1
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
! Outputs
!    OrdUH1: Vector of real, NH ordinates of the discrete hydrograph
!**********************************************************************

      Implicit None

      !! locals
      integer, parameter :: NH=20
      doubleprecision :: SS1
      integer :: I

      !! dummies
      ! in
      double precision, intent(in) :: C,D
      ! out
      double precision, dimension (NH), intent(out) :: OrdUH1
      
      DO I=1,NH
        OrdUH1(I)=SS1(I,C,D)-SS1(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


!**********************************************************************
      SUBROUTINE UH2(OrdUH2,C,D)
! Subroutine that computes the ordinates of the daily GR unit hydrograph 
! UH2 using successive differences on the S curve SS2
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
! Outputs
!    OrdUH2: Vector of real, 2*NH ordinates of the discrete hydrograph
!**********************************************************************

      Implicit None

      !! locals
      integer, parameter :: NH=20
      doubleprecision :: SS2
      integer :: I

      !! dummies
      ! in
      double precision, intent(in) :: C,D
      ! out
      double precision, dimension (2*NH), intent(out) :: OrdUH2
      
      DO I =1,2*NH
        OrdUH2(I)=SS2(I,C,D)-SS2(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


!**********************************************************************
      FUNCTION SS1(I,C,D)
! Function that computes the values of the S curve (cumulative UH 
! curve) of GR unit hydrograph UH1
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
!    I ! Integer, time-step
! Outputs
!    SS1 ! Real, value of the S curve for I
!**********************************************************************

      Implicit None

      !! dummies
      ! in
      doubleprecision, intent(in) :: C,D
      integer, intent(in) :: I
      ! out
      doubleprecision ::SS1
      
      !! locals
      integer :: FI

      FI=I
      IF(FI.LE.0) THEN
        SS1=0.
        RETURN
      ENDIF
      IF(FI.LT.C) THEN
        SS1=(FI/C)**D
        RETURN
      ENDIF
      SS1=1.
      ENDFUNCTION


!**********************************************************************
      FUNCTION SS2(I,C,D)
! Function that computes the values of the S curve (cumulative UH 
! curve) of GR unit hydrograph UH2
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
!    I ! Integer, time-step
! Outputs
!    SS2 ! Real, value of the S curve for I
!**********************************************************************

      Implicit None

      !! dummies
      ! in
      doubleprecision, intent(in) :: C,D
      integer, intent(in) :: I
      ! out
      doubleprecision ::SS2
      
      !! locals
      integer :: FI

      FI=I
      IF(FI.LE.0) THEN
        SS2=0.
        RETURN
      ENDIF
      IF(FI.LE.C) THEN
        SS2=0.5*(FI/C)**D
        RETURN
      ENDIF
      IF(FI.LT.2.*C) THEN
        SS2=1.-0.5*(2.-FI/C)**D
        RETURN
      ENDIF
      SS2=1.
      ENDFUNCTION


!**********************************************************************
      FUNCTION tanHyp(Val)
! Function that calculates the hyperbolic tangent
!**********************************************************************
! Inputs
!      Val ! Real, value
! Outputs
!      tanHyp ! Real, value of the hyperbolic tangent

      Implicit None

      !! dummies
      ! in
      doubleprecision, intent(in) :: Val
      ! out
      doubleprecision :: tanHyp
      
      !! locals
      doubleprecision :: ValExp

      ValExp=EXP(Val)
      tanHyp=(ValExp - 1./ValExp)/(ValExp + 1./ValExp)
      RETURN
      ENDFUNCTION

