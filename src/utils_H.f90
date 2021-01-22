!------------------------------------------------------------------------------
!    Subroutines relative to the hourly unit hydrographs
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : utils_H.f
!------------------------------------------------------------------------------
! AUTHORS
! Original code: Unknown soldier
! Cleaning and formatting for airGR: Coron, L.
! Further cleaning: Thirel, G.
!------------------------------------------------------------------------------
! Creation date: 2000
! Last modified: 22/11/2019
!------------------------------------------------------------------------------
! REFERENCES
!
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. UH1_H
!         2. UH2_H
!         3. SS1_H
!         4. SS2_H
!------------------------------------------------------------------------------


!**********************************************************************
      SUBROUTINE UH1_H(OrdUH1,C,D)
! Subroutine that computes the ordinates of the hourly GR unit hydrograph 
! UH1 using successive differences on the S curve SS1
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
! Outputs
!    OrdUH1: Vector of real, NH ordinates of the discrete hydrograph
!**********************************************************************

      Implicit None

      !! locals
      integer, parameter :: NH=480
      doubleprecision :: SS1_H
      integer :: I

      !! dummies
      ! in
      double precision, intent(in) :: C,D
      ! out
      double precision, dimension (NH), intent(out) :: OrdUH1
      
      DO I=1,NH
        OrdUH1(I)=SS1_H(I,C,D)-SS1_H(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


!**********************************************************************
      SUBROUTINE UH2_H(OrdUH2,C,D)
! Subroutine that computes the ordinates of the hourly GR unit hydrograph 
! UH2 using successive differences on the S curve SS2
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
! Outputs
!    OrdUH2: Vector of real, 2*NH ordinates of the discrete hydrograph
!**********************************************************************

      Implicit None

      !! locals
      integer, parameter :: NH=480
      doubleprecision :: SS2_H
      integer :: I

      !! dummies
      ! in
      double precision, intent(in) :: C,D
      ! out
      double precision, dimension (2*NH), intent(out) :: OrdUH2

      DO I =1,2*NH
        OrdUH2(I)=SS2_H(I,C,D)-SS2_H(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


!**********************************************************************
      FUNCTION SS1_H(I,C,D)
! Function that computes the values of the S curve (cumulative UH 
! curve) of GR unit hydrograph UH1
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
!    I ! Integer, time-step
! Outputs
!    SS1_H ! Real, value of the S curve for I
!**********************************************************************
 
      Implicit None

      !! dummies
      ! in
      doubleprecision, intent(in) :: C,D
      integer, intent(in) :: I
      ! out
      doubleprecision ::SS1_H
      
      !! locals
      integer :: FI

      FI=I
      IF(FI.LE.0) THEN
        SS1_H=0.
        RETURN
      ENDIF
      IF(FI.LT.C) THEN
        SS1_H=(FI/C)**D
        RETURN
      ENDIF
      SS1_H=1.
      ENDFUNCTION


!**********************************************************************
      FUNCTION SS2_H(I,C,D)
! Function that computes the values of the S curve (cumulative UH 
! curve) of GR unit hydrograph UH2
! Inputs
!    C ! Real, time constant
!    D ! Real, exponent
!    I ! Integer, time-step
! Outputs
!    SS2_H ! Real, value of the S curve for I
!**********************************************************************

      Implicit None

      !! dummies
      ! in
      doubleprecision, intent(in) :: C,D
      integer, intent(in) :: I
      ! out
      doubleprecision ::SS2_H
      
      !! locals
      integer :: FI

      FI=I
      IF(FI.LE.0) THEN
      SS2_H=0.
      RETURN
      ENDIF
      IF(FI.LE.C) THEN
        SS2_H=0.5*(FI/C)**D
        RETURN
      ENDIF
      IF(FI.LT.2.*C) THEN
        SS2_H=1.-0.5*(2.-FI/C)**D
        RETURN
      ENDIF
      SS2_H=1.
      ENDFUNCTION

