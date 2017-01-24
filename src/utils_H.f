

C**********************************************************************
      SUBROUTINE UH1_H(OrdUH1,C,D)
C Computation of ordinates of GR unit hydrograph UH2 using successive differences on the S curve SS1
C Inputs:
C    C: time constant
C    D: exponent
C Outputs:
C    OrdUH1: NH ordinates of discrete hydrograph
C**********************************************************************
      Implicit None
      INTEGER NH
      PARAMETER (NH=480)
      DOUBLEPRECISION OrdUH1(NH)
      DOUBLEPRECISION C,D,SS1_H
      INTEGER I

      DO I=1,NH
      OrdUH1(I)=SS1_H(I,C,D)-SS1_H(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


C**********************************************************************
      SUBROUTINE UH2_H(OrdUH2,C,D)
C Computation of ordinates of GR unit hydrograph UH2 using successive differences on the S curve SS2
C Inputs:
C    C: time constant
C    D: exponent
C Outputs:
C    OrdUH2: 2*NH ordinates of discrete hydrograph
C**********************************************************************
      Implicit None
      INTEGER NH
      PARAMETER (NH=480)
      DOUBLEPRECISION OrdUH2(2*NH)
      DOUBLEPRECISION C,D,SS2_H
      INTEGER I

      DO I =1,2*NH
      OrdUH2(I)=SS2_H(I,C,D)-SS2_H(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


C**********************************************************************
      FUNCTION SS1_H(I,C,D)
C Values of the S curve (cumulative HU curve) of GR unit hydrograph HU1
C Inputs:
C    C: time constant
C    D: exponent
C    I: time-step
C Outputs:
C    SS1: Values of the S curve for I
C**********************************************************************
      Implicit None
      DOUBLEPRECISION C,D,SS1_H
      INTEGER I,FI

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


C**********************************************************************
      FUNCTION SS2_H(I,C,D)
C Values of the S curve (cumulative HU curve) of GR unit hydrograph HU2
C Inputs:
C    C: time constant
C    D: exponent
C    I: time-step
C Outputs:
C    SS2: Values of the S curve for I
C**********************************************************************
      Implicit None
      DOUBLEPRECISION C,D,SS2_H
      INTEGER I,FI

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

