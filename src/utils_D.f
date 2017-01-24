C**********************************************************************
      SUBROUTINE UH1(OrdUH1,C,D)
C Computation of ordinates of GR unit hydrograph UH1 using successive differences on the S curve SS1
C Inputs:
C    C: time constant
C    D: exponent
C Outputs:
C    OrdUH1: NH ordinates of discrete hydrograph
C**********************************************************************
      Implicit None
      INTEGER NH
      PARAMETER (NH=20)
      DOUBLEPRECISION OrdUH1(NH)
      DOUBLEPRECISION C,D,SS1
      INTEGER I

      DO I=1,NH
      OrdUH1(I)=SS1(I,C,D)-SS1(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


C**********************************************************************
      SUBROUTINE UH2(OrdUH2,C,D)
C Computation of ordinates of GR unit hydrograph HU2 using successive differences on the S curve SS2
C Inputs:
C    C: time constant
C    D: exponent
C Outputs:
C    OrdUH2: 2*NH ordinates of discrete hydrograph
C**********************************************************************
      Implicit None
      INTEGER NH
      PARAMETER (NH=20)
      DOUBLEPRECISION OrdUH2(2*NH)
      DOUBLEPRECISION C,D,SS2
      INTEGER I

      DO I =1,2*NH
      OrdUH2(I)=SS2(I,C,D)-SS2(I-1,C,D)
      ENDDO
      ENDSUBROUTINE


C**********************************************************************
      FUNCTION SS1(I,C,D)
C Values of the S curve (cumulative HU curve) of GR unit hydrograph UH1
C Inputs:
C    C: time constant
C    D: exponent
C    I: time-step
C Outputs:
C    SS1: Values of the S curve for I
C**********************************************************************
      Implicit None
      DOUBLEPRECISION C,D,SS1
      INTEGER I,FI

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


C**********************************************************************
      FUNCTION SS2(I,C,D)
C Values of the S curve (cumulative HU curve) of GR unit hydrograph UH2
C Inputs:
C    C: time constant
C    D: exponent
C    I: time-step
C Outputs:
C    SS2: Values of the S curve for I
C**********************************************************************
      Implicit None
      DOUBLEPRECISION C,D,SS2
      INTEGER I,FI

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


C**********************************************************************
      FUNCTION tanHyp(Val)
C Computation of hyperbolic tangent
C**********************************************************************
      Implicit None
      DOUBLEPRECISION Val,ValExp,tanHyp

      ValExp=EXP(Val)
      tanHyp=(ValExp - 1./ValExp)/(ValExp + 1./ValExp)
      RETURN
      ENDFUNCTION

