



      SUBROUTINE frun_cemaneige(
                                 !inputs
     &                             LInputs              , ! [integer] length of input and output series
     &                             InputsPrecip         , ! [double]  input series of total precipitation [mm/time step]
     &                             InputsFracSolidPrecip, ! [double]  input series of fraction of solid precipitation [0-1]
     &                             InputsTemp           , ! [double]  input series of air mean temperature [degC]
     &                             MeanAnSolidPrecip    , ! [double]  value of annual mean solid precip [mm/y]
     &                             NParam               , ! [integer] number of model parameter
     &                             Param                , ! [double]  parameter set
     &                             NStates              , ! [integer] number of state variables used for model initialisation = 4
     &                             StateStart           , ! [double]  state variables used when the model run starts
     &                             IsHyst               , ! [logical] whether we should use the linear hysteresis or not
     &                             NOutputs             , ! [integer] number of output series
     &                             IndOutputs           , ! [integer] indices of output series
                                 !outputs
     &                             Outputs              , ! [double]  output series
     &                             StateEnd             ) ! [double]  state variables at the end of the model run


      !DEC$ ATTRIBUTES DLLEXPORT :: frun_cemaneige


      Implicit None
      !### input and output variables
      integer, intent(in) :: LInputs,NParam,NStates,NOutputs
      doubleprecision, intent(in) :: MeanAnSolidPrecip
      doubleprecision, intent(in), dimension(LInputs) :: InputsPrecip
      doubleprecision, intent(in), dimension(LInputs) :: 
     & InputsFracSolidPrecip
      doubleprecision, intent(in), dimension(LInputs) :: InputsTemp
      doubleprecision, intent(in), dimension(NParam)  :: Param
      doubleprecision, intent(in), dimension(NStates) :: StateStart
      logical, intent(in) :: IsHyst                                              ! TRUE if linear hysteresis is used, FALSE otherwise
      doubleprecision, intent(out), dimension(NStates) :: StateEnd
      integer, intent(in), dimension(NOutputs) :: IndOutputs
      doubleprecision, intent(out), dimension(LInputs,NOutputs) :: 
     & Outputs

      !parameters, internal states and variables
      doubleprecision CTG,Kf
      doubleprecision G,eTG,PliqAndMelt,dG,prct
      doubleprecision Tmelt,Gthreshold,MinSpeed,Gacc,Glocalmax
      doubleprecision Pliq,Psol,Gratio,PotMelt,Melt,Ginit
      integer I,K

      !--------------------------------------------------------------
      !Initialisations
      !--------------------------------------------------------------

      !initialisation of constants
      Tmelt=0.
      MinSpeed=0.1

      !initialisation of model states using StateStart
      G=StateStart(1)
      eTG=StateStart(2)
      Gratio=0.
      PliqAndMelt=0.

      !setting parameter values
      CTG=Param(1)
      Kf=Param(2)
      IF (IsHyst) THEN
        Gthreshold=StateStart(3)
        Glocalmax=StateStart(4)
        Gacc=Param(3)
        prct=Param(4)

        IF (Gthreshold .EQ. 0.) Gthreshold=prct*MeanAnSolidPrecip
        IF (Glocalmax  .EQ. 0.) Glocalmax=Gthreshold

      ELSE
        Gthreshold=0.9*MeanAnSolidPrecip
      ENDIF

      !initialisation of model outputs
c      StateEnd = -999.999 !initialisation made in R
c      Outputs = -999.999  !initialisation made in R



      !--------------------------------------------------------------
      !Time loop
      !--------------------------------------------------------------
      DO k=1,LInputs

        !SolidPrecip and LiquidPrecip
        Pliq=(1.-InputsFracSolidPrecip(k))*InputsPrecip(k)
        Psol=InputsFracSolidPrecip(k)*InputsPrecip(k)

        !Snow pack volume before melt
        Ginit=G
        G=G+Psol
        

        !Snow pack thermal state before melt
        eTG=CTG*eTG + (1.-CTG)*InputsTemp(k)
        IF(eTG.GT.0.) eTG=0.

        !Potential melt
        IF(eTG.EQ.0..AND.InputsTemp(k).GT.Tmelt) THEN
          PotMelt=Kf*(InputsTemp(k)-Tmelt)
          IF(PotMelt.GT.G) PotMelt=G
        ELSE
          PotMelt=0.
        ENDIF

        
        IF (IsHyst) THEN
          IF (Potmelt.GT.0.) THEN
            IF (G.LT.Glocalmax.AND.Gratio.EQ.1.) Glocalmax=G !Update in case of potential melt and G lower than Gseuil
            Gratio=MIN(G/Glocalmax,1.d0)
          ENDIF
        ELSE
          IF(G.LT.Gthreshold) THEN
            Gratio=G/Gthreshold
          ELSE
            Gratio=1.
          ENDIF
        ENDIF

        !Actual melt
        Melt=((1.-MinSpeed)*Gratio+MinSpeed)*PotMelt

        !Update of snow pack volume
        G=G-Melt
        IF (IsHyst) THEN
          dG=G-Ginit !Melt in case of negative dG, accumulation otherwise

        
          IF (dG.GT.0.) THEN
            Gratio = MIN(Gratio+(Psol-Melt)/Gacc,1.d0) !Psol - Melt = dG
            IF (Gratio.EQ.1.) Glocalmax = Gthreshold
          ENDIF
          IF (dG.LT.0.) THEN
            Gratio=MIN(G/Glocalmax,1.d0)
          ENDIF
        ELSE
          IF(G.LT.Gthreshold) THEN
            Gratio=G/Gthreshold
          ELSE
            Gratio=1.
          ENDIF
        ENDIF


        !Water volume to pass to the hydrological model
        PliqAndMelt=Pliq+Melt

        !Storage of outputs
        DO I=1,NOutputs
          IF(IndOutputs(I).EQ.1)  Outputs(k,I)=Pliq          ! Pliq         ! observed liquid precipitation [mm/time step]
          IF(IndOutputs(I).EQ.2)  Outputs(k,I)=Psol          ! Psol         ! observed solid precipitation [mm/time step]
          IF(IndOutputs(I).EQ.3)  Outputs(k,I)=G             ! SnowPack     ! snow pack [mm]
          IF(IndOutputs(I).EQ.4)  Outputs(k,I)=eTG           ! ThermalState ! thermal state [°C]
          IF(IndOutputs(I).EQ.5)  Outputs(k,I)=Gratio        ! Gratio       ! Gratio [-]
          IF(IndOutputs(I).EQ.6)  Outputs(k,I)=PotMelt       ! PotMelt      ! potential snow melt [mm/time step]
          IF(IndOutputs(I).EQ.7)  Outputs(k,I)=Melt          ! Melt         ! melt [mm/time step]
          IF(IndOutputs(I).EQ.8)  Outputs(k,I)=PliqAndMelt   ! PliqAndMelt  ! liquid precipitation + melt [mm/time step]
          IF(IndOutputs(I).EQ.9)  Outputs(k,I)=InputsTemp(k) ! Temp         ! air temperature [°C]
          IF(IndOutputs(I).EQ.10) Outputs(k,I)=Gthreshold    ! Gthreshold   ! melt threshold [mm]
          IF(IndOutputs(I).EQ.11) Outputs(k,I)=Glocalmax     ! Glocalmax    ! local melt threshold for hysteresis [mm]
        ENDDO

      ENDDO

      StateEnd(1)=G
      StateEnd(2)=eTG
      StateEnd(3)=Gthreshold
      StateEnd(4)=Glocalmax

      RETURN

      ENDSUBROUTINE

