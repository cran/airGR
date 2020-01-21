!------------------------------------------------------------------------------
!    Subroutines relative to the annual GR4J model
!------------------------------------------------------------------------------
! TITLE   : airGR
! PROJECT : airGR
! FILE    : frun_CEMANEIGE.f
!------------------------------------------------------------------------------
! AUTHORS
! Original code: A. Valéry, P. Riboust
! Cleaning and formatting for airGR: L. Coron
! Further cleaning: G. Thirel
!------------------------------------------------------------------------------
! Creation date: 2011
! Last modified: 22/11/2019
!------------------------------------------------------------------------------
! REFERENCES
! Riboust, P., G. Thirel, N. Le Moine and P. Ribstein (2019), Revisiting a 
! simple degree-day model for integrating satellite data: implementation of 
! SWE-SCA hystereses. Journal of Hydrology and Hydromechanics, 
! doi:10.2478/johh-2018-0004, 67, 1, 70–81. 
!
! Valéry, A., V. Andréassian and C. Perrin (2014), "As simple as possible but 
! not simpler": what is useful in a temperature-based snow-accounting routine? 
! Part 1 - Comparison of six snow accounting routines on 380 catchments, 
! Journal of Hydrology, doi:10.1016/j.jhydrol.2014.04.059. 
!
! Valéry, A., V. Andréassian and C. Perrin (2014), "As simple as possible but 
! not simpler": What is useful in a temperature-based snow-accounting routine? 
! Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 
! 380 catchments, Journal of Hydrology, doi:10.1016/j.jhydrol.2014.04.058.
!------------------------------------------------------------------------------
! Quick description of public procedures:
!         1. frun_cemaneige
!------------------------------------------------------------------------------


      SUBROUTINE frun_cemaneige(LInputs,InputsPrecip, &
                                InputsFracSolidPrecip,InputsTemp, &
                                MeanAnSolidPrecip,NParam,Param,NStates, &
                                StateStart,IsHyst,NOutputs,IndOutputs, &
                                Outputs,StateEnd)
! Subroutine that runs the CemaNeige model at each time step, and stores the final states
! Inputs
!       LInputs      ! Integer, length of input and output series
!       InputsPrecip ! Vector of real, input series of total precipitation [mm/time step]
!       InputsFracSolidPrecip ! Vector of real, input series of fraction of solid precipitation [0-1]
!       InputsTemp   ! Vector of real, input series of air mean temperature [degC]
!       MeanAnSolidPrecip     ! Real, value of annual mean solid precip [mm/y]
!       NParam       ! Integer, number of model parameters
!       Param        ! Vector of real, parameter set
!       NStates      ! Integer, number of state variables
!       StateStart   ! Vector of real, state variables used when the model run starts (store levels [mm] and [-] and thresholds [mm])
!       IsHyst       ! integer, whether we should use the linear hysteresis [1] or not [0]
!       NOutputs     ! Integer, number of output series
!       IndOutputs   ! Vector of integer, indices of output series
! Outputs      
!       Outputs      ! Vector of real, output series
!       StateEnd     ! Vector of real, state variables at the end of the model run (store levels [mm] and [-] and thresholds [mm])



      !DEC$ ATTRIBUTES DLLEXPORT :: frun_cemaneige


      Implicit None

      !! dummies
      ! in
      integer, intent(in) :: LInputs,NParam,NStates,NOutputs
      doubleprecision, intent(in) :: MeanAnSolidPrecip
      doubleprecision, intent(in), dimension(LInputs) :: InputsPrecip
      doubleprecision, intent(in), dimension(LInputs) :: InputsFracSolidPrecip
      doubleprecision, intent(in), dimension(LInputs) :: InputsTemp
      doubleprecision, intent(in), dimension(NParam)  :: Param
      doubleprecision, intent(in), dimension(NStates) :: StateStart
      integer, intent(in) :: IsHyst ! 1 if linear hysteresis is used, 0 otherwise
      integer, intent(in), dimension(NOutputs) :: IndOutputs
      ! out
      doubleprecision, intent(out), dimension(NStates) :: StateEnd
      doubleprecision, intent(out), dimension(LInputs,NOutputs) :: Outputs

      !! locals
      logical :: IsHystBool         ! TRUE if linear hysteresis is used, FALSE otherwise
      doubleprecision :: CTG,Kf
      doubleprecision :: G,eTG,PliqAndMelt,dG,prct
      doubleprecision :: Tmelt,Gthreshold,MinSpeed,Gacc,Glocalmax
      doubleprecision :: Pliq,Psol,Gratio,PotMelt,Melt,Ginit
      integer :: I,K

      IF (IsHyst .EQ. 1) IsHystBool = .TRUE.
      IF (IsHyst .EQ. 0) IsHystBool = .FALSE.
      

      !--------------------------------------------------------------
      ! Initializations
      !--------------------------------------------------------------

      ! initialization of constants
      Tmelt=0.
      MinSpeed=0.1

      ! initialization of model states using StateStart
      G=StateStart(1)
      eTG=StateStart(2)
      Gratio=0.
      PliqAndMelt=0.

      ! setting parameter values
      CTG=Param(1)
      Kf=Param(2)
      IF (IsHystBool) THEN
        Gthreshold=StateStart(3)
        Glocalmax=StateStart(4)
        Gacc=Param(3)
        prct=Param(4)

        IF (Gthreshold .EQ. 0.) Gthreshold=prct*MeanAnSolidPrecip
        IF (Glocalmax  .EQ. 0.) Glocalmax=Gthreshold

      ELSE
        Gthreshold=0.9*MeanAnSolidPrecip
        Glocalmax=-999.999
        Gacc=-999.999
        prct=-999.999
      ENDIF

      ! initialization of model outputs
!      StateEnd = -999.999 !initialization made in R
!      Outputs = -999.999  !initialization made in R



      !--------------------------------------------------------------
      ! Time loop
      !--------------------------------------------------------------
      DO k=1,LInputs

        ! SolidPrecip and LiquidPrecip
        Pliq=(1.-InputsFracSolidPrecip(k))*InputsPrecip(k)
        Psol=InputsFracSolidPrecip(k)*InputsPrecip(k)

        ! Snow pack volume before melt
        Ginit=G
        G=G+Psol
        

        ! Snow pack thermal state before melt
        eTG=CTG*eTG + (1.-CTG)*InputsTemp(k)
        IF(eTG.GT.0.) eTG=0.

        ! Potential melt
        IF(eTG.EQ.0..AND.InputsTemp(k).GT.Tmelt) THEN
          PotMelt=Kf*(InputsTemp(k)-Tmelt)
          IF(PotMelt.GT.G) PotMelt=G
        ELSE
          PotMelt=0.
        ENDIF

        
        IF (IsHystBool) THEN
          IF (Potmelt.GT.0.) THEN
            IF (G.LT.Glocalmax.AND.Gratio.EQ.1.) Glocalmax=G ! Update in case of potential melt and G lower than Gseuil
            Gratio=MIN(G/Glocalmax,1.d0)
          ENDIF
        ELSE
          IF(G.LT.Gthreshold) THEN
            Gratio=G/Gthreshold
          ELSE
            Gratio=1.
          ENDIF
        ENDIF

        ! Actual melt
        Melt=((1.-MinSpeed)*Gratio+MinSpeed)*PotMelt

        ! Update of snow pack volume
        G=G-Melt
        IF (IsHystBool) THEN
          dG=G-Ginit ! Melt in case of negative dG, accumulation otherwise

        
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


        ! Water volume to pass to the hydrological model
        PliqAndMelt=Pliq+Melt

        ! Storage of outputs
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

      END SUBROUTINE

