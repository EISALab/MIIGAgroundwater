      SUBROUTINE RES1AL(ISUM,LCIRES,LCIRSL,LCBRES,LCCRES,LCBBRE,
     1      LCHRES,LCHRSE,IN,IOUT,NRES,IRESCB,NRESOP,IRESPT,
     2      NPTS,NCOL,NROW,ISUMI)
C
C-----VERSION 1700 20JUN1996 RES1AL
C-----VERSION 12MAR1999 -- modified to use character strings longer
C-----than 4 characters and to change VBVL and VBNM DIMENSION to MSUM
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR RESERVOIRS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE AND INITIALIZE
      WRITE(IOUT,1)IN
    1 FORMAT(/,'RES1 -- RESERVOIR PACKAGE, VERSION 1, 1/15/95',
     1' INPUT READ FROM UNIT',I3)
C
C2------READ & PRINT NUMBER OF RESERVOIRS AND FLAGS FOR
C2------RESERVOIR OPTIONS
      READ(IN,2) NRES,IRESCB,NRESOP,IRESPT,NPTS
    2 FORMAT(5I10)
C
C2A-----CHECK TO SEE THAT NUMBER OF RESERVOIRS IS AT LEAST 1,
C2A-----PRINT VALUE
      IF(NRES.GT.0) THEN
       WRITE(IOUT,6) NRES
    6 FORMAT(1X,'TOTAL NUMBER OF RESERVOIRS: ',I3)
      ELSE
       WRITE (IOUT,7)
    7 FORMAT(1X,'ABORTING, NUMBER OF RESERVOIRS LESS THAN 1...')
      STOP
      ENDIF 
C
C2B-----CHECK FLAG FOR CELL-BY-CELL OUTPUT, PRINT VALUE
      IF(IRESCB.GT.0) WRITE(IOUT,10) IRESCB
 10   FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I3)
C2C-----CHECK TO SEE THAT RESERVOIR LAYER OPTION FLAG IS LEGAL,
C2C-----PRINT VALUE
      IF(NRESOP.GE.1.AND.NRESOP.LE.3)GO TO 200
C
C2C1----IF ILLEGAL PRINT A MESSAGE AND ABORT SIMULATION
      WRITE(IOUT,8)
    8 FORMAT(1X,'ILLEGAL OPTION CODE. SIMULATION ABORTING')
      STOP
C
C2C2----IF OPTION IS LEGAL PRINT OPTION CODE.
 200  CONTINUE
      IF(NRESOP.EQ.1) WRITE(IOUT,201)
  201 FORMAT(1X,'OPTION 1 -- RESERVOIR CONNECTED TO TOP LAYER')
      IF(NRESOP.EQ.2) WRITE(IOUT,202)
  202 FORMAT(1X,'OPTION 2 -- RESERVOIR CONNECTED TO ONE SPECIFIED',
     1        ' NODE IN EACH VERTICAL COLUMN')
      IF(NRESOP.EQ.3) WRITE(IOUT,203)
  203 FORMAT(1X,'OPTION 3 -- RESERVOIR CONNECTED TO HIGHEST',
     1        ' ACTIVE NODE IN EACH VERTICAL COLUMN')
C
C2D-----PRINT VALUE FOR RESERVIOR PRINT OPTION FLAG
      IF(IRESPT.GT.0) WRITE(IOUT,14) 
 14   FORMAT(1X,'RESERVOIR HEADS, AREAS, AND VOLUMES ',
     1 'WILL BE PRINTED EACH TIME STEP')
C2E-----PRINT NUMBER OF POINTS TO BE USED IN CALCULATING TABLE
C2E-----OF RESERVOIR STAGE VS. AREA AND VOLUME
      IF(NPTS.LT.1) THEN
       WRITE(IOUT,*) ' Table of reservoir areas and volumes ',
     1 'will not be calculated.'
      ELSE
       WRITE(IOUT,9) NPTS
 9     FORMAT(I5,' points will be used in constructing table of ',
     1  'reservoir areas and volumes.')
      ENDIF
C
C3------ALLOCATE SPACE FOR ARRAYS.
      LCRESR=ISUM
      NRCL=NROW*NCOL
      LCIRES=ISUMI
      ISUMI=ISUMI+NRCL
      LCIRSL=ISUMI
      ISUMI=ISUMI+NRCL
      LCBRES=ISUM
      ISUM=ISUM+NRCL
      LCCRES=ISUM
      ISUM=ISUM+NRCL
      LCBBRE=ISUM
      ISUM=ISUM+NRCL
      LCHRES=ISUM
      ISUM=ISUM+NRES
      LCHRSE=ISUM
      ISUM=ISUM+NRES+NRES
C
C4------PRINT AMOUNT OF SPACE USED BY RESERVOIR PACKAGE.
      ISP=ISUM-LCRESR
      WRITE (IOUT,30)ISP
 30   FORMAT(1X,I8,' ELEMENTS IN RX ARRAY ARE USED FOR RESERVOIRS')
      ISP=ISUMI-LCIRES
      WRITE (IOUT,40)ISP
 40   FORMAT(1X,I8,' ELEMENTS IN IR ARRAY ARE USED FOR RESERVOIRS')
C
C5------RETURN
      RETURN
      END
      SUBROUTINE RES1RP(IRES,IRESL,BRES,CRES,BBRES,HRESSE,IBOUND,
     1                   DELR,DELC,NRES,NRESOP,NPTS,NCOL,NROW,NLAY,
     2                   PERLEN,DELT,NSTP,TSMULT,IN,IOUT)
C
C
C-----VERSION 1700 27MAR1996 RES1RP
C     ******************************************************************
C     READ RESERVOIR LOCATIONS, LAYER, CONDUCTANCE, BOTTOM ELEVATION,
C      AND BED THICKNESS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME(5)
      LOGICAL FIRST
      SAVE FIRST
      DIMENSION IRES(NCOL,NROW),IRESL(NCOL,NROW),BRES(NCOL,NROW),
     1 CRES(NCOL,NROW),BBRES(NCOL,NROW),HRESSE(2,NRES),
     2 IBOUND(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW)
C
      DATA ANAME(1) /'      RESERVOIR LOCATION'/
      DATA ANAME(2) /'   RESERVOIR LAYER INDEX'/
      DATA ANAME(3) /'RESERVOIR LAND SURF ELEV'/
      DATA ANAME(4) /'  RES. BED VERT HYD COND'/
      DATA ANAME(5) /' RESERVOIR BED THICKNESS'/
      DATA FIRST/.TRUE./
C     ------------------------------------------------------------------
C
C
C1------READ INDICATOR ARRAY SHOWING LOCATIONS OF RESERVOIRS (IRES)
C
      IF(.NOT.FIRST) GO TO 70
      FIRST=.FALSE.
      KK=1
      CALL U2DINT(IRES,ANAME(1),NROW,NCOL,KK,IN,IOUT)
C2------VERIFY LOCATIONS EXIST FOR ALL RESERVOIRS
      DO 36 N=1,NRES
      NCELL=0
      DO 30 I=1,NROW
      DO 20 J=1,NCOL
      IF(IBOUND(J,I,1).LE.0) IRES(J,I)=0
      IF(IRES(J,I).EQ.N) NCELL=NCELL+1
   20 CONTINUE
   30 CONTINUE
      IF(NCELL.GT.0) THEN
       WRITE(IOUT,32) N,NCELL
   32  FORMAT(1X,'NUMBER OF CELLS IN RESERVOIR ',I2,':',I6)
      ELSE
       WRITE(IOUT,34)
   34 FORMAT(1X,'NO ACTIVE CELLS FOUND FOR RESERVOIR ',I2,'.',
     1 '  ABORTING...')
      ENDIF
   36 CONTINUE
C
C3------IF NRESOP=2 THEN A LAYER INDICATOR ARRAY IS NEEDED.
      IF (NRESOP.NE.2)GO TO 37
      CALL U2DINT(IRESL,ANAME(2),NROW,NCOL,0,IN,IOUT)
C3------READ IN BOTTOM ELEVATION, BED CONDUCTIVITY, AND BED THICKNESS
   37 CALL U2DREL(BRES,ANAME(3),NROW,NCOL,KK,IN,IOUT)
      CALL U2DREL(CRES,ANAME(4),NROW,NCOL,KK,IN,IOUT)
      CALL U2DREL(BBRES,ANAME(5),NROW,NCOL,KK,IN,IOUT)
C4------CONVERT RESERVOIR BED HYDRAULIC CONDUCTIVITY TO CONDUCTANCE
C4------BED THICKNESS TO ELEVATION OF BOTTOM OF RESERVOIR BED  
      DO 40 I=1,NROW
      DO 38 J=1,NCOL
      IF(IRES(J,I).LE.0) GO TO 38
      IF(IRES(J,I).GT.NRES) GO TO 38
       CRES(J,I)=CRES(J,I)*DELC(I)*DELR(J)/BBRES(J,I)
       BBRES(J,I)=BRES(J,I)-BBRES(J,I)
   38 CONTINUE
   40 CONTINUE
C5------MAKE STAGE-VOLUME TABLE FOR EACH RESERVOIR
      DO 60 N=1,NRES
C5A-----FIND MAX AND MIN BOTTOM ELEVATION
      ELMIN=9.99E10
      ELMAX=-9.99E10
      DO 44 I=1,NROW
      DO 42 J=1,NCOL
      IF(IRES(J,I).NE.N) GO TO 42
      IF(BRES(J,I).LT.ELMIN) ELMIN=BRES(J,I)
      IF(BRES(J,I).GT.ELMAX) ELMAX=BRES(J,I)
   42 CONTINUE
   44 CONTINUE
C5B-----CONSTRUCT TABLE
      WRITE(IOUT,46) N,ELMIN
   46 FORMAT(1X,'STAGE-VOLUME TABLE FOR RESERVOIR',I2,/,6X,
     1 'STAGE       VOLUME         AREA',/,
     2 3X,36('-'),/,1X,G10.5,2(11X,'0.0'))
      IF(NPTS.LT.1) GO TO 60
      DEL=(ELMAX-ELMIN)/FLOAT(NPTS)
      STAGE=ELMIN
      DO 56 NP=1,NPTS
      STAGE=STAGE+DEL
      VOL=0.0
      TAREA=0.0
      DO 50 I=1,NROW
      DO 48 J=1,NCOL
      IF(IRES(J,I).NE.N) GO TO 48
      IF(STAGE.GT.BRES(J,I))THEN
       AREA=DELR(J)*DELC(I)
       TAREA=TAREA+AREA
       VOL=VOL+AREA*(STAGE-BRES(J,I))
      ENDIF
   48 CONTINUE
   50 CONTINUE
      WRITE(IOUT,54) STAGE,VOL,TAREA
   54 FORMAT(1X,G10.5,2G14.5)
   56 CONTINUE
      WRITE(IOUT,58)
   58 FORMAT(1X,' ')
   60 CONTINUE
C
C6------READ START AND END HEADS FOR EACH RESERVOIR FOR CURRENT
C6------STRESS PERIOD
   70 DO 80 N=1,NRES
      READ(IN,64) HRESSE(1,N),HRESSE(2,N)
   64 FORMAT(2F10.0)
   80 CONTINUE
C
C7------RECOMPUTE LENGTH OF PERIOD, PERLEN, A LOCAL VARIABLE IN
C7------SUBROUTINE BAS1AD
      PERLEN=DELT*FLOAT(NSTP)
      IF(TSMULT.NE.1.) PERLEN=DELT*(1.-TSMULT**NSTP)/(1.-TSMULT)
C
C8------RETURN
      RETURN
      END
      SUBROUTINE RES1AD(HRES,HRESSE,IRES,BRES,DELR,DELC,NRES,IRESPT,
     2 NCOL,NROW,PERLEN,PERTIM,TOTIM,KKSTP,KKPER,IOUT)
C
C
C-----VERSION 1700 20JUN1996 RES1AD
C     ******************************************************************
C     GET RESERVOIR HEADS FOR CURRENT TIME STEP
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION HRES(NRES),HRESSE(2,NRES),IRES(NCOL,NROW),
     1          BRES(NCOL,NROW),DELR(NCOL),DELC(NROW)
C     ------------------------------------------------------------------
C
C1------COMPUTE PROPORTION OF STRESS PERIOD TO END OF THIS TIME STEP
      FRAC=PERTIM/PERLEN
C
C2------PROCESS EACH RESERVOIR
      DO 10 N=1,NRES
      HSTART=HRESSE(1,N)
      HEND=HRESSE(2,N)
C
C3------COMPUTE HEAD FOR RESERVOIR N BY LINEAR INTERPOLATION.
      HRES(N)=HSTART+(HEND-HSTART)*FRAC
  10  CONTINUE
      IF(IRESPT.LE.0) RETURN
C4------MAKE A TABLE OF HEAD, AREA AND VOLUME FOR EACH RESERVOIR
      WRITE(IOUT,20) KKPER,KKSTP,TOTIM
 20   FORMAT(1X,'RESERVOIR CONDITIONS FOR STRESS PERIOD ',I3,', STEP ',
     1 I3,' TIME ',G12.5,/,2X,'RESERVOIR   HEAD',9X,'AREA',8X,'VOLUME',
     2 /,2X,46('-'))
      DO 60 N=1,NRES
      STAGE=HRES(N)
      VOL=0.0
      TAREA=0.0
      DO 50 I=1,NROW
      DO 48 J=1,NCOL
      IF(IRES(J,I).NE.N) GO TO 48
      IF(STAGE.GT.BRES(J,I))THEN
       AREA=DELR(J)*DELC(I)
       TAREA=TAREA+AREA
       VOL=VOL+AREA*(STAGE-BRES(J,I))
      ENDIF
   48 CONTINUE
   50 CONTINUE
      WRITE(IOUT,54) N,STAGE,TAREA,VOL
   54 FORMAT(3X,I5,3X,3G12.5)
   60 CONTINUE
C
C5------RETURN
      RETURN
      END
      SUBROUTINE RES1FM(IRES,IRESL,BRES,CRES,BBRES,HRES,IBOUND,
     1     HNEW,HCOF,RHS,NRES,NRESOP,NCOL,NROW,NLAY)
C
C-----VERSION 1700 27MAR1996 RES1FM
C     ******************************************************************
C     ADD RESERVOIR TERMS TO RHS AND HCOF
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION HNEW
      DIMENSION IRES(NCOL,NROW),IRESL(NCOL,NROW),BRES(NCOL,NROW),
     1 CRES(NCOL,NROW),BBRES(NCOL,NROW),HRES(NRES),
     2 IBOUND(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),
     3 HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
C
C1------PROCESS EACH ACTIVE RESERVOIR CELL
      DO 100 I=1,NROW
      DO 90 J=1,NCOL
      NR=IRES(J,I)
      IF(NR.LE.0) GO TO 90
      IF(NR.GT.NRES) GO TO 90
      IR=I
      IC=J
C
C2------FIND LAYER NUMBER FOR RESERVOIR CELL
      IF(NRESOP.EQ.1) THEN
       IL=1
      ELSE IF(NRESOP.EQ.2) THEN
       IL=IRESL(IC,IR)
      ELSE
       DO 60 K=1,NLAY
       IL=K
C2A-----UPPERMOST ACTIVE CELL FOUND, SAVE LAYER INDEX IN 'IL'
       IF(IBOUND(IC,IR,IL).GT.0) GO TO 70
C2B-----SKIP THIS CELL IF VERTICAL COLUMN CONTAINS A CONSTANT-
C2B-----HEAD CELL ABOVE RESERVOIR LOCATION
       IF(IBOUND(IC,IR,IL).LT.0) GO TO 90
   60  CONTINUE
       GO TO 90
      ENDIF
C
C3------IF THE CELL IS EXTERNAL SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 90
C
C4------IF RESERVOIR STAGE IS BELOW RESERVOIR BOTTOM, SKIP IT
   70 HR=HRES(NR)
      IF(HR.LE.BRES(IC,IR))  GO TO 90
C5------SINCE RESERVOIR IS ACTIVE AT THIS LOCATION,
C5------CELL IS INTERNAL GET THE RESERVOIR DATA.
      CR=CRES(IC,IR)
      RBOT=BBRES(IC,IR)
      HHNEW=HNEW(IC,IR,IL)
C
C6------COMPARE AQUIFER HEAD TO BOTTOM OF RESERVOIR BED.
      IF(HHNEW.LE.RBOT) GO TO 80
C
C7------SINCE HEAD>BOTTOM ADD TERMS TO RHS AND HCOF.
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-CR*HR
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-CR
      GO TO 90
C
C8------SINCE HEAD<BOTTOM ADD TERM ONLY TO RHS.
   80 RHS(IC,IR,IL)=RHS(IC,IR,IL)-CR*(HR-RBOT)
   90 CONTINUE
  100 CONTINUE
C
C9------RETURN
      RETURN
      END
      SUBROUTINE RES1BD(IRES,IRESL,BRES,CRES,BBRES,HRES,IBOUND,HNEW,
     1      BUFF,VBVL,VBNM,MSUM,KSTP,KPER,NRES,NRESOP,NCOL,NROW,NLAY,
     2      DELT,IRESCB,ICBCFL,IOUT)
C-----VERSION 1700 27MAR1996 RES1BD
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR RESERVOIRS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 VBNM(MSUM),TEXT
      DOUBLE PRECISION HNEW
      DIMENSION IRES(NCOL,NROW),IRESL(NCOL,NROW),BRES(NCOL,NROW),
     1    CRES(NCOL,NROW),BBRES(NCOL,NROW),HRES(NRES),
     1    IBOUND(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),VBVL(4,MSUM),
     2    BUFF(NCOL,NROW,NLAY)
      DATA TEXT/' RESERV. LEAKAGE'/
C     ------------------------------------------------------------------
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      IBD=0
      RATIN=0.
      RATOUT=0.
C
C2------TEST TO SEE IF CELL-BY-CELL FLOW TERMS ARE NEEDED.
      IF(ICBCFL.EQ.0 .OR. IRESCB.LE.0 ) GO TO 10
C
C2A-----CELL-BY-CELL FLOW TERMS ARE NEEDED SET IBD AND CLEAR BUFFER.
      IBD=1
      DO 5 IL=1,NLAY
      DO 5 IR=1,NROW
      DO 5 IC=1,NCOL
      BUFF(IC,IR,IL)=0.
    5 CONTINUE
C
C3------FOR EACH RESERVOIR REACH ACCUMULATE RESERVOIR FLOW (STEPS 5-15)
 10   DO 200 I=1,NROW
      DO 190 J=1,NCOL
      NR=IRES(J,I)
      IF(NR.LE.0) GO TO 190
      IF(NR.GT.NRES) GO TO 190
      IR=I
      IC=J
C
C4------FIND LAYER NUMBER FOR RESERVOIR CELL
      IF(NRESOP.EQ.1) THEN
       IL=1
      ELSE IF(NRESOP.EQ.2) THEN
       IL=IRESL(IC,IR)
      ELSE
       DO 60 K=1,NLAY
       IL=K
C4A-----UPPERMOST ACTIVE CELL FOUND, SAVE LAYER INDEX IN 'IL'
       IF(IBOUND(IC,IR,IL).GT.0) GO TO 70
C4B-----SKIP THIS CELL IF VERTICAL COLUMN CONTAINS A CONSTANT-
C4B-----HEAD CELL ABOVE RESERVOIR LOCATION
       IF(IBOUND(IC,IR,IL).LT.0) GO TO 190
   60  CONTINUE
       GO TO 190
      ENDIF
C
C5------IF THE CELL IS EXTERNAL SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 190
C
C6------IF RESERVOIR STAGE IS BELOW RESERVOIR BOTTOM, SKIP IT
 70   HR=HRES(NR)
      IF(HR.LE.BRES(IC,IR))  GO TO 190
C7------SINCE RESERVOIR IS ACTIVE AT THIS LOCATION, 
C7------GET THE RESERVOIR DATA.
      CR=CRES(IC,IR)
      RBOT=BBRES(IC,IR)
      HHNEW=HNEW(IC,IR,IL)
C
C8------COMPUTE RATE OF FLOW BETWEEN GROUND-WATER SYSTEM AND RESERVOIR.
C
C8A-----GROUND-WATER HEAD > BOTTOM THEN RATE=CR*(HR-HNEW).
      IF(HHNEW.GT.RBOT)RATE=CR*(HR-HHNEW)
C
C8B-----GROUND-WATER HEAD < BOTTOM THEN RATE=CR*(HR-RBOT)
      IF(HHNEW.LE.RBOT)RATE=CR*(HR-RBOT)
C
C9-------IF C-B-C FLOW TERMS ARE TO BE SAVED THEN ADD RATE TO BUFFER.
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C10-----SEE IF FLOW IS INTO GROUND-WATER SYSTEM OR INTO RESERVOIR.
      IF(RATE)94,190,96
C
C11-----GROUND-WATER SYSTEM IS DISCHARGING TO RESERVOIR
C11-----SUBTRACT RATE FROM RATOUT.
   94 RATOUT=RATOUT-RATE
      GO TO 190
C
C12-----GROUND-WATER SYSTEM IS RECHARGED FROM RESERVOIR
C12-----ADD RATE TO RATIN.
   96 RATIN=RATIN+RATE
  190 CONTINUE
  200 CONTINUE
C
C13-----IF C-B-C FLOW TERMS WILL BE SAVED CALL UBUDSV TO RECORD THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IRESCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C14-----MOVE RATES,VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
      VBVL(3,MSUM)=RATIN
      VBVL(4,MSUM)=RATOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
      VBNM(MSUM)=TEXT
C
C15-----INCREMENT BUDGET TERM COUNTER
      MSUM=MSUM+1
C
C16-----RETURN
      RETURN
      END
