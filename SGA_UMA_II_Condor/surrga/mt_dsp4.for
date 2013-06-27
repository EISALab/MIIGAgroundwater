C
      SUBROUTINE DSP4AL(INDSP,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & LCAL,LCTRPT,LCTRPV,LCDM,LCDXX,LCDXY,LCDXZ,LCDYX,LCDYY,LCDYZ,
     & LCDZX,LCDZY,LCDZZ)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED IN THE DISPERSION
C (DSP) PACKAGE.
C **********************************************************************
C last modified: 08-12-2001
C
      IMPLICIT  NONE
      INTEGER   INDSP,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,ISOLD,ISOLD2,
     &          LCDXX,LCDXY,LCDXZ,LCDYX,LCDYY,LCDYZ,LCDZX,LCDZY,LCDZZ,
     &          LCAL,LCTRPT,LCTRPV,LCDM,NODES,ISUMX,ISUMIX
C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1030) INDSP
 1030 FORMAT(1X,'DSP4 -- DISPERSION PACKAGE,',
     & ' VERSION 4, AUGUST 2001, INPUT READ FROM UNIT',I3)
C
C--ALLOCATE SPACE FOR ARRAYS
      ISOLD=ISUM
      ISOLD2=ISUM2
      NODES=NCOL*NROW*NLAY
C
C--REAL ARRAYS
      LCAL=ISUM
      ISUM=ISUM+NODES
      LCTRPT=ISUM
      ISUM=ISUM+NLAY
      LCTRPV=ISUM
      ISUM=ISUM+NLAY
      LCDM=ISUM
      ISUM=ISUM+NLAY
      LCDXX=ISUM
      ISUM=ISUM+NODES
      LCDXY=ISUM
      ISUM=ISUM+NODES
      LCDXZ=ISUM
      ISUM=ISUM+NODES
      LCDYX=ISUM
      ISUM=ISUM+NODES
      LCDYY=ISUM
      ISUM=ISUM+NODES
      LCDYZ=ISUM
      ISUM=ISUM+NODES
      LCDZX=ISUM
      ISUM=ISUM+NODES
      LCDZY=ISUM
      ISUM=ISUM+NODES
      LCDZZ=ISUM
      ISUM=ISUM+NODES
C
C--CHECK WHETHER ARRAYS X AND IX ARE DIMENSIONED LARGE ENOUGH
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I10,' ELEMENTS OF THE  X ARRAY USED BY THE DSP PACKAGE'
     & /1X,I10,' ELEMENTS OF THE IX ARRAY USED BY THE DSP PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP4RP(IN,IOUT,NCOL,NROW,NLAY,AL,TRPT,TRPV,DMCOEF)
C ********************************************************************
C THIS SUBROUTINE READS AND PREPARES INPUT DATA NEEDED BY THE
C DISPERSION (DSP) PACKAGE.
C*********************************************************************
C last modified: 08-12-2001
C
      IMPLICIT  NONE
      INTEGER   IN,IOUT,NCOL,NROW,NLAY,K
      REAL      AL,TRPT,TRPV,DMCOEF
      CHARACTER ANAME*24
      DIMENSION AL(NCOL,NROW,NLAY),TRPT(NLAY),TRPV(NLAY),DMCOEF(NLAY)
C
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'DISPERSION PARAMETERS'/1X,21('-')/)
C
C--CALL RARRAY TO READ LONGITUDINAL DISPERSIVITY ONE LAYER A TIME
      DO K=1,NLAY
        ANAME='LONG. DISPERSIVITY (AL)'
        CALL RARRAY(AL(1,1,K),ANAME,NROW,NCOL,K,IN,IOUT)
      ENDDO
C
C--CALL RARRAY TO READ RATIO OF HORIZONAL TRANSVERSE
C--TO LONGITUDINAL DISPERSIVITY ONE VALUE PER LAYER
      ANAME='H. TRANS./LONG. DISP.'
      CALL RARRAY(TRPT(1),ANAME,1,NLAY,0,IN,IOUT)
C
C--CALL RARRAY TO READ RATIO OF VERTICAL TRANSVERSE TO
C--LONGITUDINAL DISPERSIVITY ONE VALUE PER LAYER
      ANAME='V. TRANS./LONG. DISP.'
      CALL RARRAY(TRPV(1),ANAME,1,NLAY,0,IN,IOUT)
C
C--CALL RARRAY TO READ EFFECTIVE MOLECULAR DIFFUSION COEFFICIENT
C--ONE VALUE PER LAYER
      ANAME='DIFFUSION COEFFICIENT'
      CALL RARRAY(DMCOEF(1),ANAME,1,NLAY,0,IN,IOUT)
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP4CF(IOUT,KSTP,KPER,NCOL,NROW,NLAY,ICBUND,PRSITY,
     & DELR,DELC,DZ,QX,QY,QZ,ALPHAL,TRPT,TRPV,DMCOEF,DTDISP,
     & DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,IFMTDP)
C***********************************************************************
C THIS SUBROUTINE CALCULATES COMPONENTS OF THE HYDRODYNAMIC DISPERSION
C COEFFICIENT (Dij) AT CELL INTERFACES AND DETERMINES THE MAXIMUM TIME
C INTERVAL WHICH MEETS THE STABILITY CRITERIA FOR SOLVING THE EXPLICIT
C FINITE DIFFERENCE DISPERSION-DIFFUSION EQUATION.
C NOTE: Dij IS CALCULATED USING DARCY VELOCITY COMPONENTS INSTEAD OF
C ====  LINEAR VELOCITY COMPONENTS.  TO CONVERT THIS APPARENT Dij TO
C       ACTUAL DISPERSION COEFFICIENT, DIVIDE IT BY POROSITY.
C***********************************************************************
C last modified: 08-12-2001
C
      IMPLICIT  NONE
      INTEGER   IOUT,KSTP,KPER,NCOL,NROW,NLAY,K,I,J,ICBUND,KM1,IM1,JM1,
     &          KP1,IP1,JP1,JD,ID,KD,IFMTDP
      REAL      DELR,DELC,DZ,QX,QY,QZ,PRSITY,DXX,DXY,DXZ,DYX,DYY,DYZ,
     &          DZX,DZY,DZZ,V,WW,PF,AL,AT,AV,DM,VX,VY,VZ,ALPHAL,
     &          TRPT,TRPV,DMCOEF,DTDISP,TD,AREA
      CHARACTER TEXT*16
      DIMENSION ICBUND(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     & DZ(NCOL,NROW,NLAY),PRSITY(NCOL,NROW,NLAY),
     & ALPHAL(NCOL,NROW,NLAY),TRPT(NLAY),TRPV(NLAY),DMCOEF(NLAY),
     & QX(NCOL,NROW,NLAY), QY(NCOL,NROW,NLAY), QZ(NCOL,NROW,NLAY),
     & DXX(NCOL,NROW,NLAY),DXY(NCOL,NROW,NLAY),DXZ(NCOL,NROW,NLAY),
     & DYX(NCOL,NROW,NLAY),DYY(NCOL,NROW,NLAY),DYZ(NCOL,NROW,NLAY),
     & DZX(NCOL,NROW,NLAY),DZY(NCOL,NROW,NLAY),DZZ(NCOL,NROW,NLAY)
C
C--FOR THE COMPONENTS ALONG THE X DIRECTION
C  ========================================
      IF(NCOL.LT.2) GOTO 100
C
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
            IF(ICBUND(J,I,K).EQ.0.OR.ICBUND(JP1,I,K).EQ.0) GOTO 80
C
C--CALCULATE VALUES AT INTERFACES
            WW=DELR(JP1)/(DELR(J)+DELR(JP1))
            PF=1.
            AL=ALPHAL(J,I,K)*WW+ALPHAL(JP1,I,K)*(1.-WW)
            AT=AL*TRPT(K)
            AV=AL*TRPV(K)
            DM=DMCOEF(K)*(PRSITY(J,I,K)*WW+PRSITY(JP1,I,K)*(1.-WW))
            VX=QX(J,I,K)
            IF(NROW.GT.1) THEN
              VY=0.5*(QY(J,IM1,K)+QY(J,I,K))*WW
     &         +0.5*(QY(JP1,IM1,K)+QY(JP1,I,K))*(1.-WW)
            ELSE
              VY=0
            ENDIF
            IF(NLAY.GT.1) THEN
              VZ=0.5*(QZ(J,I,KM1)+QZ(J,I,K))*WW
     &         +0.5*(QZ(JP1,I,KM1)+QZ(JP1,I,K))*(1.-WW)
            ELSE
              VZ=0
            ENDIF
            V=SQRT(VX*VX+VY*VY+VZ*VZ)
C
C--CALCULATE DISPERSION COEFFICIENTS
            IF(V.EQ.0) THEN
              DXX(J,I,K)=DM
              IF(NROW.GT.1) DXY(J,I,K)=0
              IF(NLAY.GT.1) DXZ(J,I,K)=0
            ELSE
              DXX(J,I,K)=AL*VX*VX/V/PF+AT*VY*VY/V/PF+AV*VZ*VZ/V/PF+DM
              IF(NROW.GT.1) DXY(J,I,K)=(AL-AT)*VX*VY/V/PF
              IF(NLAY.GT.1) DXZ(J,I,K)=(AL-AV)*VX*VZ/V/PF
            ENDIF
C
   80     ENDDO
        ENDDO
      ENDDO
C
C--FOR THE COMPONENTS ALONG THE Y DIRECTION
C  ========================================
  100 IF(NROW.LT.2) GOTO 200
C
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO J=1,NCOL
          JP1=MIN(J+1,NCOL)
          JM1=MAX(1,J-1)
          DO I=1,NROW
            IP1=MIN(I+1,NROW)
            IM1=MAX(1,I-1)
            IF(ICBUND(J,I,K).EQ.0.OR.ICBUND(J,IP1,K).EQ.0) GOTO 180
C
C--CALCULATE VALUES AT INTERFACES
            WW=DELC(IP1)/(DELC(I)+DELC(IP1))
            PF=1.
            AL=ALPHAL(J,I,K)*WW+ALPHAL(J,IP1,K)*(1.-WW)
            AT=AL*TRPT(K)
            AV=AL*TRPV(K)
            DM=DMCOEF(K)*(PRSITY(J,I,K)*WW+PRSITY(J,IP1,K)*(1.-WW))
            VY=QY(J,I,K)
            IF(NCOL.GT.1) THEN
              VX=0.5*(QX(J,I,K)+QX(JM1,I,K))*WW
     &         +0.5*(QX(J,IP1,K)+QX(JM1,IP1,K))*(1.-WW)
            ELSE
              VX=0
            ENDIF
            IF(NLAY.GT.1) THEN
              VZ=0.5*(QZ(J,I,K)+QZ(J,I,KM1))*WW
     &         +0.5*(QZ(J,IP1,K)+QZ(J,IP1,KM1))*(1.-WW)
            ELSE
              VZ=0
            ENDIF
            V=SQRT(VX*VX+VY*VY+VZ*VZ)
C
C--CALCULATE DISPERSION COEFFICIENTS
            IF(V.EQ.0) THEN
              DYY(J,I,K)=DM
              IF(NCOL.GT.1) DYX(J,I,K)=0
              IF(NLAY.GT.1) DYZ(J,I,K)=0
            ELSE
              DYY(J,I,K)=AL*VY*VY/V/PF+AT*VX*VX/V/PF+AV*VZ*VZ/V/PF+DM
              IF(NCOL.GT.1) DYX(J,I,K)=(AL-AT)*VY*VX/V/PF
              IF(NLAY.GT.1) DYZ(J,I,K)=(AL-AV)*VY*VZ/V/PF
            ENDIF
C
  180     ENDDO
        ENDDO
      ENDDO
C
C--FOR THE COMPONENTS ALONG THE Z DIRECTION
C  ========================================
  200 IF(NLAY.LT.2) GOTO 300
C
      DO I=1,NROW
        IP1=MIN(I+1,NROW)
        IM1=MAX(1,I-1)
        DO J=1,NCOL
          JP1=MIN(J+1,NCOL)
          JM1=MAX(1,J-1)
          DO K=1,NLAY
            KP1=MIN(K+1,NLAY)
            KM1=MAX(1,K-1)
            IF(ICBUND(J,I,K).EQ.0.OR.ICBUND(J,I,KP1).EQ.0) GOTO 280
C
C--CALCULATE VALUES AT INTERFACES
            WW=DZ(J,I,KP1)/(DZ(J,I,K)+DZ(J,I,KP1))
            PF=1.
            AL=ALPHAL(J,I,K)*WW+ALPHAL(J,I,KP1)*(1.-WW)
            AT=ALPHAL(J,I,K)*TRPT(K)*WW+
     &       ALPHAL(J,I,KP1)*TRPT(KP1)*(1.-WW)
            AV=ALPHAL(J,I,K)*TRPV(K)*WW+
     &       ALPHAL(J,I,KP1)*TRPV(KP1)*(1.-WW)
            DM=(DMCOEF(K)*WW+DMCOEF(KP1)*(1.-WW))*
     &       (PRSITY(J,I,K)*WW+PRSITY(J,I,KP1)*(1.-WW))
            VZ=QZ(J,I,K)
            IF(NCOL.GT.1) THEN
              VX=0.5*(QX(JM1,I,K)+QX(J,I,K))*WW
     &         +0.5*(QX(JM1,I,KP1)+QX(J,I,KP1))*(1.-WW)
            ELSE
              VX=0
            ENDIF
            IF(NROW.GT.1) THEN
              VY=0.5*(QY(J,IM1,K)+QY(J,I,K))*WW
     &         +0.5*(QY(J,IM1,KP1)+QY(J,I,KP1))*(1.-WW)
            ELSE
              VY=0
            ENDIF
            V=SQRT(VX*VX+VY*VY+VZ*VZ)
C
C--CALCULATE DISPERSION COEFFICIENTS
            IF(V.EQ.0) THEN
              DZZ(J,I,K)=DM
              IF(NCOL.GT.1) DZX(J,I,K)=0
              IF(NROW.GT.1) DZY(J,I,K)=0
            ELSE
              DZZ(J,I,K)=AL*VZ*VZ/V/PF+AV*VX*VX/V/PF+AV*VY*VY/V/PF+DM
              IF(NCOL.GT.1) DZX(J,I,K)=(AL-AV)*VZ*VX/V/PF
              IF(NROW.GT.1) DZY(J,I,K)=(AL-AV)*VZ*VY/V/PF
            ENDIF
C
  280     ENDDO
        ENDDO
      ENDDO
C
  300 CONTINUE
C
C--SET DISPERSION COEFFICIENTS TO ZERO IN INACTIVE CELLS
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
            IF(ICBUND(J,I,K).NE.0) GOTO 13
C
            IF(NCOL.GT.1) THEN
              DXX(J  ,I,K)=0.
              DXX(JM1,I,K)=0.
              IF(NROW.GT.1) THEN
                DXY(J,  IM1,K)=0.
                DXY(J,  I,  K)=0.
                DXY(J,  IP1,K)=0.
                DXY(JM1,IM1,K)=0.
                DXY(JM1,I,  K)=0.
                DXY(JM1,IP1,K)=0.
              ENDIF
              IF(NLAY.GT.1) THEN
                DXZ(J,  I,KM1)=0.
                DXZ(J,  I,K  )=0.
                DXZ(J,  I,KP1)=0.
                DXZ(JM1,I,KM1)=0.
                DXZ(JM1,I,K  )=0.
                DXZ(JM1,I,KP1)=0.
              ENDIF
            ENDIF
C
            IF(NROW.GT.1) THEN
              DYY(J,IM1,K)=0.
              DYY(J,  I,K)=0.
              IF(NCOL.GT.1) THEN
                DYX(JM1,I  ,K)=0.
                DYX(J  ,I  ,K)=0.
                DYX(JP1,I  ,K)=0.
                DYX(JM1,IM1,K)=0.
                DYX(J  ,IM1,K)=0.
                DYX(JP1,IM1,K)=0.
              ENDIF
              IF(NLAY.GT.1) THEN
                DYZ(J,I  ,KM1)=0.
                DYZ(J,I  ,K  )=0.
                DYZ(J,I  ,KP1)=0.
                DYZ(J,IM1,KM1)=0.
                DYZ(J,IM1,K  )=0.
                DYZ(J,IM1,KP1)=0.
              ENDIF
            ENDIF
C
            IF(NLAY.GT.1) THEN
              DZZ(J,I,K  )=0.
              DZZ(J,I,KM1)=0.
              IF(NCOL.GT.1) THEN
                DZX(JM1,I,K)=0.
                DZX(J  ,I,K)=0.
                DZX(JP1,I,K)=0.
                DZX(JM1,I,KM1)=0.
                DZX(J  ,I,KM1)=0.
                DZX(JP1,I,KM1)=0.
              ENDIF
              IF(NROW.GT.1) THEN
                DZY(J,IM1,K  )=0.
                DZY(J,I  ,K  )=0.
                DZY(J,IP1,K  )=0.
                DZY(J,IM1,KM1)=0.
                DZY(J,I  ,KM1)=0.
                DZY(J,IP1,KM1)=0.
              ENDIF
            ENDIF
C
   13     ENDDO
        ENDDO
      ENDDO
C
C--CALCULATE MAXIMUM TIME INCREMENT WHICH MEETS STABILITY CRITERION
C--FOR SOLVING THE EXPLICIT FINITE-DIFFERENCE DISPERSION EQUATION.
      DTDISP=1.E30
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
C
        IF(ICBUND(J,I,K).NE.0) THEN
          TD=0.
          IF(NCOL.GT.1.AND.J.LT.NCOL) THEN
            IF(ICBUND(J+1,I,K).NE.0)
     &       TD=TD+DXX(J,I,K)/(0.5*DELR(J)+0.5*DELR(J+1))**2
          ENDIF
          IF(NROW.GT.1.AND.I.LT.NROW) THEN
            IF(ICBUND(J,I+1,K).NE.0)
     &       TD=TD+DYY(J,I,K)/(0.5*DELC(I)+0.5*DELC(I+1))**2
          ENDIF
          IF(NLAY.GT.1.AND.K.LT.NLAY) THEN
            IF(ICBUND(J,I,K+1).NE.0)
     &       TD=TD+DZZ(J,I,K)/(0.5*DZ(J,I,K)+0.5*DZ(J,I,K+1))**2
          ENDIF
          IF(TD.GT.0) THEN
            TD=0.5/TD*PRSITY(J,I,K)
            IF(TD.LT.DTDISP) THEN
              DTDISP=TD
              JD=J
              ID=I
              KD=K
            ENDIF
          ENDIF
        ENDIF
C
      ENDDO
      ENDDO
      ENDDO
C
C--PRINT OUT INFORMATION ON DTDISP
      WRITE(IOUT,500) DTDISP,KD,ID,JD
  500 FORMAT(/1X,'MAXIMUM STEPSIZE WHICH MEETS STABILITY CRITERION',
     & ' OF THE DISPERSION TERM'/1X,'=',G11.4,
     & '(WHEN MIN. R.F.=1)  AT K=',I4,', I=',I4,
     & ', J=',I4)
C
C--PRINT OUT DISPERSION COEFFICIENT IF REQUESTED
      IF(IFMTDP.EQ.0) GOTO 980
C
      IF(NCOL.LT.2) GOTO 920
      TEXT='APPARENT Dxx^   '
      DO K=1,NLAY
        CALL RPRINT(DXX(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
      IF(NROW.LT.2) GOTO 910
      TEXT='APPARENT Dxy^   '
      DO K=1,NLAY
        CALL RPRINT(DXY(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  910 IF(NLAY.LT.2) GOTO 920
      TEXT='APPARENT Dxz^   '
      DO K=1,NLAY
        CALL RPRINT(DXZ(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  920 IF(NROW.LT.2) GOTO 950
      TEXT='APPARENT Dyy^   '
      DO K=1,NLAY
        CALL RPRINT(DYY(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
      IF(NCOL.LT.2) GOTO 940
      TEXT='APPARENT Dyx^   '
      DO K=1,NLAY
        CALL RPRINT(DYX(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  940 IF(NLAY.LT.2) GOTO 950
      TEXT='APPARENT Dyz^   '
      DO K=1,NLAY
        CALL RPRINT(DYZ(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  950 IF(NLAY.LT.2) GOTO 980
      TEXT='APPARENT Dzz^   '
      DO K=1,NLAY
        CALL RPRINT(DZZ(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
      IF(NCOL.LT.2) GOTO 970
      TEXT='APPARENT Dzx^   '
      DO K=1,NLAY
        CALL RPRINT(DZX(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  970 IF(NROW.LT.2) GOTO 980
      TEXT='APPARENT Dzy^   '
      DO K=1,NLAY
        CALL RPRINT(DZY(1,1,K),TEXT,
     &    0,KSTP,KPER,NCOL,NROW,K,IFMTDP,IOUT)
      ENDDO
C
  980 CONTINUE
C
C--CONVERT DISPERSION COEFFICIENTS TO DISPERSION CONDUCTANCES
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
            IF(ICBUND(J,I,K).NE.0) THEN
C
C--COMPONENTS IN THE X-DIRECTION: DXX, DXY AND DXZ
              WW=DELR(JP1)/(DELR(J)+DELR(JP1))
              AREA=DELC(I)*(DZ(J,I,K)*WW+DZ(JP1,I,K)*(1.-WW))
              IF(NCOL.GT.1.AND.AREA.GT.0) THEN
                DXX(J,I,K)=AREA*DXX(J,I,K)/(0.5*DELR(JP1)+0.5*DELR(J))
                IF(NROW.GT.1) THEN
                  DXY(J,I,K)=AREA*DXY(J,I,K)/
     &             (0.5*DELC(IM1)+DELC(I)+0.5*DELC(IP1))
                ENDIF
                IF(NLAY.GT.1) THEN
                  DXZ(J,I,K)=AREA*DXZ(J,I,K)/((0.5*DZ(J,I,KM1)
     &             +DZ(J,I,K)+0.5*DZ(J,I,KP1))*WW + (0.5*DZ(JP1,I,KM1)
     &             +DZ(JP1,I,K)+0.5*DZ(JP1,I,KP1))*(1.-WW) )
                ENDIF
              ENDIF
C
C--COMPONENTS IN THE Y-DIRECTION: DYX, DYY AND DYZ
              WW=DELC(IP1)/(DELC(I)+DELC(IP1))
              AREA=DELR(J)*(DZ(J,I,K)*WW+DZ(J,IP1,K)*(1.-WW))
              IF(NROW.GT.1.AND.AREA.GT.0) THEN
                DYY(J,I,K)=AREA*DYY(J,I,K)/(0.5*DELC(IP1)+0.5*DELC(I))
                IF(NCOL.GT.1) THEN
                  DYX(J,I,K)=AREA*DYX(J,I,K)/
     &             (0.5*DELR(JM1)+DELR(J)+0.5*DELR(JP1))
                ENDIF
                IF(NLAY.GT.1) THEN
                  DYZ(J,I,K)=AREA*DYZ(J,I,K)/((0.5*DZ(J,I,KM1)
     &             +DZ(J,I,K)+0.5*DZ(J,I,KP1))*WW + (0.5*DZ(J,IP1,KM1)
     &             +DZ(J,IP1,K)+0.5*DZ(J,IP1,KP1))*(1.-WW) )
                ENDIF
              ENDIF
C
C--COMPONENTS IN THE Z DIRECTION: DZX, DZY AND DZZ
              AREA=DELR(J)*DELC(I)
              IF(NLAY.GT.1.AND.AREA.GT.0) THEN
                DZZ(J,I,K)=AREA*DZZ(J,I,K)/
     &           (0.5*DZ(J,I,KP1)+0.5*DZ(J,I,K))
                IF(NCOL.GT.1) THEN
                  DZX(J,I,K)=AREA*DZX(J,I,K)/
     &             (0.5*DELR(JM1)+DELR(J)+0.5*DELR(JP1))
                ENDIF
                IF(NROW.GT.1) THEN
                  DZY(J,I,K)=AREA*DZY(J,I,K)/
     &             (0.5*DELC(IM1)+DELC(I)+0.5*DELC(IP1))
                ENDIF
              ENDIF
C
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP4SV(NCOL,NROW,NLAY,MCOMP,ICOMP,ICBUND,DELR,DELC,DZ,
     & RETA,PRSITY,DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,CNEW,COLD,BUFF,
     & DTRANS,RMASIO)
C **********************************************************************
C THIS SUBROUTINE SOLVES DISPERSION-DIFFUSION EQUATION WITH THE CENTRAL
C EXPLICIT FINITE DIFFERENCE FORMULATION.
C **********************************************************************
C last modified: 08-12-2001
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,K,I,J,KP1,KM1,IP1,IM1,JP1,JM1,ICBUND,
     &          MCOMP,ICOMP
      REAL      DCFLUX,RETA,DTRANS,BUFF,
     &          RMASIO,WXP,WXM,WYP,WYM,WZP,WZM,DCDSP,DELR,DELC,
     &          DZ,DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,CNEW,COLD,
     &          PRSITY
      DIMENSION ICBUND(NCOL,NROW,NLAY,MCOMP),DELR(NCOL),DELC(NROW),
     &          DZ(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),
     &          COLD(NCOL,NROW,NLAY,MCOMP),CNEW(NCOL,NROW,NLAY,MCOMP),
     &          RETA(NCOL,NROW,NLAY,MCOMP),PRSITY(NCOL,NROW,NLAY),
     &          DXX(NCOL,NROW,NLAY),DXY(NCOL,NROW,NLAY),
     &          DXZ(NCOL,NROW,NLAY),DYX(NCOL,NROW,NLAY),
     &          DYY(NCOL,NROW,NLAY),DYZ(NCOL,NROW,NLAY),
     &          DZX(NCOL,NROW,NLAY),DZY(NCOL,NROW,NLAY),
     &          DZZ(NCOL,NROW,NLAY),RMASIO(122,2,MCOMP)
C
C--LOAD COLD FOR COMPONENT [ICOMP] INTO BUFF
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=COLD(J,I,K,ICOMP)
          ENDDO
        ENDDO
      ENDDO
C
C--LOOP THROUGH EVERY FINITE DIFFERENCE CELL
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
C--SKIP IF CELL IS INACTIVE
            IF(ICBUND(J,I,K,ICOMP).EQ.0) CYCLE
C
C--CALCULATE INTERFACE WEIGHTING FACTORS
            WXP=DELR(JP1)/(DELR(J)+DELR(JP1))
            WXM=DELR(J)/(DELR(JM1)+DELR(J))
            WYP=DELC(IP1)/(DELC(I)+DELC(IP1))
            WYM=DELC(I)/(DELC(IM1)+DELC(I))
            WZP=DZ(J,I,KP1)/(DZ(J,I,K)+DZ(J,I,KP1))
            WZM=DZ(J,I,K)/(DZ(J,I,KM1)+DZ(J,I,K))
C
C--ACCUMULATE ALL COMPONENTS OF THE DISPERSIVE FLUX
            DCFLUX=0.
C
C--COMPONENTS ACROSS LEFT AND RIGHT FACES IN THE X-DIRECTION
            IF(NCOL.GT.1) THEN
              DCFLUX=DCFLUX+DXX(J,I,K)*(BUFF(JP1,I,K)-BUFF(J,I,K))
     &         -DXX(JM1,I,K)*(BUFF(J,I,K)-BUFF(JM1,I,K))
              IF(NROW.GT.1) THEN
                DCFLUX=DCFLUX+DXY(J,I,K)*(BUFF(JP1,IP1,K)*(1.-WXP)
     &           +BUFF(J,IP1,K)*WXP
     &           -BUFF(JP1,IM1,K)*(1.-WXP)-BUFF(J,IM1,K)*WXP)
                IF(J.GT.1) THEN
                  DCFLUX=DCFLUX-DXY(JM1,I,K)*(BUFF(J,IP1,K)*(1.-WXM)
     &             +BUFF(JM1,IP1,K)*WXM
     &             -BUFF(J,IM1,K)*(1.-WXM)-BUFF(JM1,IM1,K)*WXM)
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                DCFLUX=DCFLUX+DXZ(J,I,K)*(BUFF(JP1,I,KP1)*(1.-WXP)
     &           +BUFF(J,I,KP1)*WXP
     &           -BUFF(JP1,I,KM1)*(1.-WXP)-BUFF(J,I,KM1)*WXP)
                IF(J.GT.1) THEN
                  DCFLUX=DCFLUX-DXZ(JM1,I,K)*(BUFF(J,I,KP1)*(1.-WXM)
     &             +BUFF(JM1,I,KP1)*WXM
     &             -BUFF(J,I,KM1)*(1.-WXM)-BUFF(JM1,I,KM1)*WXM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPONENTS ACROSS BACK AND FRONT FACES IN THE Y-DIRECTION
            IF(NROW.GT.1) THEN
              DCFLUX=DCFLUX+DYY(J,I,K)*(BUFF(J,IP1,K)-BUFF(J,I,K))
     &         -DYY(J,IM1,K)*(BUFF(J,I,K)-BUFF(J,IM1,K))
              IF(NCOL.GT.1) THEN
                DCFLUX=DCFLUX+DYX(J,I,K)*(BUFF(JP1,IP1,K)*(1.-WYP)
     &           +BUFF(JP1,I,K)*WYP
     &           -BUFF(JM1,IP1,K)*(1.-WYP)-BUFF(JM1,I,K)*WYP)
                IF(I.GT.1) THEN
                  DCFLUX=DCFLUX-DYX(J,IM1,K)*(BUFF(JP1,I,K)*(1.-WYM)
     &             +BUFF(JP1,IM1,K)*WYM
     &             -BUFF(JM1,I,K)*(1.-WYM)-BUFF(JM1,IM1,K)*WYM)
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                DCFLUX=DCFLUX+DYZ(J,I,K)*(BUFF(J,IP1,KP1)*(1.-WYP)
     &           +BUFF(J,I,KP1)*WYP
     &           -BUFF(J,IP1,KM1)*(1.-WYP)-BUFF(J,I,KM1)*WYP)
                IF(I.GT.1) THEN
                  DCFLUX=DCFLUX-DYZ(J,IM1,K)*(BUFF(J,I,KP1)*(1.-WYM)
     &             +BUFF(J,IM1,KP1)*WYM
     &             -BUFF(J,I,KM1)*(1.-WYM)-BUFF(J,IM1,KM1)*WYM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPONENTS ACROSS UPPER AND LOWER FACES IN THE Z-DIRECTION
            IF(NLAY.GT.1) THEN
              DCFLUX=DCFLUX+DZZ(J,I,K)*(BUFF(J,I,KP1)-BUFF(J,I,K))
     &         -DZZ(J,I,KM1)*(BUFF(J,I,K)-BUFF(J,I,KM1))
              IF(NCOL.GT.1) THEN
                DCFLUX=DCFLUX+DZX(J,I,K)*(BUFF(JP1,I,KP1)*(1.-WZP)
     &           +BUFF(JP1,I,K)*WZP
     &           -BUFF(JM1,I,KP1)*(1.-WZP)-BUFF(JM1,I,K)*WZP)
                IF(K.GT.1) THEN
                  DCFLUX=DCFLUX-DZX(J,I,KM1)*(BUFF(JP1,I,K)*(1.-WZM)
     &             +BUFF(JP1,I,KM1)*WZM
     &             -BUFF(JM1,I,K)*(1.-WZM)-BUFF(JM1,I,KM1)*WZM)
                ENDIF
              ENDIF
              IF(NROW.GT.1) THEN
                DCFLUX=DCFLUX+DZY(J,I,K)*(BUFF(J,IP1,KP1)*(1.-WZP)
     &           +BUFF(J,IP1,K)*WZP
     &           -BUFF(J,IM1,KP1)*(1.-WZP)-BUFF(J,IM1,K)*WZP)
                IF(K.GT.1) THEN
                  DCFLUX=DCFLUX-DZY(J,I,KM1)*(BUFF(J,IP1,K)*(1.-WZM)
     &             +BUFF(J,IP1,KM1)*WZM
     &             -BUFF(J,IM1,K)*(1.-WZM)-BUFF(J,IM1,KM1)*WZM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPUTE NET CHANGE IN CONCENTRATION
            DCDSP=DCFLUX*DTRANS/(DELR(J)*DELC(I)*DZ(J,I,K)
     &       *RETA(J,I,K,ICOMP))/PRSITY(J,I,K)
C
C--IF AT CONSTANT-CONCENTRATION CELL, ACCUMULATE MASS IN OR OUT.
C--OTHERWISE, UPDATE CELL CONCENTRATION.
            IF(ICBUND(J,I,K,ICOMP).LT.0) THEN
              IF(DCDSP.GT.0) THEN
                RMASIO(6,2,ICOMP)=RMASIO(6,2,ICOMP)
     &           -DCDSP*RETA(J,I,K,ICOMP)*
     &           DELR(J)*DELC(I)*DZ(J,I,K)*PRSITY(J,I,K)
              ELSE
                RMASIO(6,1,ICOMP)=RMASIO(6,1,ICOMP)
     &           -DCDSP*RETA(J,I,K,ICOMP)*
     &           DELR(J)*DELC(I)*DZ(J,I,K)*PRSITY(J,I,K)
              ENDIF
            ELSE
              CNEW(J,I,K,ICOMP)=CNEW(J,I,K,ICOMP)+DCDSP
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP4FM(NCOL,NROW,NLAY,MCOMP,ICOMP,ICBUND,DELR,DELC,DZ,
     & DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,A,NODES,UPDLHS,COLD,RHS,NCRS)
C **********************************************************************
C THIS SUBROUTINE FORMULATES THE COEFFICIENT MATRIX FOR THE DISPERSION
C TERM IF THE IMPLICIT SCHEME IS USED.
C **********************************************************************
C last modified: 08-12-2001
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,K,I,J,KP1,KM1,IP1,IM1,JP1,JM1,ICBUND,
     &          NODES,N,NCRS,II,L,INDEX,MCOMP,ICOMP
      REAL      WXP,WXM,WYP,WYM,WZP,WZM,DELR,DELC,DZ,COLD,RHS,
     &          DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,A,TEMP1,TEMP2,BNDTMP
      LOGICAL   UPDLHS
      DIMENSION ICBUND(NODES,MCOMP),DELR(NCOL),DELC(NROW),
     &          DZ(NCOL,NROW,NLAY),DXX(NCOL,NROW,NLAY),
     &          DXY(NCOL,NROW,NLAY),DXZ(NCOL,NROW,NLAY),
     &          DYX(NCOL,NROW,NLAY),DYY(NCOL,NROW,NLAY),
     &          DYZ(NCOL,NROW,NLAY),DZX(NCOL,NROW,NLAY),
     &          DZY(NCOL,NROW,NLAY),DZZ(NCOL,NROW,NLAY),
     &          COLD(NODES,MCOMP),RHS(NODES),A(NODES,19),
     &          TEMP1(7),TEMP2(19)
      COMMON   /GCGIDX/L(19)
C
C--LOOP THROUGH EVERY FINITE DIFFERENCE CELL
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
C--CALCULATE THE CELL INDEX
            N=(K-1)*NCOL*NROW+(I-1)*NCOL+J
C
C--SET TEMP. ARRAYS FOR PRINCIPAL AND CROSS TERMS
            DO II=1,7
              TEMP1(II)=0.
            ENDDO
            DO II=1,19
              TEMP2(II)=0.
            ENDDO
C
C--SKIP IF AT CONSTANT-CONCENTRATION OR INACTIVE CELL
            IF(ICBUND(N,ICOMP).LE.0) CYCLE
C
C--CALCULATE CELL INTERFACE WEIGHTING FACTORS
            WXP=DELR(JP1)/(DELR(J)+DELR(JP1))
            WXM=DELR(J)/(DELR(JM1)+DELR(J))
            IF(J.EQ.1) WXM=1.
            WYP=DELC(IP1)/(DELC(I)+DELC(IP1))
            WYM=DELC(I)/(DELC(IM1)+DELC(I))
            IF(I.EQ.1) WYM=1.
            WZP=DZ(J,I,KP1)/(DZ(J,I,K)+DZ(J,I,KP1))
            WZM=DZ(J,I,K)/(DZ(J,I,KM1)+DZ(J,I,K))
            IF(K.EQ.1) WZM=1.
C      
C--COEF. FOR (J,I,K)
            IF(J.GT.1) TEMP1(1)=TEMP1(1)-DXX(JM1,I,K)
            IF(I.GT.1) TEMP1(1)=TEMP1(1)-DYY(J,IM1,K)
            IF(K.GT.1) TEMP1(1)=TEMP1(1)-DZZ(J,I,KM1)
            IF(J.LT.NCOL) TEMP1(1)=TEMP1(1)-DXX(J,I,K)
            IF(I.LT.NROW) TEMP1(1)=TEMP1(1)-DYY(J,I,K)
            IF(K.LT.NLAY) TEMP1(1)=TEMP1(1)-DZZ(J,I,K)

C--BOUNDAY CONDITIONS
            BNDTMP=-DXY(J,I,K)*WXP+DXY(JM1,I,K)*(1.-WXM)
            IF(I.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(I.EQ.NROW) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DXZ(J,I,K)*WXP+DXZ(JM1,I,K)*(1.-WXM)
            IF(K.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(K.EQ.NLAY) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DYX(J,I,K)*WYP+DYX(J,IM1,K)*(1.-WYM)
            IF(J.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(J.EQ.NCOL) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DYZ(J,I,K)*WYP+DYZ(J,IM1,K)*(1.-WYM)
            IF(K.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(K.EQ.NLAY) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DZX(J,I,K)*WZP+DZX(J,I,KM1)*(1.-WZM)
            IF(J.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(J.EQ.NCOL) TEMP2(1)=TEMP2(1)-BNDTMP
            BNDTMP=-DZY(J,I,K)*WZP+DZY(J,I,KM1)*(1.-WZM)
            IF(I.EQ.1) TEMP2(1)=TEMP2(1)+BNDTMP
            IF(I.EQ.NROW) TEMP2(1)=TEMP2(1)-BNDTMP
C
C--COEF. FOR (J,I,K-1)
            IF(K.GT.1) THEN
              TEMP1(2)=DZZ(J,I,KM1)
              TEMP2(2)=-DXZ(J,I,K)*WXP+DXZ(JM1,I,K)*(1-WXM)
     &                -DYZ(J,I,K)*WYP+DYZ(J,IM1,K)*(1-WYM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(2)=TEMP2(2)+DZX(J,I,KM1)*WZM
              IF(J.EQ.NCOL) TEMP2(2)=TEMP2(2)-DZX(J,I,KM1)*WZM
              IF(I.EQ.1)    TEMP2(2)=TEMP2(2)+DZY(J,I,KM1)*WZM
              IF(I.EQ.NROW) TEMP2(2)=TEMP2(2)-DZY(J,I,KM1)*WZM
            ENDIF
C      
C--COEF. FOR (J,I,K+1)
            IF(K.LT.NLAY) THEN
              TEMP1(3)=DZZ(J,I,K)
              TEMP2(3)=+DXZ(J,I,K)*WXP-DXZ(JM1,I,K)*(1-WXM)
     &                +DYZ(J,I,K)*WYP-DYZ(J,IM1,K)*(1-WYM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(3)=TEMP2(3)-DZX(J,I,K)*(1-WZP)
              IF(J.EQ.NCOL) TEMP2(3)=TEMP2(3)+DZX(J,I,K)*(1-WZP)
              IF(I.EQ.1)    TEMP2(3)=TEMP2(3)-DZY(J,I,K)*(1-WZP)
              IF(I.EQ.NROW) TEMP2(3)=TEMP2(3)+DZY(J,I,K)*(1-WZP)
            ENDIF
C      
C--COEF. FOR (J,I-1,K)
            IF(I.GT.1) THEN
              TEMP1(4)=DYY(J,IM1,K)
              TEMP2(4)=-DXY(J,I,K)*WXP+DXY(JM1,I,K)*(1-WXM)
     &                -DZY(J,I,K)*WZP+DZY(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(4)=TEMP2(4)+DYX(J,IM1,K)*WYM
              IF(J.EQ.NCOL) TEMP2(4)=TEMP2(4)-DYX(J,IM1,K)*WYM
              IF(K.EQ.1)    TEMP2(4)=TEMP2(4)+DYZ(J,IM1,K)*WYM
              IF(K.EQ.NLAY) TEMP2(4)=TEMP2(4)-DYZ(J,IM1,K)*WYM
            ENDIF
C      
C--COEF. FOR (J,I+1,K)
            IF(I.LT.NROW) THEN
              TEMP1(5)=DYY(J,I,K)
              TEMP2(5)=+DXY(J,I,K)*WXP-DXY(JM1,I,K)*(1-WXM)
     &                +DZY(J,I,K)*WZP-DZY(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
              IF(J.EQ.1)    TEMP2(5)=TEMP2(5)-DYX(J,I,K)*(1-WYP)
              IF(J.EQ.NCOL) TEMP2(5)=TEMP2(5)+DYX(J,I,K)*(1-WYP)
              IF(K.EQ.1)    TEMP2(5)=TEMP2(5)-DYZ(J,I,K)*(1-WYP)
              IF(K.EQ.NLAY) TEMP2(5)=TEMP2(5)+DYZ(J,I,K)*(1-WYP)
            ENDIF
C      
C--COEF. FOR (J-1,I,K)
            IF(J.GT.1) THEN
              TEMP1(6)=DXX(JM1,I,K)
              TEMP2(6)=-DYX(J,I,K)*WYP+DYX(J,IM1,K)*(1-WYM)
     &                -DZX(J,I,K)*WZP+DZX(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
         IF(I.EQ.1)    TEMP2(6)=TEMP2(6)+DXY(JM1,I,K)*WXM
         IF(I.EQ.NROW) TEMP2(6)=TEMP2(6)-DXY(JM1,I,K)*WXM
         IF(K.EQ.1)    TEMP2(6)=TEMP2(6)+DXZ(JM1,I,K)*WXM
         IF(K.EQ.NLAY) TEMP2(6)=TEMP2(6)-DXZ(JM1,I,K)*WXM
            ENDIF
C      
C--COEF. FOR (J+1,I,K)
            IF(J.LT.NCOL) THEN
              TEMP1(7)=DXX(J,I,K)
              TEMP2(7)=+DYX(J,I,K)*WYP-DYX(J,IM1,K)*(1-WYM)
     &                +DZX(J,I,K)*WZP-DZX(J,I,KM1)*(1-WZM)
C--BOUNDARY CONDITION
         IF(I.EQ.1)    TEMP2(7)=TEMP2(7)-DXY(J,I,K)*(1-WXP)
         IF(I.EQ.NROW) TEMP2(7)=TEMP2(7)+DXY(J,I,K)*(1-WXP)
         IF(K.EQ.1)    TEMP2(7)=TEMP2(7)-DXZ(J,I,K)*(1-WXP)
         IF(K.EQ.NLAY) TEMP2(7)=TEMP2(7)+DXZ(J,I,K)*(1-WXP)
            ENDIF
C
C--COEF. FOR (J,I-1,K-1)
            IF(I.GT.1.AND.K.GT.1) 
     &         TEMP2(8)=DYZ(J,IM1,K)*WYM+DZY(J,I,KM1)*WZM
C      
C--COEF. FOR (J-1,I,K-1)
            IF(J.GT.1.AND.K.GT.1) 
     &         TEMP2(9)=DXZ(JM1,I,K)*WXM+DZX(J,I,KM1)*WZM
C      
C--COEF. FOR (J+1,I,K-1)
            IF(J.LT.NCOL.AND.K.GT.1) 
     &         TEMP2(10)=-DXZ(J,I,K)*(1-WXP)-DZX(J,I,KM1)*WZM
C      
C--COEF. FOR (J,I+1,K-1)
            IF(I.LT.NROW.AND.K.GT.1) 
     &         TEMP2(11)=-DYZ(J,I,K)*(1-WYP)-DZY(J,I,KM1)*WZM
C      
C--COEF. FOR (J,I-1,K+1)
            IF(I.GT.1.AND.K.LT.NLAY) 
     &         TEMP2(12)=-DYZ(J,IM1,K)*WYM-DZY(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J-1,I,K+1)
            IF(J.GT.1.AND.K.LT.NLAY) 
     &         TEMP2(13)=-DXZ(JM1,I,K)*WXM-DZX(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J+1,I,K+1)
            IF(J.LT.NCOL.AND.K.LT.NLAY) 
     &         TEMP2(14)=+DXZ(J,I,K)*(1-WXP)+DZX(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J,I+1,K+1)
            IF(I.LT.NROW.AND.K.LT.NLAY) 
     &         TEMP2(15)=+DYZ(J,I,K)*(1-WYP)+DZY(J,I,K)*(1-WZP)
C      
C--COEF. FOR (J-1,I-1,K)
            IF(I.GT.1.AND.J.GT.1) 
     &         TEMP2(16)=+DXY(JM1,I,K)*WXM+DYX(J,IM1,K)*WYM
C      
C--COEF. FOR (J+1,I-1,K)
            IF(I.GT.1.AND.J.LT.NCOL) 
     &         TEMP2(17)=-DXY(J,I,K)*(1-WXP)-DYX(J,IM1,K)*WYM
C      
C--COEF. FOR (J-1,I+1,K)
            IF(I.LT.NROW.AND.J.GT.1)
     &         TEMP2(18)=-DXY(JM1,I,K)*WXM-DYX(J,I,K)*(1-WYP)
C      
C--COEF. FOR (J+1,I+1,K)
            IF(I.LT.NROW.AND.J.LT.NCOL) THEN
              TEMP2(19)=+DXY(J,I,K)*(1-WXP)+DYX(J,I,K)*(1-WYP)
            ENDIF
C
C--ASSIGN COEF. OF PRINCIPAL DIRECTIONS TO ARRAY [A] OR [RHS]
            DO II=1,7
               INDEX=N+L(II)
               IF(INDEX.GE.1.AND.INDEX.LE.NODES) THEN
C
C--UPDATE MATRIX A IF NEIGHBOR CELL IS ACTIVE
                 IF(ICBUND(INDEX,ICOMP).GT.0) THEN
                   IF(UPDLHS) A(N,II)=A(N,II)+TEMP1(II)
C
C--SHIFT COEF. TO THE RIGHT-HAND-SIDE, OTHERWISE
                 ELSE
                   RHS(N)=RHS(N)-TEMP1(II)*COLD(INDEX,ICOMP)
                 ENDIF
               ENDIF
            ENDDO
C
C--ASSIGN COEF. OF CROSS TERMS TO ARRAY [A] OR [RHS]
            DO II=1,19
               INDEX=N+L(II)
               IF(INDEX.GE.1.AND.INDEX.LE.NODES) THEN
C
C--IF CROSS TERMS INCLUDED
                 IF(NCRS.GT.0.AND.ICBUND(INDEX,ICOMP).GT.0) THEN
                   IF(UPDLHS) A(N,II)=A(N,II)+TEMP2(II)
C
C--SHIFT CROSSING TERMS TO THE RIGHT-HAND-SIDE, OTHERWISE
                 ELSE
                   RHS(N)=RHS(N)-TEMP2(II)*COLD(INDEX,ICOMP)
                 ENDIF
               ENDIF
            ENDDO
C
          ENDDO
        ENDDO
      ENDDO
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE DSP4BD(NCOL,NROW,NLAY,MCOMP,ICOMP,ICBUND,DELR,DELC,DH,
     & DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,CNEW,BUFF,DTRANS,RMASIO)
C **********************************************************************
C THIS SUBROUTINE CALCULATES MASS BUDGET OF CONSTANT-CONCENTRATION NODES
C DUE TO DISPERSION.
C **********************************************************************
C last modified: 08-12-2001
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,K,I,J,KP1,KM1,IP1,IM1,JP1,JM1,ICBUND,
     &          MCOMP,ICOMP
      REAL      DCFLUX,DTRANS,BUFF,RMASIO,WXP,WXM,WYP,WYM,WZP,WZM,DELR,
     &          DELC,DH,DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,CNEW
      DIMENSION ICBUND(NCOL,NROW,NLAY,MCOMP),DELR(NCOL),DELC(NROW),
     &          DH(NCOL,NROW,NLAY),CNEW(NCOL,NROW,NLAY,MCOMP),
     &          DXX(NCOL,NROW,NLAY),DXY(NCOL,NROW,NLAY),
     &          DXZ(NCOL,NROW,NLAY),DYX(NCOL,NROW,NLAY),
     &          DYY(NCOL,NROW,NLAY),DYZ(NCOL,NROW,NLAY),
     &          DZX(NCOL,NROW,NLAY),DZY(NCOL,NROW,NLAY),
     &          DZZ(NCOL,NROW,NLAY),BUFF(NCOL,NROW,NLAY),
     &          RMASIO(122,2,MCOMP)
C
C--LOAD CNEW FOR COMPONENT [ICOMP] INTO BUFF
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            BUFF(J,I,K)=CNEW(J,I,K,ICOMP)
          ENDDO
        ENDDO
      ENDDO
C
C--LOOP THROUGH EVERY FINITE DIFFERENCE CELL
      DO K=1,NLAY
        KP1=MIN(K+1,NLAY)
        KM1=MAX(1,K-1)
        DO I=1,NROW
          IP1=MIN(I+1,NROW)
          IM1=MAX(1,I-1)
          DO J=1,NCOL
            JP1=MIN(J+1,NCOL)
            JM1=MAX(1,J-1)
C
C--SKIP IF CELL IS NOT CONSTANT-CONCENTRATION
            IF(ICBUND(J,I,K,ICOMP).GE.0) CYCLE
C
C--CALCULATE INTERFACE WEIGHTING FACTORS
            WXP=DELR(JP1)/(DELR(J)+DELR(JP1))
            WXM=DELR(J)/(DELR(JM1)+DELR(J))
            WYP=DELC(IP1)/(DELC(I)+DELC(IP1))
            WYM=DELC(I)/(DELC(IM1)+DELC(I))
            WZP=DH(J,I,KP1)/(DH(J,I,K)+DH(J,I,KP1))
            WZM=DH(J,I,K)/(DH(J,I,KM1)+DH(J,I,K))
C
C--ACCUMULATE ALL COMPONENTS OF THE DISPERSIVE FLUX
            DCFLUX=0.
C
C--COMPONENTS ACROSS LEFT AND RIGHT FACES IN THE X-DIRECTION
            IF(NCOL.GT.1) THEN
              DCFLUX=DCFLUX+DXX(J,I,K)*(BUFF(JP1,I,K)-BUFF(J,I,K))
     &         -DXX(JM1,I,K)*(BUFF(J,I,K)-BUFF(JM1,I,K))
              IF(NROW.GT.1) THEN
                DCFLUX=DCFLUX+DXY(J,I,K)*(BUFF(JP1,IP1,K)*(1.-WXP)
     &           +BUFF(J,IP1,K)*WXP
     &           -BUFF(JP1,IM1,K)*(1.-WXP)-BUFF(J,IM1,K)*WXP)
                IF(J.GT.1) THEN
                  DCFLUX=DCFLUX-DXY(JM1,I,K)*(BUFF(J,IP1,K)*(1.-WXM)
     &             +BUFF(JM1,IP1,K)*WXM
     &             -BUFF(J,IM1,K)*(1.-WXM)-BUFF(JM1,IM1,K)*WXM)
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                DCFLUX=DCFLUX+DXZ(J,I,K)*(BUFF(JP1,I,KP1)*(1.-WXP)
     &           +BUFF(J,I,KP1)*WXP
     &           -BUFF(JP1,I,KM1)*(1.-WXP)-BUFF(J,I,KM1)*WXP)
                IF(J.GT.1) THEN
                  DCFLUX=DCFLUX-DXZ(JM1,I,K)*(BUFF(J,I,KP1)*(1.-WXM)
     &             +BUFF(JM1,I,KP1)*WXM
     &             -BUFF(J,I,KM1)*(1.-WXM)-BUFF(JM1,I,KM1)*WXM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPONENTS ACROSS BACK AND FRONT FACES IN THE Y-DIRECTION
            IF(NROW.GT.1) THEN
              DCFLUX=DCFLUX+DYY(J,I,K)*(BUFF(J,IP1,K)-BUFF(J,I,K))
     &         -DYY(J,IM1,K)*(BUFF(J,I,K)-BUFF(J,IM1,K))
              IF(NCOL.GT.1) THEN
                DCFLUX=DCFLUX+DYX(J,I,K)*(BUFF(JP1,IP1,K)*(1.-WYP)
     &           +BUFF(JP1,I,K)*WYP
     &           -BUFF(JM1,IP1,K)*(1.-WYP)-BUFF(JM1,I,K)*WYP)
                IF(I.GT.1) THEN
                  DCFLUX=DCFLUX-DYX(J,IM1,K)*(BUFF(JP1,I,K)*(1.-WYM)
     &             +BUFF(JP1,IM1,K)*WYM
     &             -BUFF(JM1,I,K)*(1.-WYM)-BUFF(JM1,IM1,K)*WYM)
                ENDIF
              ENDIF
              IF(NLAY.GT.1) THEN
                DCFLUX=DCFLUX+DYZ(J,I,K)*(BUFF(J,IP1,KP1)*(1.-WYP)
     &           +BUFF(J,I,KP1)*WYP
     &           -BUFF(J,IP1,KM1)*(1.-WYP)-BUFF(J,I,KM1)*WYP)
                IF(I.GT.1) THEN
                  DCFLUX=DCFLUX-DYZ(J,IM1,K)*(BUFF(J,I,KP1)*(1.-WYM)
     &             +BUFF(J,IM1,KP1)*WYM
     &             -BUFF(J,I,KM1)*(1.-WYM)-BUFF(J,IM1,KM1)*WYM)
                ENDIF
              ENDIF
            ENDIF
C
C--COMPONENTS ACROSS UPPER AND LOWER FACES IN THE Z-DIRECTION
            IF(NLAY.GT.1) THEN
              DCFLUX=DCFLUX+DZZ(J,I,K)*(BUFF(J,I,KP1)-BUFF(J,I,K))
     &         -DZZ(J,I,KM1)*(BUFF(J,I,K)-BUFF(J,I,KM1))
              IF(NCOL.GT.1) THEN
                DCFLUX=DCFLUX+DZX(J,I,K)*(BUFF(JP1,I,KP1)*(1.-WZP)
     &           +BUFF(JP1,I,K)*WZP
     &           -BUFF(JM1,I,KP1)*(1.-WZP)-BUFF(JM1,I,K)*WZP)
                IF(K.GT.1) THEN
                  DCFLUX=DCFLUX-DZX(J,I,KM1)*(BUFF(JP1,I,K)*(1.-WZM)
     &             +BUFF(JP1,I,KM1)*WZM
     &             -BUFF(JM1,I,K)*(1.-WZM)-BUFF(JM1,I,KM1)*WZM)
                ENDIF
              ENDIF
              IF(NROW.GT.1) THEN
                DCFLUX=DCFLUX+DZY(J,I,K)*(BUFF(J,IP1,KP1)*(1.-WZP)
     &           +BUFF(J,IP1,K)*WZP
     &           -BUFF(J,IM1,KP1)*(1.-WZP)-BUFF(J,IM1,K)*WZP)
                IF(K.GT.1) THEN
                  DCFLUX=DCFLUX-DZY(J,I,KM1)*(BUFF(J,IP1,K)*(1.-WZM)
     &             +BUFF(J,IP1,KM1)*WZM
     &             -BUFF(J,IM1,K)*(1.-WZM)-BUFF(J,IM1,KM1)*WZM)
                ENDIF
              ENDIF
            ENDIF
C
C--ACCUMULATE MASS IN OR OUT.
            IF(DCFLUX.GT.0) THEN
              RMASIO(6,2,ICOMP)=RMASIO(6,2,ICOMP)-DCFLUX*DTRANS
            ELSE
              RMASIO(6,1,ICOMP)=RMASIO(6,1,ICOMP)-DCFLUX*DTRANS
            ENDIF
C
          ENDDO
        ENDDO
      ENDDO
C
C--RETURN
      RETURN
      END
