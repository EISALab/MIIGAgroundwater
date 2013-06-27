C     Last change:  ERB  10 Jul 2002    4:05 pm
C=======================================================================
      SUBROUTINE PES1BAS6DF(IBEFLG,IFO,IOUB,IPES,IPR,IPRAR,IPRINT,
     &                      ITERPF,ITERPK,ITMXP,IUPES,IYCFLG,JMAX,LASTX,
     &                      LCDMXA,LCNIPR,LCNPAR,LCPRM,LCWP,LCWTP,
     &                      LCWTPS,LCW3,LCW4,MPR,MPRAR,NPNGAR,SOSC,SOSR,
     &                      BEFIRST,LCBPRI,LCPARE,LCAMPA,LCAMCA,LCAAP)
C     VERSION 20000124
C     ******************************************************************
C     INITIALIZE VARIABLES FOR PARAMETER-ESTIMATION PROCESS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IFO, IOUB, IPES, IPR, IPRAR, IPRINT, ITERPF,
     &        ITMXP, IUPES, JMAX, LASTX, LCNIPR, LCPRM, LCWP,
     &        LCWTP, LCWTPS, MPR, MPRAR, NPNGAR
      REAL SOSC, SOSR
      LOGICAL BEFIRST
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C-----INITIALIZE GLOBAL VARIABLES THAT BELONG PRIMARILY TO THE
C     PARAMETER-ESTIMATION PROCESS
      BEFIRST = .TRUE.
      IFO = 0
      IOUB = 0
      IBEFLG = 0
      IYCFLG = -1
      IPES = 0
      IF (IUPES.GT.0) IPES = 1
      IPRINT = 0
      ITERPF = 0
      ITERPK = 0
      ITMXP = 1
      JMAX = 1
      LASTX = 0
      SOSC = 0.0
      SOSR = 0.0
C
C-----INITIALIZE POINTERS FOR ARRAYS THAT MAY BE REFERENCED BUT MAY NOT
C     OTHERWISE GET ALLOCATED
      IPR = 0
      IPRAR = 1
      LCAAP = 1
      LCAMCA = 1
      LCAMPA = 1
      LCBPRI = 1
      LCDMXA = 1
      LCNIPR = 1
      LCNPAR = 1
      LCPARE = 1
      LCPRM = 1
      LCWP = 1
      LCWTP = 1
      LCWTPS = 1
      LCW3 = 1
      LCW4 = 1
      MPR = 0
      MPRAR = 1
      NPNGAR = 1
C
C     Read ITMXP if PES is active
      IF (IUPES.GT.0) THEN
        CALL URDCOM(IUPES,0,LINE)
        LLOC = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMXP,DUM,IOUT,IUPES)
        REWIND(IUPES)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6AL(ISUM,ISUMZ,ISUMI,IOUT,NPLIST,LCC,LCSCLE,LCG,
     &                     LCDD,LCWP,MPR,LCPRM,LCPV,LCR,LCU,LCGD,LCS,
     &                     NOPT,IPR,LCWTP,LCWTPS,LCW3,LCW4,LCNIPR,
     &                     LCEIGL,LCEIGV,LCEIGW,LCIPNG,IU,NPNG,MPRAR,
     &                     IPRAR,NPNGAR,IREWND,LCPRNT,LCPARE,ITMXP,
     &                     LCSSPI,LCSSTO,DMAX,TOL,SOSC,IOSTAR,NFIT,
     &                     SOSR,IPRC,IPRINT,LPRINT,CSA,FCONV,LASTX,
     &                     ISEN,IPES,IPAR,IBEFLG,IYCFLG,LCDMXA,LCNPAR,
     &                     LCBPRI,RMARM,IAP,LCAAP,LCAMCA,LCAMPA,RMAR)
C     VERSION 19990722 ERB
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR THE PARAMETER-ESTIMATION PROCESS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, IPR, ISOLD, ISP, ISUM, LCC, LCDD, LCEIGL, LCEIGV,
     &        LCEIGW, LCG, LCGD, LCNIPR, LCPRM, LCPV, LCR, LCS, LCSCLE,
     &        LCU, LCWP, LCWTP, LCWTPS, LCW3, LCW4, MPR, NOPT, NPLIST,
     &        LCPRNT, LCPARE, LCAAP, LCAMCA
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      IREWND=0
C
C1------IDENTIFY PROCESS
      WRITE(IOUT,500) IU
  500 FORMAT (/,' PES1BAS6 -- PARAMETER-ESTIMATION PROCESS, ',
     &'VERSION 1.0, 07/22/99',/,' INPUT READ FROM UNIT ',I4)
C
C     READ AND PRINT ITEM 1 OF THE PES INPUT FILE
      CALL URDCOM(IU,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMXP,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DMAX,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,TOL,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,SOSC,IOUT,IU)
      WRITE(IOUT,510) ITMXP,DMAX,TOL,SOSC
  510 FORMAT (/,
     &' MAXIMUM NUMBER OF PARAMETER-ESTIMATION ITERATIONS (MAX-ITER)  ='
     &,1X,I5,/,
     &' MAXIMUM PARAMETER CORRECTION (MAX-CHANGE) ------------------- ='
     &,1X,G11.5,/,
     &' CLOSURE CRITERION (TOL) ------------------------------------- ='
     &,1X,G11.5,/,
     &' SUM OF SQUARES CLOSURE CRITERION (SOSC) --------------------- ='
     &,1X,G11.5)
C
      ITMXPO = ITMXP
      IF (ITMXP.EQ.0) THEN
C       ASSIGN VARIABLES TO FORCE CONVERGENCE IN ONE PARAMETER-
C       ESTIMATION ITERATION
        ITMXP = 1
        DMAX = 1.0E-6
        TOL = 1.0E35
        WRITE (IOUT,515) DMAX,TOL
      ENDIF
  515 FORMAT (/,
     &1X,'MAX-ITER SPECIFIED AS ZERO -- MAX-CHANGE HAS BEEN SET TO ',
     &1P,G8.1,' AND TOL HAS',/,' BEEN SET TO ',G8.1,' TO FORCE',
     &' CONVERGENCE IN ONE PARAMETER-ESTIMATION ITERATION')
C
C     READ AND PRINT ITEM 2 OF THE PES INPUT FILE
      READ(IU,'(A)') LINE
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBEFLG,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IYCFLG,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOSTAR,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NOPT,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NFIT,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,SOSR,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RMAR,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RMARM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IAP,DUM,IOUT,IU)
      WRITE(IOUT,520) IBEFLG,IYCFLG,IOSTAR,NOPT,NFIT,SOSR,RMAR,RMARM,IAP
  520 FORMAT (/,
     &' FLAG TO GENERATE INPUT NEEDED BY BEALE-2000 (IBEFLG) -------- ='
     &,1X,I5,/,
     &' FLAG TO GENERATE INPUT NEEDED BY YCINT-2000 (IYCFLG) -------- ='
     &,1X,I5,/,
     &' OMIT PRINTING TO SCREEN (IF = 1) (IOSTAR) ------------------- ='
     &,1X,I5,/,
     &' ADJUST GAUSS-NEWTON MATRIX WITH NEWTON UPDATES (IF = 1)(NOPT) ='
     &,1X,I5,/,
     &' NUMBER OF FLETCHER-REEVES ITERATIONS (NFIT) ----------------- ='
     &,1X,I5,/,
     &' CRITERION FOR ADDING MATRIX R (SOSR) ------------------------ ='
     &,1X,G11.5,/,
     &' VALUE USED TO INCREMENT MARQUARDT PARAMETER (RMAR) ---------- ='
     &,1X,G11.5,/,
     &' MARQUARDT PARAMETER MULTIPLIER (RMARM) ---------------------- ='
     &,1X,G11.5,/,
     &' APPLY MAX-CHANGE IN REGRESSION SPACE (IF = 1) (IAP) --------- ='
     &,1X,I5)
C
      IF (IBEFLG.LT.0 .OR. IBEFLG.GT.2) THEN
        WRITE (IOUT,521)
        STOP
      ENDIF
  521 FORMAT (/,1X,'ERROR: IBEFLG MUST BE 0, 1, OR 2 -- STOP',
     &' EXECUTION (PES1BAS6AL)')
C
      IF (IYCFLG.LT.-1 .OR. IYCFLG.GT.2) THEN
        WRITE (IOUT,522)
        STOP
      ENDIF
  522 FORMAT (/,1X,'ERROR: IYCFLG MUST BE -1, 0, 1, OR 2 -- STOP',
     &' EXECUTION (PES1BAS6AL)')
C
      IF (IYCFLG.GT.0 .AND. ITMXPO.NE.0) THEN
        WRITE (IOUT,523)
        STOP
      ENDIF
  523 FORMAT(/,' ERROR: MAX-ITER MUST BE SET TO 0 WHEN IYCFLG > 0',
     &' -- STOP EXECUTION (PES1BAS6AL)')
C
      IF (RMAR.LE.0.0) THEN
        RMAR = 0.001
        WRITE (IOUT,524) RMAR
      ENDIF
  524 FORMAT(/,1X,
     &'*** RMAR MUST BE > 0.  IT HAS BEEN SET TO',F6.3)
C
      IF (IAP.NE.0 .AND. IAP.NE.1) THEN
        WRITE (IOUT,526)
        STOP
      ENDIF
  526 FORMAT(/,1X,
     &'ERROR: IAP MUST BE 0 OR 1 -- STOP EXECUTION (PES1BAS6AL)')
C
C     READ AND PRINT ITEM 3 OF THE PES INPUT FILE
      READ(IU,'(A)') LINE
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPRC,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPRINT,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LPRINT,DUM,IOUT,IU)
      WRITE (IOUT,528) IPRC, IPRINT, LPRINT
  528 FORMAT (/,
     &' FORMAT CODE FOR COVARIANCE AND CORRELATION MATRICES (IPRCOV)  ='
     &,1X,I5,/,
     &' PRINT PARAMETER-ESTIMATION STATISTICS',/,
     &'     EACH ITERATION (IF > 0)  (IPRINT) ----------------------- ='
     &,1X,I5,/,
     &' PRINT EIGENVALUES AND EIGENVECTORS OF',/,
     &'     COVARIANCE MATRIX (IF > 0)  (LPRINT) -------------------- ='
     &,1X,I5)
C
C     READ AND PRINT ITEM 4 OF THE PES INPUT FILE
      READ(IU,'(A)') LINE
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,CSA,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FCONV,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LASTX,DUM,IOUT,IU)
      WRITE (IOUT,530) CSA, FCONV, LASTX
  530 FORMAT (/,
     &' SEARCH DIRECTION ADJUSTMENT PARAMETER (CSA) ----------------- ='
     &,1X,G11.5,/,
     &' MODIFY CONVERGENCE CRITERIA (IF > 0) (FCONV) ---------------- ='
     &,1X,G11.5,/,
     &' CALCULATE SENSITIVITIES USING FINAL',/,
     &'     PARAMETER ESTIMATES (IF > 0) (LASTX) -------------------- ='
     &,1X,I5)
C
C     READ AND PRINT ITEM 5 OF THE PES INPUT FILE
      READ(IU,'(A)') LINE
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPNG,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPR,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MPR,DUM,IOUT,IU)
      WRITE(IOUT,535) NPNG,IPR,MPR
  535 FORMAT (/,
     &' NUMBER OF USUALLY POS. PARAMETERS THAT MAY BE NEGATIVE (NPNG) ='
     &,1X,I5,/,
     &' NUMBER OF PARAMETERS WITH CORRELATED PRIOR INFORMATION (IPR)  ='
     &,1X,I5,/,
     &' NUMBER OF PRIOR-INFORMATION EQUATIONS (MPR) ----------------- ='
     &,1X,I5)
C
C-----IF IBEFLG = 2, DEACTIVATE SEN AND PES PROCESSES TO GENERATE
C     SECOND INPUT FILE (OUTNAM._b2) FOR BEALE-2000
      IF (IBEFLG.EQ.2) THEN
        ISEN = 0
        IPES = 0
        IPAR = -1
        LASTX = 0
        IYCFLG = -1
        ITMXP = 1
        IOSTAR = 0
        IPRINT = 0
        LPRINT = 0
        WRITE (IOUT,550)
      ENDIF
  550 FORMAT (/,
     &' SEN AND PES PROCESSES ARE DEACTIVATED BECAUSE IBEFLG = 2 -- ',
     &'SECOND INPUT',/,' FILE FOR BEALE-2000 WILL BE GENERATED')
C
C-------DEFINE NON-ZERO ARRAY DIMENSIONS
      MPRAR = MPR
      IF (MPR.EQ.0) MPRAR = 1
      IPRAR = IPR
      IF (IPR.EQ.0) IPRAR = 1
      NPNGAR = NPNG
      IF (NPNG.EQ.0) NPNGAR = 1
C
C-------STORE, IN ISOLD, LOCATION OF FIRST UNALLOCATED SPACE IN X.
      ISOLD = ISUM
      ISOLDI = ISUMI
      ISOLDZ = ISUMZ
C----------ARRAYS USED ONLY FOR PARAMETER ESTIMATION
      LCWP = ISUM
      ISUM = ISUM + MPR
      IPR = IPR
      LCBPRI = ISUM
      ISUM = ISUM + IPR
      LCWTP = ISUM
      ISUM = ISUM + (IPR*IPR) + 1
      LCWTPS = ISUM
      ISUM = ISUM + (IPR*IPR) + 1
      LCW3 = ISUMZ
      ISUMZ = ISUMZ + (IPR*IPR) + 1
      LCW4 = ISUMZ
      ISUMZ = ISUMZ + IPR + 1
      LCNIPR = ISUMI
      ISUMI = ISUMI + IPR + 1
      LCIPNG = ISUMI
      ISUMI = ISUMI + NPNG
C
      LCPARE = ISUM
      ISUM = ISUM + (ITMXP+1)*(NPLIST)
      LCPRM = ISUM
      ISUM = ISUM + (NPLIST+1)*(MPR+1)
      LCPRNT = ISUM
      ISUM = ISUM + NPLIST*9
      LCG = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      LCC = ISUMZ
      ISUMZ = ISUMZ + NPLIST*NPLIST
      LCSCLE = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      IF (IPR.GE.NPLIST) ISUMZ = ISUMZ + (IPR-NPLIST+1)
      LCPV = ISUMZ
      LCDD = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      LCEIGL = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      LCEIGV = ISUMZ
      ISUMZ = ISUMZ + NPLIST*NPLIST
      LCEIGW = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      LCSSPI = ISUM
      ISUM = ISUM + ITMXP + 1
      LCSSTO = ISUM
      ISUM = ISUM + ITMXP + 1
      LCDMXA = ISUMZ
      ISUMZ = ISUMZ + ITMXP + 1
      LCNPAR = ISUMI
      ISUMI = ISUMI + ITMXP + 1
      LCAAP = ISUM
      ISUM = ISUM + ITMXP
      LCAMCA = ISUM
      ISUM = ISUM + ITMXP
      LCAMPA = ISUM
      ISUM = ISUM + ITMXP
C-------FOR QUASI-NEWTON ADDITION TO THE GAUSS-NEWTON MATRIX
      LCR = ISUMZ
      ISUMZ = ISUMZ + (NPLIST*NPLIST/2+NPLIST)
      LCU = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      LCS = ISUMZ
      ISUMZ = ISUMZ + NPLIST
      LCGD = ISUMZ
      ISUMZ = ISUMZ + NPLIST
C
C8------PRINT AMOUNT OF SPACE USED BY PARAMETER-ESTIMATION PACKAGE.
      ISP = ISUM - ISOLD
      WRITE (IOUT,600) ISP
  600 FORMAT (/,1X,I10,
     &        ' ELEMENTS IN X ARRAY ARE USED FOR PARAMETER ESTIMATION')
      ISP = ISUMZ - ISOLDZ
      WRITE (IOUT,610) ISP
  610 FORMAT (1X,I10,
     &        ' ELEMENTS IN Z ARRAY ARE USED FOR PARAMETER ESTIMATION')
      ISP = ISUMI - ISOLDI
      WRITE (IOUT,620) ISP
  620 FORMAT (1X,I10,
     &        ' ELEMENTS IN IX ARRAY ARE USED FOR PARAMETER ESTIMATION')
C
C9------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6RP(IU,IOUT,NPE,WP,LN,DMAX,DD,FCONV,EV,MPR,PRM,
     &                      ISENS,NPLIST,WTP,WTPS,W3,W4,IPR,NIPR,DETWTP,
     &                      ND,ADMX,AP,DMX,NIPRNAM,EQNAM,MPRAR,IPRAR,
     &                      IPNG,NPNG,NPNGAR,IPLOT,NAMES,PARNEG,MXPN,
     &                      LBUFF,FSTAT,BPRI,IERR,IYCFLG,NPAR,ITMXP,
     &                      IBEFLG)
C     VERSION 19980917 ERB
C     ******************************************************************
C     READ, CHECK AND STORE DATA FOR PARAMETER ESTIMATION, AND
C        INITIALIZE SOME VARIABLES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL ADMX, BPRI, DETWTP, DMAX, DMX, EV, FCONV, PRM, WP
      INTEGER ICOUNT, IERR, ISENS, IFL2, IOUT, IP, IPM, IPR, IU, LN,
     &        MPR, ND, NIPR, NPE, NPLIST
      INTEGER IPLOT(ND+IPR+MPR)
      CHARACTER*10 NIPRNAM, EQNAM, PARNEG, PARNAMU
      CHARACTER*12 NAMES(ND+IPR+MPR)
      LOGICAL LOP
      DOUBLE PRECISION AP, DD(NPLIST), W3(IPRAR,IPRAR), W4(IPRAR)
      DIMENSION WP(MPRAR), LN(NPLIST),WTP(IPRAR,IPRAR),
     &          WTPS(IPRAR,IPRAR), PRM(NPLIST+1,MPRAR), ISENS(NPLIST),
     &          NIPR(IPRAR), NIPRNAM(IPRAR), EQNAM(MPRAR), IPNG(NPNGAR),
     &          PARNEG(MXPN), BPRI(IPRAR), NPAR(ITMXP+1)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  525 FORMAT (/,' PRIOR ESTIMATES OF PARAMETER SUMS EQUATION',I3,
     &      ' INCLUDES MULTIPLE PARAMETERS BUT ONLY SOME WITH LN > 0 --'
     &      ,/,' STOP EXECUTION (PES1BAS6RP)',/)
  530 FORMAT (/,' PRIOR ESTIMATES OF PARAMETER SUMS',/,' EQUATION:',3X,
     &        6(1X,A10),/,20(13X,6(1X,A10),/))
  532 FORMAT (/,
     &' F STATISTIC FOR BEALE''S MEASURE SET TO (FSTAT) --------------',
     &' = ',G11.5)
  535 FORMAT (' PARAMETER')
  541 FORMAT (/,
     &' PARAMETERS FOR WHICH PARTYP = HK, VK, VANI, VKCB, SS, SY,',
     &' OR EVT,',/,' BUT FOR WHICH PARAMETERS CAN BE NEGATIVE:')
  542 FORMAT(3X,A)
  585 FORMAT (/,' WARNING -- NPE > ND/3 : YOU MAY BE TRYING TO ESTIMATE'
     &       ,' TOO MANY',/,4X,'PARAMETERS FOR THE DATA (PES1BAS6RP)',/)
  590 FORMAT (/,' ND MUST BE GREATER THAN NPE',
     &        ' -- STOP EXECUTION (PES1BAS6RP)')
  620 FORMAT (/,' ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &        ' -- STOP EXECUTION (PES1BAS6RP)')
  640 FORMAT (/,' ERROR: PARAMETER NAME "',A,'" NOT DEFINED',/,
     &          ' -- STOP EXECUTION (PES1BAS6RP)')
C
C-------READ ITEM 6
      IF (NPNG.GT.0) THEN
        READ (IU,*) (PARNEG(I),I=1,NPNG)
        DO 8 I = 1,NPNG
          CALL UCASE(PARNEG(I),PARNEG(I),1)
          DO 4 IP = 1,NPLIST
            CALL UCASE(PARNAM(IP),PARNAMU,1)
            IF (PARNAMU.EQ.PARNEG(I)) THEN
              IPNG(I) = IP
              GOTO 6
            ENDIF
    4     CONTINUE
          WRITE (IOUT,640) PARNEG(I)
          STOP
    6     CONTINUE
    8   CONTINUE
        WRITE (IOUT,541)
        WRITE (IOUT,542) (PARNEG(I),I=1,NPNG)
      ENDIF
C
C-------INITIALIZE NIPR AND WP
      IF (IPR.GT.0) THEN
        DO 9 I = 1,IPR
          NIPR(I) = 0
    9   CONTINUE
      ENDIF
      IF (MPR.GT.0) THEN
        DO 10 I = 1,MPR
          WP(I) = 0.0
   10   CONTINUE
      ENDIF
C
C-----INITIALIZE NPAR
      DO 20 I = 1, ITMXP+1
        NPAR(I) = 0
   20 CONTINUE
C
C-------READ FULL WEIGHT MATRIX FOR PRIOR INFORMATION
C       (ITEMS 7, 8, AND 9)
      IF (IPR.GT.0) CALL SPES1BAS6WR(WTP,IPR,NIPR,IOUT,IU,DETWTP,
     &                               NPLIST,MPR,EV,IERR,ISENS,NIPRNAM,
     &                               IPRAR,IPLOT,NAMES,ND,BPRI,LN,
     &                               WTPS,W3,W4)
C-------READ PRIOR-INFORMATION EQUATIONS FOR SUMS OF PARAMETERS
C       (ITEM 10)
      IF (MPR.GT.0) CALL SPES1BAS6PI(EV,EQNAM,IOUT,IU,LN,MPR,NPLIST,PRM,
     &                               WP,MPRAR,IPLOT,NAMES,IPR,ND,IERR,
     &                               NIPR,IPRAR)
C
      IF (IBEFLG.GT.0) THEN
        IDOF = ND + IPR + MPR - NPE
        CALL SPES1BAS6FS(NPE,IDOF,FSTAT)
        WRITE (IOUT,532) FSTAT
      ENDIF
C-------INITIALIZE VARIABLES USED FOR PARAMETER ESTIMATION AND RESIDUAL
C       ANALYSIS
      DMX = DMAX
      AP = 1.
      ADMX = 10.*FCONV
      IF (MPRAR.GT.LBUFF) LBUFF = MPRAR
C-------PRIOR ESTIMATES OF PARAMETER SUMS
      IF (MPR.NE.0) THEN
C----------CONVERT FROM WP**.5 TO WP FOR USE IN REGRESSION
        DO 100 IPM = 1, MPR
          WP(IPM) = WP(IPM)**2.
  100   CONTINUE
C-------LOG-TRANSFORM ESTIMATES FOR LOG-TRANSFORMED PARAMETERS
        DO 120 IPM = 1, MPR
          DO 110 IP = 1, NPLIST
            IF (PRM(IP,IPM).NE.0. .AND. LN(IP).GT.0) PRM(NPLIST+1,IPM)
     &          = LOG(PRM(NPLIST+1,IPM))
  110     CONTINUE
  120   CONTINUE
C----------ERROR CHECKING
        DO 140 IPM = 1, MPR
          ICOUNT = 0
          IFL2 = 0
          DO 130 IP = 1, NPLIST
            IF (PRM(IP,IPM).NE.0.) THEN
              ICOUNT = ICOUNT + 1
              IF (IP.LE.NPLIST) THEN
                IF (LN(IP).GT.0) IFL2 = IFL2 + 1
              ENDIF
            ENDIF
  130     CONTINUE
          IF (ICOUNT.NE.IFL2 .AND. IFL2.NE.0) THEN
            WRITE (IOUT,525) IPM
            IERR = 1
          ENDIF
  140   CONTINUE
      ENDIF
C-------CHECK NPE VERSUS ND
      IF (IYCFLG.LE.0) THEN
        IF (NPE.GT.(ND/3) .AND. NPE.LT.ND) WRITE (IOUT,585)
        IF (NPE.GE.ND) THEN
          WRITE (IOUT,590)
          IERR = 1
        ENDIF
      ENDIF
C-------STOP IF THERE ARE ERRORS IN THE DATA
      IF (IERR.GT.0) THEN
        WRITE(IOUT,620)
        STOP
      ENDIF
C-------INITIALIZE DD
      DO 150 IP = 1, NPLIST
        DD(IP) = 0.0
  150 CONTINUE
C
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF (LOP) CLOSE(UNIT=IU)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6CN(B1,IOUT,IPNG,ITERP,LN,NPE,NPLIST,NPNG,
     &                     NPNGAR)
C-----VERSION 19990804 ERB
C     ******************************************************************
C     CHECK FOR PARAMETER VALUES <= 0 THAT SHOULD BE > 0.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL B1
      INTEGER I, IIP, IOUT, IPNG, LN, NPE, NPNG, NPLIST
      CHARACTER*4 PIDTMP
      DIMENSION B1(NPLIST), IPNG(NPNGAR), LN(NPLIST)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT (/,'  PARAMETER "',A,
     &        '" < 0 : NOT PHYSICALLY REASONABLE.',/,
     &        '  CHANGED TO ',G13.6,' (PES1BAS6CN)')
  505 FORMAT (/,'  LN PARAMETER "',A,'" <= 0 : NOT ',
     &        'PHYSICALLY OR MATHEMATICALLY REASONABLE.  CHANGED TO ',
     &        G13.6,' (PES1BAS6CN)')
C
      IF (ITERP.GT.1) THEN
        DO 20 IIP = 1, NPE
          IIPP = IPPTR(IIP)
          PIDTMP = PARTYP(IIPP)
          IF (B(IIPP).LE.B1(IIPP)/1.E6 .AND. LN(IIPP).LE.0 .AND.
     &        (PIDTMP.EQ.'HK  ' .OR. PIDTMP.EQ.'SS  ' .OR.
     &        PIDTMP.EQ.'SY  ' .OR. PIDTMP.EQ.'VK ' .OR.
     &        PIDTMP.EQ.'VANI' .OR. PIDTMP.EQ.'GHB ' .OR.
     &        PIDTMP.EQ.'RIV ' .OR. PIDTMP.EQ.'STR ' .OR.
     &        PIDTMP.EQ.'DRN ' .OR. PIDTMP.EQ.'ANI ' .OR.
     &        PIDTMP.EQ.'EVT ' .OR. PIDTMP.EQ.'VKCB' .OR.
     &        PIDTMP.EQ.'DRT ' .OR. PIDTMP.EQ.'ETS ')) THEN
            IF (NPNG.GT.0) THEN
              DO 10 I = 1, NPNG
                IF (IIPP.EQ.IPNG(I)) GOTO 20
   10         CONTINUE
            ENDIF
            B(IIPP) = B1(IIPP)/100.
            WRITE (IOUT,500) PARNAM(IIPP), B(IIPP)
          ENDIF
          IF (B(IIPP).LT.1.E-14 .AND. LN(IIPP).GT.0) THEN
            B(IIPP) = 1.E-14
            WRITE (IOUT,505) PARNAM(IIPP), B(IIPP)
          ENDIF
   20   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6RW(INUNIT,FNAME,CUNIT,IREWND,NIUNIT,IOUT,IOUTG,
     &                      VERSION,ISENS,ITERP,ITERPF,LASTX,NPLIST,
     &                      ITERPK)
C
C-----VERSION 19990811ERB
C     ******************************************************************
C     REWIND FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE MPI_MODULE
      DIMENSION ISENS(NPLIST), IREWND(NIUNIT)
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*(*) FNAME
      CHARACTER*200 FILENAME
      CHARACTER*40 VERSION, SPACES
      CHARACTER*200 LINE
      LOGICAL LOP
      INCLUDE 'openspec.inc'
C     ---------------------------------------------------------------
  510 FORMAT(/,1X,'THIS FILE CONTAINS OUTPUT UNIQUE TO PARAMETER',
     &       '-ESTIMATION ITERATION ',I5)
  520 FORMAT(/,1X,'THIS FILE CONTAINS OUTPUT UNIQUE TO FINAL',
     &' PARAMETER VALUES',/,'   --REGRESSION HAS CONVERGED')
  530 FORMAT(1X,'SENSITIVITIES ARE CALCULATED USING PREVIOUS SET OF',
     &' PARAMETER VALUES')
  540 FORMAT(//,
     &16X,43('-'),/,
     &16X,'ITERATION ',I4,' OF PARAMETER-ESTIMATION LOOP',/,
     &16X,43('-'))
C
      SPACES=' '
      LENVER=NONB_LEN(VERSION,40)
      INDENT=40-(LENVER+8)/2
C
C1------OPEN THE NAME FILE.
c     APPEND_PID called by Eva Sinha
      FILENAME=FNAME
      CALL APPEND_PID(FILENAME)
      OPEN(UNIT=INUNIT,FILE=FILENAME,STATUS='OLD',ACTION=ACTION(1))
C      OPEN(UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
C
C2------READ A LINE; IGNORE BLANK LINES AND COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') GO TO 10
C
C3------DECODE THE FILE TYPE, UNIT NUMBER, AND NAME.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUT,INUNIT)
C
C4------REWIND IF THE FILE IS A MAJOR OPTION FILE AND IF IREWND FLAG
C4------IS SET.
      DO 20 I=1,NIUNIT
         IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
            IF(IREWND(I).NE.0) GO TO 30
            GO TO 10
         END IF
20    CONTINUE
C
C5------REWIND IF BAS OR NONGLOBAL LIST FILE
      IF(LINE(ITYP1:ITYP2).EQ.'BAS6') GO TO 30
      IF(LINE(ITYP1:ITYP2).EQ.'LIST') THEN
         IF(IU.EQ.IOUTG) THEN
            WRITE(IOUT,540) ITERPK
            CALL SPES1BAS6ID(ISENS,IOUT,NPLIST)
            GO TO 10
         ENDIF
         GO TO 30
      END IF
C
C5------DO NOT REWIND GLOBAL FILE, BUT PRINT TABLE OF PARAMETER VALUES
C       IF GLOBAL FILE INCLUDES LIST OUTPUT
      IF(LINE(ITYP1:ITYP2).EQ.'GLOBAL') THEN
         IF(IU.EQ.IOUT) THEN
            WRITE(IOUT,540) ITERPK
            CALL SPES1BAS6ID(ISENS,IOUT,NPLIST)
         ENDIF
         GO TO 10
      END IF
C
C5------NOT A MAJOR OPTION.  REWIND IF FILE TYPE IS DATA.
      IF(LINE(ITYP1:ITYP2).NE.'DATA'         .AND.
     1   LINE(ITYP1:ITYP2).NE.'DATA(BINARY)' ) GO TO 10
C
C6------REWIND THE FILE
   30 REWIND(IU)
      IF(LINE(ITYP1:ITYP2).EQ.'LIST') THEN
         WRITE(IOUT,60) SPACES(1:INDENT),VERSION(1:LENVER)
60       FORMAT(34X,'MODFLOW-2000',/,
     &          6X,'U.S. GEOLOGICAL SURVEY MODULAR',
     &          ' FINITE-DIFFERENCE GROUND-WATER FLOW MODEL',/,
     &          A,'VERSION ',A,/)
         WRITE(IOUT,80)'LIST file.'
80       FORMAT(/,1X,'This model run produced both GLOBAL and ',
     &             'LIST files.  This is the ',A,/)
C
         IF (ITERPF.EQ.0) THEN
           WRITE (IOUT,510) ITERP
         ELSE
           WRITE (IOUT,520)
           IF (LASTX.EQ.0) WRITE (IOUT,530)
         ENDIF
         CALL SPES1BAS6ID(ISENS,IOUT,NPLIST)
      ENDIF
      WRITE(IOUT,35) LINE(INAM1:INAM2),LINE(ITYP1:ITYP2),IU
   35 FORMAT(1X,/1X,'REWOUND ',A,/
     1     1X,'FILE TYPE:',A,'   UNIT ',I4)
      GO TO 10
C
C  900 WRITE(IOUT,*) ' Error rewinding:',LINE(INAM1:INAM2)
C      GO TO 10

C7------END OF NAME FILE.
 1000 CONTINUE
      INQUIRE(UNIT=INUNIT,OPENED=LOP)
      IF (LOP) CLOSE(UNIT=INUNIT)
      RETURN
C
      END
C=======================================================================
      SUBROUTINE PES1BAS6CK(BL,BU,ISENS,IOUB,IOUTG,IPNG,LN,NPNG,NPLIST,
     &                      NPNGAR,ITERPK,FAC,FCONV,AP,ADMX,TOL,LAYHDT,
     &                      NLAY,BSCAL,PAREST,ITMXP)
C-----VERSION 19990326 ERB
C     ******************************************************************
C     CHECK THAT ALL PARAMETER NAMES IN SEN FILE HAVE BEEN DEFINED.
C     CHECK FOR PROPER USAGE OF IPNG.
C     CHECK FOR PARAMETER VALUES <=0 THAT SHOULD BE >0.
C     WRITE PARAMETER VALUES TO IOUB.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BL, BU
      DOUBLE PRECISION AP
      INTEGER I, ISENS, IOUB, IPNG, LN, NPNG, NPLIST
      CHARACTER*4 PIDTMP
      DIMENSION BL(NPLIST), BU(NPLIST), ISENS(NPLIST), IPNG(NPNGAR),
     &          LAYHDT(NLAY), LN(NPLIST), BSCAL(NPLIST),
     &          PAREST(ITMXP+1,NPLIST)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  480 FORMAT(' PARAMETER-VALUE SET ',I4,':')
  525 FORMAT (//' CONVERGENCE CRITERIA INCREASED BY FACTOR OF',
     &        G14.5,//)
  530 FORMAT (6G13.6)
  535 FORMAT(/,' PARAMETER "',A,'" FOUND IN PES FILE IS NOT DEFINED',/
     &         '   -- STOP EXECUTION (PES1BAS6CK)')
  540 FORMAT (/,' IPNG(',I3,')=',I3,'; PARTYP(',I3,')= ',A4,/,
     &    ' INCLUSION OF THIS PARAMETER IN IPNG IS WRONG OR UNNECESSARY'
     &    ,'   -- STOP EXECUTION (PES1BAS6CK)')
  610 FORMAT (/,' ***SUGGESTION FOR CONVERTIBLE LAYERS:',/,
     &' PERFORM INITIAL PARAMETER ESTIMATION RUNS WITH THE LAYERS',/,
     &' DESIGNATED AS CONFINED WITH ASSIGNED APPROXIMATE THICKNESSES',/,
     &' TO AVOID LONG EXECUTION TIMES AND EXCESSIVE FRUSTRATION.',/,
     &' CONVERT TO UNCONFINED AND CONVERTIBLE FOR FINAL PARAMETER ',/,
     &' ESTIMATION RUNS WHEN PARAMETERS ARE ALREADY CLOSE TO ',/,
     &' OPTIMAL.')
C
C-------PREPARE FACTOR FOR CONVERGENCE CRITERIA FOR FCONV OPTION
      IF (FCONV.GT.0.) THEN
        FAC = (AP*ADMX)/TOL
        IF (FAC.GT.1000.) FAC = 1000.
        IF (FAC.LT.10.) FAC = 1.
        WRITE (IOUTG,525) FAC
      ENDIF
C
      IF (ITERPK.EQ.1) THEN
C
C-------WARNING MESSAGE FOR UNCONFINED AND CONVERTIBLE LAYERS
        DO 20 NL = 1, NLAY
          IF (LAYHDT(NL).NE.0) THEN
            WRITE (IOUTG,610)
            GOTO 30
          ENDIF
   20   CONTINUE
   30   CONTINUE
C
C-------CHECK NPNG PARAMETER NUMBERS
        IF (NPNG.GT.0) THEN
          DO 60 I = 1, NPNG
            PIDTMP = PARTYP(IPNG(I))
            IF (PIDTMP.EQ.'HK  ' .OR. PIDTMP.EQ.'VK  ' .OR.
     &          PIDTMP.EQ.'VANI' .OR. PIDTMP.EQ.'VKCB' .OR.
     &          PIDTMP.EQ.'SS  ' .OR. PIDTMP.EQ.'SY  ' .OR.
     &          PIDTMP.EQ.'EVT ' .OR. PIDTMP.EQ.'ETS ') THEN
              CONTINUE
            ELSE
              WRITE (IOUTG,540) I, IPNG(I), IPNG(I), PIDTMP
              STOP
            ENDIF
   60     CONTINUE
        ENDIF
C
      ELSE
C
C-------WRITE PARAMETER VALUES TO THE IOUB FILE
        IF (IOUB.GT.0) WRITE(IOUB,480) ITERPK
        CALL SPES1BAS6WB(BL,BU,ISENS,IOUB,LN,NPLIST,BSCAL,PAREST,ITMXP,
     &                   ITERPK)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6ER(IOUT,ITERPK,NPLIST)
C     VERSION 20010921 ERB
C     ******************************************************************
C     SOLVER AP ROUTINE HAS SET THE ERROR FLAG WHILE TRYING TO SOLVE
C     FLOW EQUATION FOR A SET OF PARAMETERS GENERATED TO DETERMINE
C     BEALE'S MEASURE.  WRITE APPROPRIATE MESSAGE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, ITERPK, NPLIST
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  100 FORMAT(/,1X,'ERROR RESULTS FROM BEALE''S PARAMETER SET NUMBER ',
     &I3,', WITH VALUES:')
  110 FORMAT(3X,A,' = ',G12.5,24X,A,' = ',G12.5)
  120 FORMAT(/,1X,
     &'THIS SET OF PARAMETER VALUES PRODUCES A PROBLEM THAT THE',
     &' SELECTED SOLVER',/,1X,
     &'PACKAGE CANNOT SOLVE. IF HYDRAULIC-CONDUCTIVITY OR CONDUCTANCE',
     &' PARAMETERS ARE',/,1X,
     &'NEGATIVE, LOG-TRANSFORM THESE PARAMETERS, RERUN THE REGRESSION,',
     &' AND CALCULATE',/,1X,
     &'BEALE''S MEASURE AGAIN. OTHERWISE, CONSIDER USING A DIFFERENT',
     &' SOLVER. IF THAT',/,1X,
     &'IS NOT SUCCESSFUL, IT WILL NOT BE POSSIBLE TO OBTAIN A VALUE',
     &' FOR BEALE''S',/,1X,
     &'MEASURE AND THE MODEL NEEDS TO BE RATED AS VERY NONLINEAR.')
C
      WRITE(IOUT,100) ITERPK
      WRITE(IOUT,110) (PARNAM(I),B(I),I=1,NPLIST)
      WRITE(IOUT,120)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6NC(CC,CR,CV,HCOF,HNEW,IBEFLG,IBOUND,IOUTG,
     &                      ITERPK,KPER,KSTP,NCOL,NLAY,NROW,RHS)
C     VERSION 20010921 ERB
C     ******************************************************************
C     SOLUTION HAS FAILED TO CONVERGE.  WRITE APPROPRIATE MESSAGE,
C     CALCULATE AND WRITE PERCENT DISCREPANCY.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUTG
      DOUBLE PRECISION HNEW,DZERO,Q,C,SN,SP
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY), HCOF(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
  500 FORMAT(/,1X,
     &'*** WARNING: FOR BEALE''S PARAMETER SET NUMBER ',I4,
     &', SOLUTION DID NOT CONVERGE')
  510 FORMAT(/,1X,
     &'*** WARNING: FOR PARAMETER SET NUMBER ',I4,
     &', SOLUTION DID NOT CONVERGE')
  520 FORMAT(1X,
     &'IN SOLVING FOR HEADS IN TIME STEP ',I4,
     &' OF STRESS PERIOD ',I4)
  530 FORMAT(1X,'SUM OF POSITIVE RATES=',1PE12.5,
     &        '    SUM OF NEGATIVE RATES=',1PE12.5)
  540   FORMAT(1X,'              PERCENT DISCREPANCY=',F8.2)
C
C     WRITE MESSAGE DEPENDING ON EXECUTION MODE
      IF (IBEFLG.EQ.2) THEN
        WRITE(IOUTG,500)ITERPK
      ELSE
        WRITE(IOUTG,510)ITERPK
      ENDIF
      WRITE(IOUTG,520) KSTP,KPER
C
C     CALCULATE AND PERCENT DISCREPANCY
C-------ASSIGN VALUES TO FIELDS THAT ARE CONSTANT DURING AN ITERATION
      DZERO=0.
      SP=DZERO
      SN=DZERO
C
C-------STEP THROUGH CELLS CALCULATING FLOWS
      DO 150 K=1,NLAY
      DO 150 I=1,NROW
      DO 150 J=1,NCOL
C
      IF(IBOUND(J,I,K).LE.0)GO TO 150
C
C1----NEIGHBOR IS 1 ROW BACK
      IF(I.NE.1) THEN
      IF(IBOUND(J,I-1,K).LT.0) THEN
         C=CC(J,I-1,K)
         Q=C*(HNEW(J,I-1,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C2----NEIGHBOR IS 1 ROW AHEAD
      IF(I.NE.NROW) THEN
      IF(IBOUND(J,I+1,K).LT.0) THEN
         C=CC(J,I,K)
         Q=C*(HNEW(J,I+1,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C3----NEIGHBOR IS 1 COLUMN BACK
      IF(J.NE.1) THEN
      IF(IBOUND(J-1,I,K).LT.0) THEN
         C=CR(J-1,I,K)
         Q=C*(HNEW(J-1,I,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C4----NEIGHBOR IS 1 COLUMN AHEAD
      IF(J.NE.NCOL) THEN
      IF(IBOUND(J+1,I,K).LT.0) THEN
         C=CR(J,I,K)
         Q=C*(HNEW(J+1,I,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C5----NEIGHBOR IS 1 LAYER BEHIND
      IF(K.NE.1) THEN
      IF(IBOUND(J,I,K-1).LT.0) THEN
         C=CV(J,I,K-1)
         Q=C*(HNEW(J,I,K-1)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C6----NEIGHBOR IS 1 LAYER AHEAD
      IF(K.NE.NLAY) THEN
      IF(IBOUND(J,I,K+1).LT.0) THEN
         C=CV(J,I,K)
         Q=C*(HNEW(J,I,K+1)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
      Q=-RHS(J,I,K)
      IF(Q.GT.DZERO) THEN
         SP=SP+Q
      ELSE
         SN=SN-Q
      END IF
      Q=HCOF(J,I,K)
      Q=Q*HNEW(J,I,K)
      IF(Q.GT.DZERO) THEN
         SP=SP+Q
      ELSE
         SN=SN-Q
      END IF
C
  150 CONTINUE
C
      WRITE(IOUTG,530) SP,SN
      E=SP-SN
      A=(SP+SN)/2.
      IF (A.NE.0.0) THEN
        PE=100.*E/A
        WRITE(IOUTG,540) PE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6WB(BL,BU,ISENS,IOUB,ITERP,ITS,LN,NPLIST,BSCAL,
     &                      IOSTAR,NPE,PAREST,ITMXP,ITERPF,ITERPK)
C     VERSION 19981019 ERB
C     ******************************************************************
C     WRITE STARTING PARAMETER VALUES ON FILE IOUB AND TO SCREEN
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ISENS, IOUB, ITERP, ITS, LN, NPLIST
      REAL BL, BU
      DIMENSION ISENS(NPLIST), BL(NPLIST), BU(NPLIST),
     &          LN(NPLIST), BSCAL(NPLIST), PAREST(ITMXP+1,NPLIST)
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
  480 FORMAT(' PARAMETER-VALUE SET ',I4,':')
  560 FORMAT (/,' STARTING VALUES OF REGRESSION PARAMETERS :',/)
  570 FORMAT (6(3X,A10))
  580 FORMAT (6(2X,1PG11.4))
C
      IF (ITERPK.EQ.1 .AND. ITS.EQ.1) THEN
C
C-----WRITE STARTING PARAMETER VALUES ON FILE IOUB
        IF (IOUB.GT.0) WRITE(IOUB,480) ITERP
        CALL SPES1BAS6WB(BL,BU,ISENS,IOUB,LN,NPLIST,BSCAL,PAREST,ITMXP,
     &                   ITERP)
C
C     WRITE STARTING PARAMETER VALUES TO SCREEN
        IF (IOSTAR.NE.1 .AND. MYID.EQ.MPROC) THEN
          WRITE (*,560)
          WRITE (*,570) (PARNAM(IPPTR(IP)),IP=1,NPE)
          WRITE (*,'(1X)')
          WRITE (*,580) (B(IPPTR(IP)),IP=1,NPE)
        ENDIF
      ENDIF
C
C     IF FINAL TIME THROUGH PARAMETER-ESTIMATION LOOP, STORE PARAMETER
C     VALUES
      IF (ITERPF.GT.0) THEN
        DO 10 I = 1,NPLIST
          PAREST(ITERPK,I) = B(I)
   10  CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6PR(ITERSS,ITMXP,IUSS,SSPI)
C
C     VERSION 20010613 ERB
C     ******************************************************************
C     WRITE CONTRIBUTION TO SSWR OF PRIOR INFORMATION TO _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ITMXP, IUSS
      LOGICAL LOP
      DIMENSION SSPI(ITMXP+1)
C     ------------------------------------------------------------------
  660 FORMAT(1X,'ITERATION',2X,A)
  670 FORMAT(1X,I5,6X,G14.7)
C
      INQUIRE(UNIT=IUSS,OPENED=LOP)
      IF (LOP) THEN
        WRITE (IUSS,660)'SSWR-(PRIOR INFORMATION ONLY)'
C       WRITE CONTRIBUTION TO SSWR FOR EACH ITERATION
        DO 10 IT = 1, ITERSS
          WRITE(IUSS,670) IT,SSPI(IT)
   10   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6OT(C,WT,NPE,RSQ,IOUT,BUFF,ND,IPRC,IFO,IND,SCLE,
     &                      HOBS,H,B1,WP,ITERPF,LN,MPR,PRM,LPRINT,IDRY,
     &                      EV,RSQP,VAR,IPR,NIPR,WTPS,DETWTP,BL,BU,EIGL,
     &                      EIGV,EIGW,NHT,WTQ,WTQS,DTLWTQ,IOWTQ,NDMH,
     &                      NPLIST,MPRAR,IPRAR,IOUB,ISENS,IBEALE,ITERP,
     &                      ITMXP,NDMHAR,PRNT,OUTNAM,PAREST,SSPI,SSTO,
     &                      NPAR,DMXA,BPRI,BSCAL,IPRINT,AAP,AMCA,RSQA,
     &                      RSPA,AMPA,ITERPK)
C     VERSION 20010613 ERB
C     ******************************************************************
C     FINAL GAUSS-NEWTON OUTPUT FOR PARAMETER ESTIMATION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL B1, BL, BPRI, BU, BUFF, DETWTP, DTLWTQ, DTMPA, EV, H, HOBS,
     &     PRM, R, R1, RSQ, RSQP, WP, WT, WTQ, WTQS
      INTEGER I, IDRY, ISENS, IFO, IND, IOUT, IOWTQ, IP, IPR, IPRC,
     &        ITERPF, LN, LPRINT, MPR, ND, NDMH, NHT, NIPR, NPE
      CHARACTER*45 ANAME1
      CHARACTER*200 OUTNAM
      DOUBLE PRECISION C(NPE,NPE), SCLE(NPE), EIGL(NPE),
     &                 EIGV(NPE,NPE), EIGW(NPE), DMXA(ITMXP+1), VAR
      DIMENSION BUFF(NPE,NPE+2), WT(ND), HOBS(ND), H(ND), BPRI(IPRAR),
     &          WP(MPRAR), B1(NPLIST), LN(NPLIST), PRM(NPLIST+1,MPRAR),
     &          BL(NPLIST), BU(NPLIST), ISENS(NPLIST), PRNT(NPE,9),
     &          PAREST(ITMXP+1,NPLIST), BSCAL(NPLIST), WTPS(IPRAR,IPRAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR), SSPI(ITMXP+1),
     &          SSTO(ITMXP+1), NPAR(ITMXP+1), AAP(ITMXP), AMCA(ITMXP),
     &          RSQA(ITMXP+1), RSPA(ITMXP+1), AMPA(ITMXP+1),
     &          NIPR(IPRAR)
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
      DATA ANAME1 /'VARIANCE-COVARIANCE MATRIX FOR THE PARAMETERS'/
C     ------------------------------------------------------------------
C
  480 FORMAT(' FINAL PARAMETER ESTIMATES:')
  500 FORMAT (//,' LEAST SQUARES COEFFICIENT MATRIX SINGULAR -- ',
     &        'STOP EXECUTION (PES1BAS6OT)')
  505 FORMAT (//,
     &        '  LEAST-SQUARES OBJ FUNC (DEP.VAR. ONLY)- = ',G11.5,/,
     &        '  LEAST-SQUARES OBJ FUNC (W/PARAMETERS)-- = ',G11.5,/,
     &        '  CALCULATED ERROR VARIANCE-------------- = ',G11.5,/,
     &        '  STANDARD ERROR OF THE REGRESSION------- = ',G11.5,/,
     &        '  CORRELATION COEFFICIENT---------------- = ',G11.5,/,
     &       '       W/PARAMETERS---------------------- = ',G11.5,/,
     &        '  ITERATIONS----------------------------- = ',I7)
  510 FORMAT (//,' MAXIMUM NUMBER OF PARAMETER ESTIMATION ITERATIONS',
     &        ' EXCEEDED',/,' -- STOP EXECUTION (PES1BAS6OT)')
  565 FORMAT (26(13X,6(1X,1PE10.3),/))
  570 FORMAT (/,10X,A,/,10X,45('-'))
  575 FORMAT (16G13.6)
  580 FORMAT (16D13.6)
  585 FORMAT (1X)
  600 FORMAT (20A4)
  605 FORMAT (7I5,I10,E13.6)
  645 FORMAT (13X,6(5X,I2,4X),/,25(13X,6(5X,I2,4X),/))
C
C-----WRITE FINAL SET OF PARAMETER VALUES TO IOUB FILE IF PARAMETER
C     ESTIMATION DID NOT CONVERGE
      IF (ITERP.EQ.ITMXP .AND. IFO.EQ.0) THEN
        WRITE(IOUB,480)
        CALL SPES1BAS6WB(BL,BU,ISENS,IOUB,LN,NPLIST,BSCAL,PAREST,
     &                   ITMXP,ITERP)
      ENDIF
C
C-----WRITE FINAL PARAMETER-ESTIMATION OUTPUT
      IF (IFO.GT.0 .OR. ITERP.EQ.ITMXP)
     &    CALL SPES1GAU1PR(NPE,IOUT,NPLIST,IPR,MPR,IFO,OUTNAM,PAREST,
     &                     ITMXP,NPAR,DMXA,IPRINT,AAP,AMCA,RSQA,RSPA,
     &                     AMPA,ITERPK,SSPI,SSTO)
      CALL PLL1BR()
C-----SINGULAR COEFFICIENT MATRIX
      IF (IND.GT.0) THEN
        WRITE (IOUT,500)
        STOP
      ENDIF
C
      IF (IFO.EQ.0) THEN
        IF (ITERP.EQ.ITMXP) THEN
C---------EXCEEDED MAXIMUM NUMBER OF PARAMETER-ESTIMATION ITERATIONS
          WRITE (IOUT,510)
        ENDIF
        RETURN
      ENDIF
C
C-----COMPLETE COMPUTATION OF INVERSE OF THE SCALED COEFFICIENT
C-----MATRIX C STARTING FROM DECOMPOSED MATRIX FROM SPES1GAU1SL.
      IF (MYID.EQ.MPROC) THEN
        IF (NPE.LT.2) THEN
          C(1,1) = 1.0/C(1,1)
          GOTO 10
        ENDIF
        CALL SPES1GAU1IN(NPE,C)
      ENDIF
C-----COMPUTE FINAL CALCULATED ERROR VARIANCE
   10 VAR = RSQP/REAL(ND-IDRY+MPR+IPR-NPE)
C-----COMPUTE VARIANCE-COVARIANCE MATRIX FOR PARAMETERS BY UNSCALING
C-----C-INVERSE AND MULTIPLYING BY THE FINAL ERROR VARIANCE.
      IF (MYID.EQ.MPROC) THEN
        DO 30 IP = 1, NPE
          DTMPA = SCLE(IP)
          DO 20 I = IP, NPE
            C(I,IP) = VAR*C(I,IP)/(SCLE(I)*DTMPA)
            C(IP,I) = C(I,IP)
   20     CONTINUE
          SCLE(IP) = DSQRT(C(IP,IP))
   30   CONTINUE
C-------PRINT VARIANCE-COVARIANCE MATRIX.
        DO 50 IP = 1, NPE
          DO 40 I = 1, NPE
            BUFF(I,IP) = C(I,IP)
   40     CONTINUE
   50   CONTINUE
        WRITE (IOUT,570) ANAME1
        CALL UPARPM(BUFF,NPE,IPRC,IOUT)
C-------EIGENVALUES AND EIGENVECTORS OF COV MATRIX SCALED BY PARAMETERS
        IF (LPRINT.NE.0) CALL SPES1BAS6EV(C,EIGV,EIGW,NPE,NPLIST,EIGL,
     &                                    IOUT,B1,ITERPF,BUFF,IPRC)
C-------PRINT PARAMETER STATISTICS
        CALL SPES1BAS6WS(NPE,BUFF,LN,IOUT,SCLE,ND,MPR,BL,BU,C,IPRC,
     &                   NPLIST,PRNT,OUTNAM)
C-------CALCULATE CORRELATION COEFFICIENT
        CALL SOBS1BAS6CC(ND,WT,HOBS,H,R,R1,MPR,NPE,WP,B,PRM,BUFF,IPR,
     &                   NIPR,WTPS,NHT,WTQ,WTQS,IOWTQ,NDMH,NPLIST,MPRAR,
     &                   IPRAR,NDMHAR,BPRI)
C-------PRINT FINAL RSQ'S, ERROR VARIANCE, CORRELATION, ITERATIONS
        WRITE (IOUT,505) RSQ, RSQP, VAR, VAR**.5, R, R1, ITERPF
C-------CALC AND PRINT STATISTICS BASED ON MAX LIKELIHOOD OBJ FUNCTION
        CALL SOBS1BAS6ML(RSQP,WT,NPE,ND,WP,MPR,IOUT,EV,IPR,DETWTP,NHT,
     &                   DTLWTQ,MPRAR)
      ENDIF
C
C-----OUTPUT FOR PROGRAM BEALEP
      IBEALE = 0
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6RS(NPE,ND,NDMH,VAR,C,WT,NHT,WTQS,X,MPR,PRM,WP,
     &                      NPLIST,MPRAR,NDMHAR,OUTNAM,WTPS,IPR,IPRAR,
     &                      NIPR,RSQP,IDRY)
C
C     VERSION 19990329 ERB
C     ******************************************************************
C     WRITE INPUT FILE FOR RESIDUAL ANALYSIS POST-PROCESSOR RESAN-2000
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE MPI_MODULE
      REAL PRM, WP, WT, WTQS, X
      INTEGER IOUR, MPR, ND, NDMH, NHT, NPE, NIPR
      CHARACTER*200 OUTNAM
      CHARACTER*84 FN, FILENAME
      LOGICAL LOP
      DOUBLE PRECISION C(NPE,NPE), VAR
      DIMENSION SQRWT(16), WT(ND), WP(MPRAR), X(NPE,ND),
     &          PRM(NPLIST+1,MPRAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR),NIPR(IPRAR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT(/,' WARNING: UNABLE TO OPEN INPUT FILE FOR RESIDUAL',
     &' ANALYSIS PROGRAM -- (PES1BAS6RS)')
  575 FORMAT (1P,16E15.8)
  580 FORMAT (1P,16D25.16)
  600 FORMAT (6(A12,1X))
  605 FORMAT (7I5,I10,1P,D25.16)
  610 FORMAT (1P,8E15.8)
  615 FORMAT (6(A10,1X))
  620 FORMAT (16I5)
C
C
      IF (OUTNAM.EQ.'NONE') RETURN
C     OPEN INPUT FILE FOR RESIDUAL ANALYSIS PROGRAM
      LENGNAM = NONB_LEN(OUTNAM,200)
      IOUR = IGETUNIT(1,1000)
      IF (IOUR.GT.0) THEN
        FN = OUTNAM(1:LENGNAM)//'._rs'
C     APPEND_PID called by Eva Sinha
        FILENAME = FN
        CALL APPEND_PID(FILENAME)
        OPEN(IOUR,FILE=FILENAME,ERR=20,RECL=440)
C        OPEN(IOUR,FILE=FN,ERR=20,RECL=440)
      ELSE
        GOTO 20
      ENDIF
      GOTO 30
   20 CONTINUE
      WRITE(*,500)
      RETURN
   30 CONTINUE
C
C-----WRITE DATA ONLY FOR OBSERVATIONS WITH WT >= 0
      NNEG = 0
      DO 40 I = 1, NHT
        IF (WT(I).LT.0.0) NNEG = NNEG + 1
   40 CONTINUE
      NDPOS = ND - NNEG
      NHPOS = NHT - NNEG
C
C-------WRITE INPUT FOR RESIDUAL ANALYSIS PROGRAM
      N10059=10059
      VAR = RSQP/REAL(ND-IDRY+MPR+IPR-NPE)
      WRITE (IOUR,605) NPE, NDPOS, NHPOS, NDMH, MPR, IPR, 4, N10059, VAR
      WRITE (IOUR,615) (PARNAM(IPPTR(I)),I=1,NPE)
      DO 60 IP = 1, NPE
        WRITE (IOUR,580) (C(I,IP),I=IP,NPE)
   60 CONTINUE
C
C-----WRITE THE SQUARE-ROOT OF THE WEIGHTING
C          OBSERVATIONS WITH DIAGONAL WEIGHT MATRIX
      NLINES = INT((REAL(NHPOS)+15.5)/16.0)
      KH = 0
      DO 100 L = 1, NLINES
        KC = 0
        DO 80 I = 1, 16
   70     CONTINUE
          KH = KH + 1
          IF (KH.LE.NHT) THEN
            IF (WT(KH).GT.0.0) THEN
              SQRWT(I) = SQRT(WT(KH))
              KC = KC + 1
            ELSE
              GOTO 70
            ENDIF
          ELSE
            GOTO 90
          ENDIF
   80   CONTINUE
   90   CONTINUE
        WRITE (IOUR,575) (SQRWT(I),I=1,KC)
  100 CONTINUE
C           OBSERVATIONS WITH FULL WEIGHT MATRIX
      IF (NDMH.GT.0) THEN
        DO 105 I=1,NDMH
          WRITE (IOUR,575) (WTQS(I,J),J=1,NDMH)
  105   CONTINUE
      ENDIF
C------SENSITIVITIES
      DO 110 N = 1, ND
        IF (WT(N).GE.0) WRITE (IOUR,575) (X(IP,N),IP=1,NPE)
  110 CONTINUE
C------PRIOR FROM EQUATIONS
      IF (MPR.GT.0) THEN
        DO 120 IPM=1,MPR
          WRITE(IOUR,610) (PRM(IPPTR(IP),IPM),IP=1,NPE),WP(IPM)**.5
  120   CONTINUE
      ENDIF
C-------PRIOR WITH FULL WEIGHT MATRIX
      IF(IPR.GT.0) THEN
        WRITE (IOUR,620) (INDXPTR(NIPR(I),NPE),I=1,IPR)
        DO 140 K=1,IPR
          WRITE (IOUR,610) (WTPS(K,I),I=1,IPR)
  140   CONTINUE
      ENDIF
C
C     CLOSE FILE FOR RESIDUAL ANALYSIS PROGRAM
      INQUIRE(UNIT=IOUR,OPENED=LOP)
      IF (LOP) CLOSE(IOUR)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6BE(NPE,ND,MPR,VAR,H,WT,X,WP,LN,PRM,HOBS,C,
     &                     IBEALE,ITERPK,IOUT,OBSNAM,BUFF,NHT,NDMH,WTQ,
     &                     NPLIST,MPRAR,IBEFLG,OUTNAM,IUBE,BEFIRST,
     &                     FSTAT,IERR,IERRU,NDMHAR,WTP,IPR,IPRAR,BPRI,
     &                     NIPR)
C-----VERSION 1001 05APR1993
C     VERSION 19980302 ERB
C     ******************************************************************
C     PRINT DATA FOR PROGRAM BEALE-2000
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE MPI_MODULE
      REAL BUFF, H, HOBS, PRM, SB, TEMP, WP, WT, WTQ, X
      INTEGER I, IBEALE, IMP, IOUT, IP, IS, ITERPK, IUBE(2),
     &        J, LN, MPR, N, ND, NHT, NPE, NDMH
      DOUBLE PRECISION C(NPE,NPE), VAR
      DIMENSION H(ND), WT(ND), X(NPE,ND), WP(MPRAR), LN(NPLIST),
     &          HOBS(ND), PRM(NPLIST+1,MPRAR), BUFF(NPE),
     &          WTP(IPRAR,IPRAR), BPRI(IPRAR),NIPR(IPRAR)
      DIMENSION WTQ(NDMHAR,NDMHAR)
      CHARACTER*4 CHAR, SUF(2)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      CHARACTER*84 FN, FILENAME
      LOGICAL BEFIRST, LOP
      INCLUDE 'param.inc'
      DATA (SUF(I),I=1,2)/'._b1','._b2'/
C     ------------------------------------------------------------------
C
  500 FORMAT (8G15.8)
  505 FORMAT (8G13.6)
  510 FORMAT (5I10,1X,G13.6)
  515 FORMAT (' THE PARAMETER SETS FOLLOW')
  520 FORMAT (6G13.6)
  525 FORMAT (8I10)
  530 FORMAT (20A4)
  535 FORMAT (8F13.0)
  540 FORMAT (//,
     &        ' PARAMETER SETS NOT FOUND IN BEALE.DAT -- STOP EXECUTION'
     &        )
  550 FORMAT (6(A12,1X))
  560 FORMAT (1X,'ERROR FINDING AVAILABLE FILE UNIT -- STOP',
     &' EXECUTION (PES1BAS6BE)')
  570 FORMAT (1X,'ERROR OPENING FILE ON UNIT ',I5,' FOR BEALE-2000',
     &' INPUT',/' TRYING TO OPEN FILE: "',A,'"'/,
     &' -- STOP EXECUTION (PES1BAS6BE)')
  580 FORMAT (6(A10,1X))
C
      IF (OUTNAM.EQ.'NONE') RETURN
C-----OPEN FILE(S) FOR BEALE-2000 INPUT
      IF (BEFIRST) THEN
        DO 8 I = 1, IBEFLG
          IUBE(I) = IGETUNIT(1,1000)
          LENGNAM = NONB_LEN(OUTNAM,200)
          FN = OUTNAM(1:LENGNAM)//SUF(I)
C     APPEND_PID added by Eva Sinha
          FILENAME = FN
          CALL APPEND_PID(FILENAME)
          IF (IUBE(I).GT.0) THEN
            OPEN(IUBE(I),FILE=FILENAME,ERR=4)
C            OPEN(IUBE(I),FILE=FN,ERR=4)
            IF (IBEFLG.EQ.1) THEN
              INQUIRE(UNIT=IUBE(I),OPENED=LOP)
              IF (LOP) CLOSE(UNIT=IUBE(I),STATUS='DELETE')
              OPEN(IUBE(I),FILE=FILENAME,ERR=4)
C              OPEN(IUBE(I),FILE=FN,ERR=4)
            ENDIF
          ELSE
            WRITE(IOUT,560)
            WRITE(IERRU,560)
            IERR = 1
            RETURN
          ENDIF
          GOTO 6
    4     CONTINUE
          WRITE (IOUT,570) IUBE(I),FN
          WRITE (IERRU,570) IUBE(I),FN
          IERR = 1
          RETURN
    6     CONTINUE
    8   CONTINUE
        BEFIRST = .FALSE.
      ENDIF
C
      IBEALE = 0
      IF (IBEFLG.EQ.1) THEN
C       WRITE ITEM 1 OF _b1 FILE
        WRITE (IUBE(1),510) NPE, ND, NDMH, MPR, IPR, VAR
        DO 10 IP = 1, NPE
          IIPP = IPPTR(IP)
          IF (LN(IIPP).GT.0) B(IIPP) = EXP(B(IIPP))
   10   CONTINUE
C       WRITE ITEMS 2 AND 3 OF _b1 FILE
        WRITE (IUBE(1),580) (PARNAM(IPPTR(IP)),IP=1,NPE)
        WRITE (IUBE(1),520) (B(IPPTR(IP)),IP=1,NPE)
        DO 20 IP = 1, NPE
          IIPP = IPPTR(IP)
          IF (LN(IIPP).GT.0) B(IIPP) = LOG(B(IIPP))
   20   CONTINUE
C       WRITE ITEMS 4-7 OF _b1 FILE
        WRITE (IUBE(1),550) (OBSNAM(N),N=1,ND)
        WRITE (IUBE(1),520) (H(N),N=1,ND)
        WRITE (IUBE(1),520) (HOBS(N),N=1,ND)
        IF (NHT.GT.0) WRITE (IUBE(1),500) (WT(N),N=1,NHT)
        IF (NDMH.GT.0) THEN
          DO 30 I = 1, NDMH
C           WRITE ITEM 8 OF _b1 FILE
            WRITE (IUBE(1),500) (WTQ(I,J),J=1,NDMH)
   30     CONTINUE
        ENDIF
        DO 40 N = 1, ND
C         WRITE ITEM 9 OF _b1 FILE
          WRITE (IUBE(1),520) (X(IP,N),IP=1,NPE)
   40   CONTINUE
        IF (MPR.GT.0) THEN
          DO 50 IMP = 1, MPR
C           WRITE ITEM 10 OF _b1 FILE
            WRITE (IUBE(1),500) (PRM(IPPTR(IP),IMP),IP=1,NPE),
     &                       PRM(NPLIST+1,IMP), WP(IMP)
   50     CONTINUE
        ENDIF
        IF (IPR.GT.0) THEN
C         WRITE ITEM 11 AND FIRST ITEM 12 OF _b1 FILE
          WRITE (IUBE(1),525) (NIPR(I),I=1,IPR)
          WRITE (IUBE(1),505) (BPRI(I),I=1,IPR)
          DO 55 I = 1, IPR
C           WRITE SECOND ITEM 12 OF _b1 FILE
            WRITE (IUBE(1),505) (WTP(I,J),J=1,IPR)
   55     CONTINUE
        ENDIF
C       WRITE ITEM 13 OF _b1 FILE
        WRITE (IUBE(1),525) (LN(IPPTR(IP)),IP=1,NPE)
C         CALCULATE PARAMETER SETS
        WRITE (IUBE(1),515)
        TEMP = (REAL(NPE)*FSTAT)**.5
        DO 80 IP = 1, NPE
          SB = C(IP,IP)**.5
          DO 70 IS = -1, 1, 2
            DO 60 I = 1, NPE
              IIPP = IPPTR(I)
              BUFF(I) = B(IIPP) + REAL(IS)*(TEMP/SB)*C(IP,I)
              IF (LN(IIPP).GT.0) BUFF(I) = EXP(BUFF(I))
   60       CONTINUE
C           WRITE ITEM 14 OF _b1 FILE
            WRITE (IUBE(1),505) (BUFF(I),I=1,NPE)
   70     CONTINUE
   80   CONTINUE
      ELSE
C       IBEFLG MUST EQUAL 2
        IF (ITERPK.EQ.1) THEN
          DO 90 I = 1, 100000000
            READ (IUBE(1),530) CHAR
            IF (CHAR.EQ.' THE') GOTO 100
   90     CONTINUE
          WRITE (IOUT,540)
          WRITE (IERRU,540)
          IERR = 1
          RETURN
        ENDIF
  100   IF (ITERPK.GT.1) THEN
          WRITE (IUBE(2),505) (B(IPPTR(IP)),IP=1,NPE)
          WRITE (IUBE(2),520) (H(N),N=1,ND)
        ENDIF
        IF (ITERPK.LT.(2*NPE)+1) THEN
          READ (IUBE(1),535) (B(IPPTR(IP)),IP=1,NPE)
          IBEALE = 1
        ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6YC(NPE,ND,MPR,H,WT,X,C,IOUT,OBSNAM,NHT,NDMH,
     &                      WTQ,OUTNAM,IYCFLG,IPR,IPLOT,IERR,IERRU,
     &                      NDMHAR)
C-----VERSION 1001 05APR1993
C     VERSION 19990812 ERB
C     ******************************************************************
C     PRINT DATA FOR PROGRAM YCINT-2000
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE MPI_MODULE
      REAL H, WT, WTQ, X
      INTEGER I, IOUT, IP, IPLOT, J, MPR, N, ND, NHT, NPE, NDMH
      DOUBLE PRECISION C(NPE,NPE)
      DIMENSION IPLOT(ND), H(ND), WT(ND), X(NPE,ND), WTQ(NDMHAR,NDMHAR)
      CHARACTER*4 SUF(0:2)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      CHARACTER*84 FN, FILENAME
      LOGICAL LOP
      DATA (SUF(I),I=0,2)/'._y0','._y1','._y2'/
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
  500 FORMAT (8G15.8)
  510 FORMAT (4I10,2X,'Item 1: NVAR NOBS NHT IFSTAT')
  520 FORMAT (6G13.6)
  525 FORMAT (8I10)
  530 FORMAT (20A4)
  535 FORMAT (8F13.0)
  540 FORMAT (16I5)
  541 FORMAT (I10,2X,'Item 1: NDCALIB')
  542 FORMAT (I10,2X,'Item 2: MPR')
  543 FORMAT (I10,2X,'Item 3: IPR')
  544 FORMAT (I10,2X,'Item 4: IDIF')
  550 FORMAT (6(A12,1X))
  560 FORMAT (1X,'ERROR FINDING AVAILABLE FILE UNIT -- STOP',
     &' EXECUTION (PES1BAS6YC)')
  570 FORMAT (1X,'ERROR OPENING FILE ON UNIT ',I5,' FOR YCINT-2000',
     &' INPUT',/,' -- STOP EXECUTION (PES1BAS6YC)')
  580 FORMAT (16D13.6)
  590 FORMAT (6(1X,A10))
C
      IF (OUTNAM.EQ.'NONE') RETURN
C
C-----OPEN APPRORIATE FILE FOR YCINT-2000 INPUT
      IOUY = IGETUNIT(1,1000)
      LENGNAM = NONB_LEN(OUTNAM,200)
      FN = OUTNAM(1:LENGNAM)//SUF(IYCFLG)
C     APPEND_PID called by Eva Sinha
      FILENAME=FN
      CALL APPEND_PID(FILENAME)
      IF (IOUY.GT.0) THEN
        OPEN(IOUY,FILE=FILENAME,ERR=4)
C        OPEN(IOUY,FILE=FN,ERR=4)
        CLOSE(UNIT=IOUY,STATUS='DELETE')
        OPEN(IOUY,FILE=FILENAME,ERR=4)
C        OPEN(IOUY,FILE=FN,ERR=4)
      ELSE
        WRITE(IOUT,560)
        WRITE(IERRU,560)
        IERR = 1
        RETURN
      ENDIF
      GOTO 6
    4 CONTINUE
      WRITE (IOUT,570) IOUY
      WRITE (IERRU,570) IOUY
      IERR = 1
      RETURN
    6 CONTINUE
C
c-----WRITE OUTPUT TO ._y0, ._y1, OR ._y2
      IF (IYCFLG.GT.0) THEN
        IFSTAT = 0
        IF (IYCFLG.EQ.1) WRITE (IOUY,510) NPE, ND, NHT, IFSTAT
        WRITE (IOUY,550) (OBSNAM(N),N=1,ND)
        WRITE (IOUY,540) (IPLOT(N),N=1,ND)
        WRITE (IOUY,520) (H(N),N=1,ND)
C SC-CHANGE 28.02.96: LOOP CHANGED DUE TO FULL COV. ON HEAD DEPENDENT FLOWS
C        WRITE(IOUY,510)(WT(N),N=1,ND)
        IF (NHT.GT.0) WRITE (IOUY,500) (WT(N),N=1,NHT)
        IF (NDMH.GT.0) THEN
          WRITE (IOUY,500) (WTQ(I,I),I=1,NDMH)
        ENDIF
        DO 40 N = 1, ND
          WRITE (IOUY,520) (X(IP,N),IP=1,NPE)
   40   CONTINUE
      ELSE
        WRITE (IOUY,541) ND
        WRITE (IOUY,542) MPR
        WRITE (IOUY,543) IPR
        WRITE (IOUY,544) 0
        WRITE (IOUY,590) (PARNAM(IPPTR(I)),I=1,NPE)
        DO 50 J = 1,NPE
          WRITE (IOUY,580) (C(I,J),I=J,NPE)
   50   CONTINUE
      ENDIF
C
      INQUIRE(UNIT=IOUY,OPENED=LOP)
      IF (LOP) CLOSE(IOUY)
      RETURN
      END
C=======================================================================
      SUBROUTINE PES1BAS6FO(ICNVGP,IFO,IOUTG)
C
C     VERSION 19990625 ERB
C     ******************************************************************
C     PRINT FINAL PARAMETER-ESTIMATION OUTPUT
C     ******************************************************************
C
 620  FORMAT (/,
     &' **** IN LAST PARAMETER ITERATION, THE SOLVER DID NOT CONVERGE',
     &' FOR HEADS AND(OR)',/,
     &6X,'SENSITIVITIES.  RESULTS MAY BE UNRELIABLE.',/,
     &'      SEARCH ABOVE FOR: "SOLUTION DID NOT CONVERGE"')
 630  FORMAT (/,1X,'PARAMETER ESTIMATION DID NOT CONVERGE IN THE ',
     &        'ALLOTTED NUMBER OF ITERATIONS')
 640  FORMAT (/,1X,'*** PARAMETER ESTIMATION CONVERGED BY SATISFYING',
     &        ' THE TOL CRITERION ***')
 650  FORMAT (/,1X,'*** PARAMETER ESTIMATION CONVERGED BY SATISFYING',
     &        ' THE SOSC CRITERION ***')
C
      IF (ICNVGP.EQ.0) WRITE (IOUTG,620)
      IF (IFO.EQ.0) WRITE (IOUTG,630)
      IF (IFO.EQ.1) WRITE (IOUTG,640)
      IF (IFO.EQ.2) WRITE (IOUTG,650)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6ID(ISENS,IOUT,NPLIST)
C     VERSION 19990427
C     ******************************************************************
C     PRINT TABLE LISTING PARAMETER TYPES AND CURRENT VALUES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*1 ASTER
      DIMENSION ISENS(NPLIST)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  550 FORMAT(/,' CURRENT VALUES OF PARAMETERS LISTED IN THE SEN FILE:',
     &      //,' PARAMETER   PARAMETER   PARAMETER    FOOT-',/,
     &         '    NAME        TYPE       VALUE      NOTE',/,
     &         ' ----------  ---------  ------------  -----')
  560 FORMAT(1X,A,5X,A,4X,1PG12.5,4X,A)
  570 FORMAT(  ' ------------------------------------------',/,
     &         ' * INDICATES VALUE ADJUSTABLE BY PARAMETER-',/,
     &         '   ESTIMATION PROCESS',/)
C
C-----PRINT TABLE LISTING PARAMETER TYPES AND CURRENT VALUES
      WRITE (IOUT,550)
      DO 10 I=1,NPLIST
        ASTER = ' '
        IF (ISENS(I).GT.0) ASTER = '*'
        WRITE (IOUT,560)PARNAM(I),PARTYP(I),B(I),ASTER
   10 CONTINUE
      WRITE (IOUT,570)
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6WB(BL,BU,ISENS,IOUB,LN,NPLIST,BSCAL,PAREST,
     &                       ITMXP,ITERP)
C
C     VERSION 20000622 ERB
C     ******************************************************************
C     WRITE PARAMETER INFORMATION TO THE IOUB FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION PAREST(ITMXP+1,NPLIST)
C     ------------------------------------------------------------------
C
      INTEGER ISENS, LN, NPLIST
      REAL BL, BU
      DIMENSION BL(NPLIST), BU(NPLIST), ISENS(NPLIST), LN(NPLIST),
     &          BSCAL(NPLIST)
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
C
 100  FORMAT (A10,2(2X,I3),4(2X,G13.6))
C
C-------WRITE PARAMETER VALUES TO THE IOUB FILE
      IF (IOUB.GT.0) THEN
        DO 10 I=1,NPLIST
          WRITE(IOUB,100) PARNAM(I),ISENS(I),LN(I),B(I),BL(I),BU(I),
     &                    BSCAL(I)
 10     CONTINUE
      ENDIF
C
C     STORE PARAMETER VALUES
      DO 20 I=1,NPLIST
        PAREST(ITERP,I) = B(I)
   20 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6WS(NPE,BUFF,LN,IOUT,SCLE,ND,MPR,BL,BU,C,IPRC,
     &                       NPLIST,PRNT,OUTNAM)
C
C     VERSION 19980925 ERB
C     ******************************************************************
C     WRITE PARAMETER STATISTICS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE MPI_MODULE
      REAL BL, BU, BUFF, PRNT
      INTEGER IOUT, IPRC, LN, LNFLAG, MPR, ND, NPE
      CHARACTER*1 LID(5)
      CHARACTER*37 ANAME2
      CHARACTER*80 LINE
      CHARACTER*200 OUTNAM, FN, FILENAME
      DOUBLE PRECISION C(NPE,NPE), SCLE(NPE)
      DIMENSION BUFF(NPE,NPE+2), LN(NPLIST), BL(NPLIST), BU(NPLIST),
     &          PRNT(NPE,9)
      INCLUDE 'param.inc'
      DATA ANAME2 /'CORRELATION MATRIX FOR THE PARAMETERS'/
C     ------------------------------------------------------------------
  515 FORMAT (/,
     &        ' THE CORRELATION OF THE FOLLOWING PARAMETER PAIRS >= .95'
     &        ,/,5X,'PARAMETER   PARAMETER   CORRELATION')
  520 FORMAT (5X,A10,2X,A10,2X,F8.2)
  525 FORMAT (/,' THE CORRELATION OF THE FOLLOWING PARAMETER PAIRS IS ',
     &        'BETWEEN .90 AND .95',/,5X,
     &        'PARAMETER   PARAMETER   CORRELATION')
  530 FORMAT (/,' THE CORRELATION OF THE FOLLOWING PARAMETER PAIRS IS ',
     &        'BETWEEN .85 AND .90',/,5X,
     &        'PARAMETER   PARAMETER   CORRELATION')
  533 FORMAT (/,2(' ----------------------------------------',
     &        '--------------------------------',/))
  534 FORMAT (' ________________________________________',
     &        '________________________________',/)
  535 FORMAT (//,' _________________',//,' PARAMETER SUMMARY'/,
     &        ' _________________',/)
  536 FORMAT (' PARAMETER VALUES IN "REGRESSION" SPACE --- ',
     &        'LOG TRANSFORMED AS APPLICABLE')
  537 FORMAT (' PHYSICAL PARAMETER VALUES --- ',
     &        'NONE OF THE PARAMETERS IS LOG TRANSFORMED')
  538 FORMAT (' PHYSICAL PARAMETER VALUES --- ',
     &        'EXP10 OF LOG TRANSFORMED PARAMETERS')
  540 FORMAT (' PARAMETER:',7X,5(1x,A10))
  541 FORMAT (' * = LOG TRNS:',8X,A1,4(10X,A1))
  545 FORMAT (' FINAL VALUES')
  550 FORMAT ('   EXP10 **')
  555 FORMAT (' STD. DEV.')
  560 FORMAT (' COEF. OF VAR. (STD. DEV. / FINAL VALUE); "--"',
     &        ' IF FINAL VALUE = 0.0')
  562 FORMAT (A)
  564 FORMAT (' UPPER 95% C.I.  ',1x,1PE10.2,1x,1PE10.2,
     &        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
  566 FORMAT (' LOWER 95% C.I.  ',1x,1PE10.2,1x,1PE10.2,
     &        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
  567 FORMAT ('    UPPER LIMIT  ',1x,1PE10.2,1x,1PE10.2,
     &        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
  568 FORMAT (' FINAL VALUES    ',1x,1PE10.2,1x,1PE10.2,
     &        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
  569 FORMAT ('    LOWER LIMIT  ',1x,1PE10.2,1x,1PE10.2,
     &        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
  570 FORMAT (/,10X,37('-'),/,10X,A,/,10X,37('-'))
  575 FORMAT (16G13.6)
  580 FORMAT (16D13.6)
  585 FORMAT (1X)
  598 FORMAT (' STD. DEV.       ',1x,1PE10.2,1x,1PE10.2,
     &        1x,1PE10.2,1x,1PE10.2,1x,1PE10.2)
  600 FORMAT (6A12)
  605 FORMAT (7I5,I10,E13.6)
  615 FORMAT (' UPPER REASONABLE LIMIT')
  620 FORMAT ('     REASONABLE')
  625 FORMAT (' ESTIMATE ABOVE (1)')
  630 FORMAT (' ENTIRE CONF. INT.')
  635 FORMAT (
     &' SOME PARAMETER VALUES ARE OUTSIDE THEIR USER-SPECIFIED',
     &' REASONABLE',/,
     &' RANGES TO A STATISTICALLY SIGNIFICANT EXTENT, BASED ON',
     &' LINEAR THEORY.',/,
     &' THIS IMPLIES THAT THERE ARE PROBLEMS WITH THE OBSERVATIONS,',
     &' THE MODEL',/,
     &' DOES NOT ADEQUATELY REPRESENT THE PHYSICAL SYSTEM, THE DATA',
     &' ARE NOT',/,
     &' CONSISTENT WITH THEIR SIMULATED EQUIVALENTS, OR THE SPECIFIED',
     &' MINIMUM',/,
     &' AND/OR MAXIMUM ARE NOT REASONABLE.  CHECK YOUR DATA,',
     &' CONCEPTUAL MODEL,',/,' AND MODEL DESIGN.')
  640 FORMAT (/,
     &' CORRELATIONS GREATER THAN 0.95 COULD INDICATE THAT THERE IS',
     &' NOT ENOUGH',/,
     &' INFORMATION IN THE OBSERVATIONS AND PRIOR USED IN THE',
     &' REGRESSION TO ESTIMATE',/,' PARAMETER VALUES INDIVIDUALLY.',/,
     &' TO CHECK THIS,',
     &' START THE REGRESSION FROM SETS OF INITIAL PARAMETER VALUES',/,
     &' THAT DIFFER BY MORE THAT TWO STANDARD DEVIATIONS FROM THE',
     &' ESTIMATED ',/,
     &' VALUES.  IF THE RESULTING ESTIMATES ARE WELL WITHIN ONE STANDARD
     & DEVIATION',/,
     &' OF THE PREVIOUSLY ESTIMATED VALUE, THE ESTIMATES ARE PROBABLY',
     &/,
     &' DETERMINED INDEPENDENTLY WITH THE OBSERVATIONS AND PRIOR USED IN
     &',/,
     &' THE REGRESSION.  OTHERWISE, YOU MAY ONLY BE ESTIMATING THE RATIO
     &',/,' OR SUM OF THE HIGHLY CORRELATED PARAMETERS.')
  645 FORMAT (' BELOW(-1)LIMITS',I9,5I11)
  646 FORMAT (' ABOVE(1)BELOW(-1)',I7,5I11)
  650 FORMAT (' THE INITIAL PARAMETER VALUES ARE IN THE SEN FILE.')
  660 FORMAT (/,
     &' *** WARNING: ONE OR MORE CONFIDENCE LIMIT(S) FOR LOG-',
     &'TRANSFORMED PARAMETER(S)',/,
     &'              ARTIFICIALLY LIMITED TO 0.99E29 TO AVOID NUMERIC',
     &' OVERFLOW ***')
  670 FORMAT(A,2X,I5,2X,5(G14.7,2X))
C
C-------PRINT FINAL PARAMETER VALUES, STD. DEV., COEFFICIENTS OF
C-------VARIATION, AND PARAMETER 95-PERCENT LINEAR CONFIDENCE INTERVALS
C---------BUFF(IP,3) IS LOG10(B) AND THEN THE PHYSICAL PARAMETER VALUES
C---------BUFF(IP,1) IS THE PHYSICAL PARAMETER VALUES, THEN  THE
C---------           STANDARD DEVIATION OF LOG10(B), AND THEN 
C---------           THE LOWER LIMIT OF THE CONFIDENCE INTERVAL OF LOG10(B)
C---------BUFF(10,2) IS THE UPPER CONFIDENCE INTERVAL OF LOG10(B) 
C---------LOG TRANSFORM
      I95 = 0
      LNFLAG = 0
      IEXPFLG = 0
C-----PARAMETER SUMMARY CALCULATIONS
      DO 90 IP = 1, NPE
        IIPP = IPPTR(IP)
        BUFF(IP,3)=B(IIPP)
        PRNT(IP,1) = BUFF(IP,3)
        IF (LN(IIPP).GT.0) THEN
          LNFLAG = 1
          BUFF(IP,3)=LOG10(B(IIPP))
          PRNT(IP,1) = BUFF(IP,3)
          B(IIPP) = LOG(B(IIPP))
        ENDIF
   90 CONTINUE
C---------PHYSICAL PARAMETER VALUES FOR LOG-TRANSFORMED PARAMETERS
      IF (LNFLAG.NE.0) THEN
        DO 100 IP = 1, NPE
          IIPP = IPPTR(IP)
          BUFF(IP,1) = 0.
          IF (LN(IIPP).GT.0) BUFF(IP,1) = EXP(B(IIPP))
          PRNT(IP,2) = BUFF(IP,1)
          IF (LN(IIPP).LE.0) PRNT(IP,2) = B(IIPP)
  100   CONTINUE
      ENDIF
C---------PUT STANDARD DEVIATIONS IN SINGLE-PRECISION ARRAY
      DO 110 IP = 1, NPE
        BUFF(IP,1) = SNGL(SCLE(IP))
        IF(LN(IPPTR(IP)).GT.0) BUFF(IP,1)=BUFF(IP,1)/2.302585093
        PRNT(IP,3) = BUFF(IP,1)
  110 CONTINUE
C---------COMPUTE UPPER CONFIDENCE LIMITS AND PRINT
      CALL SPES1BAS6TS(ND+MPR-NPE, TST)
      DO 130 IP = 1, NPE
        BUFF(IP,2) = BUFF(IP,3) + TST*BUFF(IP,1)
        PRNT(IP,4) = BUFF(IP,2)
        PRNT(IP,5) = PRNT(IP,4)
  130 CONTINUE
      IF (LNFLAG.NE.0) THEN
        DO 140 IP = 1, NPE
          IIPP = IPPTR(IP)
          IF (LN(IIPP).LE.0) THEN
            BUFF(IP,2) = B(IIPP) + TST*SNGL(SCLE(IP))*2.302585093
          ELSE
            IF (BUFF(IP,2).LT. 29.) THEN
              BUFF(IP,2) = 10.0**BUFF(IP,2)
            ELSE
              BUFF(IP,2) = 0.99E29
              IEXPFLG = 1
            ENDIF
            PRNT(IP,5) = BUFF(IP,2)
          ENDIF
  140   CONTINUE
      ENDIF
C---------COMPUTE LOWER CONFIDENCE LIMITS AND PRINT
      DO 150 IP = 1, NPE
        BUFF(IP,1) = BUFF(IP,3) - TST*BUFF(IP,1)
        PRNT(IP,6) = BUFF(IP,1)
        PRNT(IP,7) = PRNT(IP,6)
  150 CONTINUE
      IF (LNFLAG.NE.0) THEN
        DO 160 IP = 1, NPE
          IIPP = IPPTR(IP)
          IF (LN(IIPP).LE.0) THEN
            BUFF(IP,1) = 0.
          ELSE
            IF (BUFF(IP,1).LT. 29.) THEN
              BUFF(IP,1) = 10.0**BUFF(IP,1)
            ELSE
              BUFF(IP,1) = 0.99E29
              IEXPFLG = 1
            ENDIF
            PRNT(IP,7) = BUFF(IP,1)
          ENDIF
  160   CONTINUE
        DO 170 IP = 1, NPE
          IIPP = IPPTR(IP)
          IF (LN(IIPP).LE.0) BUFF(IP,1) = B(IIPP) -
     &                                  TST*SNGL(SCLE(IP))*2.302585093
  170   CONTINUE
      ENDIF
C---------SAVE PHYSICAL PARAMETER VALUES
      DO 180 IP = 1, NPE
        IIPP = IPPTR(IP)
        BUFF(IP,3) = B(IIPP)
        IF (LN(IIPP).GT.0) BUFF(IP,3) = EXP(B(IIPP))
  180 CONTINUE
C--------COMPARE ESTIMATES AND CONFIDENCE INTERVALS TO UPPER
C--------AND LOWER REASONABLE PARAMETER VALUES
      IFLAG = 0
      DO 190 IP = 1, NPE
        IIPP = IPPTR(IP)
        BB = PRNT(IP,1)
        IF (LN(IIPP).GT.0) BB = PRNT(IP,2)
        BBU = PRNT(IP,5)
        BBL = PRNT(IP,7)
        PRNT(IP,8) = 0.
        PRNT(IP,9) = 0.
        IF (BB.GT.BU(IIPP)) PRNT(IP,8) = 1.
        IF (BB.LT.BL(IIPP)) PRNT(IP,8) = -1.
        IF (BBU.LE.BL(IIPP)) PRNT(IP,9) = -1.
        IF (BBL.GE.BU(IIPP)) PRNT(IP,9) = 1.
        IF (BBU.LE.BL(IIPP) .OR. BBL.GE.BU(IIPP)) IFLAG = 1
  190 CONTINUE
C-----PRINT PARAMETER SUMMARY
      WRITE (IOUT,535)
      DO 193 JJ = 1,NPE,5
        JTOP = JJ + 4
        IF (JTOP.GT.NPE) JTOP = NPE
        DO 191 IP = JJ,JTOP
          IL = IP-JJ+1
          IIPP = IPPTR(IP)
          IF (LN(IIPP).GT.0) LID(IL) = '*'
          IF (LN(IIPP).LE.0) LID(IL) = ' '
  191   CONTINUE
C-------PRINT HEADER
        WRITE(IOUT,534)
        IF (LNFLAG.EQ.1) WRITE (IOUT,536)
        IF (LNFLAG.EQ.0) WRITE (IOUT,537)
        WRITE (IOUT,534)
        WRITE (IOUT,540) (PARNAM(IPPTR(IP)),IP=JJ,JTOP)
        WRITE (IOUT,541) (LID(IP-JJ+1),IP=JJ,JTOP)
        WRITE (IOUT,585)
C-------PRINT UPPER CONFIDENCE LIMITS
        WRITE (IOUT,585)
        WRITE (IOUT,564) (PRNT(IP,4),IP=JJ,JTOP)
C-------PRINT ESTIMATED PARAMETERS
        WRITE (IOUT,568) (PRNT(IP,1),IP=JJ,JTOP)
C-------PRINT LOWER CONFIDENCE LIMITS
        WRITE (IOUT,566) (PRNT(IP,6),IP=JJ,JTOP)
C-------PRINT STANDARD DEVIATIONS
        WRITE (IOUT,585)
        WRITE (IOUT,598) (PRNT(IP,3),IP=JJ,JTOP)
C-------PRINT COEFFICIENTS OF VARIATION
        WRITE (IOUT,585)
        WRITE (IOUT,560)
        LINE = ' '
        JPT1 = 19
        DO 192 IP = JJ,JTOP
          JPT2 = JPT1 + 9
          IF (PRNT(IP,1).NE.0.0) THEN
            TEMP = PRNT(IP,3)/ABS(PRNT(IP,1))
            WRITE (LINE(JPT1:JPT2),'(1PE10.2)') TEMP
          ELSE
            WRITE (LINE(JPT1:JPT2),'(4X,A2,4X)') '--'
          ENDIF
          JPT1 = JPT1 + 11
  192   CONTINUE
        WRITE (IOUT,562) LINE
        IF (LNFLAG.EQ.0) THEN
          WRITE (IOUT,585)
          WRITE (IOUT,620)
          WRITE (IOUT,567) (BU(IPPTR(IP)),IP=JJ,JTOP)
          WRITE (IOUT,620)
          WRITE (IOUT,569) (BL(IPPTR(IP)),IP=JJ,JTOP)
          WRITE (IOUT,585)
          WRITE (IOUT,625)
          WRITE (IOUT,645) (INT(PRNT(IP,8)),IP=JJ,JTOP)
          WRITE (IOUT,630)
          WRITE (IOUT,646) (INT(PRNT(IP,9)),IP=JJ,JTOP)
          WRITE (IOUT,585)
        ENDIF
  193 CONTINUE
C
C-----IF THERE ARE LOG-TRANSFORMED PARAMETERS, PRINT SUMMARY STATISTICS
C     FOR THE PHYSICAL PARAMETER VALUES
      IF (LNFLAG.NE.0) THEN
        WRITE (IOUT,533)
        DO 197 JJ = 1,NPE,5
          JTOP = JJ+4
          IF (JTOP.GT.NPE) JTOP=NPE
          DO 195 IP=JJ,JTOP
            IL = IP-JJ+1
            IIPP = IPPTR(IP)
            IF (LN(IIPP).GT.0) LID(IL) = '*'
            IF (LN(IIPP).LE.0) LID(IL) = ' '
  195     CONTINUE
          WRITE (IOUT,534)
          WRITE (IOUT,538)
          WRITE (IOUT,534)
          WRITE (IOUT,540) (PARNAM(IPPTR(IP)),IP=JJ,JTOP)
          WRITE (IOUT,541) (LID(IP-JJ+1),IP=JJ,JTOP)
          WRITE (IOUT,585)
          WRITE (IOUT,564) (PRNT(IP,5),IP=JJ,JTOP)
          WRITE (IOUT,568) (PRNT(IP,2),IP=JJ,JTOP)
          WRITE (IOUT,566) (PRNT(IP,7),IP=JJ,JTOP)
          WRITE (IOUT,585)
          WRITE (IOUT,620)
          WRITE (IOUT,567) (BU(IPPTR(IP)),IP=JJ,JTOP)
          WRITE (IOUT,620)
          WRITE (IOUT,569) (BL(IPPTR(IP)),IP=JJ,JTOP)
          WRITE (IOUT,585)
          WRITE (IOUT,625)
          WRITE (IOUT,645) (INT(PRNT(IP,8)),IP=JJ,JTOP)
          WRITE (IOUT,630)
          WRITE (IOUT,646) (INT(PRNT(IP,9)),IP=JJ,JTOP)
  197   CONTINUE
        IF (IEXPFLG.EQ.1) WRITE (IOUT,660)
        WRITE (IOUT,585)
      ENDIF
C
C     IF REQUESTED, WRITE (NATIVE) CONFIDENCE LIMITS TO _pc FILE
      IF (OUTNAM.NE.'NONE') THEN
        LENGNAM = NONB_LEN(OUTNAM,200)
        IOPC = IGETUNIT(1,1000)
        IF (IOPC.GT.0) THEN
          FN = OUTNAM(1:LENGNAM)//'._pc'
c     APPEND_PID called by Eva Sinha
          FILENAME = FN
          CALL APPEND_PID(FILENAME)
          OPEN(IOPC,FILE=FILENAME)
C          OPEN(IOPC,FILE=FN)
        ENDIF
        DO 199 JJ = 1,NPE
          IIPP = IPPTR(JJ)
          WRITE (IOPC,670) PARNAM(IIPP),LN(IIPP),BUFF(JJ,3),BL(IIPP),
     &                     BU(IIPP),PRNT(JJ,7),PRNT(JJ,5)
  199   CONTINUE
        CLOSE(IOPC)
      ENDIF
C
      IF (IFLAG.EQ.1) THEN
        WRITE (IOUT,635)
        WRITE (IOUT,585)
      ENDIF
C
C--------COMPUTE AND PRINT CORRELATION MATRIX FOR PARAMETERS
C     BUFF REPLACES C FOR NEXT 30 LINES
      IF (NPE.GT.1) THEN
        DO 210 IP = 1, NPE
          DTMPA = SCLE(IP)
          DO 200 I = IP, NPE
            BUFF(I,IP) = C(I,IP)/(SCLE(I)*DTMPA)
            BUFF(IP,I) = BUFF(I,IP)
  200     CONTINUE
  210   CONTINUE
        WRITE (IOUT,570) ANAME2
        CALL UPARPM(BUFF,NPE,IPRC,IOUT)
C--------CHECK FOR HIGHLY CORRELATED PARAMETER PAIRS
        WRITE (IOUT,515)
        DO 230 IP = 1, NPE
          IIPP = IPPTR(IP)
          I1 = IP + 1
          DO 220 I = I1, NPE
            IF (ABS(BUFF(IP,I)).GE..95) THEN
              WRITE (IOUT,520) PARNAM(IIPP),PARNAM(IPPTR(I)),BUFF(IP,I)
              I95 = 1
            ENDIF
  220     CONTINUE
  230   CONTINUE
        WRITE (IOUT,525)
        DO 250 IP = 1, NPE
          IIPP = IPPTR(IP)
          I1 = IP + 1
          DO 240 I = I1, NPE
            TMP = ABS(BUFF(IP,I))
            IF (TMP.GE..90 .AND. TMP.LT..95) 
     &          WRITE (IOUT,520) PARNAM(IIPP),PARNAM(IPPTR(I)),
     &                           BUFF(IP,I)
  240     CONTINUE
  250   CONTINUE
        WRITE (IOUT,530)
        DO 270 IP = 1, NPE
          IIPP = IPPTR(IP)
          I1 = IP + 1
          DO 260 I = I1, NPE
            TMP = ABS(BUFF(IP,I))
            IF (TMP.GE..85 .AND. TMP.LT..90) 
     &          WRITE (IOUT,520) PARNAM(IIPP),PARNAM(IPPTR(I)),
     &                           BUFF(IP,I)
  260     CONTINUE
  270   CONTINUE
      ENDIF
      IF (I95.EQ.1) THEN
        WRITE (IOUT,640)
        WRITE (IOUT,650)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6PI(EV,EQNAM,IOUT,IUPAR,LN,MPR,NPLIST,PRM,WP,
     &                      MPRAR,IPLOT,NAMES,IPR,ND,IERR,NIPR,IPRAR)
C     VERSION 19990504 ERB
C     ******************************************************************
C     READ PRIOR-INFORMATION EQUATIONS (PES ITEM 9)
C     ******************************************************************
C       SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER I, ICOL, ISP, ISTART, ISTOP, KLOGS, L, LN, MPR, NPLIST
      INTEGER IPLOT(ND+IPR+MPR), NIPR(IPRAR)
      CHARACTER LINE*200, EQNAM*10, EQUAL*1, RW*20, TIMES*1, PNAM*10,
     &          PLUS*1, MINUS*1
      CHARACTER*12 NAMES(ND+IPR+MPR)
      CHARACTER*10 PNTEMP, PNAM1
      REAL COEF, EV, PRM, WP
      DIMENSION EQNAM(MPRAR), LN(NPLIST), PRM(NPLIST+1,MPRAR), WP(MPRAR)
      LOGICAL ISNUM
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
 1000 FORMAT(A200)
 1010 FORMAT(/,
     &1X,'EQUATIONS OF PRIOR INFORMATION ON PARAMETERS',//,
     &2X,'EQUATION',5X,'PRIOR',6X,'----EQUATION TERMS-----',15X,
     &'STAT-   PLOT',/,
     &4X,'NAME',6X,'ESTIMATE',4X,'COEFFICIENT  PARAMETER',4X,
     &'STATISTIC   FLAG   SYMBOL',/,1X,
     &10('-'),2(2X,11('-')),2X,10('-'),2X,11('-'),2X,5('-'),2X,6('-'))
 1015 FORMAT(1X,A,1P,2(2X,G11.4),2X,A,2X,G11.4,3X,I2,4X,I4)
 1016 FORMAT(9X,'Additional term:',1X,1P,G11.4,2X,A)
C
      VARMIN = 1.0E-23
      EQUAL = '='
      PLUS = '+'
      MINUS = '-'
      TIMES = '*'
C     CVLOG2LN = LN(10) ~= 2.302585
      CVLOG2LN = LOG(10.0)
      LENG = LEN(LINE)
      CALL UPOPRELARR(PRM,((NPLIST+1)*MPRAR),0.0)
C
C     PRINT TABLE HEADING
      WRITE (IOUT,1010)
C
C-----READ MPR EQUATIONS
      DO 80 I = 1,MPR
        ICOL = 1
        ISTART = 1
        ISTOP = LENG
        KLOGS = 0
        KTERMS = 0
        READ(IUPAR,1000,END=90) LINE
C
C-------GET EQUATION NAME
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,1,N,R,IOUT,0)
        EQNAM(I) = LINE(ISTART:ISTOP)
        NAMES(ND+I) = EQNAM(I)
C
C-------GET PRIOR ESTIMATE
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,3,N,PRM(NPLIST+1,I),IOUT,0)
C
C-------GET EQUAL SIGN
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,0,N,R,IOUT,0)
        IF (LINE(ISTART:ISTOP).NE.EQUAL) THEN
          WRITE(IOUT,1020)EQNAM(I)
 1020     FORMAT(' "=" SYMBOL NOT FOUND IN PRIOR-INFORMATION',
     &           ' EQUATION "',A10,'"',/,' -- STOP EXECUTION')
          STOP
        ENDIF
        SIGN = 1.0
C
C-------GET COEFFICIENTS AND PARAMETER NAMES
 20     CONTINUE
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,1,N,R,IOUT,0)
        IF (LINE(ISTART:ISTOP).EQ.'STAT') GOTO 60
        IF (LINE(ISTART:ISTOP).EQ.PLUS) THEN
          SIGN = 1.0
          GOTO 20
        ENDIF
        IF (LINE(ISTART:ISTOP).EQ.MINUS) THEN
          SIGN = -1.0
          GOTO 20
        ENDIF
C
        PNAM = LINE(ISTART:ISTOP)
C
C-------CHECK TO SEE IF STRING CAN BE INTERPRETED AS A NUMBER
        RW=' '
        L=20-ISTOP+ISTART
        RW(L:20)=LINE(ISTART:ISTOP)
        ISNUM=.FALSE.
        READ(RW,'(F20.0)',ERR=30) COEF
C
C-------IF NO READ ERROR, STRING IS A COEFFICIENT
        ISNUM=.TRUE.
 30     CONTINUE
C
        IF (ISNUM) THEN
C---------IF STRING IS A COEFFICIENT, GET "*" AND PARAMETER NAME
          CALL URWORD(LINE,ICOL,ISTART,ISTOP,0,N,R,IOUT,0)
          IF (LINE(ISTART:ISTOP).NE.TIMES) THEN
            WRITE(IOUT,1030) EQNAM(I)
 1030       FORMAT(' "*" SYMBOL NOT FOUND IN PRIOR-INFORMATION',
     &             ' EQUATION "',A10,'"',/,' -- STOP EXECUTION')
            STOP
          ENDIF
          CALL URWORD(LINE,ICOL,ISTART,ISTOP,1,N,R,IOUT,0)
          PNAM = LINE(ISTART:ISTOP)
        ELSE
C---------IF STRING IS NOT A NUMBER, SET COEF = 1
          COEF = 1.0
        ENDIF
C
C-------LOOK UP PARAMETER NUMBER
        DO 40 KP=1,NPLIST
          CALL UCASE(PARNAM(KP),PNTEMP,1)
          IF (PNAM.EQ.PNTEMP) THEN
            IP = KP
            GOTO 50
          ENDIF
 40     CONTINUE
C
C-------IF NO MATCH FOUND FOR PARAMETER NAME, PRINT ERROR MESSAGE
        WRITE(IOUT,1040)PNAM,EQNAM(I)
 1040   FORMAT(' PARAMETER NAME "',A10,'" IN PRIOR-',
     &         'INFORMATION EQUATION "',A10,'"',/' NOT FOUND',
     &         ' -- STOP EXECUTION (SPES1BAS6PI)')
        STOP
C
 50     CONTINUE
C
C       PRINT WARNING IF PARAMETER WAS INCLUDED IN FULL WEIGHT MATRIX
        IF (IPR.GT.0) THEN
          DO 52 KP = 1,IPR
            IF (NIPR(KP).EQ.IP) THEN
              WRITE(IOUT,1042) PARNAM(IP)
              IF (IERR.EQ.0) IERR = -1
            ENDIF
   52     CONTINUE
        ENDIF
 1042   FORMAT(/,
     &' WARNING: THE SAME PARAMETER (',A,') HAS BEEN SPECIFIED',/,
     &' IN MORE THAN ONE TYPE OF PRIOR INFORMATION.  THIS USUALLY',/,
     &' IS A MISTAKE.')
C
C       IF PARAMETER IS USED IN MORE THAN ONE EQUATION, PRINT WARNING
        DO 55 KI = 1, I
          IF (PRM(IP,KI).NE.0.0) THEN
            WRITE(IOUT,1045) PNAM
            IF (IERR.EQ.0) IERR = -1
          ENDIF
 55     CONTINUE
 1045   FORMAT(/,1X,48('*'),/,
     &  ' *  WARNING: PARAMETER "',   A10,'" IS USED IN  *',/
     &  ' *  MORE THAN ONE PRIOR-INFORMATION EQUATION    *',/
     &  ,1X,48('*'),/)
        PRM(IP,I) = SIGN*COEF
        KTERMS = KTERMS + 1
        IF (KTERMS.EQ.1) THEN
          COEF1 = SIGN*COEF
          PNAM1 = PNAM
        ENDIF
C
C-------COUNT PARAMETERS IN THIS EQUATION THAT ARE ESTIMATED USING
C       LOG-TRANSFORMATION
        IF (LN(IP).GT.0) KLOGS = KLOGS + 1
C
C-------LOOP BACK TO GET NEXT STRING
        SIGN = 1.0
        GOTO 20
C
 60     CONTINUE
C
C       ALL TERMS IN EQUATION i HAVE BEEN READ.  NOW READ STATISTIC.
C
C-------IF THERE IS A LOG-TRANSFORMED PARAMETER IN THE EQUATION, THERE
C       SHOULD BE ONLY ONE PARAMETER
        IF (KLOGS.GT.0 .AND. KTERMS.GT.1) THEN
          WRITE(IOUT,1050)EQNAM(I)
 1050     FORMAT(' PRIOR-INFORMATION EQUATION "',A10,
     &           '" CONTAINS A LOG-TRANSFORMED PARAMETER',/,
     &           ' AND MORE THAN ONE PARAMETER',/,
     &           ' -- STOP EXECUTION (SPES1BAS6PI)')
          STOP
        ENDIF
C
C-------GET STATP AND ISP (STAT-FLAG)
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,3,N,STATP,IOUT,0)
        WP(I) = STATP
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,ISP,R,IOUT,0)
C
C-------GET PLOT SYMBOL
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,IPLOT(ND+I),R,IOUT,0)
C
C       PRINT PRIOR-INFORMATION EQUATION TO IOUT
        WRITE (IOUT,1015) EQNAM(I),PRM(NPLIST+1,I),COEF1,PNAM1,STATP,
     &                    ISP,IPLOT(ND+I)
C
C       CHECK FOR VALID VALUE OF ISP
        ISP1 = ISP
        IF (KLOGS.GT.0) THEN
          IF (ISP.GE.10) ISP1 = ISP - 10
          IF (ISP1.LT.0 .OR. ISP1.GT.2) THEN
            WRITE(IOUT,1055) EQNAM(I),ISP
            STOP
          ENDIF
        ELSE
          IF (ISP.LT.0 .OR. ISP.GT.2) THEN
            WRITE(IOUT,1056) EQNAM(I),ISP
            STOP
          ENDIF
        ENDIF
 1055   FORMAT(/,' *** ERROR: PRIOR-INFORMATION EQUATION "',A,
     &'" USES AN INVALID VALUE OF',/,
     &' STAT-FLAG (STAT-FLAG = ',I5,').  FOR AN',
     &' EQUATION CONTAINING A LOG-TRANSFORMED',/,
     &' PARAMETER, STAT-FLAG MUST BE 0, 1, 2, 10, 11, OR 12.',/,
     &' -- STOP EXECUTION (SPES1BAS6PI)')
 1056   FORMAT(/,' *** ERROR: PRIOR-INFORMATION EQUATION "',A,
     &'" USES AN INVALID VALUE OF',/,
     &' STAT-FLAG (STAT-FLAG = ',I5,').  FOR AN',
     &' EQUATION CONTAINING PARAMETERS',/,
     &' THAT ARE NOT LOG-TRANSFORMED,',
     &' STAT-FLAG MUST BE 0, 1, OR 2.',/,
     &' -- STOP EXECUTION (SPES1BAS6PI)')
C
        IF (KTERMS.GT.1) THEN
C         PRINT COEFFICIENT AND PARAMETER FOR ADDITIONAL EQUATION TERMS
          IP1ST = 1
          DO 65 J = 2,KTERMS
            DO 63 IPCK = IP1ST,NPLIST
              IF (PRM(IPCK,I).NE.0. .AND. PARNAM(IPCK).NE.PNAM1) THEN
                WRITE (IOUT,1016) PRM(IPCK,I),PARNAM(IPCK)
                IPNEXT1 = IPCK + 1
                GOTO 64
              ENDIF
   63       CONTINUE
   64       CONTINUE
            IP1ST = IPNEXT1
   65     CONTINUE
        ENDIF
C
C----------CONVERT STATISTICS TO WEIGHTS; CALCULATE WP**.5 FOR PRINTING
        WPP = WP(I)
        BB = PRM(NPLIST+1,I)
C
C       IF STATISTIC IS NOT VARIANCE, CONVERT TO VARIANCE (ALSO CONVERT
C       TO REGRESSION SPACE, IF REQUIRED)
        IF (KLOGS.GT.0) THEN
C         EQUATION IS FOR A LOG-TRANSFORMED PARAMETER
          BB = LOG(BB)
          IF (ISP.EQ.2) THEN
C           STAT IS C.V. IN NATIVE SPACE - CONVERT TO S.D. IN NATIVE SP.
            WPP = WPP*EXP(BB)
          ELSEIF (ISP.EQ.12) THEN
C           STAT IS C.V. IN LOG10 SPACE - CONVERT TO S.D. IN LOG10 SPACE
            WPP = WPP*BB/CVLOG2LN
          ENDIF
C         CONVERT S.D. TO VARIANCE
          IF (ISP1.GT.0) WPP = WPP**2.
C         ... CONVERT VARIANCE ...
          IF (ISP.GE.10) THEN
C           ... FROM LOG BASE 10 SPACE TO LOG BASE e SPACE
            WPP = WPP*CVLOG2LN**2
          ELSE
C           ... FROM NATIVE SPACE TO LOG BASE e SPACE
            WPP = LOG((WPP/(EXP(BB))**2) + 1.0)
          ENDIF
        ELSE
C         EQUATION IS FOR UNTRANSFORMED PARAMETER(S)
          IF (ISP.EQ.2) WPP = WPP*BB
          IF (ISP.GT.0) WPP = WPP**2.
        ENDIF
C
C       ENSURE THAT WPP IS NOT TOO SMALL
        IF (WPP.EQ.0.0) THEN
          WRITE(IOUT,1057) EQNAM(I)
 1057     FORMAT(/,
     &' ERROR: THE VARIANCE FOR EQUATION "',A,'"  EVALUATES AS ZERO',/,
     &' -- STOP EXECUTION (SPES1BAS6PI)')
          STOP
        ELSEIF (WPP.LT.VARMIN) THEN
          WRITE(IOUT,1058) EQNAM(I),WPP,VARMIN
 1058     FORMAT(/,
     &' WARNING: THE VARIANCE CALCULATED FOR EQUATION "',A,'" IS ',
     &1PG11.4,/,
     &' SUCH SMALL VARIANCES CAN LEAD TO COMPUTATIONAL PROBLEMS.',/,
     &' TRY TO REDEFINE THE PARAMETER(S) SUCH THAT THE VARIANCE IS',/,
     &' AT LEAST ',G11.4)
        ENDIF
        WP(I) = (EV/WPP)**.5
C
 80   CONTINUE
C
      RETURN
C
C-----PRINT END-OF-FILE ERROR MESSAGE
 90   CONTINUE
      WRITE(IOUT,1060)
 1060 FORMAT(' END-OF-FILE REACHED DURING ATTEMPT TO ',
     &        'READ PRIOR INFORMATION--STOP EXECUTION')
      STOP
C
      END
C=======================================================================
      SUBROUTINE SPES1BAS6PE(IO,IOUT,MPR,PRM,LN,WP,D,RSQP,NRUNS,AVET,
     &                      NPOST,NNEGT,IPR,ND,WTRL,NRES,NPLIST,MPRAR,
     &                      IUGDO,OUTNAM,IPLOT,EQNAM,IPLPTR,
     &                      ISSWR,SSPI,ITMXP)
C-----VERSION 1000 01OCT1996
C     VERSION 19990504 ERB
C     FORMERLY SPR21O
C     ******************************************************************
C     CALCULATE AND PRINT WEIGHTED RESIDUALS FOR PARAMETER SUMS WITH
C     PRIOR INFORMATION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVE, AVET, BB, BDIF, BWP, D, PRM, RSQP, TEMP, TEMP1, 
     &     TEMP1E, TEMPE, VMAX, VMIN, WP, WPSR, WTRL
      INTEGER IIP, IMP, IO, IOUT, IPR, LFLAG, LN, MPR, ND, NNEG, NNEGT,
     &        NPOS, NPOST, NRES, NRESPE, NRUNS, NRUNSPE
      INTEGER IUGDO(6), IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*200 OUTNAM
      CHARACTER*10 EQNAM(MPRAR)
      DIMENSION WP(MPRAR), PRM(NPLIST+1,MPRAR), LN(NPLIST),
     &          D(ND+MPR+IPR), SSPI(ITMXP+1)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
  500 FORMAT (/,' PARAMETER SUMS WITH PRIOR INFORMATION',//,
     &        1X,'EQUATION',3X,'-------VALUES--------',14X,
     &        'WEIGHT    WEIGHTED',/,
     &        3X,'NAME',7X,'MEAS.',6X,'CALC.',5X,'RESIDUAL',5X,'**.5',
     &        5X,'RESIDUAL')
  505 FORMAT (1X,A,1X,E10.3,1X,E10.3,1X,E10.3,1X,G10.3,1X,G10.3)
  510 FORMAT (/,' STATISTICS FOR THESE RESIDUALS :',/,
     &        ' MAXIMUM WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' MINIMUM WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',G10.3,//,
     &        ' STATISTICS FOR PRIOR-INFORMATION EQUATIONS:',/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
  515 FORMAT (2G20.7)
  520 FORMAT (' ')
  540 FORMAT (2(G15.7,1X),I5,2X,A)
  550 FORMAT (G15.7,1X,I5,2X,A)
C
      IF (IO.EQ.1) WRITE (IOUT,500)
      NNEG = 0
      NPOS = 0
      NRESPE = 0
      NRUNSPE = 1
      WTRLPE = 0.0
      RSQMPR = 0.0
      VMAX = -1.E20
      VMIN = 1.E20
      AVE = 0.
      DO 20 IMP = 1, MPR
        IPLPTR(NRES+IMP) = ND + IMP
        TEMP = 0.0
        LFLAG = 0
        DO 10 IIP = 1, NPLIST
C           PRM ALLOWED TO SUM LOGS
          BB = B(IIP)
          IF (PRM(IIP,IMP).NE.0. .AND. LN(IIP).GT.0) THEN
            BB = LOG(BB)
            LFLAG = 1
          ENDIF
          TEMP = TEMP + PRM(IIP,IMP)*BB
   10   CONTINUE
        WPSR = WP(IMP)**.5
        TEMP1 = PRM(NPLIST+1,IMP)
        TEMP1E = TEMP1
        TEMPE = TEMP
        IF (LFLAG.NE.0) THEN
          TEMPE = EXP(TEMP)
          TEMP1E = EXP(TEMP1)
        ENDIF
        BDIF = TEMP1 - TEMP
        BWP = BDIF*WPSR
        IF (IO.EQ.1) THEN
          WRITE (IOUT,505) EQNAM(IMP), TEMP1E, TEMPE, BDIF, WPSR, BWP
          IF (OUTNAM.NE.'NONE') THEN
            WRITE (IUGDO(1),540) TEMP, TEMP1, IPLOT(ND+IMP),
     &                           EQNAM(IMP)
            IF (LFLAG.EQ.0) THEN
              WRITE (IUGDO(2),540) WPSR*TEMP, WPSR*TEMP1, IPLOT(ND+IMP),
     &                             EQNAM(IMP)
              WRITE (IUGDO(3),540) WPSR*TEMP, BWP, IPLOT(ND+IMP),
     &                             EQNAM(IMP)
            ELSE
              WRITE (IUGDO(2),540) WPSR*TEMPE, WPSR*TEMP1E,
     &                             IPLOT(ND+IMP), EQNAM(IMP)
              WRITE (IUGDO(3),540) WPSR*TEMPE, BWP, IPLOT(ND+IMP),
     &                             EQNAM(IMP)
            ENDIF
            WRITE (IUGDO(4),550) BDIF, IPLOT(ND+IMP), EQNAM(IMP)
            WRITE (IUGDO(5),550) BWP, IPLOT(ND+IMP), EQNAM(IMP)
          ENDIF
          IF (OUTNAM.NE.'NONE') D(NRES+IMP) = BWP
        ENDIF
        RSQP = RSQP + (BWP**2)
        RSQMPR = RSQMPR + (BWP**2)
        IF (BWP.GT.VMAX) VMAX = BWP
        IF (BWP.LT.VMIN) VMIN = BWP
        IF (BWP.GE.0.) NPOS = NPOS + 1
        IF (BWP.LT.0.) NNEG = NNEG + 1
        IF (NRES+IMP.GT.1 .AND. (WTRL*BWP).LT.0.) NRUNS = NRUNS + 1
        IF (IMP.GT.1 .AND. (WTRLPE*BWP).LT.0.)
     &      NRUNSPE = NRUNSPE + 1
        WTRL = BWP
        WTRLPE = BWP
        AVE = AVE + BWP
   20 CONTINUE
      IF (ISSWR.GT.0) SSPI(ISSWR) = SSPI(ISSWR) + RSQMPR
      NRES = NRES + MPR
      NRESPE = NRESPE + MPR
      AVET = AVET + AVE
      NPOST = NPOST + NPOS
      NNEGT = NNEGT + NNEG
      AVE = AVE/REAL(MPR)
      IF (IO.EQ.1) WRITE (IOUT,510) VMAX, VMIN, AVE, NPOS, NNEG,
     &                              NRUNSPE, NRESPE
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6PC(IPR,NIPR,BUFF,PRM,LN,IO,IOUT,WTPS,D,NRUNS,
     &                      AVET,NPOST,NNEGT,RSQP,MPR,ND,WTRL,NRES,
     &                      NPLIST,MPRAR,IPRAR,IPLOT,NAMES,OUTNAM,IUGDO,
     &                      IPLPTR,ISSWR,SSPI,ITMXP,BPRI)
C     VERSION 19990504 ERB
C     ******************************************************************
C     CALCULATE AND PRINT WEIGHTED RESIDUALS FOR PARAMETERS AND
C     PARAMETER SUMS WITH CORRELATED PRIOR INFORMATION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVE, AVET, BCALWP, BDIF1, BDIF2, BOBSWP, BPRI, BUFF, BWP, D,
     &     PRM, RSQP, VMAX, VMIN, WPSR, WTRL
      INTEGER I, I1, IO, IOUT, IPR, L, L1, LN, MPR, N, ND, NIPR, NNEG,
     &        NNEGT, NPOS, NPOST, NRES, NRESPC, NRUNS, NRUNSPC
      INTEGER IPLOT(ND+IPR+MPR), IUGDO(6), IPLPTR(ND+IPR+MPR)
      CHARACTER*12 NAMES(ND+IPR+MPR)
      CHARACTER*200 OUTNAM
      DIMENSION BPRI(IPRAR), PRM(NPLIST+1,MPRAR), LN(NPLIST),
     &          NIPR(IPRAR), BUFF(IPRAR), D(ND+MPR+IPR), SSPI(ITMXP+1),
     &          WTPS(IPRAR,IPRAR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
  500 FORMAT (/,' PARAMETERS AND PARAMETER SUMS WITH CORRELATED PRIOR',
     &        ' INFORMATION',//,16X,'UNWEIGHTED VALUES',2X,'UNWEIGHTED',
     &        4X,'WEIGHTED VALUES',6X,'WEIGHTED',/,1X,'PARAMETER',7X,
     &        'MEAS.',6X,'CALC.',3X,'RESIDUAL',5X,'MEAS.',6X,'CALC.',5X,
     &        'RESIDUAL')
  505 FORMAT (1X,A,2X,6G11.3)
  510 FORMAT (/,' STATISTICS FOR THESE RESIDUALS :',/,
     &        ' MAXIMUM WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' MINIMUM WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',G10.3,//,
     &        ' STATISTICS FOR CORRELATED PRIOR INFORMATION:',/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
  515 FORMAT (2G20.7)
  540 FORMAT (2(G15.7,1X),I5,2X,A)
  550 FORMAT (G15.7,1X,I5,2X,A)
C
      RSQIPR = 0.0
      IF (IO.EQ.1) WRITE (IOUT,500)
      NNEG = 0
      NPOS = 0
      NRESPC = 0
      NRUNSPC = 1
      WTRLPC = 0.0
      VMAX = -1.E20
      VMIN = 1.E20
      AVE = 0.0
      N = 0
      DO 40 I1 = 1, IPR
        IPLPTR(NRES+I1) = ND + MPR + I1
        I = NIPR(I1)
        BOBSWP = 0.0
        BCALWP = 0.0
        DO 30 L1 = 1, IPR
          L = NIPR(L1)
          WPSR = WTPS(I1,L1)
          IF (L.LE.NPLIST) THEN
            BDIF1 = BPRI(L1)
C---CHANGES SUGGESTED BY STEEN FOR LOG-TRANSFORMED PARAMETERS,  NEW NOT
CC MH: "LET STEEN LOOK AT THIS"
            IF (LN(L).GT.0) THEN
              BDIF2 = LOG(B(L))
            ELSE
              BDIF2 = B(L)
            ENDIF
          ELSEIF (L.GT.NPLIST) THEN
            BDIF1 = PRM(NPLIST+1,L-NPLIST)
            BDIF2 = BUFF(L-NPLIST)
          ENDIF
          BOBSWP = BOBSWP + WPSR*BDIF1
          BCALWP = BCALWP + WPSR*BDIF2
   30   CONTINUE
        BWP = BOBSWP - BCALWP
        RSQP = RSQP + BWP**2
        RSQIPR = RSQIPR + BWP**2
        IF (IO.EQ.1) THEN
          BOBS = BPRI(I1)
          BCAL = B(I)
          WRITE (IOUT,505) PARNAM(I), BOBS, BCAL, BOBS - BCAL,
     &                     BOBSWP, BCALWP, BWP
          IF (OUTNAM.NE.'NONE') THEN
            WRITE (IUGDO(1),540) BCAL, BOBS, IPLOT(ND+MPR+I1),
     &                           NAMES(ND+MPR+I1)
            WRITE (IUGDO(2),540) BCALWP, BOBSWP, IPLOT(ND+MPR+I1),
     &                           NAMES(ND+MPR+I1)
            WRITE (IUGDO(3),540) BCALWP, BWP, IPLOT(ND+MPR+I1),
     &                           NAMES(ND+MPR+I1)
            WRITE (IUGDO(4),550) BOBS-BCAL, IPLOT(ND+MPR+I1),
     &                           NAMES(ND+MPR+I1)
            WRITE (IUGDO(5),550) BWP, IPLOT(ND+MPR+I1), NAMES(ND+MPR+I1)
          ENDIF
          IF (OUTNAM.NE.'NONE') D(NRES+I1) = BWP
        ENDIF
        IF (BWP.GT.VMAX) VMAX = BWP
        IF (BWP.LT.VMIN) VMIN = BWP
        IF (BWP.GE.0.) NPOS = NPOS + 1
        IF (BWP.LT.0.) NNEG = NNEG + 1
        N = N + 1
        IF (N.GT.1 .AND. (WTRL*BWP).LT.0.) NRUNS = NRUNS + 1
        IF (N.GT.1 .AND. (WTRLPC*BWP).LT.0.) NRUNSPC = NRUNSPC + 1
        WTRL = BWP
        WTRLPC = BWP
        AVE = AVE + BWP
   40 CONTINUE
      IF (ISSWR.GT.0) SSPI(ISSWR) = SSPI(ISSWR) + RSQIPR
      NRES = NRES + IPR
      NRESPC = NRESPC + IPR
      AVET = AVET + AVE
      NPOST = NPOST + NPOS
      NNEGT = NNEGT + NNEG
      AVE = AVE/REAL(IPR)
      IF (IO.EQ.1) WRITE (IOUT,510) VMAX, VMIN, AVE, NPOS, NNEG,
     &                              NRUNSPC, NRESPC
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6EV(C,EIGV,EIGW,NPE,NPLIST,EIGL,IOUT,B1,ITERPF,
     &                       BUFF,IPRC)
C-----VERSION 1000 01FEB1992
C     ******************************************************************
C     CALCULATE EIGENVALUES AND EIGENVECTORS OF COVARIANCE MATRIX ON THE
C     PARAMETERS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL B1, BUFF, RATIO, TEMP
      INTEGER I, IDUM, IIP, IOUT, IP, IPRC, ITERPF,
     &        J, K, KK, L, LL, NPE, NPLIST
      CHARACTER*16 ANAME
      DOUBLE PRECISION C(NPE,NPE), EIGL(NPE), EIGV(NPE,NPE), EIGW(NPE)
      DIMENSION B1(NPLIST), BUFF(NPE,NPE)
      INCLUDE 'param.inc'
      DATA ANAME /'SCALED W/ PARAM.'/
C     ------------------------------------------------------------------
  500 FORMAT (/,A4,8D13.4,/,10(4X,8D13.4))
  505 FORMAT (/,4X,8D13.4,/,10(4X,8D13.4))
  510 FORMAT (/,' EIGENVALUES')
  515 FORMAT (/,' EIGENVECTORS')
  520 FORMAT (/,10X,A16,/,10X,16('-'))
C---------SCALE VARIANCE-COVARIANCE MATRIX WITH PARAMETER VALUES
      DO 20 IP = 1, NPE
        DO 10 IIP = 1, NPE
          EIGV(IP,IIP) = 0.0
   10   CONTINUE
        EIGL(IP) = 0.0
        EIGW(IP) = 0.0
   20 CONTINUE
      IF (ITERPF.GT.1) THEN
        DO 40 IP = 1, NPE
          IIPP = IPPTR(IP)
          DO 30 IIP = 1, NPE
            C(IP,IIP) = C(IP,IIP)/(B(IIPP)*B(IPPTR(IIP)))
   30     CONTINUE
   40   CONTINUE
      ELSE
        DO 60 IP = 1, NPE
          IIPP = IPPTR(IP)
          DO 50 IIP = 1, NPE
            C(IP,IIP) = C(IP,IIP)/(B1(IIPP)*B1(IPPTR(IIP)))
   50     CONTINUE
   60   CONTINUE
      ENDIF
      DO 66 J = 1,NPE
        DO 64 I = 1, NPE
          BUFF(I,J) = C(I,J)
   64   CONTINUE
   66 CONTINUE
      DO 80 IP = 1, NPE
        DO 70 I = 1, NPE
          BUFF(I,IP) = C(I,IP)
   70   CONTINUE
   80 CONTINUE
      IDUM = 1
      WRITE (IOUT,520) ANAME
      CALL ULAPRW(BUFF,ANAME,IDUM,IDUM,NPE,NPE,-IDUM,IPRC,IOUT)
C-------CALCULATE EIGENVALUES AND EIGENVECTORS
      DO 100 IP = 1, NPE
        DO 90 I = 1, NPE
          EIGV(I,IP) = C(I,IP)
   90   CONTINUE
  100 CONTINUE
      CALL SPES1BAS6TR(EIGV,NPE,NPE,EIGL,EIGW)
      CALL SPES1BAS6TQ(EIGL,EIGW,NPE,NPE,EIGV)
      CALL SPES1BAS6ES(EIGL,EIGV,NPE,NPE)
C-------SCALE EIGENVECTORS TO FORM UNIT VECTORS
      DO 130 I = 1, NPE
        TEMP = 0.0
        DO 110 J = 1, NPE
          TEMP = TEMP + EIGV(J,I)**2
  110   CONTINUE
        TEMP = TEMP**.5
        DO 120 J = 1, NPE
          EIGV(J,I) = EIGV(J,I)/TEMP
  120   CONTINUE
  130 CONTINUE
C-------PRINT EIGENVALUES
      WRITE (IOUT,510)
      WRITE (IOUT,505) (EIGL(J),J=1,NPE)
C-------PRINT EIGENVECTORS
      WRITE (IOUT,515)
      DO 140 J = 1, NPE
        WRITE (IOUT,500) PARTYP(IPPTR(J)), (EIGV(J,I),I=1,NPE)
  140 CONTINUE
C-------EIGENVECTOR TEST (FROM PRESS AND OTHERS, 1989)
      WRITE (IOUT,'(/1X,A)') 'EIGENVECTOR TEST'
      DO 180 J = 1, NPE
        DO 160 L = 1, NPE
          EIGW(L) = 0.0
          DO 150 K = 1, NPE
            IF (K.GT.L) THEN
              KK = L
              LL = K
            ELSE
              KK = K
              LL = L
            ENDIF
            EIGW(L) = EIGW(L) + C(LL,KK)*EIGV(K,J)
  150     CONTINUE
  160   CONTINUE
        WRITE (IOUT,'(/1X,A,I3)') 'VECTOR NUMBER', J
        WRITE (IOUT,'(/1X,T7,A,T18,A,T31,A)') 'VECTOR', 'MTRX*VEC ', 
     &                                        'RATIO'
        DO 170 L = 1, NPE
          RATIO = EIGW(L)/EIGV(L,J)
          WRITE (IOUT,'(1X,3G12.5)') EIGV(L,J), EIGW(L), RATIO
  170   CONTINUE
  180 CONTINUE
C---------UNSCALE VARIANCE-COVARIANCE MATRIX
      IF (ITERPF.GT.1) THEN
        DO 200 IP = 1, NPE
          IIPP = IPPTR(IP)
          DO 190 IIP = 1, NPE
            C(IP,IIP) = C(IP,IIP)*(B(IIPP)*B(IPPTR(IIP)))
  190     CONTINUE
  200   CONTINUE
      ELSE
        DO 220 IP = 1, NPE
          IIPP = IPPTR(IP)
          DO 210 IIP = 1, NPE
            C(IP,IIP) = C(IP,IIP)*(B1(IIPP)*B1(IPPTR(IIP)))
  210     CONTINUE
  220   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6WR(WTP,IPR,NIPR,IOUT,IU,DETWTP,NPLIST,
     &                       MPR,EV,IERR,ISENS,NIPRNAM,IPRAR,IPLOT,
     &                       NAMES,ND,BPRI,LN,WTPS,W3,W4)
C     VERSION 20000201 ERB
C     ******************************************************************
C     READ AND PREPARE FULL WEIGHT MATRIX FOR PARAMETERS
C     ******************************************************************
C      SPECIFICATIONS
      REAL BPRI, DETWTP, EV
      INTEGER I, IERR, IOUT, IPR, IU, J, K, LN, MPR, N,
     &        NIPR, NPLIST
      INTEGER IPLOT(ND+IPR+MPR)
      CHARACTER*10 NIPRNAM, PNAME, NIPRNAMU
      CHARACTER*12 NAMES(ND+IPR+MPR)
      CHARACTER*16 ANAME
      DOUBLE PRECISION CONVLOG, TEN, W3(IPRAR,IPRAR),
     &                 W4(IPRAR)
      DIMENSION NIPR(IPRAR), ISENS(NPLIST), LN(NPLIST),
     &          NIPRNAM(IPRAR), BPRI(IPRAR), WTP(IPRAR,IPRAR),
     &          WTPS(IPRAR,IPRAR)
      DATA ANAME/'                '/
      INCLUDE 'param.inc'
C-----------------------------------------------------------------------
  500 FORMAT (//,6X,'WEIGHT MATRIX FOR CORRELATED PRIOR',/,6X,75('-'))
  505 FORMAT (//,6X,'SQUARE-ROOT OF WEIGHT MATRIX FOR CORREALTED PRIOR',
     &        /,6X,75('-'))
C
  510 FORMAT (1X,6G13.6)
  515 FORMAT (/,' PARAMETER NUMBER',I5,' PID=',A4,' HAS WEIGHTING ',
     &        'SPECIFIED IN DATA SET 10 AND WITH THE FULL WEIGHT',
     &        ' MATRIX.',/,' THE DATA SET 10 WT VALUE IS IGNORED.',/)
  520 FORMAT (/' MULTIPLE PRIOR NUMBER',I5,' IS INCLUDED IN THE FULL ',
     &        'WEIGHT MATRIX.  THE DATA SET 12 WT VALUE IS IGNORED.')
  525 FORMAT (/,' SQUARE-ROOT OF THE WEIGHT MATRIX FOR PARAMETERS ',
     &        'WITH CORRELATIONS')
  530 FORMAT (/,' WEIGHT MATRIX FOR PARAMETERS WITH CORRELATIONS',/)
  535 FORMAT (//,' NIPR NAME',3X,'PLOT-SYMBOL',/,1X,10('-'),2X,11('-'))
C
  540 FORMAT (//,' CORRELATED PRIOR INFORMATION STATISTICS',/,
     &        ' DIAGONAL TERMS ARE COEFFICIENTS OF VARIATION; ',
     &        'OFF-DIAGONALS ARE CORRELATION COEFFICIENTS',/)
  545 FORMAT (/,' VARIANCE-COVARIANCE MATRIX IS NOT POSITIVE-DEFINITE',
     &        /,' -- STOP EXECUTION (SPES1BAS6WR)',/)
  550 FORMAT (/,' CAN NOT USE AN UNESTIMATED PARAMETER',I5,
     &        ' -- STOP EXECUTION (SPES1BAS6WR)',/)
  555 FORMAT (/,' "',A10,'" IN ITEM 7 IS NOT DEFINED AS A ',
     &        'PARAMETER -- STOP EXECUTION (SPES1BAS6WR)',/)
  560 FORMAT (1X,A10,2X,I7)
  565 FORMAT (/,' CORRELATED PRIOR INFORMATION STATISTICS',/,' DIAGONAL'
     &        ,' TERMS ARE VARIANCES; OFF-DIAGONALS ARE COVARIANCES',/)
C
      TEN = 10.0
      CONVLOG = LOG(TEN)
C-----READ WHICH PARAMETERS OR PARAMETER SUMS ARE INVOLVED IN THE
C-----FULL WEIGHT MATRIX, AND CHECK AGAINST OTHER PRIOR (PES ITEM 7)
      WRITE (IOUT,535)
      DO 8 I = 1,IPR
C       READ PES ITEM 7
        READ (IU,*) NIPRNAM(I), BPRI(I), IPLOT(ND+MPR+I)
        NAMES(ND+MPR+I) = NIPRNAM(I)
        CALL UCASE(NIPRNAM(I),NIPRNAMU,1)
        DO 4 J = 1,NPLIST
          CALL UCASE(PARNAM(J),PNAME,1)
          IF (PNAME.EQ.NIPRNAMU) THEN
            NIPR(I) = J
            GOTO 6
          ENDIF
 4      CONTINUE
        WRITE (IOUT,555) NIPRNAM(I)
        IERR = 1
 6      CONTINUE
        WRITE (IOUT,560) NIPRNAM(I), IPLOT(ND+MPR+I)
 8    CONTINUE
C
      DO 10 I = 1, IPR
        N = NIPR(I)
        IF (ISENS(N).LE.0) THEN
          WRITE (IOUT,550) N
          IERR = 1
        ENDIF
   10 CONTINUE
C------READ AND WRITE MATRIX USED TO DEFINE THE WEIGHTING
C     READ PES ITEM 8
      READ(IU,*) IWTP
      IF(IWTP .EQ. 0) WRITE (IOUT,565)
      IF(IWTP .EQ. 1) WRITE (IOUT,540)
      DO 20 K = 1, IPR
        READ (IU,*) (WTP(K,I),I=1,IPR)
        WRITE (IOUT,510) (WTP(K,I),I=1,IPR)
C    CONVERT LOG10 TO LOG
        IF(IWTP .EQ. 0) THEN
          DO 15 I = 1,IPR
            IF (LN(NIPR(K)).GT.0) WTP(K,I) = WTP(K,I) * CONVLOG
            IF (LN(NIPR(I)).GT.0) WTP(K,I) = WTP(K,I) * CONVLOG
   15     CONTINUE
        ENDIF
C     CONVERT COEFFICIENTS OF VARIATION TO STANDARD DEVIATIONS
        IF(IWTP .EQ. 1) THEN
          IF (LN(NIPR(K)).LE.0 .AND. BPRI(K).NE.0.0)
     &               WTP(K,K) = WTP(K,K) * BPRI(K)
          IF (LN(NIPR(K)).GT.0 .AND. LOG(BPRI(K)).NE.0.0)
     &               WTP(K,K) = WTP(K,K) * LOG(BPRI(K))
        ENDIF
   20 CONTINUE
C     CONVERT CORRELATIONS TO COVARIANCES, AND STANDARD DEVIATIONS TO
C     VARIANCES
      IF(IWTP.EQ.1) THEN
        DO 30 K=1,IPR
          DO 25 I=1,IPR
            IF(K .NE. I) WTP(K,I)=WTP(K,I)*WTP(I,I)*WTP(K,K)
   25     CONTINUE
   30   CONTINUE
        DO 40 K=1,IPR
          WTP(K,K) = WTP(K,K)**2
   40   CONTINUE
      ENDIF
C     DECOMPOSE
      CALL SVD (IPR,WTP,WTPS,DETWTP,W3,W4,IPRAR)
C     MULTIPLY BY COMMON ERROR VARIANCE
      DETWTP = -LOG(EV)+DETWTP
      TMP = SQRT(EV)
      DO 70 I = 1, IPR
        DO 60 J = 1, IPR
          WTP(I,J) = EV*WTP(I,J)
          WTPS(I,J) = TMP*WTPS(I,J)
   60   CONTINUE
   70 CONTINUE
C     PRINT, USING FORMAT 6G11.4
      IPRN = 20
      WRITE (IOUT,500)
      CALL ULAPRW(WTP,ANAME,0,0,IPR,IPR,0,IPRN,IOUT)
      WRITE (IOUT,505)
      CALL ULAPRW(WTPS,ANAME,0,0,IPR,IPR,0,IPRN,IOUT)
C--------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6TR(A,N,NP,D,E)
C-----VERSION 1000 01FEB1992
C
C     ******************************************************************
C     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, SUBROUTINE TRED2,
C     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
C     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
C     MODIFIED FOR DOUBLE PRECISION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION A, D, E, F, G, H, HH, SCALE
      INTEGER I, J, K, L, N, NP
      DIMENSION A(NP,NP), D(NP), E(NP)
C     ------------------------------------------------------------------
      IF (N.GT.1) THEN
        DO 80 I = N, 2, -1
          L = I - 1
          H = 0.0
          SCALE = 0.0
          IF (L.GT.1) THEN
            DO 10 K = 1, L
              SCALE = SCALE + DABS(A(I,K))
   10       CONTINUE
            IF (SCALE.EQ.0.0) THEN
              E(I) = A(I,L)
            ELSE
              DO 20 K = 1, L
                A(I,K) = A(I,K)/SCALE
                H = H + A(I,K)**2
   20         CONTINUE
              F = A(I,L)
              G = -SIGN(DSQRT(H),F)
              E(I) = SCALE*G
              H = H - F*G
              A(I,L) = F - G
              F = 0.0
              DO 50 J = 1, L
                A(J,I) = A(I,J)/H
                G = 0.0
                DO 30 K = 1, J
                  G = G + A(J,K)*A(I,K)
   30           CONTINUE
                IF (L.GT.J) THEN
                  DO 40 K = J+1, L
                    G = G + A(K,J)*A(I,K)
   40             CONTINUE
                ENDIF
                E(J) = G/H
                F = F + E(J)*A(I,J)
   50         CONTINUE
              HH = F/(H+H)
              DO 70 J = 1, L
                F = A(I,J)
                G = E(J) - HH*F
                E(J) = G
                DO 60 K = 1, J
                  A(J,K) = A(J,K) - F*E(K) - G*A(I,K)
   60           CONTINUE
   70         CONTINUE
            ENDIF
          ELSE
            E(I) = A(I,L)
          ENDIF
          D(I) = H
   80   CONTINUE
      ENDIF
      D(1) = 0.0
      E(1) = 0.0
      DO 130 I = 1, N
        L = I - 1
        IF (D(I).NE.0.0) THEN
          DO 110 J = 1, L
            G = 0.0
            DO 90 K = 1, L
              G = G + A(I,K)*A(K,J)
   90       CONTINUE
            DO 100 K = 1, L
              A(K,J) = A(K,J) - G*A(K,I)
  100       CONTINUE
  110     CONTINUE
        ENDIF
        D(I) = A(I,I)
        A(I,I) = 1.0
        IF (L.GE.1) THEN
          DO 120 J = 1, L
            A(I,J) = 0.0
            A(J,I) = 0.0
  120     CONTINUE
        ENDIF
  130 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6TQ(D,E,N,NP,Z)
C-----VERSION 1000 01FEB1992
C
C     ******************************************************************
C     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, TQLI,
C     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
C     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
C     MODIFIED FOR DOUBLE PRECISION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION B, C, D, DD, E, F, G, P, R, S, Z
      INTEGER I, ITER, K, L, M, N, NP
      CHARACTER*1 CHR
      DIMENSION D(NP), E(NP), Z(NP,NP)
C     ------------------------------------------------------------------
      IF (N.GT.1) THEN
        DO 10 I = 2, N
          E(I-1) = E(I)
   10   CONTINUE
        E(N) = 0.0
        DO 70 L = 1, N
          ITER = 0
   20     DO 30 M = L, N-1
            DD = DABS(D(M)) + DABS(D(M+1))
            IF (DABS(E(M))+DD.EQ.DD) GOTO 40
   30     CONTINUE
          M = N
   40     IF (M.NE.L) THEN
            IF (ITER.EQ.30) THEN
              WRITE (*,*) ' TOO MANY ITERATIONS'
              READ (*,'(A)') CHR
            ENDIF
            ITER = ITER + 1
            G = (D(L+1)-D(L))/(2.0*E(L))
            R = DSQRT(G**2+1.0)
            G = D(M) - D(L) + E(L)/(G+SIGN(R,G))
            S = 1.0
            C = 1.0
            P = 0.0
            DO 60 I = M-1, L, -1
              F = S*E(I)
              B = C*E(I)
              IF (DABS(F).GE.DABS(G)) THEN
                C = G/F
                R = DSQRT(C**2+1.0)
                E(I+1) = F*R
                S = 1.0/R
                C = C*S
              ELSE
                S = F/G
                R = DSQRT(S**2+1.0)
                E(I+1) = G*R
                C = 1.0/R
                S = S*C
              ENDIF
              G = D(I+1) - P
              R = (D(I)-G)*S + 2.0*C*B
              P = S*R
              D(I+1) = G + P
              G = C*R - B
              DO 50 K = 1, N
                F = Z(K,I+1)
                Z(K,I+1) = S*Z(K,I) + C*F
                Z(K,I) = C*Z(K,I) - S*F
   50         CONTINUE
   60       CONTINUE
            D(L) = D(L) - P
            E(L) = G
            E(M) = 0.0
            GOTO 20
          ENDIF
   70   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6ES(D,V,N,NP)
C-----VERSION 1000 01FEB1992
C
C     ******************************************************************
C     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, EIGSRT,
C     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
C     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
C     MODIFIED FOR DOUBLE PRECISION AND TO ORDER THE EIGENVALUES FROM
C     SMALLEST TO LARGEST
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION D, P, V
      INTEGER I, J, K, N, NP
      DIMENSION D(NP), V(NP,NP)
C     ------------------------------------------------------------------
      DO 30 I = 1, N-1
        K = I
        P = D(I)
        DO 10 J = I+1, N
          IF (D(J).LE.P) THEN
            K = J
            P = D(J)
          ENDIF
   10   CONTINUE
        IF (K.NE.I) THEN
          D(K) = D(I)
          D(I) = P
          DO 20 J = 1, N
            P = V(J,I)
            V(J,I) = V(J,K)
            V(J,K) = P
   20     CONTINUE
        ENDIF
   30 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6TS(IDOF,TST)
C-----VERSION 1000 01DEC1997
C     ******************************************************************
C     DETERMINE THE VALUE OF THE T STATISTIC NEEDED TO CALCULATE LINEAR
C     INDIVIDUAL CONFIDENCE INTERVALS FOR A TWO-SIDED SIGNIFICANCE LEVEL
C     OF 0.05
C     ******************************************************************
C        SPECIFICATIONS:
      REAL TST, TABLE
      INTEGER IDOF, I, ITABLE
C     ------------------------------------------------------------------
      DIMENSION ITABLE(35), TABLE(35)
      DATA (ITABLE(I),I=1,35)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
     &      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
     &      28, 29, 30, 40, 60, 120, 240, 500/
      DATA (TABLE(I),I=1,35)/12.706, 4.303, 3.182, 2.776, 2.571, 2.447,
     &      2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160, 2.145,
     &      2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074,
     &      2.069, 2.064, 2.060, 2.056, 2.052, 2.048, 2.045, 2.042, 
     &      2.021, 2.000, 1.980, 1.970, 1.960/
C     ------------------------------------------------------------------
C
      IF (IDOF.LE.30) THEN  
        TST=TABLE(IDOF)
        RETURN
      ENDIF
      DO 10 I=31,35
        IF(IDOF.LE.ITABLE(I)) THEN
          TST = TABLE(I-1)+(TABLE(I)-TABLE(I-1))*
     &         REAL(IDOF-ITABLE(I-1))/REAL(ITABLE(I)-ITABLE(I-1))
          RETURN
        ENDIF
   10 CONTINUE
      TST=TABLE(35)
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1BAS6FS(NP,IDOF,TST)
C     ******************************************************************
C     DETERMINE THE VALUE OF THE F STATISTIC NEEDED TO CALCULATE
C     BEALE'S MEASURE OF LINEARITY AND SCHEFFE CONFIDENCE INTERVALS
C     -- MODIFIED FROM UCODE VERSION -- ERB 9/23/99
C     ******************************************************************
C        SPECIFICATIONS:
      REAL TST, T
      INTEGER IDOF, I, J, ITABLE1, ITABLE2, NP
C     ------------------------------------------------------------------
      DIMENSION ITABLE1(19), ITABLE2(34), T(19,34)
C     NP is indicator for table1
      DATA (ITABLE1(I),I=1,19)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12,
     &      15, 20, 24, 30, 40, 60, 120, 32000/
C     IDOF is indicator for table2
      DATA (ITABLE2(I),I=1,34)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
     &      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
     &      28, 29, 30, 40, 60, 120, 32000/
C     TABLE IS SET UP AS (NP,IDOF)
C
      DATA (T(1,I),I=1,34)
     &     /161.4, 18.51, 10.13, 7.71, 6.61, 5.99, 5.59,
     &      5.32, 5.12, 4.96, 4.84, 4.75, 4.67, 4.60, 4.54, 4.49,
     &      4.45, 4.41, 4.38, 4.35, 4.32, 4.30, 4.28, 4.26, 4.24,
     &      4.23, 4.21, 4.20, 4.18, 4.17, 4.08, 4.00, 3.92, 3.84/
C
      DATA (T(2,I),I=1,34)
     &     /199.5,19.00,9.55,6.94,5.79,5.14,4.74,
     &      4.46, 4.26, 4.10, 3.98, 3.89, 3.81, 3.74, 3.68, 3.63,
     &      3.59, 3.55, 3.52, 3.49, 3.47, 3.44, 3.42, 3.40, 3.39,
     &      3.37, 3.35, 3.34, 3.33, 3.32, 3.23, 3.15, 3.07, 3.00/
C
      DATA (T(3,I),I=1,34)/215.7,19.16,9.28,6.59,5.41,4.76,4.35,
     &      4.07, 3.86, 3.71, 3.59, 3.49, 3.41, 3.34, 3.29, 3.24,
     &      3.20, 3.16, 3.13, 3.10, 3.07, 3.05, 3.03, 3.01, 2.99,
     &      2.98, 2.96, 2.95, 2.93, 2.92, 2.84, 2.76, 2.68, 2.60/
C
      DATA (T(4,I),I=1,34)/224.6,19.25,9.12,6.39,5.19,4.53,4.12,
     &      3.84, 3.63, 3.48, 3.36, 3.26, 3.18, 3.11, 3.06, 3.01,
     &      2.96, 2.93, 2.90, 2.87, 2.84, 2.82, 2.80, 2.78, 2.76,
     &      2.74, 2.73, 2.71, 2.70, 2.69, 2.61, 2.53, 2.45, 2.37/
C
      DATA (T(5,I),I=1,34)/230.2,19.30,9.01,6.26,5.05,4.39,3.97,
     &      3.69, 3.48, 3.33, 3.20, 3.11, 3.03, 2.96, 2.90, 2.85,
     &      2.81, 2.77, 2.74, 2.71, 2.68, 2.66, 2.64, 2.62, 2.60,
     &      2.59, 2.57, 2.56, 2.55, 2.53, 2.45, 2.37, 2.29, 2.21/
C
      DATA (T(6,I),I=1,34)/234.0,19.33,8.94,6.16,4.95,4.28,3.87,
     &      3.58, 3.37, 3.22, 3.09, 3.00, 2.92, 2.85, 2.79, 2.74,
     &      2.70, 2.66, 2.63, 2.60, 2.57, 2.55, 2.53, 2.51, 2.49,
     &      2.47, 2.46, 2.45, 2.43, 2.42, 2.34, 2.25, 2.17, 2.10/
C
      DATA (T(7,I),I=1,34)/236.8,19.35,8.89,6.09,4.88,4.21,3.79,
     &      3.50, 3.29, 3.14, 3.01, 2.91, 2.83, 2.76, 2.71, 2.66,
     &      2.61, 2.58, 2.54, 2.51, 2.49, 2.46, 2.44, 2.42, 2.40,
     &      2.39, 2.37, 2.36, 2.35, 2.33, 2.25, 2.17, 2.09, 2.01/
C
      DATA (T(8,I),I=1,34)/238.9,19.37,8.85,6.04,4.82,4.15,3.73,
     &      3.44, 3.23, 3.07, 2.95, 2.85, 2.77, 2.70, 2.64, 2.59,
     &      2.55, 2.51, 2.48, 2.45, 2.42, 2.40, 2.37, 2.36, 2.34,
     &      2.32, 2.31, 2.29, 2.28, 2.27, 2.18, 2.10, 2.02, 1.94/
C
      DATA (T(9,I),I=1,34)/240.5,19.38,8.81,6.00,4.77,4.10,3.68,
     &      3.39, 3.18, 3.02, 2.90, 2.80, 2.71, 2.65, 2.59, 2.54,
     &      2.49, 2.46, 2.42, 2.39, 2.37, 2.34, 2.32, 2.30, 2.28,
     &      2.27, 2.25, 2.24, 2.22, 2.21, 2.12, 2.04, 1.96, 1.88/
C
      DATA (T(10,I),I=1,34)/241.9,19.40,8.79,5.96,4.74,4.06,3.64,
     &      3.35, 3.14, 2.98, 2.85, 2.75, 2.67, 2.60, 2.54, 2.49,
     &      2.45, 2.41, 2.38, 2.35, 2.32, 2.30, 2.27, 2.25, 2.24,
     &      2.22, 2.20, 2.19, 2.18, 2.16, 2.08, 1.99, 1.91, 1.83/
C
      DATA (T(11,I),I=1,34)/243.9,19.41,8.74,5.91,4.68,4.00,3.57,
     &      3.28, 3.07, 2.91, 2.79, 2.69, 2.60, 2.53, 2.48, 2.42,
     &      2.38, 2.34, 2.31, 2.28, 2.25, 2.23, 2.20, 2.18, 2.16,
     &      2.15, 2.13, 2.12, 2.10, 2.09, 2.00, 1.92, 1.83, 1.75/
C
      DATA (T(12,I),I=1,34)/245.9,19.43,8.70,5.86,4.62,3.94,3.51,
     &      3.22, 3.01, 2.85, 2.72, 2.62, 2.53, 2.46, 2.40, 2.35,
     &      2.31, 2.27, 2.23, 2.20, 2.18, 2.15, 2.13, 2.11, 2.09,
     &      2.07, 2.06, 2.04, 2.03, 2.01, 1.92, 1.84, 1.75, 1.67/
C
      DATA (T(13,I),I=1,34)/248.0,19.45,8.66,5.80,4.56,3.87,3.44,
     &      3.15, 2.94, 2.77, 2.65, 2.54, 2.46, 2.39, 2.33, 2.28,
     &      2.23, 2.19, 2.16, 2.12, 2.10, 2.07, 2.05, 2.03, 2.01,
     &      1.99, 1.97, 1.96, 1.94, 1.93, 1.84, 1.75, 1.66, 1.57/
C
      DATA (T(14,I),I=1,34)/249.1,19.45,8.64,5.77,4.53,3.84,3.41,
     &      3.12, 2.90, 2.74, 2.61, 2.51, 2.42, 2.35, 2.29, 2.24,
     &      2.19, 2.15, 2.11, 2.08, 2.05, 2.03, 2.01, 1.98, 1.96,
     &      1.95, 1.93, 1.91, 1.90, 1.89, 1.79, 1.70, 1.61, 1.52/
C
      DATA (T(15,I),I=1,34)/250.1,19.46,8.62,5.75,4.50,3.81,3.38,
     &      3.08, 2.86, 2.70, 2.57, 2.47, 2.38, 2.31, 2.25, 2.19,
     &      2.15, 2.11, 2.07, 2.04, 2.01, 1.98, 1.96, 1.94, 1.92,
     &      1.90, 1.88, 1.87, 1.85, 1.84, 1.74, 1.65, 1.55, 1.46/
C
      DATA (T(16,I),I=1,34)/251.1,19.47,8.59,5.72,4.46,3.77,3.34,
     &      3.04, 2.83, 2.66, 2.53, 2.43, 2.34, 2.27, 2.20, 2.15,
     &      2.10, 2.06, 2.03, 1.99, 1.96, 1.94, 1.91, 1.89, 1.87,
     &      1.85, 1.84, 1.82, 1.81, 1.79, 1.69, 1.59, 1.50, 1.39 /
C
      DATA (T(17,I),I=1,34)/252.2,19.48,8.57,5.69,4.43,3.74,3.30,
     &      3.01, 2.79, 2.62, 2.49, 2.38, 2.30, 2.22, 2.16, 2.11,
     &      2.06, 2.02, 1.98, 1.95, 1.92, 1.89, 1.86, 1.84, 1.82,
     &      1.80, 1.79, 1.77, 1.75, 1.74, 1.64, 1.53, 1.43, 1.32/
C
      DATA (T(18,I),I=1,34)/253.3,19.49,8.55,5.66,4.40,3.70,3.27,
     &      2.97, 2.75, 2.58, 2.45, 2.34, 2.25, 2.18, 2.11, 2.06,
     &      2.01, 1.97, 1.93, 1.90, 1.87, 1.84, 1.81, 1.79, 1.77,
     &      1.75, 1.73, 1.71, 1.70, 1.68, 1.58, 1.47, 1.35, 1.22/
C
      DATA (T(19,I),I=1,34)/254.3,19.50,8.53,5.63,4.36,3.67,3.23,
     &      2.93, 2.71, 2.54, 2.40, 2.30, 2.21, 2.13, 2.07, 2.01,
     &      1.96, 1.92, 1.88, 1.84, 1.81, 1.78, 1.76, 1.73, 1.71,
     &      1.69, 1.67, 1.65, 1.64, 1.62, 1.51, 1.39, 1.25, 1.00/
C
C     ------------------------------------------------------------------
C
      IF (NP.LE.10.AND.IDOF.LE.30) THEN
C       ENTRIES ARE EXACT FOR FIRST AND SECOND SUBSCRIPTS
        TST=T(NP,IDOF)
        RETURN
      ENDIF
C
      IF (NP.GT.10.AND.IDOF.LE.30) THEN
C       INTERPOLATE BETWEEN ENTRIES FOR FIRST SUBSCRIPT;
C       ENTRIES FOR SECOND SUBSCRIPT ARE EXACT
        DO 10 I=11,19
          IF(NP.LE.ITABLE1(I)) THEN
            TST = ((T(I,IDOF)-T(I-1,IDOF))*REAL(NP-ITABLE1(I-1))
     &            /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,IDOF)
            RETURN
          ENDIF
   10   CONTINUE
        TST = T(19,IDOF)
        RETURN
      ENDIF
C
      IF (NP.LE.10.AND.IDOF.GT.30) THEN
C       INTERPOLATE BETWEEN ENTRIES FOR SECOND SUBSCRIPT;
C       ENTRIES FOR FIRST SUBSCRIPT ARE EXACT
        DO 20 J=31,34
          IF(IDOF.LE.ITABLE2(J)) THEN
            TST = ((T(NP,J)-T(NP,J-1))*REAL(IDOF-ITABLE2(J-1))
     &            /REAL(ITABLE2(J)-ITABLE2(J-1)))+T(NP,J-1)
            RETURN
          ENDIF
   20   CONTINUE
        TST = T(NP,34)
        RETURN
      ENDIF
C
      IF (NP.LE.ITABLE1(19)) THEN
        IF (IDOF.LE.ITABLE2(34)) THEN
C         INTERPOLATE BETWEEN ENTRIES FOR FIRST AND SECOND SUBSCRIPTS
          DO 40 I=11,19
            IF(NP.LE.ITABLE1(I)) THEN
              DO 30 J=31,34
                IF(IDOF.LE.ITABLE2(J)) THEN
                  TST1 = ((T(I,J)-T(I-1,J))*REAL(NP-ITABLE1(I-1))
     &                   /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,J)
                  TST2 = ((T(I,J-1)-T(I-1,J-1))*REAL(NP-ITABLE1(I-1))
     &                   /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,J-1)
                  TST = ((TST1-TST2)*REAL(IDOF-ITABLE2(J-1))
     &                   /REAL(ITABLE2(J)-ITABLE2(J-1)))+TST2
                  RETURN
                ENDIF
   30         CONTINUE
            ENDIF
   40     CONTINUE
        ELSE
C         NP IS WITHIN RANGE OF VALUES IN ITABLE1, BUT IDOF EXCEEDS
C         LARGEST VALUE IN ITABLE2
          DO 50 I=11,19
            IF (NP.LE.ITABLE1(I)) THEN
              TST = ((T(I,34)-T(I-1,34))*REAL(NP-ITABLE1(I-1))
     &              /REAL(ITABLE1(I)-ITABLE1(I-1)))+T(I-1,34)
              RETURN
            ENDIF
   50     CONTINUE
        ENDIF
      ENDIF
C
      TST=T(19,34)
C
C     RETURN
      END
