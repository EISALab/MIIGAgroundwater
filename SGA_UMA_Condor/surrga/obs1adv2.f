C     Last change:  ERB  29 Jul 2002    9:42 am


      SUBROUTINE OBS1ADV2AL(IOUADV,NPTH,NTT2,IOUTT2,KTDIM,KTFLG,KTREV,
     &                      ADVSTP,IOUT,LCICLS,LCPRST,LCTT2,LCPOFF,
     &                      LCNPNT,ND,ISUM,ISUMI,NROW,NCOL,NLAY,IUNIT,
     &                      NIUNIT,LCDRAI,MXDRN,NDRAIN,LCRIVR,MXRIVR,
     &                      LCBNDS,MXBND,NBOUND,LCIRCH,LCRECH,ICSTRM,
     &                      LCSTRM,MXSTRM,NSTREM,IOBSUM,LCOBADV,NOBADV,
     &                      ITMXP,LCSSAD,NDRNVL,NGHBVL,NRIVVL,NRIVER,
     &                      IOBS,FSNK,NBOTM,LCHANI,LCHKCC,LCHUFTHK,NHUF,
     &                      LCWELL,NWELVL,MXWELL,NWELLS)
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR ADVECTIVE-TRANSPORT OBSERVATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      REAL ADVSTP
      INTEGER IOUADV, NPTH, NTT2, IOUTT2, KTDIM, KTFLG, KTREV,
     &        IOUT, LCICLS, LCPOFF, LCNPNT, ND, ISUM, 
     &        NROW, NCOL, NLAY, IUNIT, NBOTM
      DIMENSION IUNIT(NIUNIT)
C---LOCAL:
      INTEGER LCPRST, LCTT2
      CHARACTER*200 LINE
C----------------------------------------------------------------------
C     IDENTIFY PROCESS
      WRITE(IOUT,490) IOUADV
  490 FORMAT(/,' ADV2 -- OBSERVATION PROCESS (ADVECTIVE TRANSPORT',
     &    ' OBSERVATIONS)',/,' VERSION 2.4, 07/09/2002',/,
     &    ' INPUT READ FROM UNIT ',I3)
C
C  Turn off observation package if OBS is not active
      IF(IOBS.LE.0) THEN
        WRITE(IOUT,610)
610     FORMAT(/,1X,'WARNING: OBSERVATION (OBS) FILE IS NOT LISTED BUT',
     &      ' THE ADV OBSERVATION',/,' FILE (ADV) IS',
     &     ' LISTED -- TURNING OFF ADV OBSERVATIONS (OBS1ADV2AL)')
        IOUADV = 0
        RETURN
      ENDIF
C
C%%%%%READ PARTICLE TRACKING INFO 11FEB1995 ER ANDERMAN
      CALL URDCOM(IOUADV,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPTH,DUM,IOUT,IOUADV)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NTT2,DUM,IOUT,IOUADV)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOUTT2,DUM,IOUT,IOUADV)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KTFLG,DUM,IOUT,IOUADV)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KTREV,DUM,IOUT,IOUADV)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,ADVSTP,IOUT,IOUADV)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FSNK,IOUT,IOUADV)
      IF (NLAY.EQ.1) THEN
        KTDIM = 2
      ELSE
        KTDIM = 3
      ENDIF
C
C%%%%%WRITE TRAVEL TIME INFO
      WRITE (IOUT,505) NPTH, NTT2, IOUTT2, KTDIM, KTFLG, ADVSTP,
     &                 KTREV, FSNK
      IF (KTREV.NE.-1 .AND. KTREV.NE.1) THEN
        WRITE (IOUT,500)
        STOP
  500   FORMAT (
     &  ' KTREV NEEDS TO BE EQUAL TO -1 OR 1.  STOP EXECUTION ',
     &  '(OBS1ADV2AL)')
      ENDIF
  505 FORMAT (/,
     &  ' NUMBER OF PARTICLES ................................',I5,/,
     &  ' TOTAL NUMBER OF ADVECTIVE-TRANSPORT OBSERVATIONS ...',I5,/,
     &  ' OUTPUT UNIT NUMBER FOR ADVECTIVE-TRANSPORT INFO ....',I5,//,
     &  ' TWO- OR THREE-DIMENSIONAL TRACKING (KTDIM)..........',I5,/,
     &  ' PARTICLE-TRACKING TIME-STEP FLAG (KTFLG)............',I5,/,
     &  ' TIME STEP (IF KTFLG>1)..............................',G15.8,/,
     &  ' FORWARD OR BACKWARD PARTICLE TRACKING (KTREV).......',I5,/,
     &  ' WEAK SINK FLAG/FRACTION.............................',G15.8)
  510 FORMAT (6I5,F10.0)
C
C%%%%%%%%%%%ADVECTIVE-TRANSPORT OBSERVATION DATA ARRAYS
      LCICLS = ISUMI
      ISUMI = ISUMI + NPTH*3
      LCPRST = ISUM
      ISUM = ISUM + NROW*NCOL*NBOTM
      LCTT2 = ISUM
      ISUM = ISUM + NTT2
      LCPOFF = ISUM
      ISUM = ISUM + NPTH*3
      LCNPNT = ISUMI
      ISUMI = ISUMI + NPTH
C
      ND = ND + KTDIM*NTT2
      NOBADV = KTDIM*NTT2
      LCSSAD = ISUM
      ISUM = ISUM + ITMXP + 1
C     POINTER TO OBSERVATION ARRAYS
      LCOBADV = IOBSUM
      IOBSUM = IOBSUM + NOBADV

C
C-------INITIALIZE POINTERS AND DIMESIONS FOR ARRAYS THAT MAY BE REFERENCED
C       BUT MAY NOT GET ALLOCATED
      IF (IUNIT(2).EQ.0) THEN
        LCWELL = 1
        NWELVL = 1
        MXWELL = 1
        NWELLS = 0
      ENDIF
      IF (IUNIT(3).EQ.0) THEN
        LCDRAI = 1
        NDRNVL = 1
        MXDRN = 1
        NDRAIN = 0
      ENDIF
      IF (IUNIT(4).EQ.0) THEN
        LCRIVR = 1
        NRIVVL = 1
        MXRIVR = 1
        NRIVER = 0
      ENDIF
      IF (IUNIT(7).EQ.0) THEN
        LCBNDS = 1
        NGHBVL = 1
        MXBND = 1
        NBOUND = 0
      ENDIF
      IF (IUNIT(8).EQ.0) THEN
        LCIRCH = 1
        LCRECH = 1
      ENDIF
      IF (IUNIT(18).EQ.0) THEN
        ICSTRM = 1
        LCSTRM = 1
        NSTREM = 0
        MXSTRM = 1
      ENDIF
C
C-------ASSIGN LPF/HUF DUMMY VALUES
      IF(IUNIT(23).EQ.0) THEN
        LCHANI=1
      ENDIF
      IF(IUNIT(37).EQ.0) THEN
        LCHKCC=1
        LCHUFTHK=1
        NHUF=1
      ENDIF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE OBS1ADV2RP(IOUT,NROW,NCOL,NLAY,PRST,NPTH,NPNT,
     &                      NTT2,NHT,NQT,OBSNAM,ICLS,POFF,TT2,HOBS,DELR,
     &                      DELC,WTQ,ND,KTDIM,IOUADV,NDMH,IOWTQ,BOTM,
     &                      NBOTM,IPLOT,NAMES,IPR,MPR,JT)
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      INTEGER IOUT, NROW, NCOL, NLAY, NPTH, NPNT, NTT2, NHT, NQT, 
     &        ICLS, ND, KTDIM, IOUADV, NDMH, IOWTQ, NBOTM
      INTEGER IPLOT(ND+IPR+MPR)
      REAL PRST, POFF, TT2, HOBS, DELR, DELC, WTQ
      DIMENSION WTQ(NDMH,NDMH), DELR(NCOL), 
     &          DELC(NROW), POFF(3,NPTH), HOBS(ND), ICLS(3,NPTH), 
     &          PRST(NCOL,NROW,NBOTM), TT2(NTT2), 
     &          NPNT(NPTH),
     &          BOTM(NCOL,NROW,0:NBOTM)
      CHARACTER*12 OBSNAM(ND), NAMES(ND+IPR+MPR)
C---LOCAL:
      REAL BLANK, T2COF, T2LOF, T2ROF, ZWT
      INTEGER I, I11, I12, I13, I2, IPNT, IPRN, IXWT, IYWT, IZWT, J, K, 
     &        NTPNT, T2LAY, T2COL, T2ROW
      CHARACTER*24 ANAME(2)
      CHARACTER*20 FMTIN
      CHARACTER*200 LINE
C---COMMON:
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
C----------------------------------------------------------------------
      EXTERNAL U2DREL, ULAPRW
C----------------------------------------------------------------------
      DATA ANAME(1)/'       POROSITY OF LAYER'/
      DATA ANAME(2)/'COV OF ADV-TR OBSERVTNS '/
C----------------------------------------------------------------------
C%%%%%FORMAT STATEMENTS
  500 FORMAT (/,'   ADVECTIVE-TRANSPORT OBSERVATION DATA',/,1X,41('-'))
  505 FORMAT (/,' ERRORS IN ADVECTIVE-TRANSPORT DATA -- STOP EXECUTION')
  510 FORMAT (/,' 2D PARTICLE TRACKING USED IN LAYER 1',/)
  515 FORMAT (/,' PSUEDO-3D PARTICLE TRACKING USED IN LAYER 1',/)
  520 FORMAT (/,' 3D PARTICLE TRACKING USED IN ALL LAYERS',/)
  525 FORMAT (4I5,3F5.0)
  530 FORMAT (/,'   PATH #',I2,' INITIAL LOCATION       ',/,
     & ' LAYER   ROW    COL    LOFF    ROFF   COFF ',/,44('-'),/,
     & 3(I5,2X),3(F6.3,2X),//,
     & '  OBSERVED INTERMEDIATE PATH LOCATION(S)   ',/,
     & ' OBS #',6X,'ID',12X,'TIME',9X,'LAY ROW COL  LOFF   ROFF   COFF',
     &  /,70('-'))
  535 FORMAT (A4,1X,3I5,3F5.0,3(F5.0,I3),F10.0,I5)
  540 FORMAT (I3,'-',I3,1X,A12,2X,G15.8,3I4,1X,3(F6.3,1X))
  545 FORMAT (40I5)
  550 FORMAT ( //,'SUMMARY OF ADVECTIVE-TRANSPORT OBSERVATIONS:',/,
     &' OBS #',5X,'ID',5X,'DIRECTION',2X,'TIME',11X,'VALUE',11X,
     & 'WEIGHT',9X,'TYPE',/,80('-'))
  560 FORMAT (I5,2X,A12,3X,A1,2X,3(G15.8,1X),I5)
  570 FORMAT (I5,2X,12X,3X,A1,2X,16X,2(G15.8,1X),I5)
C=======================================================================
C
C     WRITE BANNER
      WRITE (IOUT,500)
C
C     SET TIME STEP IF NECESSARY
      IF(JT.LT.1) JT=1
C
C     READ POROSITIES (ITEM 1)
      DO 20 K = 1, NLAY
        CALL U2DREL(PRST(1,1,LBOTM(K)),ANAME(1),NROW,NCOL,K,IOUADV,IOUT)
        IF(LAYCBD(K).NE.0) CALL U2DREL(PRST(1,1,LBOTM(K)+1),
     &      ANAME(1),NROW,NCOL,K,IOUADV,IOUT)
   20 CONTINUE
C
C     WRITE 2D, PSUEDO-3D, OR 3D TRACKING
      IF (KTDIM.NE.1) THEN
        IF (NLAY.GT.1) KTDIM = 3
        IF (KTDIM.EQ.2) WRITE (IOUT,510)
        IF (KTDIM.EQ.3 .AND. NLAY.EQ.1) WRITE (IOUT,515)
        IF (KTDIM.EQ.3 .AND. NLAY.GT.1) WRITE (IOUT,520)
C----------------------------------------------------------------------
C     START READING OBSERVATIONS
        I2 = NHT + NQT + 1
        NTPNT = 0
        DO 80 I = 1, NPTH
C---------Read ADV ITEM 2
          CALL URDCOM(IOUADV,IOUT,LINE)
          LLOC = 1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPNT(I),DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLS(1,I),DUM,IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLS(2,I),DUM,IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLS(3,I),DUM,IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,POFF(1,I),IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,POFF(2,I),IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,POFF(3,I),IOUT,
     &                IOUADV)
C          READ (IOUADV,525,ERR=110) NPNT(I), (ICLS(J,I),J=1,3), 
C     &                              (POFF(J,I),J=1,3)
          WRITE (IOUT,530) I, (ICLS(J,I),J=1,3), (POFF(J,I),J=1,3)
          IF(NPNT(I).LE.0) THEN
            WRITE(IOUT,505)
            WRITE(IOUT,*) ' NPNT MUST BE LARGER THAN 0'
            STOP
          ENDIF
          DO 70 IPNT = 1, NPNT(I)
            NTPNT = NTPNT + 1
            I11 = I2 - NHT
            I12 = I11 + 1
            I13 = I11 + 2
C---------Read ADV ITEM 3
          CALL URDCOM(IOUADV,IOUT,LINE)
          LLOC = 1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,DUM,IOUT,IOUADV)
          OBSNAM(I2)=LINE(ISTART:ISTOP)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,T2LAY,DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,T2ROW,DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,T2COL,DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,T2LOF,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,T2ROF,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,T2COF,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,WTQ(I11,I11),IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IXWT,DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,WTQ(I12,I12),IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IYWT,DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,ZWT,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IZWT,DUM,IOUT,IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,TT2(NTPNT),IOUT,
     &                IOUADV)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPLOT(I2),DUM,IOUT,
     &                IOUADV)
C            READ (IOUADV,535,ERR=110) OBSNAM(I2), T2LAY, T2ROW, T2COL, 
C     &                                T2LOF, T2ROF, T2COF, WTQ(I11,I11),
C     &                                IXWT, WTQ(I12,I12), IYWT, ZWT, 
C     &                                IZWT, TT2(NTPNT), IPLOT(I2)
            NAMES(I2) = OBSNAM(I2)
            IF (KTDIM.EQ.3) WTQ(I13,I13) = ZWT
C
C%%%%%CALCULATE GLOBAL COORDINATES OF OBSERVATION
            HOBS(I2) = 0.0
            HOBS(I2+1) = 0.0
            IF (KTDIM.EQ.3) HOBS(I2+2) = 0.0
            DO 30 J = 1, T2COL-1
              HOBS(I2) = HOBS(I2) + DELR(J)
   30       CONTINUE
            DO 40 J = 1, T2ROW-1
              HOBS(I2+1) = HOBS(I2+1) + DELC(J)
   40       CONTINUE
            IF (KTDIM.EQ.3) HOBS(I2+2)=BOTM(T2COL,T2ROW,LBOTM(T2LAY)-1)
            HOBS(I2) = HOBS(I2) + (T2COF+0.5)*DELR(T2COL)
            HOBS(I2+1) = HOBS(I2+1) + (T2ROF+0.5)*DELC(T2ROW)
            IF (KTDIM.EQ.3) HOBS(I2+2) = HOBS(I2+2) - (0.5-T2LOF)
     &           *(BOTM(T2COL,T2ROW,LBOTM(T2LAY)-1)
     &           - BOTM(T2COL,T2ROW,LBOTM(T2LAY)))
C%%%%%ASSIGN DATA IDENTIFIER AND PLOT-SYMBOL TO NEXT TWO OBSERVATIONS
            OBSNAM(I2+1) = OBSNAM(I2)
            NAMES(I2+1) = OBSNAM(I2)
            IPLOT(I2+1) = IPLOT(I2)
            IF (KTDIM.EQ.3)  THEN
              OBSNAM(I2+2) = OBSNAM(I2)
              NAMES(I2+2) = OBSNAM(I2)
              IPLOT(I2+2) = IPLOT(I2)
            ENDIF
C%%%%%CALCULATE WEIGHTS
            IF (IXWT.EQ.2) WTQ(I11,I11) = WTQ(I11,I11)*HOBS(I2)
            IF (IXWT.GT.0) WTQ(I11,I11) = WTQ(I11,I11)*WTQ(I11,I11)
            IF (IYWT.EQ.2) WTQ(I12,I12) = WTQ(I12,I12)*HOBS(I2+1)
            IF (IYWT.GT.0) WTQ(I12,I12) = WTQ(I12,I12)*WTQ(I12,I12)
            IF (IZWT.EQ.2 .AND. KTDIM.EQ.3) 
     &          WTQ(I13,I13) = WTQ(I13,I13)*HOBS(I2+2)
            IF (IZWT.GT.0 .AND. KTDIM.EQ.3) 
     &          WTQ(I13,I13) = WTQ(I13,I13)*WTQ(I13,I13)
            IF (KTDIM.EQ.2) THEN
              WRITE (IOUT,540) I2, I2+1, OBSNAM(I2), TT2(NTPNT), T2LAY, 
     &                         T2ROW, T2COL, T2LOF, T2ROF, T2COF
c     &                         HOBS(I2), WTQ(I11,I11), IXWT, HOBS(I2+1), 
c     &                         WTQ(I12,I12), IYWT
            ELSEIF (KTDIM.EQ.3) THEN
              WRITE (IOUT,540) I2, I2+2, OBSNAM(I2), TT2(NTPNT), T2LAY, 
     &                         T2ROW, T2COL, T2LOF, T2ROF, T2COF 
c     &                         HOBS(I2), WTQ(I11,I11), IXWT, HOBS(I2+1), 
c     &                         WTQ(I12,I12), IYWT, HOBS(I2+2), 
c     &                         WTQ(I13,I13), IZWT
            ENDIF
            I2 = I2 + KTDIM
   70     CONTINUE
   80   CONTINUE
C     PRINT SUMMARY INFORMATION
        WRITE(IOUT,550)
        I2 = NHT + NQT + 1
        NTPNT = 0
        DO 100 I = 1, NPTH
          DO 90 IPNT = 1, NPNT(I)
            NTPNT = NTPNT + 1
            I11 = I2 - NHT
            I12 = I11 + 1
            I13 = I11 + 2
            WRITE (IOUT,560) I2, OBSNAM(I2),'X', TT2(NTPNT),
     &                       HOBS(I2),WTQ(I11,I11), IXWT
            WRITE (IOUT,570) I2+1, 'Y', HOBS(I2+1),WTQ(I12,I12), IYWT
            IF (KTDIM.EQ.3)
     &        WRITE (IOUT,570) I2+2, 'Z', HOBS(I2+2), 
     &                         WTQ(I13,I13), IZWT
            I2 = I2 + KTDIM
   90     CONTINUE
  100   CONTINUE
c%%%%%read and write full weight matrix
        IPRN = 0
        READ (IOUADV,*) IOWTQAD
        IF (IOWTQAD.GT.0) THEN
          IOWTQ = 1
          READ (IOUADV,*) FMTIN, IPRN
          NTTOB1 = NQT + 1
          NTTOB2 = NQT + NTT2*KTDIM
          DO 120 I = NTTOB1, NTTOB2
            READ (IOUADV,FMTIN) (BLANK,J=NTTOB1,I-1),
     &                          (WTQ(I,J),J=I,NTTOB2)
            DO 110 J = I, NTTOB2
              IF (I.EQ.J) THEN
                IF (WTQ(I,J).LT.0.0) WTQ(I,J) = -WTQ(I,J)
              ELSE
                WTQ(J,I) = WTQ(I,J)
              ENDIF
  110       CONTINUE
  120     CONTINUE
          IF (IPRN.GE.0) THEN
            WRITE (IOUT,555) ANAME(2)
            CALL UARRSUBPRW(WTQ,NDMH,NDMH,NTTOB1,NTTOB2,NTTOB1,NTTOB2,
     &                      IPRN,IOUT,OBSNAM(NHT+1),NDMH)
          ENDIF
        ENDIF
  555   FORMAT (//,6X,A24,/,6X,75('-'))
        RETURN
      ENDIF
c  110 WRITE (IOUT,505)
      STOP
      END
C
C=======================================================================
C
      SUBROUTINE OBS1ADV2P(NROW,NCOL,NLAY,DELC,DELR,IOUT,CR,CC,CV,HNEW,
     &                 IBOUND,OBSNAM,POFF,NHT,NQT,NTT2,NPTH,NPNT,KTDIM,
     &                 KTFLG,KTREV,ADVSTP,ICLS,PRST,IP,RMLT,HK,IZON,H,
     &                 X,NPE,ND,TT2,IPRINT,ITERP,IOUTT2,MXBND,NBOUND,
     &                 BNDS,NRCHOP,IRCH,RECH,MXSTRM,NSTREM,ISTRM,STRM,
     &                 MXRIVR,NRIVER,RIVR,MXDRN,NDRAIN,DRAI,SV,
     &                 NMLTAR,NZONAR,BOTM,NBOTM,WELL,NWELVL,MXWELL,
     &                 NWELLS,SNEW,VKA,IUHFB,HFB,MXACTFB,NHFB,HANI,
     &                 NGHBVL,NRIVVL,NDRNVL,LAYHDT,LN,NPLIST,ISCALS,
     &                 FSNK,WTQ,NDMH,BSCAL,HKCC,HUFTHK,NHUF,IULPF,IUHUF)
C     VERSION 1000 07OCT1993
C             1001 11JAN1994
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     MAIN SUBROUTINE TO TRACK A PARTICLE USING LINEAR VELOCITY
C     INTERPOLATION AND THEN THE SEMI-ANALYTICAL TRACKING SCHEME.
C     THIS IS CALLED FROM MAIN ONLY
C     SUBROUTINES CALLED ARE:
C        SOBS1ADV2L = LINEAR INTERPOLATION
C        SOBS1ADV2S = SEMI-ANALYTICAL TRACKING
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      REAL DELC, DELR, CR, CC, CV, POFF, ADVSTP, PRST, RMLT, 
     &     HK, HKCC,H, X, TT2, BNDS, RECH, STRM, RIVR,
     &     DRAI, SV, BOTM, VKA, HANI, HUFTHK
      INTEGER NROW, NCOL, NLAY, IOUT, IBOUND, NHT, NQT, NTT2, NPTH, 
     &        KTDIM, KTFLG, KTREV, ICLS, IP, 
     &        IZON, NPE, ND, IPRINT, ITERP, IOUTT2,
     &        MXBND, NBOUND, NRCHOP, IRCH, MXSTRM, NSTREM, ISTRM,
     &        MXRIVR, NRIVER, MXDRN, NDRAIN, NBOTM,
     &        LN(NPLIST)
      CHARACTER*12 OBSNAM(ND)
      INTEGER NPNT(NPTH)
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW), 
     &          POFF(3,NPTH), ICLS(3,NPTH),
     &          PRST(NCOL,NROW,NBOTM), RMLT(NCOL,NROW,NMLTAR),
     &          HK(NCOL,NROW,NLAY), IZON(NCOL,NROW,NZONAR),
     &          H(ND), X(NPE,ND), TT2(NTT2),
     &          CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY), 
     &          CV(NCOL,NROW,NLAY), BNDS(NGHBVL,MXBND), IRCH(NCOL,NROW),
     &          RECH(NCOL,NROW), ISTRM(5,MXSTRM), STRM(11,MXSTRM), 
     &          RIVR(NRIVVL,MXRIVR), DRAI(NDRNVL,MXDRN), LAYHDT(NLAY),
     &          SV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          VKA(NCOL,NROW,NLAY), HFB(MXACTFB), HANI(NCOL,NROW,NLAY),
     &          WTQ(NDMH,NDMH),BSCAL(NPLIST), WELL(NWELVL,MXWELL),
     &          HUFTHK(NCOL,NROW,NHUF,2), HKCC(NCOL,NROW,NLAY)
C---LOCAL:
      REAL AX, AY, AZ, DAX, DAY, DAZ, DDX, DDY, DDZ, DL, DT, 
     &     DTP, DVXL, DVXP, DVXR, DVYB, DVYP, DVYT, DVZB, DVZP, 
     &     DVZT, DX, DXP, DY, DYP, DZ, DZP, HD, PSTP, TP, VP, VXL, 
     &     VXP, VXR, VYB, VYP, VYT, VZB, VZP, VZT, XL, XP, XPC, YP, YPC,
     &     YT, ZP, ZPC, BB
      INTEGER I, IEXIT, IND, IPT, IPTH, J, JPT, KPT, KPTH, 
     &        NEWI, NEWJ, NEWK, NPART, NTPNT, OLDK
C---COMMON:
      INCLUDE 'param.inc'
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
C----------------------------------------------------------------------
      EXTERNAL U2DREL, ULAPRW
C-----------------------------------------------------------------------
  500 FORMAT (//,7X,'ENTERING PARTICLE TRACKING ROUTINE (OBS1ADV2P)',/,
     & 1X,57('-'),/,
     & ' SUBROUTINE (OBS1ADV2P) IS HARDWIRED WITH THE FOLLOWING ',
     & 'OPTIONS:'
     & ,/,' LINEAR VELOCITY INTERPOLATION (SOBS1ADV2L)',/,
     & ' SEMI-ANALYTICAL PARTICLE TRACKING (SOBS1ADV2S)')
C-----FORWARD RUN HEADER
  505 FORMAT (//,' ADVECTIVE-TRANSPORT OBSERVATION NUMBER',I4,/,
     &'         PARTICLE TRACKING LOCATIONS AND TIMES:',/,
     & 1X,'LAYER ROW  COL',3X,'X-POSITION',6X,'Y-POSITION',
     & 6X,'Z-POSITION',8X,'TIME',/,80('-'))
C-----SENSITIVITY HEADERS
  510 FORMAT (//,' ADVECTIVE-TRANSPORT OBSERVATION NUMBER',I4,
     &'     PARAMETER #:',I4,' TYPE: ',A4,/,
     &'  PARTICLE TRACKING LOCATIONS, TIMES, AND SENSITIVITIES:')
  511 FORMAT ('     SENSITIVITIES ARE SCALED BY B*(WT**.5)')
  512 FORMAT (
     & 1X,'LAYER ROW  COL',2X,'X-SENSIVITY',5X,'Y-SENSIVITY',
     & 5X,'Z-SENSIVITY',8X,'TIME',/,80('-'))
C-----VELOCITY TAG LINE AT END OF PATH
  525 FORMAT ('END OF PATH ',I4,' REACHED',/,
     &  ' AVERAGE PARTICLE VELOCITY ALONG PATH: ',G15.8)
  530 FORMAT (' PARTICLE STEPPED OUT OF GRID PREMATURELY AND WILL BE',
     &        ' PROJECTED UNTIL TIME OF OBSERVATION (OBS1ADV2P)')
  531 FORMAT (8X,'PROJECTED')
  535 FORMAT (' PARTICLE NO. ',I4,' ABOVE WATER TABLE - MOVED TO WATER',
     &        ' TABLE (OBS1ADV2P)')
  540 FORMAT (//,' ALL PARTICLES MOVED EXITING (OBS1ADV2P)',//)
  550 FORMAT (/,' SENSITIVITIES ARE SCALED USING ALTERNATE SCALING',
     &' FACTOR')
  560 FORMAT ('PARTICLE ENTERING CONFINING UNIT')
C-----------------------------------------------------------------------
C-----INITIALIZE PARTICLE TIME STEPS
      IF (KTFLG.EQ.1) ADVSTP = 1.E+20
C-----INITIALIZE POINTERS
      IND = NHT + NQT + 1
      NTPNT = 0
C-----PRINT HEADER TO INDENTIFY ROUTINE AND OPTIONS SELECTED
      IF (IPRINT.EQ.0 .AND. ITERP.LT.2) WRITE (IOUT,500)
C
C------START OUTER ITERATIONS FOR EACH PARTICLE
      DO 100 NPART = 1, NPTH
        PSTP = 0.0
C
C------ASSIGN PARTICLE TO CELL AND CALCULATE INITIAL X AND Y POSITION
C
        KPT = ICLS(1,NPART)
        IPT = ICLS(2,NPART)
        JPT = ICLS(3,NPART)
        XL = 0.0
        YT = 0.0
        DO 10 I = 1, IPT-1
          YT = YT + DELC(I)
   10   CONTINUE
        DO 20 J = 1, JPT-1
          XL = XL + DELR(J)
   20   CONTINUE
        IF (KTDIM.EQ.3) THEN
          ZB = BOTM(JPT,IPT,LBOTM(KPT))
        ELSE
          ZP = 0.0
          ZPC = 0.0
        ENDIF
C
C     CALCULATE THE CELL AND GLOBAL COORDINATES OF THE PARTICLE
        XPC = (POFF(3,NPART)+0.5)*DELR(JPT)
        YPC = (POFF(2,NPART)+0.5)*DELC(IPT)
        HD = HNEW(JPT,IPT,KPT)
        IF (KTDIM.EQ.3) THEN
          IF(LAYHDT(KPT).NE.0.AND.HD.LT.BOTM(JPT,IPT,LBOTM(KPT)-1)) THEN
            DL=HD-BOTM(JPT,IPT,LBOTM(KPT))
          ELSE
            DL=BOTM(JPT,IPT,LBOTM(KPT)-1)-BOTM(JPT,IPT,LBOTM(KPT))
          ENDIF
          ZPC = (POFF(1,NPART)+0.5)*DL
        ENDIF
        XP = XL + XPC
        YP = YT + YPC
        IF (KTDIM.EQ.3) ZP = ZB + ZPC
        TP = 0.0
        VP = 0.0
        DTP = 0.0
        IEXIT = 0
        VXP = 0.0
        VYP = 0.0
        VZP = 0.0
        DXP = 0.0
        DYP = 0.0
        DZP = 0.0
        DT = 0.0
        DTC = 0.0
C
C     CHECK TO SEE IF Z POSITION IS ABOVE WATER TABLE FOR VARIABLE
C       SATURATED THICKNESS LAYERS
        IF (LAYHDT(KPT).NE.0 .AND. ZP.GT.HD) THEN
          WRITE (IOUT,535) NPART
          ZPC = HD-BOTM(JPT,IPT,LBOTM(KPT))
          ZP = HD
        ENDIF
C
C------WRITE INITIAL POSITION TO A FILE
C
        IF (IP.GT.0) THEN
          IIPP = IPPTR(IP)
          LNIIPP = LN(IIPP)
        ENDIF
        IF (IP.GT.0 .AND. (IPRINT.EQ.0.AND.ITERP.LT.2)) THEN
          WRITE (IOUT,510) NPART, IIPP, PARTYP(IIPP)
          IF(ISCALS.GT.0) THEN
            IF (LN(IIPP).LE.0) THEN
C             PARAMETER IS NOT LOG-TRANSFORMED
              IF (ABS(BB).LT.BSCAL(IIPP)) THEN
                WRITE (IOUT,550)
              ELSE
                WRITE(IOUT,511)
              ENDIF
            ELSE
C             PARAMETER IS LOG-TRANSFORMED
              WRITE(IOUT,511)
            ENDIF
          ENDIF
          WRITE(IOUT,512)
          IF (IOUTT2.GT.0) WRITE (IOUTT2,*) IIPP,PARTYP(IIPP)
        ELSEIF (IPRINT.EQ.0 .AND. ITERP.LT.2) THEN
          WRITE (IOUT,505) NPART
          IF (IOUTT2.GT.0) WRITE (IOUTT2,*) NPART
        ENDIF
        IF (IP.NE.0) THEN
          BB = ABS(B(IIPP))
          IF (LN(IIPP).LE.0) THEN
C           PARAMETER IS NOT LOG-TRANSFORMED
            IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
          ELSE
C           PARAMETER IS LOG-TRANSFORMED
            IF (BB.EQ.0.0) BB = 1.0
          ENDIF
        ELSE
          BB = 0.0
        ENDIF
        CALL SOBS1ADV2WR(1,IP,IPRINT,ITERP,PSTP,IOUT,KPT,IPT,JPT,XP,YP,
     &               ZP,TP,DXP,DYP,DZP,BB,IOUTT2,IND,VXP,VYP,VZP,VP,
     &               KTDIM,OBSNAM(IND),KTFLG,ISCALS,LNIIPP,WTQ(IND-NHT,
     &               IND-NHT))
 
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C------START ITERATIONS TO MOVE PARTICLE THROUGH GRID
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        DO 90 IPTH = 1, NPNT(NPART)
          NTPNT = NTPNT + 1
   50     IF ((IPT.LT.0) .OR. (IPT.GT.NROW) .OR. (JPT.LT.0) .OR. 
     &        (JPT.GT.NCOL) .OR. (IBOUND(JPT,IPT,KPT).EQ.0) .OR. 
     &        (KPT.LT.1.OR.KPT.GT.NLAY)) GOTO 100
          IF(DTC.GT.0) GOTO 60
C
C     CALL SOBS1ADV2L TO CALCULATE INTERPOLATED VELOCITY (AND
C           SENSITIVITIES IF IP.GT.0)
          CALL SOBS1ADV2L(IPT,JPT,KPT,NROW,NCOL,NLAY,CR,CC,CV,HNEW,
     &                IBOUND,PRST,KTREV,DELC,DELR,DL,XPC,YPC,ZPC,
     &                AX,AY,AZ,VXP,VYP,VZP,VXL,VXR,VYT,VYB,VZT,VZB,
     &                IEXIT,IOUT,IP,RMLT,HK,IZON,DXP,DYP,DZP,
     &                DVXL,DVXR,DVYT,DVYB,DVZT,DVZB,DAX,DAY,DAZ,DVXP,
     &                DVYP,DVZP,MXBND,NBOUND,BNDS,NRCHOP,IRCH,RECH,
     &                MXSTRM,NSTREM,ISTRM,STRM,MXRIVR,NRIVER,RIVR,MXDRN,
     &                NDRAIN,DRAI,SV,KTDIM,NMLTAR,
     &                NZONAR,BOTM,NBOTM,SNEW,VKA,IUHFB,HFB,MXACTFB,
     &                NHFB,HANI,NGHBVL,NRIVVL,NDRNVL,LAYHDT,FSNK,
     &                HKCC,HUFTHK,NHUF,IULPF,IUHUF,WELL,NWELVL,MXWELL,
     &                NWELLS)
          IF (IEXIT.GT.0) GOTO 100
C
C     CALL SOBS1ADV2S TO MOVE THE PARTICLE AND CALCULATE SENSITIVITIES
          CALL SOBS1ADV2S(NCOL,NROW,NLAY,IOUT,IBOUND,HNEW,DELR,DELC,DL,
     &                VXL,VXR,VYT,VYB,VZT,VZB,VXP,VYP,VZP,AX,AY,AZ,
     &                XPC,YPC,ZPC,IPT,JPT,KPT,TP,ZP,DX,DY,DZ,DT,ADVSTP,
     &                KTFLG,NEWI,NEWJ,NEWK,IEXIT,IP,DXP,DYP,DZP,
     &                DVXL,DVYT,DVZB,DAX,DAY,DAZ,DVXP,DVYP,DVZP,DDX,DDY,
     &                DDZ,TT2(NTPNT),PSTP,IPRINT,ITERP,KTDIM,
     &                BOTM,NBOTM,LAYHDT)
C
C     UPDATE POSITION
          IF(IEXIT.EQ.0.OR.IEXIT.EQ.3.OR.IEXIT.EQ.4) THEN
            CALL SOBS1ADV2UP(XP,DX,YP,DY,ZP,DZ,TP,DT,VP,OLDK,KPT,IPT,
     &                   NEWI,JPT,NEWJ,NEWK,H,IND,KTDIM,IP,DXP,DDX,
     &                   DYP,DDY,DZP,DDZ,X,LN,NPE,ND,NPLIST,IIPP,DTP)
            IEXITOLD=IEXIT
            IF(IEXIT.EQ.4) IEXIT=0
C
C      AND PRINT OUT INFORMATION
            IF (IP.NE.0) THEN
              BB = ABS(B(IIPP))
              IF (LN(IIPP).LE.0) THEN
C               PARAMETER IS NOT LOG-TRANSFORMED
                IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
              ELSE
C               PARAMETER IS LOG-TRANSFORMED
                IF (BB.EQ.0.0) BB = 1.0
              ENDIF
            ELSE
              BB = 0.0
            ENDIF
            CALL SOBS1ADV2WR(IEXIT,IP,IPRINT,ITERP,PSTP,IOUT,KPT,IPT,
     &                 JPT,XP,YP,ZP,TP,DXP,DYP,DZP,BB,IOUTT2,IND,VXP,
     &                 VYP,VZP,VP,KTDIM,OBSNAM(IND),KTFLG,ISCALS,LNIIPP,
     &                 WTQ(IND-NHT,IND-NHT))
            IEXIT=IEXITOLD
          ENDIF
C
C     OTHERWISE CHECK TO SEE IF THE PARTICLE IS GOING INTO A CONFINING LAYER
   60     IF (((NEWK.GT.OLDK .AND. LAYCBD(OLDK).GT.0) .OR.
     &         (NEWK.LT.OLDK .AND. LAYCBD(NEWK).GT.0) .OR.
     &         (DTC.GT.0.0))
     &         .AND. IEXIT.EQ.0) THEN
            IF(DTC.EQ.0.0) WRITE (IOUT,560)
            CALL SOBS1ADV2CC(NLAY,OLDK,NEWK,JPT,IPT,KPT,KTREV,PRST,
     &                    VZB,VZT,IP,DVZB,DVZT,TT2(NTPNT),TP,
     &                    ADVSTP,PSTP,IEXIT,VXP,VYP,VZP,KTFLG,ZP,
     &                    H,DZP,X,LN,IIPP,NCOL,NROW,IND,
     &                    OBSNAM(IND),IPRINT,ITERP,IOUT,XP,YP,DXP,DYP,
     &                    IOUTT2,KTDIM,ISCALS,NPE,ND,NPLIST,BOTM,NBOTM,
     &                    DTP,LNIIPP,WTQ(IND-NHT,IND-NHT),BSCAL,DTC,ZPC)
          ENDIF
C     END OF CONFINING LAYER CALCULATIONS
C
C     IF TIME OF OBSERVED ADVECTIVE-TRANSPORT LOCATION HAS BEEN REACHED,
C        (I.E. IEXIT=3) THEN INCREMENT IND AND GOTO NEXT OBSERVATION
          IF (IEXIT.EQ.3) THEN
            IEXIT = 0
            IND = IND + KTDIM
            GOTO 90
          ENDIF
C
C     IF PARTICLE IS ON EDGE OF GRID AND TIME OF OBSERVATION HAS NOT BEEN
C        REACHED, THEN PROJECT FINAL POSITION BASED ON CURRENT VELOCITIES
C        (IF IEXIT=4 MEANS NEW POSITION IS NOT IN GRID),THEN GOTO NEXT
C        PARTICLE
          IF (IEXIT.EQ.2.OR.IEXIT.EQ.4) THEN
            WRITE (IOUT,530)
            IF (IOUTT2.NE.0) WRITE (IOUTT2,531)
            IEXIT = 3
            IF (KTFLG.GT.1) PSTP = 0.0
            DO 80 KPTH = IPTH, NPNT(NPART)
              DT = TT2(NTPNT) - TP
              DX = VXP*DT
              DY = VYP*DT
              DZ = VZP*DT
              IF (IP.GT.0) THEN
                DDX = DVXP*DT
                DDY = DVYP*DT
                DDZ = DVZP*DT
              ENDIF
C     UPDATE POSITION
            CALL SOBS1ADV2UP(XP,DX,YP,DY,ZP,DZ,TP,DT,VP,OLDK,KPT,IPT,
     &                   NEWI,JPT,NEWJ,NEWK,H,IND,KTDIM,IP,DXP,DDX,
     &                   DYP,DDY,DZP,DDZ,X,LN,NPE,ND,NPLIST,IIPP,DTP)
C      AND PRINT OUT INFORMATION
              IF (IP.NE.0) THEN
                BB = ABS(B(IIPP))
                IF (LN(IIPP).LE.0) THEN
C                 PARAMETER IS NOT LOG-TRANSFORMED
                  IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
                ELSE
C                 PARAMETER IS LOG-TRANSFORMED
                  IF (BB.EQ.0.0) BB = 1.0
                ENDIF
              ELSE
                BB = 0.0
              ENDIF
              CALL SOBS1ADV2WR(IEXIT,IP,IPRINT,ITERP,PSTP,IOUT,KPT,IPT,
     &                     JPT,XP,YP,ZP,TP,DXP,DYP,DZP,BB,IOUTT2,IND,
     &                     VXP,VYP,VZP,VP,KTDIM,OBSNAM(IND),KTFLG,
     &                     ISCALS,LNIIPP,WTQ(IND-NHT,IND-NHT))
              IND = IND + KTDIM
              IF (KPTH.LT.NPNT(NPART)) NTPNT = NTPNT + 1
   80       CONTINUE
C
C      AND GO TO NEXT PATH
            GOTO 100
          ENDIF
C
C     CONTINUE ITERATIONS IF ROUTINE HAS NOT TERMINATED
          IF (IEXIT.EQ.0) GOTO 50
   90   CONTINUE
        IF (IPRINT.EQ.0 .AND. ITERP.LT.2) WRITE (IOUT,525) NPART,DTP/TP
  100 CONTINUE
      IF (IPRINT.EQ.0 .AND. ITERP.LT.2) WRITE (IOUT,540)
      RETURN
      END
C
C=======================================================================
      SUBROUTINE OBS1ADV2PR(ITERSS,ITMXP,IUSS,SSAD)
C
C     VERSION 20010613 ERB
C     ******************************************************************
C     WRITE CONTRIBUTION TO SSWR OF ADVECTIVE-TRANSPORT OBSERVATIONS TO
C     _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ITMXP, IUSS
      LOGICAL LOP
      DIMENSION SSAD(ITMXP+1)
C     ------------------------------------------------------------------
  660 FORMAT(1X,'ITERATION',2X,A)
  670 FORMAT(1X,I5,6X,G14.7)
C
      INQUIRE(UNIT=IUSS,OPENED=LOP)
      IF (LOP) THEN
        WRITE (IUSS,660)'SSWR-(ADVECTIVE-TRANSPORT OBSERVATIONS ONLY)'
C       WRITE CONTRIBUTION TO SSWR FOR EACH ITERATION
        DO 10 IT = 1, ITERSS
          WRITE(IUSS,670) IT,SSAD(IT)
   10   CONTINUE
      ENDIF
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2L(IPT,JPT,KPT,NROW,NCOL,NLAY,CR,CC,CV,HNEW,
     &                  IBOUND,PRST,KTREV,DELC,DELR,DL,XPC,YPC,ZPC,AX,
     &                  AY,AZ,VXP,VYP,VZP,VXL,VXR,VYT,VYB,VZT,VZB,IEXIT,
     &                  IOUT,IP,RMLT,HK,IZON,DXP,DYP,DZP,DVXL,DVXR,DVYT,
     &                  DVYB,DVZT,DVZB,DAX,DAY,DAZ,DVXP,DVYP,DVZP,MXBND,
     &                  NBOUND,BNDS,NRCHOP,IRCH,RECH,MXSTRM,NSTREM,
     &                  ISTRM,STRM,MXRIVR,NRIVER,RIVR,MXDRN,NDRAIN,DRAI,
     &                  SV,KTDIM,NMLTAR,NZONAR,BOTM,NBOTM,SNEW,
     &                  VKA,IUHFB,HFB,MXACTFB,NHFB,HANI,NGHBVL,
     &                  NRIVVL,NDRNVL,LAYHDT,FSNK,HKCC,HUFTHK,NHUF,
     &                  IULPF,IUHUF,WELL,NWELVL,MXWELL,NWELLS)
C     VERSION 1000 22OCT1993
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE VELOCITY OF A PARTICLE WITHIN CELL IPT,JPT,K
C     USING LINEAR VELOCITY INTERPOLATION
C     THIS IS CALLED FROM OBS1ADV2P ONLY
C     RETURNS IEXIT = 1 IF CELL IS INACTIVE
C                     2 IF CELL IS A WEAK SINK
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      REAL CR, CC, CV, PRST, DELC, DELR, DL, XPC, YPC, ZPC, 
     &     AX, AY, AZ, VXP, VYP, VZP, VXL, VXR, VYT, VYB, VZT, VZB,
     &     RMLT, HK, DXP, DYP, DZP, DVXL, DVXR, DVYT,
     &     DVYB, DVZT, DVZB, DAX, DAY, DAZ, DVXP, DVYP, DVZP, BNDS, 
     &     RECH, STRM, RIVR, DRAI, SV, VKA, HANI, HKCC, HUFTHK
      INTEGER IPT, JPT, KPT, NROW, NCOL, NLAY, IBOUND, KTREV, 
     &        IEXIT, IOUT, IP, IULPF, IUHUF,
     &        IZON, MXBND, NBOUND, NRCHOP, IRCH, MXSTRM, NSTREM, 
     &        ISTRM, MXRIVR, NRIVER, MXDRN, NDRAIN, KTDIM
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW), 
     &          CC(NCOL,NROW,NLAY), CR(NCOL,NROW,NLAY), 
     &          RMLT(NCOL,NROW,NMLTAR),
     &          HK(NCOL,NROW,NLAY), IZON(NCOL,NROW,NZONAR),
     &          LAYHDT(NLAY), CO(6),WELL(NWELVL,MXWELL),
     &          CV(NCOL,NROW,NLAY), BNDS(NGHBVL,MXBND), IRCH(NCOL,NROW),
     &          RECH(NCOL,NROW), ISTRM(5,MXSTRM), STRM(11,MXSTRM),
     &          RIVR(NRIVVL,MXRIVR), DRAI(NDRNVL,MXDRN),
     &          PRST(NCOL,NROW,NBOTM), SV(NCOL,NROW,NLAY), 
     &          BOTM(NCOL,NROW,0:NBOTM), HANI(NCOL,NROW,NLAY),
     &          VKA(NCOL,NROW,NLAY), HFB(7,MXACTFB), 
     &          HUFTHK(NCOL,NROW,NHUF,2), HKCC(NCOL,NROW,NLAY)
C---LOCAL:
      DOUBLE PRECISION HN, HNXL, HNXR, HNYT, HNYB, HNZT, HNZB
C     1      ,HXL,HXR,HYT,HYB,HZT,HZB,H0
      CHARACTER*4 PIDTMP
      REAL CCB, CCT, CO, CRL, CRR, CVB, CVT, DC, DCND, DDL, 
     &     DFZB, DFZL, DFZR, DFZT, DHXL, DHXR, DHYB, DHYT, DHZB, DHZT, 
     &     DR, FZB, FZL, FZR, FZT, H0, HD, HDB, HDL, HDR, HDT, HXL,
     &     HXR, HYB, HYT, HZB, HZT, PDC, PDR, PDRDC, SF,
     &     TOPB, TOPL, TOPR, TOPT, ZB, ZL, ZR, ZT, HNSG
      INTEGER IBB, IBL, IBR, IBT, IZ, LTP, NZ, BND
C---COMMON:
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /LPFCOM/LAYTYP(200),LAYAVG(200),CHANI(200),LAYVKA(200),
     1               LAYWET(200)
      INCLUDE 'param.inc'
C----------------------------------------------------------------------
      EXTERNAL SOBS1ADV2LPF, SOBS1ADV2HUF
      INTRINSIC IABS
C----------------------------------------------------------------------
  500 FORMAT (/,' CELL (',3I4,') IS NOT ACTIVE. (SOBS1ADV2L)')
  505 FORMAT (' PARTICLE DISCHARGED AT WEAK SINK, CELL (',3I4,').',
     &  ' (SOBS1ADV2L)')
C----------------------------------------------------------------------
C
C     TEST TO MAKE SURE CELL IS ACTIVE
      IF (IBOUND(JPT,IPT,KPT).EQ.0) IEXIT = 1
      IF (IPT.LT.1 .OR. IPT.GT.NROW) IEXIT = 1
      IF (JPT.LT.1 .OR. JPT.GT.NCOL) IEXIT = 1
      IF (KPT.LT.1 .OR. KPT.GT.NLAY) IEXIT = 1
      IF (IEXIT.EQ.1) THEN
        WRITE (IOUT,500) IPT, JPT, KPT
        RETURN
      ENDIF
C     INITIALIZE VARIABLES THAT MAY NOT BE USED
      HXL = 0.0
      HXR = 0.0
      HYT = 0.0
      HYB = 0.0
      HZT = 0.0
      HZB = 0.0
      HNSG = 0.0
      HN = 0.0
      HNXL = 0.0
      HNXR = 0.0
      HNYT = 0.0
      HNYB = 0.0
      HNZT = 0.0
      HNZB = 0.0
      DC = DELC(IPT)
      DR = DELR(JPT)
      VXL = 0.0
      VXR = 0.0
      VYT = 0.0
      VYB = 0.0
      VZT = 0.0
      VZB = 0.0
      DVXL = 0.0
      DVXR = 0.0
      DVYT = 0.0
      DVYB = 0.0
      DVZT = 0.0
      DVZB = 0.0
      CRL = 0.0
      CRR = 0.0
      CCT = 0.0
      CCB = 0.0
      CVT = 0.0
      CVB = 0.0
      IBL = 0
      IBR = 0
      IBT = 0
      IBB = 0
      TOPL = 0.0
      TOPR = 0.0
      TOPT = 0.0
      TOPB = 0.0
      PIDTMP = '    '
      IF(IP.GT.0) THEN
        IIPP= IPPTR(IP)
        PIDTMP = PARTYP(IPPTR(IP))
      ENDIF
C
C     GET NECESSARY VARIABLES THAT ARE USED
      LTP = LAYHDT(KPT)
      IF (IPT.GT.1) THEN
        CCT = CC(JPT,IPT-1,KPT)
        IBT = IBOUND(JPT,IPT-1,KPT)
C       DCT = DELC(IPT-1)
        IF (LTP.GT.0) TOPT = BOTM(JPT,IPT-1,LBOTM(KPT)-1)
      ENDIF
      IF (IPT.LT.NROW) THEN
        CCB = CC(JPT,IPT,KPT)
        IBB = IBOUND(JPT,IPT+1,KPT)
C       DCB = DELC(IPT+1)
        IF (LTP.GT.0) TOPB = BOTM(JPT,IPT+1,LBOTM(KPT)-1)
      ENDIF
      IF (JPT.GT.1) THEN
        CRL = CR(JPT-1,IPT,KPT)
        IBL = IBOUND(JPT-1,IPT,KPT)
C       DRL = DELR(JPT-1)
        IF (LTP.GT.0) TOPL = BOTM(JPT-1,IPT,LBOTM(KPT)-1)
      ENDIF
      IF (JPT.LT.NCOL) THEN
        CRR = CR(JPT,IPT,KPT)
        IBR = IBOUND(JPT+1,IPT,KPT)
C       DRR = DELR(JPT+1)
        IF (LTP.GT.0) TOPR = BOTM(JPT+1,IPT,LBOTM(KPT)-1)
      ENDIF
      IF (KPT.GT.1) CVT = CV(JPT,IPT,KPT-1)
      IF (KPT.LT.NLAY) CVB = CV(JPT,IPT,KPT)
C
C     PULL HEADS FROM HNEW
      H0 = HNEW(JPT,IPT,KPT)
      IF (IP.LT.1) THEN
        IF (IBL.NE.0) HXL = HNEW(JPT-1,IPT,KPT)
        IF (IBR.NE.0) HXR = HNEW(JPT+1,IPT,KPT)
        IF (IBT.NE.0) HYT = HNEW(JPT,IPT-1,KPT)
        IF (IBB.NE.0) HYB = HNEW(JPT,IPT+1,KPT)
        IF (KPT.GT.1) HZT = HNEW(JPT,IPT,KPT-1)
        IF (KPT.LT.NLAY) HZB = HNEW(JPT,IPT,KPT+1)
      ELSE
        HN = SNEW(JPT,IPT,KPT)
        HNSG = HN
        IF (IBL.NE.0) THEN
          HXL = HNEW(JPT-1,IPT,KPT)
          HNXL = SNEW(JPT-1,IPT,KPT)
        ENDIF
        IF (IBR.NE.0) THEN
          HXR = HNEW(JPT+1,IPT,KPT)
          HNXR = SNEW(JPT+1,IPT,KPT)
        ENDIF
        IF (IBT.NE.0) THEN
          HYT = HNEW(JPT,IPT-1,KPT)
          HNYT = SNEW(JPT,IPT-1,KPT)
        ENDIF
        IF (IBB.NE.0) THEN
          HYB = HNEW(JPT,IPT+1,KPT)
          HNYB = SNEW(JPT,IPT+1,KPT)
        ENDIF
        IF (KPT.GT.1) THEN
          HZT = HNEW(JPT,IPT,KPT-1)
          HNZT = SNEW(JPT,IPT,KPT-1)
        ENDIF
        IF (KPT.LT.NLAY) THEN
          HZB = HNEW(JPT,IPT,KPT+1)
          HNZB = SNEW(JPT,IPT,KPT+1)
        ENDIF
      ENDIF
      DHXL = HXL - H0
      DHXR = H0 - HXR
      DHYT = HYT - H0
      DHYB = H0 - HYB
      DHZT = H0 - HZT
      DHZB = HZB - H0
C
C     COMPUTE SATURATED THICKNESS
      HD = H0
      DL = 1.0
      DDL = 0.0
      ZL = 1.0
      ZR = 1.0
      ZT = 1.0
      ZB = 1.0
C
C     IF LAYER IS CONFINED, I.E. LAYER TYPE 0
C        GET SATURATED THICKNESS
      IF (LTP.EQ.0) THEN
        DL = BOTM(JPT,IPT,LBOTM(KPT)-1) - BOTM(JPT,IPT,LBOTM(KPT))
        IF (IBL.NE.0) ZL = BOTM(JPT-1,IPT,LBOTM(KPT)-1)
     &                     - BOTM(JPT-1,IPT,LBOTM(KPT))
        IF (IBR.NE.0) ZR = BOTM(JPT+1,IPT,LBOTM(KPT)-1)
     &                     - BOTM(JPT+1,IPT,LBOTM(KPT))
        IF (IBT.NE.0) ZT = BOTM(JPT,IPT-1,LBOTM(KPT)-1)
     &                     - BOTM(JPT,IPT-1,LBOTM(KPT))
        IF (IBB.NE.0) ZB = BOTM(JPT,IPT+1,LBOTM(KPT)-1)
     &                     - BOTM(JPT,IPT+1,LBOTM(KPT))
      ELSEIF (LTP.GT.0) THEN
        HDL = HXL
        HDR = HXR
        HDT = HYT
        HDB = HYB
        IF (HD.GT.BOTM(JPT,IPT,LBOTM(KPT)-1))
     &      HD = BOTM(JPT,IPT,LBOTM(KPT)-1)
        IF (HDL.GT.TOPL) HDL = TOPL
        IF (HDR.GT.TOPR) HDR = TOPR
        IF (HDT.GT.TOPT) HDT = TOPT
        IF (HDB.GT.TOPB) HDB = TOPB
        DL = HD - BOTM(JPT,IPT,LBOTM(KPT))
        IF (IBL.NE.0) ZL = HDL - BOTM(JPT-1,IPT,LBOTM(KPT))
        IF (IBR.NE.0) ZR = HDR - BOTM(JPT+1,IPT,LBOTM(KPT))
        IF (IBT.NE.0) ZT = HDT - BOTM(JPT,IPT-1,LBOTM(KPT))
        IF (IBB.NE.0) ZB = HDB - BOTM(JPT,IPT+1,LBOTM(KPT))
      ENDIF
C
C     COMPUTE INTERPOLATED SATURATED THICKNESSES
      FZL = (ZL+DL)/2.0
      FZR = (DL+ZR)/2.0
      FZT = (ZT+DL)/2.0
      FZB = (DL+ZB)/2.0
      PDC = KTREV*PRST(JPT,IPT,LBOTM(KPT))*DC
      PDR = KTREV*PRST(JPT,IPT,LBOTM(KPT))*DR
      PDRDC = KTREV*PRST(JPT,IPT,LBOTM(KPT))*DR*DC
C     COMPUTE CELL FACE VELOCITIES
      VXL = CRL*DHXL/PDC/FZL
      VXR = CRR*DHXR/PDC/FZR
      VYT = CCT*DHYT/PDR/FZT
      VYB = CCB*DHYB/PDR/FZB
      VZT = CVT*DHZT/PDRDC
      VZB = CVB*DHZB/PDRDC
C
C     INITIALIZE WEAK-SINK INFORMATION
      QI=0.0
      IF(DHXL.GT.0.0) QI=QI+CRL*DHXL
      IF(DHXR.LT.0.0) QI=QI-CRR*DHXR
      IF(DHYT.GT.0.0) QI=QI+CCT*DHYT
      IF(DHYB.LT.0.0) QI=QI-CCB*DHYB
      IF(DHZT.LT.0.0) QI=QI-CVT*DHZT
      IF(DHZB.GT.0.0) QI=QI+CVB*DHZB
      QS=0.0
C
C     CORRECT VELOCITIES TO INCLUDE BOUNDARY FLOWS----------------------
      IF (KTDIM.EQ.3) THEN
C
C     ...FOR RECHARGE
        IF(KPT.EQ.1) THEN
          KRCH=1        
        ELSE
          KRCH=KPT-1
        ENDIF
        IF (NRCHOP.EQ.2) THEN
          IRCHPT=IRCH(JPT,IPT)
        ELSE
          IRCHPT=0
        ENDIF
        IF ((NRCHOP.EQ.1.AND.KPT.EQ.1) .OR. 
     &      (NRCHOP.EQ.2.AND.IRCHPT.EQ.KPT) .OR. 
     &      (NRCHOP.EQ.3.AND.(KPT.EQ.1.
     &                  OR.(KPT.GT.1.AND.IBOUND(JPT,IPT,KRCH).EQ.0))))
     &      THEN
          VZT = VZT - (RECH(JPT,IPT)/PDRDC)
          QS=QS+RECH(JPT,IPT)
          IF (IP.GT.0 .AND. PIDTMP.EQ.'RCH ') THEN
C           GO THROUGH CLUSTERS FOR THIS PARAMETER TO FIND CLUSTER, MULTIPLIER
C           ARRAY, ZONE ARRAY AND ZONE THAT APPLY TO THE CURRENT CELL
            SF = 0.0
            DO 7 IC = IPLOC(1,IIPP), IPLOC(2,IIPP)
              NMA = IPCLST(2,IC)
              NZA = IPCLST(3,IC)
              IF (NZA.GT.0) THEN
                DO 5 IZ = 5, IPCLST(4,IC)
                  NZ = IPCLST(IZ,IC)
C                 SEE IF CURRENT CELL IS IN ZONE NZ
                  IF (NZ.EQ.IZON(JPT,IPT,NZA)) THEN
                    IF (NMA.EQ.0) THEN
                      SF = 1.0
                    ELSE
                      SF = RMLT(JPT,IPT,NMA)
                    ENDIF
                    GOTO 8
                  ENDIF
    5           CONTINUE
              ELSE
                IF (NMA.EQ.0) THEN
                  SF = 1.0
                ELSE
                  SF = RMLT(JPT,IPT,NMA)
                ENDIF
              ENDIF
    7       CONTINUE
    8       CONTINUE
            DVZT = DVZT - SF/PRST(JPT,IPT,LBOTM(KPT))
          ENDIF
        ENDIF
C
C     ...FOR GHB
        DO 10 BND = 1, NBOUND
          IF (BNDS(1,BND).EQ.KPT .AND. BNDS(2,BND).EQ.IPT .AND. 
     &        BNDS(3,BND).EQ.JPT) THEN
            QS=QS-BNDS(5,BND)*(H0-BNDS(4,BND))
C
C           TOP FACE
            IF (KPT.EQ.1) THEN
              VZT = VZT + BNDS(5,BND)*(H0-BNDS(4,BND))/PDRDC
              IF (IP.GT.0) THEN
                DCND = 0.0
                IF (PIDTMP.EQ.'GHB ') DCND = BNDS(5,BND)/B(IIPP)
                DVZT = DVZT + ((DCND*(H0-BNDS(4,BND))+BNDS(5,BND)*HNSG)
     &                 /PDRDC)
              ENDIF
            ENDIF
C
C           BOTTOM FACE
            IF (KPT.EQ.NLAY) THEN
              VZB = VZB + BNDS(5,BND)*(H0-BNDS(4,BND))/PDRDC
              IF (IP.GT.0) THEN
                DCND = 0.0
                IF (PIDTMP.EQ.'GHB ') DCND = BNDS(5,BND)/B(IIPP)
                DVZB = DVZB + ((DCND*(H0-BNDS(4,BND))+BNDS(5,BND)*HNSG)
     &                 /PDRDC)
              ENDIF
            ENDIF
C
C           SIDE FACE (NOT DONE YET)
          ENDIF
   10   CONTINUE
C
C     ...FOR STREAM PACKAGE SEEPAGE HAS ALREADY BEEN CALCULATED
        DO 20 BND = 1, NSTREM
          IF (ISTRM(1,BND).EQ.KPT .AND. ISTRM(2,BND).EQ.IPT .AND. 
     &        ISTRM(3,BND).EQ.JPT) THEN
            VZT = VZT - STRM(11,BND)/PDRDC
            QS=QS+STRM(11,BND)
            IF (IP.GT.0) THEN
              DCND = 0.0
              IF (PIDTMP.EQ.'STR ') DCND = STRM(3,BND)/B(IIPP)
              DVZT = DVZT + ((DCND*(H0-STRM(2,BND))+STRM(3,BND)*HNSG)
     &               /PDRDC)
            ENDIF
          ENDIF
   20   CONTINUE
C
C     ...FOR RIVER PACKAGE
        DO 30 BND = 1, NRIVER
          IF (RIVR(1,BND).EQ.KPT .AND. RIVR(2,BND).EQ.IPT .AND. 
     &        RIVR(3,BND).EQ.JPT) THEN
C
C           IF HEAD<RBOT THEN V=C(RBOT-HRIV)/N/DC/DR
            IF (H0.LE.RIVR(6,BND)) THEN
              VZT = VZT + RIVR(5,BND)*(RIVR(6,BND)-RIVR(4,BND))/PDRDC
              QS=QS + RIVR(5,BND)*(RIVR(6,BND)-RIVR(4,BND))
              IF (IP.GT.0 .AND. PIDTMP.EQ.'RIV ') THEN
                DCND = RIVR(5,BND)/B(IIPP)
                DVZT = DVZT + DCND*(RIVR(6,BND)-RIVR(4,BND))/PDRDC
              ENDIF
C
C           ELSE V=C(HEAD-HRIV)/N/DR/DC
            ELSE
              VZT = VZT + RIVR(5,BND)*(H0-RIVR(4,BND))/PDRDC
              QS = QS - RIVR(5,BND)*(H0-RIVR(4,BND))
              IF (IP.GT.0) THEN
                DCND = 0.0
                IF (PIDTMP.EQ.'RIV ') DCND = RIVR(5,BND)/B(IIPP)
                DVZT = DVZT + ((DCND*(H0-RIVR(4,BND))+RIVR(5,BND)*HNSG)
     &                 /PDRDC)
              ENDIF
            ENDIF
          ENDIF
   30   CONTINUE
C
C     ...FOR DRAIN PACKAGE
        DO 40 BND = 1, NDRAIN
          IF (DRAI(1,BND).EQ.KPT .AND. DRAI(2,BND).EQ.IPT .AND. 
     &        DRAI(3,BND).EQ.JPT) THEN
C           COMPUTE ONLY IF HEAD > DRAIN ELEVATION
            IF (H0.GT.DRAI(4,BND)) THEN
              VZT = VZT + DRAI(5,BND)*(H0-DRAI(4,BND))/PDRDC
              QS = QS - DRAI(5,BND)*(H0-DRAI(4,BND))
              IF (IP.GT.0) THEN
                DCND = 0.0
                IF (PIDTMP.EQ.'DRN ') DCND = DRAI(5,BND)/B(IIPP)
                DVZT = DVZT + ((DCND*(H0-DRAI(4,BND))+DRAI(5,BND)*HNSG)
     &                 /PDRDC)
              ENDIF
            ENDIF
          ENDIF
   40   CONTINUE
      ENDIF
C
C     WEAK SINK CALCULATIONS
C
C     ...FOR WELL PACKAGE
      DO 50 L=1,NWELLS
        IR=WELL(2,L)
        IC=WELL(3,L)
        IL=WELL(1,L)
        Q=WELL(4,L)
        IF(JPT.EQ.IC .AND. IPT.EQ.IR .AND. KPT.EQ.IL) QS = QS + Q
   50 CONTINUE      
      IF(QS.LT.0.0) THEN
        F=-QS/QI
        IF(FSNK.LT.0.0.OR.(FSNK.GT.0.0.AND.F.GT.FSNK)) THEN
          WRITE(IOUT,505) IPT,JPT,KPT
          IEXIT=2
          RETURN
        ENDIF
      ENDIF
C------COMPUTE LINEAR VELOCITY INTERPOLATION COEFFICIENTS
      AX = (VXR-VXL)/DR
      AY = (VYB-VYT)/DC
      AZ = (VZT-VZB)/DL
C------COMPUTE VELOCITY OF THE PARTICLE
      VXP = AX*XPC + VXL
      VYP = AY*YPC + VYT
      VZP = AZ*ZPC + VZB
      if(abs(zpc-dl)/dl.lt.1.e-3) vzp=vzt
C
C     COMPUTE SENSITIVITIES IF IP > 0
      IF (IP.GT.0) THEN
        CO(1) = 0.0
        CO(2) = 0.0
        CO(3) = 0.0
        CO(4) = 0.0
        CO(5) = 0.0
        CO(6) = 0.0
C
C---CALL SUBROUTINES TO COMPUTE CONDUCTANCE SENSITIVITIES IF NEEDED
        IF (PIDTMP.EQ.'HANI' .OR. PIDTMP.EQ.'HK  ' .OR. 
     &      PIDTMP.EQ.'VK  ' .OR. PIDTMP.EQ.'VANI' .OR.
     &      PIDTMP.EQ.'VKCB') THEN
C-----CALL SOBS1ADV2LPF IF LPF PACKAGE IS ACTIVE
          IF(IULPF.GT.0)
     &     CALL SOBS1ADV2LPF(CO,IPT,JPT,KPT,IIPP,LAYHDT,PIDTMP,IBOUND,
     &                    RMLT,NMLTAR,IZON,NZONAR,BOTM,NBOTM,
     &                    HNEW,NCOL,NROW,NLAY,
     &                    DELR,DELC,HANI,IUHFB,HFB,MXACTFB,NHFB,
     &                    HK,CV,SV,VKA)
C-----CALL SOBS1ADV2HUF IF HUF PACKAGE IS ACTIVE
          IF(IUHUF.GT.0)
     &    CALL SOBS1ADV2HUF(CO,IPT,JPT,KPT,IIPP,HNEW,NCOL,NROW,NLAY,
     &                    PIDTMP,HK,HKCC,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                    HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &                    IUHFB,HFB,MXACTFB,NHFB,IOUT)
        ENDIF
C
C     CALCULATE SATURATED THICKNESS SENSITIVITIES
        IF (LTP.EQ.0) THEN
          DFZL = 0.0
          DFZR = 0.0
          DFZT = 0.0
          DFZB = 0.0
        ENDIF
        IF (LTP.GT.0) THEN
          DFZL = (HNXL+HN)/2.0
          DFZR = (HN+HNXR)/2.0
          DFZT = (HNYT+HN)/2.0
          DFZB = (HN+HNYB)/2.0
        ENDIF
C     CALCULATE CELL FACE VELOCITY SENSITIVITIES
        IF (JPT.GT.1) DVXL = ((CO(1)*DHXL/FZL)+(CRL*((FZL*(HNXL-HN)-DHXL
     &                       *DFZL)/(FZL**2.))))/PDC
        IF (JPT.LT.NCOL) DVXR = ((CO(2)*DHXR/FZR)+(CRR*((FZR*(HN-HNXR)-
     &                          DHXR*DFZR)/(FZR**2.))))/PDC
        IF (IPT.GT.1) DVYT = ((CO(3)*DHYT/FZT)+(CCT*((FZT*(HNYT-HN)-DHYT
     &                       *DFZT)/(FZT**2.))))/PDR
        IF (IPT.LT.NROW) DVYB = ((CO(4)*DHYB/FZB)+(CCB*((FZB*(HN-HNYB)-
     &                          DHYB*DFZB)/(FZB**2.))))/PDR
        IF (KPT.GT.1) DVZT = DVZT + ((CO(5)*DHZT)+(CVT*(HN-HNZT)))/PDRDC
        IF (KPT.LT.NLAY) DVZB = DVZB + ((CO(6)*DHZB)+(CVB*(HNZB-HN)))
     &                          /PDRDC
C     CALCULATE LINEAR VELOCITY INTERPOLATION COEFFICIENT SENSITIVITIES
        DAX = (DVXR-DVXL)/DR
        DAY = (DVYB-DVYT)/DC
        DAZ = (DL*(DVZT-DVZB)-DDL*(VZT-VZB))/(DL**2.)
C     CALCULATE LINEAR VELOCITY INTERPOLATION SENSITIVITIES
        DVXP = DAX*XPC + AX*DXP + DVXL
        DVYP = DAY*YPC + AY*DYP + DVYT
        DVZP = DAZ*ZPC + AZ*DZP + DVZB
      ENDIF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2HUF(CO,IPT,JPT,KPT,IIPP,H,NCOL,NROW,NLAY,
     &                    PIDTMP,HK,HKCC,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                    HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &                    IUHFB,HFB,MXACTFB,NHFB,IOUT)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE CONDUCTANCE SENSITIVITIES FOR THE HUF PACKAGE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      DOUBLE PRECISION H(NCOL,NROW,NLAY)
      REAL DELC, DELR, RMLT, BOTM, HFB, HK, CV, CO
      INTEGER IPT, JPT, KPT, IIPP, IBOUND, IZON, NCOL, NHUF,
     &        NROW, NLAY, IUHFB, MXACTFB, NHFB, NMLTAR, NZONAR
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW), 
     &          RMLT(NCOL,NROW,NMLTAR),
     &          HK(NCOL,NROW,NLAY), HKCC(NCOL,NROW,NLAY), 
     &          CO(6), IZON(NCOL,NROW,NZONAR),
     &          CV(NCOL,NROW,NLAY),
     &          BOTM(NCOL,NROW,0:NBOTM),
     &          HFB(7,MXACTFB), HUFTHK(NCOL,NROW,NHUF,2)
      CHARACTER*4 PIDTMP
C---LOCAL:
C      DOUBLE PRECISION
      REAL RMLT0, C, TOP1, TOP2, BOT1, BOT2, MID1,
     &     MID2
      INTEGER ICL, LT, JCNT, JJ, NZ, ICNT, II, KK
C---COMMON:
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C----------------------------------------------------------------------
      EXTERNAL SSEN1HUF1CH, SSEN1HUF1CV, SSEN1HFB6MD
      INTRINSIC IABS
C----------------------------------------------------------------------
C---FORMAT:
C----------------------------------------------------------------------
C
      DO 100 ICL = IPLOC(1,IIPP),IPLOC(2,IIPP)
        NU = IPCLST(1,ICL)
        NZ = IPCLST(3,ICL)
        NM = IPCLST(2,ICL)
        LT = LTHUF(KPT)
        IF (PIDTMP.EQ.'HK  '.OR.PIDTMP.EQ.'HANI') THEN
C-------CR--------------------------------------------------------------
          JCNT = 0
          DO 200 JJ = JPT-1, JPT
            JCNT = JCNT + 1
            IF (JJ.LT.1 .OR. JJ.GE.NCOL)
     &         GOTO 200
            IF (IBOUND(JJ,IPT,KPT).EQ.0 .OR.
     &         IBOUND(JJ+1,IPT,KPT).EQ.0) GOTO 200
            RMLT0 = 1.0
            RMLT0 = 1.0
            IF (NZ.GT.0) THEN
              RMLT0=0.
              DO 30 JZ = 5,IPCLST(4,ICL)
                IF(IZON(JJ,IPT,NZ).EQ.IPCLST(JZ,ICL)) THEN
                  IF(NM.GT.0) THEN
                    RMLT0=RMLT(JJ,IPT,NM)
                  ELSE
                    RMLT0=1.
                  ENDIF
                END IF
   30         CONTINUE
            ELSEIF(NM.GT.0) THEN
              RMLT0=RMLT(JJ,IPT,NM)
            ENDIF

C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
            TOP0=BOTM(JJ,IPT,LBOTM(KPT)-1)
            IF(LT.NE.0.AND.H(JJ,IPT,KPT).LT.TOP0) TOP0=H(JJ,IPT,KPT)
            BOT0=BOTM(JJ,IPT,LBOTM(KPT))
            CALL SSEN1HUF1THK(TOP0,BOT0,
     1                        HUFTHK(JJ,IPT,NU,1),
     2                        HUFTHK(JJ,IPT,NU,2),TH0)
            TOP1C=BOTM(JJ+1,IPT,LBOTM(KPT)-1)
            IF(LT.NE.0.AND.H(JJ+1,IPT,KPT).LT.TOP1C)
     &         TOP1C=H(JJ+1,IPT,KPT)
            BOT1C=BOTM(JJ+1,IPT,LBOTM(KPT))
            CALL SSEN1HUF1THK(TOP1C,BOT1C,
     1                        HUFTHK(JJ+1,IPT,NU,1),
     2                        HUFTHK(JJ+1,IPT,NU,2),TH1C)
            IF(TH0.EQ.0..AND.TH1C.EQ.0.) GOTO 200
            CALL SSEN1HUF1CH(COR,TH0,TH1C,HP,IPT,JJ,KPT,'CR',
     &                       RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,
     &                       H,BOTM(1,1,LBOTM(KPT)),
     &                       BOTM(1,1,LBOTM(KPT)-1),NZ,NM,ICL,IZON,
     &                       NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,HUFTHK,NHUF,
     &                       NU)
            CO(JCNT)=CO(JCNT)+COR
            IF (IUHFB.GT.0 .AND. COR.NE.0.)
     &        CALL SSEN1HFB6MD(C,'CR',COR,DELC,DELR,HFB,IPT,JJ,
     &                         KPT,MXACTFB,NCOL,NHFB,NROW,
     &                         TH0L,TH1L)
  200     CONTINUE
C-------CC--------------------------------------------------------------
          ICNT = 2
          DO 300 II = IPT-1, IPT
            ICNT = ICNT + 1
            IF (II.LT.1 .OR. II.GE.NROW .OR.
     &          IBOUND(JPT,II+1,KPT).EQ.0) GOTO 300
            RMLT0 = 1.0
            RMLT0 = 1.0
            IF (NZ.GT.0) THEN
              RMLT0=0.
              DO 310 JZ = 5,IPCLST(4,ICL)
                IF(IZON(JPT,II,NZ).EQ.IPCLST(JZ,ICL)) THEN
                  IF(NM.GT.0) THEN
                    RMLT0=RMLT(JPT,II,NM)
                  ELSE
                    RMLT0=1.
                  ENDIF
                END IF
  310         CONTINUE
            ELSEIF(NM.GT.0) THEN
              RMLT0=RMLT(JPT,II,NM)
            ENDIF

C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
            TOP0=BOTM(JPT,II,LBOTM(KPT)-1)
            IF(LT.NE.0.AND.H(JPT,II,KPT).LT.TOP0) TOP0=H(JPT,II,KPT)
            BOT0=BOTM(JPT,II,LBOTM(KPT))
            CALL SSEN1HUF1THK(TOP0,BOT0,
     1                        HUFTHK(JPT,II,NU,1),
     2                        HUFTHK(JPT,II,NU,2),TH0)
            TOP1R=BOTM(JPT,II+1,LBOTM(KPT)-1)
            IF(LT.NE.0.AND.H(JPT,II+1,KPT).LT.TOP1R)
     &         TOP1R=H(JPT,II+1,KPT)
            BOT1R=BOTM(JPT,II+1,LBOTM(KPT))
            CALL SSEN1HUF1THK(TOP1R,BOT1R,
     1                        HUFTHK(JPT,II,NU,1),
     2                        HUFTHK(JPT,II+1,NU,2),TH1R)
            IF(TH0.EQ.0..AND.TH1R.EQ.0.) GOTO 300
            CALL SSEN1HUF1CH(COC,TH0,TH1R,HP,II,JPT,KPT,'CC',
     &                       RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,
     &                       H,BOTM(1,1,LBOTM(KPT)),
     &                       BOTM(1,1,LBOTM(KPT)-1),NZ,NM,ICL,IZON,
     &                       NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,HUFTHK,NHUF,
     &                       NU)
            CO(ICNT)=CO(ICNT)+COC
            IF (IUHFB.GT.0 .AND. COC.NE.0.)
     &        CALL SSEN1HFB6MD(C,'CC',COC,DELC,DELR,HFB,II,JPT,
     &                         KPT,MXACTFB,NCOL,NHFB,NROW,
     &                         TH0L,TH1L)
  300     CONTINUE
        ENDIF
C-------CV--------------------------------------------------------------
        IF (PIDTMP.EQ.'VK  '. OR. PIDTMP.EQ.'VANI' .OR. 
     &     (PIDTMP.EQ.'HK  ' .AND. HGUVANI(NU).NE.0) .OR.
     &      PIDTMP.EQ.'VKCB') THEN
          KCNT=4
          RMLT0=1.0
          IF (NZ.GT.0) THEN
            RMLT0=0.
            DO 410 JZ = 5,IPCLST(4,ICL)
              IF(IZON(JPT,IPT,NZ).EQ.IPCLST(JZ,ICL)) THEN
                IF(NM.GT.0) THEN
                  RMLT0=RMLT(JPT,IPT,NM)
                ELSE
                  RMLT0=1.
                ENDIF
              END IF
  410       CONTINUE
          ELSEIF(NM.GT.0) THEN
            RMLT0=RMLT(JPT,IPT,NM)
          ENDIF
          IF(RMLT0.EQ.0.) GOTO 100
          DO 400 KK = KPT-1, KPT
            KCNT = KCNT + 1
            IF (KK.LT.1 .OR. KK.EQ.NLAY) GOTO 400
            TOP1=BOTM(JPT,IPT,LBOTM(KK)-1)
            IF(LTHUF(KK).NE.0.AND.H(JPT,IPT,KK).LT.TOP1)
     &         TOP1=H(JPT,IPT,KK)
            BOT1=BOTM(JPT,IPT,LBOTM(KK))
            TOP2=BOT1
            IF(LTHUF(KK+1).NE.0.AND.H(JPT,IPT,KK+1).LT.TOP2) 
     &          TOP2=H(JPT,IPT,KK+1)
            BOT2=BOTM(JPT,IPT,LBOTM(KK)+1)
            MID1=0.5*(TOP1+BOT1)
            MID2=0.5*(TOP2+BOT2)
            CALL SSEN1HUF1THK(MID1,MID2,
     1            HUFTHK(JPT,IPT,NU,1),HUFTHK(JPT,IPT,NU,2),THK1)
            IF(THK1.EQ.0.) GOTO 400
            CALL SSEN1HUF1CV(PIDTMP,COV,THK1,IBP,IBM,NCOL,NROW,
     &                       NLAY,CV,DELR,DELC,JPT,IPT,KK,IBOUND,IIPP,
     &                       NU,NZ,IZON,NZONAR,RMLT0,RMLT,NMLTAR,IOUT,
     &                       HUFTHK,NHUF)
            CO(KCNT)=CO(KCNT)+COV
  400     CONTINUE
        ENDIF
  100 CONTINUE
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2LPF(CO,IPT,JPT,KPT,IIPP,LAYHDT,PIDTMP,IBOUND,
     &                    RMLT,NMLTAR,IZON,NZONAR,BOTM,NBOTM,
     &                    HNEW,NCOL,NROW,NLAY,
     &                    DELR,DELC,HANI,IUHFB,HFB,MXACTFB,NHFB,
     &                    HK,CV,SV,VKA)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE CONDUCTANCE SENSITIVITIES FOR THE LPF PACKAGE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      REAL DELC, DELR, RMLT, BOTM, HANI, HFB, HK, CV, SV, VKA, CO
      INTEGER IPT, JPT, KPT, IIPP, IBOUND, IZON, NCOL,
     &        NROW, NLAY, IUHFB, MXACTFB, NHFB, NMLTAR, NZONAR
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW), 
     &          RMLT(NCOL,NROW,NMLTAR),
     &          HK(NCOL,NROW,NLAY), IZON(NCOL,NROW,NZONAR),
     &          LAYHDT(NLAY), CO(6),
     &          CV(NCOL,NROW,NLAY), SV(NCOL,NROW,NLAY), 
     &          BOTM(NCOL,NROW,0:NBOTM),
     &          VKA(NCOL,NROW,NLAY), HFB(7,MXACTFB), 
     &          HANI(NCOL,NROW,NLAY)
C---LOCAL:
C      DOUBLE PRECISION
      CHARACTER*4 PIDTMP
      REAL RMLT0, SF, TH1, C, COU, COD
      INTEGER ICL, IL, K, LT, LZ1, M, JCNT, JJ, IFLAG0, IZ, NZ, ICNT,
     &        II, KK
C---COMMON:
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /LPFCOM/LAYTYP(200),LAYAVG(200),CHANI(200),LAYVKA(200),
     1               LAYWET(200)
      INCLUDE 'param.inc'
C----------------------------------------------------------------------
      EXTERNAL SSEN1LPF1CH, SSEN1LPF1CV, SSEN1HFB6MD
      INTRINSIC IABS
C----------------------------------------------------------------------
C---FORMAT:
C----------------------------------------------------------------------
C
      DO 140 ICL = IPLOC(1,IIPP),IPLOC(2,IIPP)
        IL = IPCLST(1,ICL)
        IF (IL.EQ.0) GOTO 140
        K = IL
        SF = 1.0
        LT = LAYHDT(KPT)
        LZ1 = IPCLST(3,ICL)
        M = IPCLST(2,ICL)
        IF (PIDTMP.NE.'VK  '.AND.PIDTMP.NE.'VANI'.AND.KPT.EQ.K) THEN
C-------CR--------------------------------------------------------------
          JCNT = 0
          DO 90 JJ = JPT-1, JPT
            JCNT = JCNT + 1
            IF (JJ.LT.1 .OR. JJ.GE.NCOL)
     &         GOTO 90
            IF (IBOUND(JJ,IPT,KPT).EQ.0 .OR.
     &         IBOUND(JJ+1,IPT,KPT).EQ.0) GOTO 90
            RMLT0 = 1.0
            IF (IL.GT.0) THEN
              IF (M.EQ.0) RMLT0 = SF
              IF (M.GT.0) RMLT0 = SF*RMLT(JJ,IPT,M)
              IF (RMLT0.NE.0.0 .AND. LZ1.GT.0) THEN
                  IFLAG0 = 0
                DO 70 IZ = 5,IPCLST(4,ICL)
                  NZ = IPCLST(IZ,ICL)
                  IF (NZ.EQ.0 .OR. IFLAG0.EQ.1) GOTO 80
                  IF (IZON(JJ,IPT,LZ1).EQ.NZ) IFLAG0 = 1
   70           CONTINUE
   80           IF (IFLAG0.EQ.0) RMLT0 = 0.0
              ENDIF
            ENDIF
            TH1 = BOTM(JJ,IPT,LBOTM(KPT)-1)-BOTM(JJ,IPT,LBOTM(KPT))
            IF (LT.GT.0 .AND.
     &          HNEW(JJ,IPT,KPT).LT.BOTM(JJ,IPT,LBOTM(KPT)-1))
     &          TH1 = HNEW(JJ,IPT,KPT) - BOTM(JJ,IPT,LBOTM(KPT))
            CALL SSEN1LPF1CH(CO(JCNT),TH2,HP,IPT,JJ,KPT,'CR',IL,M,
     &                      RMLT0,RMLT,LZ1,IZON,SF,LT,HK,
     &                      NCOL,NROW,NLAY,DELC,DELR,HNEW,TH1,
     &                      BOTM(1,1,LBOTM(KPT)),
     &                      BOTM(1,1,LBOTM(KPT)-1),NMLTAR,NZONAR,
     &                      ICL,C,HANI)
            IF (IUHFB.GT.0 .AND. CO(JCNT).NE.0.)
     &          CALL SSEN1HFB6MD(C,'CR',CO(JCNT),DELC,DELR,HFB,IPT,
     &                           JJ,KPT,MXACTFB,NCOL,NHFB,NROW,
     &                           TH1,TH2)
   90     CONTINUE
C-------CC--------------------------------------------------------------
          ICNT = 2
          DO 120 II = IPT-1, IPT
            ICNT = ICNT + 1
            IF (II.LT.1 .OR. II.GE.NROW .OR.
     &          IBOUND(JPT,II+1,KPT).EQ.0) GOTO 120
            RMLT0 = 1.0
            IF (IL.GT.0) THEN
              IF (M.EQ.0) RMLT0 = SF
              IF (M.GT.0) RMLT0 = SF*RMLT(JPT,II,M)
              IF (RMLT0.NE.0.0 .AND. LZ1.GT.0) THEN
                IFLAG0 = 0
                DO 100 IZ = 5, IPCLST(4,ICL)
                  NZ = IPCLST(IZ,ICL)
                  IF (NZ.EQ.0 .OR. IFLAG0.EQ.1) GOTO 110
                  IF (IZON(JPT,II,LZ1).EQ.NZ) IFLAG0 = 1
  100           CONTINUE
  110           IF (IFLAG0.EQ.0) RMLT0 = 0.0
              ENDIF
            ENDIF
            TH1 = BOTM(JPT,II,LBOTM(KPT)-1)-BOTM(JPT,II,LBOTM(KPT))
            IF (LT.GT.0 .AND.
     &          HNEW(JPT,II,KPT).LT.BOTM(JPT,II,LBOTM(KPT)-1))
     &          TH1 = HNEW(JPT,II,KPT) - BOTM(JPT,II,LBOTM(KPT))
            IF (PIDTMP.EQ.'HK  ') THEN
              CALL SSEN1LPF1CH(CO(ICNT),TH2,HP,II,JPT,KPT,'CC',IL,M,
     &                        RMLT0,RMLT,LZ1,IZON,SF,LT,
     &                        HK,NCOL,NROW,NLAY,DELC,DELR,HNEW,TH1,
     &                        BOTM(1,1,LBOTM(KPT)),
     &                        BOTM(1,1,LBOTM(KPT)-1),NMLTAR,NZONAR,
     &                        ICL,C,HANI)
              IF (IUHFB.GT.0 .AND. CO(ICNT).NE.0.)
     &            CALL SSEN1HFB6MD(C,'CC',CO(ICNT),DELC,DELR,HFB,
     &                             II,JPT,KPT,MXACTFB,NCOL,
     &                             NHFB,NROW,TH1,TH2)
            ENDIF
  120     CONTINUE
        ENDIF
C-------CV--------------------------------------------------------------
        IF (PIDTMP.EQ.'VK  '. OR. PIDTMP.EQ.'VANI' .OR. 
     &     (PIDTMP.EQ.'HK  ' .AND. LAYVKA(K).NE.0) .OR.
     &      PIDTMP.EQ.'VKCB') THEN
          DO 130 KK = KPT-1, KPT+1
            IF (KK.LT.1 .OR. KK.GT.NLAY) GOTO 130
            IF(KK.NE.IPCLST(1,ICL)) GOTO 130
C            IF ( KK.LE.KPT) THEN
              CALL SSEN1LPF1CV(COD,COU,IBP,IBM,PIDTMP,IL,SF,RMLT,M,
     &                     NCOL,NROW,LZ1,CV,SV,NLAY,DELR,
     &                     DELC,JPT,IPT,KK,HK,IZON,IBOUND,
     &                     NMLTAR,NZONAR,ICL,BOTM,NBOTM,VKA,HNEW)
              IF (KK.LT.KPT) CO(5) = CO(5) + COD
              IF (KK.EQ.KPT) THEN
                CO(5) = CO(5) + COU
                CO(6) = CO(6) + COD
              ENDIF
              IF (KK.GT.KPT) CO(6) = CO(6) + COU
C            ENDIF
  130     CONTINUE
        ENDIF
  140 CONTINUE
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2S(NCOL,NROW,NLAY,IOUT,IBOUND,HNEW,DELR,DELC,
     &                  DL,VXL,VXR,VYT,VYB,VZT,VZB,VXP,VYP,VZP,AX,
     &                  AY,AZ,XPC,YPC,ZPC,IPT,JPT,KPT,TP,ZP,DX,DY,DZ,DT,
     &                  ADVSTP,KTFLG,NEWI,NEWJ,NEWK,IEXIT,IP,DXP,
     &                  DYP,DZP,DVXL,DVYT,DVZB,DAX,DAY,DAZ,DVXP,DVYP,
     &                  DVZP,DDX,DDY,DDZ,TT2,PSTP,IPRINT,ITERP,
     &                  KTDIM,BOTM,NBOTM,LAYHDT)
C     VERSION 1000 22OCT1993
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO DETERMINE THE DISPLACEMENT OF A PARTICLE USING THE
C     LINEARLY INTERPOLATED VELOCITIES AND A SEMI-ANALYTICAL TRACKING
C     METHOD.
C     RETURNS IEXIT = 1 IF CELL IS INACTIVE
C                     2 IF PARTICLE IS STAGNANT (I.E. STRONG SINK)
C                     3 IF OBSERVATION TIME HAS BEEN REACHED
C                     4 IF THE NEW POSITION IS AN INACTIVE CELL
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C     SPECIFICATIONS:
C-----------------------------------------------------------------------
C---ARGUMENTS
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      REAL DELR, DELC, DL, VXL, VXR, VYT, VYB, VZT, VZB, VXP, VYP, 
     &     VZP, AX, AY, AZ, XPC, YPC, ZPC, TP, ZP, DX, DY, DZ, DT, 
     &     ADVSTP, DXP, DYP, DZP, DVXL, DVYT, DVZB, DAX, DAY, DAZ,
     &     DVXP, DVYP, DVZP, DDX, DDY, DDZ, TT2, PSTP
      INTEGER NCOL, NROW, NLAY, IOUT, IBOUND, IPT, JPT, KPT, KTFLG, 
     &        NEWI, NEWJ, NEWK, IEXIT, IP, IPRINT, ITERP, KTDIM
      DIMENSION DELR(NCOL), DELC(NROW), IBOUND(NCOL,NROW,NLAY), 
     &          BOTM(NCOL,NROW,0:NBOTM), LAYHDT(NLAY)
C---LOCAL:
      REAL DC, DPSTP, DR, DTT2, DTX, DTY, DTZ, DVX, DVY, DVZ, EX, EY, 
     &     EZ, HN, ZT
      INTEGER NXFLG, NYFLG, NZFLG
C---COMMON:
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
C----------------------------------------------------------------------
C
C     VERSION 1000 22OCT1993
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C----------------------------------------------------------------------
      INTRINSIC ABS, LOG, EXP
C----------------------------------------------------------------------
  500 FORMAT (/,
     &   ' (SOBS1ADV2S) STOP EXECUTION - PARTICLE STAGNANT IN CELL ',
     &   '(ROW,COL)',2I5)
  505 FORMAT (/,' CELL (',3I4,') IS NOT ACTIVE. (SOBS1ADV2S)')
  510 FORMAT (12('-'),'PARTICLE DISCHARGED (SOBS1ADV2S)',12('-'))
C----------------------------------------------------------------------
C
      HN = HNEW(JPT,IPT,KPT)
      IF (IBOUND(JPT,IPT,KPT).EQ.0 .OR. HN.GT.1.E+29) THEN
        IEXIT = 1
        IF (IPRINT.EQ.0 .AND. ITERP.LT.2) WRITE (IOUT,505) IPT, JPT, KPT
      ENDIF
      IF (IEXIT.EQ.1) THEN
        IF (IPRINT.EQ.0 .AND. ITERP.LT.2) WRITE (IOUT,510)
        RETURN
      ENDIF
      NXFLG = 0
      NYFLG = 0
      NZFLG = 0
      DR = DELR(JPT)
      DC = DELC(IPT)
      DVY = 0.0
      DVX = 0.0
      DVZ = 0.0
      IF (VYP.NE.0.0) DVY = ABS(VYB-VYT)/ABS(VYP)
      IF (VXP.NE.0.0) DVX = ABS(VXR-VXL)/ABS(VXP)
      IF (VZP.NE.0.0) DVZ = ABS(VZB-VZT)/ABS(VZP)
C
C------DETERMINE X, Y AND Z TRANSIT TIMES TO EDGES OF CELL
      CALL SOBS1ADV2DT(VXL,VXR,VXP,AX,DVX,DR,XPC,DTX,NXFLG)
      CALL SOBS1ADV2DT(VYT,VYB,VYP,AY,DVY,DC,YPC,DTY,NYFLG)
      CALL SOBS1ADV2DT(VZB,VZT,VZP,AZ,DVZ,DL,ZPC,DTZ,NZFLG)
C
C-----IF PARTICLE STAGNANT IN CELL THEN PRINT WARNING AND EXIT
      IF (NXFLG.EQ.1 .AND. NYFLG.EQ.1 .AND. NZFLG.EQ.1) THEN
        IEXIT = 2
        IF (IPRINT.EQ.0 .AND. ITERP.LT.2) WRITE (IOUT,500) IPT, JPT
        RETURN
      ENDIF
      DTT2 = TT2 - TP
      DPSTP = ADVSTP - PSTP
      IF (ABS(DTT2).LT.1.E-6) THEN
        IEXIT = 2
        RETURN
      ENDIF
      IF (NXFLG.EQ.1) DTX = DTT2
      IF (NYFLG.EQ.1) DTY = DTT2
      IF (NZFLG.EQ.1) DTZ = DTT2
C
C     NOW DISPLACE PARTICLE BY EITHER...
C
C-----...PUTTING PARTICLE ON ONE CELL FACE, DISPLACING IN REMAINING DIRECTIONS
      IF (KTFLG.EQ.1 .AND. (DTX.LT.DTT2.OR.DTY.LT.DTT2.OR.DTZ.LT.DTT2)
     &    .OR. KTFLG.GT.1 .AND. 
     &    ((DTX.LT.DPSTP.OR.DTY.LT.DPSTP.OR.DTZ.LT.DPSTP).AND.
     &    (DTX.LT.DTT2.OR.DTY.LT.DTT2.OR.DTZ.LT.DTT2))) THEN
C     PUT IN X-DIRECTION, AND...
        IF (DTX.LT.DTZ .OR. DTY.LT.DTZ) THEN
          IF (DTX.LT.DTY) THEN
            DT = DTX
            NEWI = IPT
            NEWK = KPT
            IF (VXP.LT.0.0) THEN
              NEWJ = JPT - 1
              DX = -XPC
              IF (NEWJ.GT.0) XPC = DELR(NEWJ)
            ENDIF
            IF (VXP.GT.0.0) THEN
              NEWJ = JPT + 1
              DX = DR - XPC
              XPC = 0.0
            ENDIF
C           ...DISPLACE Y AND Z
            CALL SOBS1ADV2PD(VYT,VYP,AY,DVY,YPC,DT,DY)
            CALL SOBS1ADV2PD(VZB,VZP,AZ,DVZ,ZPC,DT,DZ)
            YPC = YPC + DY
            ZPC = ZPC + DZ
C          ENDIF
C
C     -OR-
C
C     PUT IN Y-DIRECTION AND...
C        IF (DTY.LT.DTX .AND. DTY.LT.DTZ) THEN
          ELSE
            DT = DTY
            NEWJ = JPT
            NEWK = KPT
            IF (VYP.LT.0.0) THEN
              NEWI = IPT - 1
              DY = -YPC
              IF (NEWI.GT.0) YPC = DELC(NEWI)
            ENDIF
            IF (VYP.GT.0.0) THEN
              NEWI = IPT + 1
              DY = DC - YPC
              YPC = 0.0
            ENDIF
C           ...DISPLACE X AND Z
            CALL SOBS1ADV2PD(VXL,VXP,AX,DVX,XPC,DT,DX)
            CALL SOBS1ADV2PD(VZB,VZP,AZ,DVZ,ZPC,DT,DZ)
            XPC = XPC + DX
            ZPC = ZPC + DZ
          ENDIF
C
C     CHECK TO SEE IF NEW POSITION IS OUTSIDE GRID BEFORE CORRECTING Z POSITION
          IF(NEWI.GT.0 .AND. NEWI.LE.NROW .AND.
     &       NEWJ.GT.0 .AND. NEWJ.LE.NCOL .AND.
     &       NEWK.GT.0 .AND. NEWK.LE.NLAY) THEN
            IF (IBOUND(NEWJ,NEWI,NEWK).EQ.0) IEXIT = 4
          ELSE
            IEXIT=4
          ENDIF

C
C     CORRECT Z-POSITION FOR DISTORTED GRID
C
          IF (KTDIM.EQ.3 .AND. IEXIT.EQ.0 .AND.
     &        (NEWK.EQ.KPT.AND.(NEWI.NE.IPT.OR.NEWJ.NE.JPT))) THEN
            LTP = LAYHDT(KPT)
            ZT = BOTM(NEWJ,NEWI,LBOTM(NEWK)-1)
            ZB = BOTM(NEWJ,NEWI,LBOTM(NEWK))
            IF(LTP.NE.0.AND.HNEW(NEWJ,NEWI,NEWK).LT.ZT)
     &        ZT=HNEW(NEWJ,NEWI,NEWK)
            ZPC = ZPC*(ZT-ZB)/DL
            DZ = ZT + ZPC - (ZT-ZB) - ZP
          ENDIF
C
C     -OR-
C
C     PUT IN Z-DIRECTION AND...
C        IF (DTZ.LT.DTX .AND. DTZ.LT.DTY) THEN
        ELSE
          DT = DTZ
          NEWI = IPT
          NEWJ = JPT
          IF (VZP.LT.0.0) THEN
            NEWK = KPT + 1
            DZ = -ZPC
            IF(NEWK.LT.NLAY) ZPC = BOTM(JPT,IPT,LBOTM(NEWK)-1)
     &                             - BOTM(JPT,IPT,LBOTM(NEWK))
          ENDIF
          IF (VZP.GT.0.0) THEN
            NEWK = KPT - 1
            DZ = DL - ZPC
            ZPC = 0.0
          ENDIF
C         ...THEN DISPLACE X AND Y
          CALL SOBS1ADV2PD(VXL,VXP,AX,DVX,XPC,DT,DX)
          CALL SOBS1ADV2PD(VYT,VYP,AY,DVY,YPC,DT,DY)
          XPC = XPC + DX
          YPC = YPC + DY
        ENDIF
        IF (KTFLG.GT.1) PSTP = PSTP + DT
C
C     ...OR JUST DISPLACING X, Y, AND Z
C        FIRST DETERMINE DT
      ELSE
        NEWI = IPT
        NEWJ = JPT
        NEWK = KPT
        IF (PSTP.EQ.0.0) THEN
          IF (DTT2.LT.ADVSTP) THEN
            DT = DTT2
            IEXIT = 3
            IF(KTFLG.GT.1) PSTP = PSTP + DT
          ELSE
            DT = ADVSTP
          ENDIF
        ELSEIF (DTT2.LT.DPSTP) THEN
          DT = DTT2
          IEXIT = 3
          IF(KTFLG.GT.1) PSTP = PSTP + DT
        ELSE
          DT = DPSTP
          PSTP = 0.0
        ENDIF
C           ...THEN DISPLACE
        CALL SOBS1ADV2PD(VXL,VXP,AX,DVX,XPC,DT,DX)
        CALL SOBS1ADV2PD(VYT,VYP,AY,DVY,YPC,DT,DY)
        CALL SOBS1ADV2PD(VZB,VZP,AZ,DVZ,ZPC,DT,DZ)
        XPC = XPC + DX
        YPC = YPC + DY
        ZPC = ZPC + DZ
      ENDIF
C
C
C-----NOW DETERMINE X, Y, AND Z SENSITIVITIES
      IF (IP.GT.0) THEN
        CALL SOBS1ADV2DD(DVX,VXL,DVXL,VXP,DVXP,AX,DAX,DXP,DT,DDX)
        CALL SOBS1ADV2DD(DVY,VYT,DVYT,VYP,DVYP,AY,DAY,DYP,DT,DDY)
        CALL SOBS1ADV2DD(DVZ,VZB,DVZB,VZP,DVZP,AZ,DAZ,DZP,DT,DDZ)
      ENDIF
C     CHECK TO SEE IF NEW POSITION IS OUTSIDE GRID
      IF(NEWI.GT.0 .AND. NEWI.LE.NROW .AND.
     &   NEWJ.GT.0 .AND. NEWJ.LE.NCOL .AND.
     &   NEWK.GT.0 .AND. NEWK.LE.NLAY) THEN
        IF (IBOUND(NEWJ,NEWI,NEWK).EQ.0) IEXIT = 4
      ELSE
        IEXIT=4
      ENDIF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2DD(DV,V1,DV1,VP,DVP,A,DA,DP,DT,DD)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE PARTICLE DISPLACEMENT SENSITIVITY
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
        IF (DV.LT.1.E-5) THEN
          DD = DVP*DT
        ELSE
          E = EXP(A*DT)
          DD = ((DA*DT*VP*E)-DV1+DVP*E)/A
          DD = DD - (DA*(VP*E-V1)/A/A)
          DD = DD - DP
        ENDIF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2PD(V1,VP,A,DV,PC,DT,DD)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE PARTICLE DISPLACEMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF (DV.LT.1.E-5) THEN
        DD = VP*DT
      ELSE
        DD = ((VP*EXP(A*DT)-V1)/A) - PC
      ENDIF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2DT(V1,V2,VP,A,DV,DR,PC,DT,NFLG)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE TRAVEL TIME OVER A GIVEN DISTANCE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      DT = 0.0
      NFLG = 0
      IF (V1.GE.0.0 .AND. V2.LE.0.0) NFLG = 1
      IF (NFLG.NE.1) THEN
C------1 TO 2
        IF (VP.GT.0.0) THEN
          IF (DV.LT.1.E-5 .OR. V2.EQ.0.0) THEN
            DT = (DR-PC)/VP
          ELSE
            DT = LOG(V2/VP)/A
          ENDIF
        ENDIF
C------2 TO 1
        IF (VP.LT.0.0) THEN
          IF (DV.LT.1.E-5 .OR. V1.EQ.0.0) THEN
            DT = -PC/VP
          ELSE
            DT = LOG(V1/VP)/A
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2O(NHT,NTT2,HOBS,H,WTQ,IOUT,D,
     &                      IDIS,IDTT,JDRY,RSQ,NRUNS,AVET,NPOST,NNEGT,
     &                      KTDIM,ND,MPR,IPR,IO,OBSNAM,N,NDMH,WTRL,NRES,
     &                      IUGDO,OUTNAM,IPLOT,IPLPTR,LCOBADV,ISSWR,
     &                      SSAD,ITMXP)
C
C     SPECIFICATIONS:
C-----------------------------------------------------------------------
C---ARGUMENTS:
      REAL HOBS, H, WTQ, D, RSQ, AVET, WTRL
      INTEGER NHT, NTT2, IOUT, IDIS, IDTT,
     &        JDRY, NRUNS, NPOST, NNEGT, KTDIM, ND, MPR, IPR, IO, 
     &        N, NDMH, NRES
      INTEGER IUGDO(6), IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      DIMENSION H(ND), HOBS(ND), WTQ(NDMH,NDMH), D(ND+MPR+IPR),
     &          SSAD(ITMXP+1)
C---LOCAL:
      REAL AVE, RES1, RES2, RES3, VMAX, VMIN, W1, W2, W3, WT12, WT22, 
     &     WT33, WTR, WTR1, WTR2, WTR3
      INTEGER I, NH1, NH2, NMAX, NMIN, NNEG, NPOS
C-----------------------------------------------------------------------
      INTRINSIC SQRT, REAL
C-----------------------------------------------------------------------
  500 FORMAT (/,' SUM OF SQUARED WEIGHTED RESIDUALS',/,' (ADVECTIVE-',
     &        'TRANSPORT OBSERVATIONS ONLY)  ',G11.5)
  505 FORMAT (/,' STATISTICS FOR ADVECTIVE-TRANSPORT RESIDUALS :',/,
     &        ' MAXIMUM WEIGHTED RESIDUAL  :',E10.3,' OBS#',I7,/,
     &        ' MINIMUM WEIGHTED RESIDUAL  :',E10.3,' OBS#',I7,/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',E10.3,/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
  510 FORMAT (2G20.7)
  515 FORMAT (' ')
  520 FORMAT (/,' ADVECTIVE-TRANSPORT OBSERVATIONS ',//,
     &26X,'OBSERVED',3X,'CALCULATED',13X,'WEIGHT',2X,'WEIGHTED',/,
     &2X,'OBS#   ID',15X,'LOCATION',4X,'LOCATION',2X,'RESIDUAL',
     &5X,'**0.5',2X,'RESIDUAL',/)
  525 FORMAT (1X,I5,1X,A12,1X,A,5(1X,G10.3),3F11.2,F11.5,F11.2)
c  510 FORMAT (1X,I5,1X,A12,1X,5(1X,G10.3))
  540 FORMAT (2(G15.7,1X),I5,2X,A)
  550 FORMAT (G15.7,1X,I5,2X,A)
C
C  INITIALIZE IDTT TO ZERO BECAUSE OTHERWISE, IT IS NEVER SET...
C  WHO KNOWS WHAT IT IS SUPPOSED TO DO...ERB
C
      IDTT = 0
      IF (IO.EQ.1) WRITE (IOUT,520)
      RES3 = 0.0
      W3 = 0.0
      WT33 = 0.0
      WTR3 = 0.0
      NNEG = 0
      NPOS = 0
      NRESAD = 0
      NRUNSAD = 1
      RSQAD = 0.0
      VMAX = -1.E20
      VMIN = 1.E20
      AVE = 0.0
      NH1 = LCOBADV
      NH2 = LCOBADV + KTDIM*(NTT2-1)
      DO 20 N = NH1, NH2, KTDIM
        IPLPTR(NRES+1) = N
        IPLPTR(NRES+2) = N + 1
        IF (KTDIM.EQ.3) IPLPTR(NRES+3) = N + 2
        NRES = NRES + KTDIM
        NRESAD = NRESAD + KTDIM
        RES1 = HOBS(N) - H(N)
        RES2 = HOBS(N+1) - H(N+1)
        IF (KTDIM.EQ.3) RES3 = HOBS(N+2) - H(N+2)
        W1 = WTQ(N-NHT,N-NHT)
        W2 = WTQ(N-NHT+1,N-NHT+1)
        IF (KTDIM.EQ.3) W3 = WTQ(N-NHT+2,N-NHT+2)
        WT12 = SQRT(W1)
        WT22 = SQRT(W2)
        IF (KTDIM.EQ.3) WT33 = SQRT(W3)
        WTR1 = RES1*WT12
        WTR2 = RES2*WT22
        IF (KTDIM.EQ.3) WTR3 = RES3*WT33
        IF (IO.EQ.1) THEN
          WRITE (IOUT,525) N, OBSNAM(N), 'X = ', HOBS(N), H(N), RES1, 
     &                     WT12, WTR1
          WRITE (IOUT,525) N+1, OBSNAM(N+1), 'Y = ', HOBS(N+1), H(N+1),
     &                     RES2, WT22, WTR2
          IF (KTDIM.EQ.3) WRITE (IOUT,525) N+2, OBSNAM(N+2), 'Z = ', 
     &                                     HOBS(N+2), H(N+2), RES3, 
     &                                     WT33, WTR3
          IF (OUTNAM.NE.'NONE') THEN
C           X COORDINATE
            WRITE (IUGDO(1),540) H(N), HOBS(N), IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(2),540) WT12*H(N), WT12*HOBS(N), IPLOT(N),
     &                           OBSNAM(N)
            WRITE (IUGDO(3),540) WT12*H(N), WTR1, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(4),550) RES1, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(5),550) WTR1, IPLOT(N), OBSNAM(N)
C           Y COORDINATE
            WRITE (IUGDO(1),540) H(N+1), HOBS(N+1), IPLOT(N+1),
     &                           OBSNAM(N+1)
            WRITE (IUGDO(2),540) WT22*H(N+1), WT22*HOBS(N+1),
     &                           IPLOT(N+1), OBSNAM(N+1)
            WRITE (IUGDO(3),540) WT22*H(N+1), WTR2, IPLOT(N+1),
     &                           OBSNAM(N+1)
            WRITE (IUGDO(4),550) RES2, IPLOT(N+1), OBSNAM(N+1)
            WRITE (IUGDO(5),550) WTR2, IPLOT(N+1), OBSNAM(N+1)
C           Z COORDINATE
            IF (KTDIM.EQ.3) THEN
              WRITE (IUGDO(1),540) H(N+2), HOBS(N+2), IPLOT(N+2),
     &                             OBSNAM(N+2)
              WRITE (IUGDO(2),540) WT33*H(N+2), WT33*HOBS(N+2),
     &                             IPLOT(N+2), OBSNAM(N+2)
              WRITE (IUGDO(3),540) WT33*H(N+2), WTR3, IPLOT(N+2),
     &                             OBSNAM(N+2)
              WRITE (IUGDO(4),550) RES3, IPLOT(N+2), OBSNAM(N+2)
              WRITE (IUGDO(5),550) WTR3, IPLOT(N+2), OBSNAM(N+2)
            ENDIF
            D(N-JDRY-IDIS-IDTT) = WTR1
            D(N-JDRY-IDIS-IDTT+1) = WTR2
            IF (KTDIM.EQ.3) D(N-JDRY-IDIS-IDTT+2) = WTR3
          ENDIF
        ENDIF
        RSQ = RSQ + (WTR1**2) + (WTR2**2) + (WTR3**2)
        RSQAD = RSQAD + (WTR1**2) + (WTR2**2) + (WTR3**2)
        IF (WTR1.GT.VMAX) THEN
          VMAX = WTR1
          NMAX = N
        ENDIF
        IF (WTR2.GT.VMAX) THEN
          VMAX = WTR2
          NMAX = N + 1
        ENDIF
        IF (WTR3.GT.VMAX) THEN
          VMAX = WTR3
          NMAX = N + 2
        ENDIF
        IF (WTR1.LT.VMIN) THEN
          VMIN = WTR1
          NMIN = N
        ENDIF
        IF (WTR2.LT.VMIN) THEN
          VMIN = WTR2
          NMIN = N + 1
        ENDIF
        IF (WTR3.LT.VMIN) THEN
          VMIN = WTR3
          NMIN = N + 2
        ENDIF
        IF (WTR1.GE.0.0) NPOS = NPOS + 1
        IF (WTR2.GE.0.0) NPOS = NPOS + 1
        IF (KTDIM.EQ.3 .AND. WTR3.GE.0.0) NPOS = NPOS + 1
        IF (WTR1.LT.0.0) NNEG = NNEG + 1
        IF (WTR2.LT.0.0) NNEG = NNEG + 1
        IF (KTDIM.EQ.3 .AND. WTR3.LT.0.0) NNEG = NNEG + 1
        WTR = WTR1
        DO 10 I = 1, KTDIM
          IF (I.EQ.2) WTR = WTR2
          IF (I.EQ.3) WTR = WTR3
          IF (N.GT.1 .AND. (WTRL*WTR).LT.0.0) NRUNS = NRUNS + 1
          IF (N.GT.LCOBADV) THEN
            IF (WTRL*WTR.LT.0.) NRUNSAD = NRUNSAD + 1
          ENDIF
          WTRL = WTR
          AVE = AVE + WTR
   10   CONTINUE
   20 CONTINUE
      IF (ISSWR.GT.0) SSAD(ISSWR) = RSQAD
      N = N - 1 - JDRY - IDIS - IDTT
      AVET = AVET + AVE
      NPOST = NPOST + NPOS
      NNEGT = NNEGT + NNEG
      AVE = AVE/REAL(KTDIM*NTT2)
      IF (IO.EQ.1) WRITE (IOUT,505) VMAX, NMAX, VMIN, NMIN, AVE, NPOS, 
     &                              NNEG, NRUNSAD, NRESAD
      IF (IO.EQ.1) WRITE (IOUT,500) RSQAD
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE SOBS1ADV2WR(IEXIT,IP,IPRINT,ITERP,PSTP,IOUT,KPT,IPT,
     &                   JPT,XP,YP,ZP,TP,DXP,DYP,DZP,B,IOUTT2,IND,VXP,
     &                   VYP,VZP,VP,KTDIM,OBSNAM,KTFLG,ISCALS,LNIIPP,
     &                   WTQ)
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      REAL PSTP, XP, YP, ZP, TP, DXP, DYP, DZP, B, VXP, VYP, VZP, VP
      INTEGER IEXIT, IP, IPRINT, ITERP, IOUT, KPT, IPT, JPT, IOUTT2, 
     &        IND, KTDIM, KTFLG
      CHARACTER*12 OBSNAM
C----------------------------------------------------------------------
  500 FORMAT (3I5,1X,4(G15.8,1X))
C  500 FORMAT (3I5,1X,4(G15.8,1X),/,5(G15.8,1X),/)
  505 FORMAT (80('.'),/,'OBS # ',I3,'-',I3,5X,'OBS NAME: ',A,/,
     &        3I5,1X,4(G15.8,1X),/,80('.'))
  600 FORMAT (20X,3I5,1X,8(E13.7,1X))
  605 FORMAT (I3,'-',I3,1X,A,3I5,1X,8(E13.7,1X))
C----------------------------------------------------------------------
C
cc    make these statements contingent on IP>0 - erb 8/18/00
      if (ip.gt.0) then
        BB=1.0
        IF(LNIIPP.GT.0) BB=BB*LOG(B)
        IF (ABS(BB).LT.1.E-25) BB = 1.0
        IF(ISCALS.GT.0) BB=BB*B*SQRT(WTQ)
        IF (ABS(BB).LT.1.E-25) BB = 1.0
      endif

      IF (IEXIT.LT.2) THEN
C     PRINT OUT NORMAL OUTPUT(I.E. IEXIT=1 OR 0)
        IF (IP.GT.0) THEN
          IF (IPRINT.EQ.0 .AND. ITERP.LT.2 .AND. 
     &        ((KTFLG.LT.3.AND.PSTP.EQ.0.0).OR.KTFLG.EQ.3)) THEN
            WRITE (IOUT,500) KPT, IPT, JPT, DXP*BB, DYP*BB, DZP*BB, TP
            IF (IOUTT2.GT.0) WRITE (IOUTT2,600) KPT, IPT, JPT, XP, YP, 
     &                            ZP, VP, TP, DXP*BB, DYP*BB, DZP*BB
          ENDIF
        ELSEIF (IPRINT.EQ.0 .AND. ITERP.LT.2 .AND. 
     &          ((KTFLG.LT.3.AND.PSTP.EQ.0.0).OR.KTFLG.EQ.3)) THEN
          WRITE (IOUT,500) KPT, IPT, JPT, XP, YP, ZP, TP
          IF (IOUTT2.GT.0) WRITE (IOUTT2,600) KPT, IPT, JPT, XP, YP, ZP, 
     &                                        VXP, VYP, VZP, VP, TP
        ENDIF
C        IF TIME OF OBSERVATION HAS BEEN REACHED (I.E. IEXIT=3)
C           PRINT OBSERVATION INFO AND GOTO NEXT PARTICLE
      ELSEIF (IP.GT.0) THEN
        IF (IPRINT.EQ.0 .AND. ITERP.LT.2 .AND. 
     &      ((KTFLG.LT.3.AND.PSTP.EQ.0.0).OR.KTFLG.EQ.3)) THEN
          IF (KTDIM.EQ.2) THEN
            WRITE (IOUT,505) IND, IND+1, OBSNAM, KPT, IPT, JPT,  
     &                     DXP*BB, DYP*BB, DZP*BB, TP
            IF (IOUTT2.GT.0)
     &        WRITE (IOUTT2,605) IND, IND + 1, OBSNAM, KPT, IPT, JPT,
     &                           XP, YP, ZP, VP, TP, DXP*BB, DYP*BB,
     &                           DZP*BB
          ELSEIF (KTDIM.EQ.3) THEN
            WRITE (IOUT,505) IND, IND+2, OBSNAM, KPT, IPT, JPT,
     &                     DXP*BB, DYP*BB, DZP*BB, TP
            IF (IOUTT2.GT.0) WRITE (IOUTT2,605) IND, IND+2,OBSNAM,KPT, 
     &                            IPT, JPT, XP, YP, ZP, VP, TP, DXP*BB, 
     &                            DYP*BB, DZP*BB
          ENDIF
        ENDIF
      ELSEIF (IPRINT.EQ.0 .AND. ITERP.LT.2) THEN
        IF (KTDIM.EQ.2) THEN
          WRITE (IOUT,505) IND, IND+1, OBSNAM, KPT, IPT, JPT, XP, YP, 
     &                     ZP, TP
          IF (IOUTT2.GT.0) WRITE (IOUTT2,605) IND, IND+1, OBSNAM, KPT, 
     &                                        IPT, JPT, XP, YP, ZP, VXP, 
     &                                        VYP, VZP, VP, TP
        ELSEIF (KTDIM.EQ.3) THEN
          WRITE (IOUT,505) IND, IND+2, OBSNAM, KPT, IPT, JPT, XP, YP,  
     &                     ZP, TP
          IF (IOUTT2.GT.0) WRITE (IOUTT2,605) IND, IND+2, OBSNAM, KPT, 
     &                                        IPT, JPT, XP, YP, ZP, VXP, 
     &                                        VYP, VZP, VP, TP
        ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1ADV2UP(XP,DX,YP,DY,ZP,DZ,TP,DT,VP,OLDK,KPT,IPT,
     &  NEWI,JPT,NEWJ,NEWK,H,IND,KTDIM,IP,DXP,DDX,DYP,DDY,
     &  DZP,DDZ,X,LN,NPE,ND,NPLIST,IIPP,DTP)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO UPDATE THE POSITION OF THE PARTICLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
      INTEGER OLDK,KPT,NEWK,IPT,NEWI,JPT,NEWJ,IND
      REAL H,XP,DX,YP,DY,ZP,DZ,TP,DT,DP,VP,DTP
      DIMENSION H(ND),X(NPE,ND),LN(NPLIST)
      INCLUDE 'param.inc'
C----------------------------------------------------------------------
      XP = XP + DX
      YP = YP + DY
      ZP = ZP + DZ
      TP = TP + DT
      DP = (DX**2.+DY**2.+DZ**2.)**0.5
      IF (DT.GT.0.0) VP = DP/DT
      DTP = DTP + DP
      OLDK = KPT
      IPT = NEWI
      JPT = NEWJ
      KPT = NEWK
      H(IND) = XP
      H(IND+1) = YP
      IF (KTDIM.EQ.3) H(IND+2) = ZP
      IF (IP.GT.0) THEN
        DXP = DXP + DDX
        DYP = DYP + DDY
        DZP = DZP + DDZ
        X(IP,IND) = DXP
        X(IP,IND+1) = DYP
        IF (KTDIM.EQ.3) X(IP,IND+2) = DZP
        IF(LN(IIPP).GT.0) THEN
          X(IP,IND)=X(IP,IND)*B(IIPP)
          X(IP,IND+1)=X(IP,IND+1)*B(IIPP)
          IF (KTDIM.EQ.3) X(IP,IND+2)=X(IP,IND+2)*B(IIPP)
        ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1ADV2CC(NLAY,OLDK,NEWK,
     &                   JPT,IPT,KPT,KTREV,PRST,VZB,VZT,IP,DVZB,DVZT,
     &                   TT2,TP,ADVSTP,PSTP,IEXIT,VXP,VYP,VZP,KTFLG,ZP,
     &                   H,DZP,X,LN,IIPP,NCOL,NROW,IND,OBSNAM,IPRINT,
     &                   ITERP,IOUT,XP,YP,DXP,DYP,IOUTT2,KTDIM,
     &                   ISCALS,NPE,ND,NPLIST,BOTM,NBOTM,DTP,LNIIPP,WTQ,
     &                   BSCAL,DTC,ZPC)
C     ******************************************************************
C     TRACK PARTICLE THROUGH CONFINING UNIT
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C---ARGUMENTS:
      INTEGER NLAY,OLDK,NEWK,JPT,IPT,KPT,KTREV,
     &  IP,IEXIT,KTFLG,LN,NCOL,NROW,IND,IPRINT,ITERP,IOUT,IOUTT2,KTDIM,
     &  ISCALS,NPE,ND,NPLIST,NBOTM
      CHARACTER*12 OBSNAM
      DIMENSION PRST(NCOL,NROW,NBOTM),LN(NPLIST),X(NPE,ND),
     &  BOTM(NCOL,NROW,0:NBOTM),BSCAL(NPLIST), H(ND)
C---COMMON:
      INCLUDE 'param.inc'
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
C     ------------------------------------------------------------------
C
      IF(DTC.GT.0.0) GOTO 60
C     GOING DOWN (MINUS SIGNS ON DZ AND VZB CANCEL)
      IF (NEWK.GT.OLDK .AND. LAYCBD(OLDK).NE.0) THEN
        DZ = -(BOTM(JPT,IPT,LBOTM(OLDK))-BOTM(JPT,IPT,LBOTM(OLDK)+1))
        DT = ((DZ*KTREV*PRST(JPT,IPT,LBOTM(OLDK)+1))
     &          /(VZB*KTREV*PRST(JPT,IPT,LBOTM(OLDK))))
        VZP=VZP*PRST(JPT,IPT,LBOTM(OLDK))/PRST(JPT,IPT,LBOTM(OLDK)+1)
        IF (IP.GT.0) DDZ = DVZB*DT
C
C     ELSE GOING UP
      ELSEIF (NEWK.LT.OLDK .AND. LAYCBD(NEWK).NE.0) THEN
        DZ = BOTM(JPT,IPT,LBOTM(NEWK))-BOTM(JPT,IPT,LBOTM(NEWK)+1)
        DT = ((DZ*KTREV*PRST(JPT,IPT,LBOTM(NEWK)+1))
     &     /(VZT*KTREV*PRST(JPT,IPT,LBOTM(OLDK))))
        VZP=VZP*PRST(JPT,IPT,LBOTM(OLDK))/PRST(JPT,IPT,LBOTM(NEWK)+1)
        IF (IP.GT.0) DDZ = DVZT*DT
      ENDIF
C
C-----IF THE PARTICLE IS STILL IN A CONFINING UNIT, CHECK TO SEE IF
C     THE TIME OF THE OBSERVATION OR PARTICLE STEP WILL BE REACHED
   60 IF (DTC.GT.0.0) THEN
        IF (DTC.GT.TT2-TP .OR. DTC.GT.ADVSTP-PSTP) THEN
          IF (TT2-TP.GT.ADVSTP-PSTP) THEN
            IF (IP.GT.0) DDZP = DDZ
            DT = ADVSTP - PSTP
            PSTP = 0.0
            DTC = DTC - DT
          ELSE
            IEXIT = 3
            DT = TT2 - TP
            DTC = DTC - DT
            IF(KTFLG.GT.1) PSTP = PSTP + DT
          ENDIF
        ELSE
          DT = DTC
          DTC = 0.0
          IF(KTFLG.GT.1) PSTP = PSTP + DT
        ENDIF
        IF (VZP.LT.0.0) THEN
          DZ = VZB*KTREV*PRST(JPT,IPT,LBOTM(OLDK))
     &         *DT/(KTREV*PRST(JPT,IPT,LBOTM(OLDK)+1))
          IF (IP.GT.0) DDZ = DVZB*DT
        ELSE
          DZ = VZT*KTREV*PRST(JPT,IPT,LBOTM(OLDK))
     &         *DT/(KTREV*PRST(JPT,IPT,LBOTM(NEWK)+1))
          IF (IP.GT.0) DDZ = DVZT*DT
        ENDIF
C
C-----ELSE CHECK TO SEE IF THE TIME OF THE OBSERVATION OR PARTICLE
C     STEP HAS BEEN REACHED
      ELSEIF (DT.GT.TT2-TP .OR. DT.GT.ADVSTP-PSTP) THEN
        IF (TT2-TP.GT.ADVSTP-PSTP) THEN
          DTC = DT
          IF (IP.GT.0) DDZP = DDZ
          DT = ADVSTP - PSTP
          PSTP = 0.0
          DTC = DTC - DT
        ELSE
          IEXIT = 3
          DTC = DT
          DT = TT2 - TP
          DTC = DTC - DT
          IF(KTFLG.GT.1) PSTP = PSTP + DT
        ENDIF
        IF (VZP.LT.0.0) THEN
          DZ = VZB*KTREV*PRST(JPT,IPT,LBOTM(OLDK))
     &         *DT/(KTREV*PRST(JPT,IPT,LBOTM(OLDK)+1))
          IF (IP.GT.0) DDZ = DVZB*DT
        ELSE
          DZ = VZT*KTREV*PRST(JPT,IPT,LBOTM(OLDK))
     &         *DT/(KTREV*PRST(JPT,IPT,LBOTM(NEWK)+1))
          IF (IP.GT.0) DDZ = DVZT*DT
        ENDIF
      ENDIF
C
C        NOW PRINT THE NEW POSITION AND TIME FROM DISPLACEMENT THROUGH
C        CONFINING UNIT
      NEWI = IPT
      NEWJ = JPT
      NEWK = KPT
      KOLD = OLDK
      DX = 0.0
      DY = 0.0
      DDX = 0.0
      DDY = 0.0
      CALL SOBS1ADV2UP(XP,DX,YP,DY,ZP,DZ,TP,DT,VP,OLDK,KPT,IPT,
     &                 NEWI,JPT,NEWJ,NEWK,H,IND,KTDIM,IP,DXP,DDX,
     &                 DYP,DDY,DZP,DDZ,X,LN,NPE,ND,NPLIST,IIPP,DTP)
      OLDK = KOLD
      IF (IP.GT.0) THEN
        BB = ABS(B(IIPP))
        IF (LN(IIPP).LE.0) THEN
C                   PARAMETER IS NOT LOG-TRANSFORMED
          IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
        ELSE
C                   PARAMETER IS LOG-TRANSFORMED
          IF (BB.EQ.0.0) BB = 1.0
        ENDIF
      ELSE
        BB = 0.0
      ENDIF
      VP=VZP
      CALL SOBS1ADV2WR(IEXIT,IP,IPRINT,ITERP,PSTP,IOUT,KPT,IPT,
     &             JPT,XP,YP,ZP,TP,DXP,DYP,DZP,BB,IOUTT2,IND,
     &             VXP,VYP,VZP,VP,KTDIM,OBSNAM,KTFLG,ISCALS,LNIIPP,
     &             WTQ)
      IF(DTC.EQ.0.0) THEN
C        CALCULATE NEW CELL DISPLACEMENT
        IF (NEWK.GT.OLDK) THEN
          ZPC = BOTM(JPT,IPT,LBOTM(NEWK)-1)
     &        - BOTM(JPT,IPT,LBOTM(NEWK))
        ELSE
          ZPC = 0.0
        ENDIF
        WRITE(IOUT,*) 'PARTICLE EXITING CONFINING UNIT'
      ENDIF
      RETURN
      END
