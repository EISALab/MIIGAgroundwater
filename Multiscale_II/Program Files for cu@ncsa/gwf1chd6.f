C     Last change:  ERB  10 Jul 2002   12:27 pm
      SUBROUTINE GWF1CHD6AL(ISUM,LCCHDS,NCHDS,MXCHD,IN,IOUT,
     1      NCHDVL,IFREFM,NPCHD,IPCBEG,NNPCHD,NOPRCH)
C
C-----VERSION 11JAN2000 GWF1CHD6AL
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR TIME-VARIANT SPECIFIED-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /CHDCOM/CHDAUX(5)
      CHARACTER*16 CHDAUX
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE AND INITIALIZE # OF SPECIFIED-HEAD CELLS
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'CHD6 -- TIME-VARIANT SPECIFIED-HEAD PACKAGE,',
     1  ' VERSION 6, 1/11/2000',/1X,'INPUT READ FROM UNIT ',I4)
      NCHDS=0
      NNPCHD=0
C
C2------READ AND PRINT MXCHD (MAXIMUM NUMBER OF SPECIFIED-HEAD
C2------CELLS TO BE SPECIFIED EACH STRESS PERIOD)
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPCHD,MXPC)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(I10)') MXACTC
         LLOC=11
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTC,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTC
    3 FORMAT(1X,'MAXIMUM OF ',I6,
     1  ' TIME-VARIANT SPECIFIED-HEAD CELLS AT ONE TIME')
C
C3------READ AUXILIARY VARIABLES
      NAUX=0
      NOPRCH=0
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            CHDAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) CHDAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY CHD VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,
     &'LISTS OF TIME-VARIANT SPECIFIED-HEAD CELLS WILL NOT BE PRINTED')
         NOPRCH = 1
         GO TO 10
      END IF
      NCHDVL=5+NAUX
C
C4------ALLOCATE SPACE IN X ARRAY FOR TIME-VARIANT SPECIFIED-HEAD LIST.
      IPCBEG=MXACTC+1
      MXCHD=MXACTC+MXPC
      LCCHDS=ISUM
      ISP=NCHDVL*MXCHD
      ISUM=ISUM+ISP
C
C5------PRINT AMOUNT OF SPACE USED BY THE CHD PACKAGE
      WRITE(IOUT,4) ISP
    4 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY CHD')
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF1CHD6RQ(IN,IOUT,NCHDVL,NCOL,NROW,NLAY,NPCHD,CHDS,
     &      IPCBEG,MXCHD,IFREFM,ITERP,INAMLOC,NOPRCH)
C
C-----VERSION 20011108 GWF1CHD6RQ
C     ******************************************************************
C     READ NAMED PARAMETERS
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION CHDS(NCHDVL,MXCHD)
      COMMON /CHDCOM/CHDAUX(5)
      CHARACTER*16 CHDAUX
C     ------------------------------------------------------------------
C
C1------READ NAMED PARAMETERS.
      IF (ITERP.EQ.1) WRITE(IOUT,1000) NPCHD
 1000 FORMAT(1X,//1X,I5,' TIME-VARIANT SPECIFIED-HEAD PARAMETERS')
      IF(NPCHD.GT.0) THEN
        NAUX=NCHDVL-5
        LSTSUM=IPCBEG
        ITERPU = ITERP
        IF (NOPRCH .EQ.1) ITERPU = 99
        DO 20 K=1,NPCHD
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXCHD,IN,IOUT,IP,'CHD','CHD',ITERP,
     &                  NUMINST,INAMLOC)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C         ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 10 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,ITERP)
            ENDIF
            CALL ULSTRD(NLST,CHDS,LB,NCHDVL,MXCHD,0,IN,IOUT,
     &     'CHD NO.   LAYER   ROW   COL   START FACTOR      END FACTOR',
     &      CHDAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,4,5,ITERPU)
            LB=LB+NLST
   10     CONTINUE
   20   CONTINUE
      END IF
C
C3------RETURN.
      RETURN
      END
      SUBROUTINE GWF1CHD6RP(CHDS,NCHDS,MXCHD,IBOUND,NCOL,NROW,NLAY,IN,
     1          IOUT,NCHDVL,IFREFM,NNPCHD,NPCHD,IPCBEG,NOPRCH)
C
C
C-----VERSION 11JAN2000 GWF1CHD6RP
C     ******************************************************************
C     READ DATA FOR CHD
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION CHDS(NCHDVL,MXCHD),IBOUND(NCOL,NROW,NLAY)
      COMMON /CHDCOM/CHDAUX(5)
      CHARACTER*16 CHDAUX
C     ------------------------------------------------------------------
C
C1------READ ITMP(FLAG TO REUSE DATA AND NUMBER OF PARAMETERS.
      IF(NPCHD.GT.0) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(2I10)') ITMP,NP
         ELSE
            READ(IN,*) ITMP,NP
         END IF
      ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      END IF
C
C2------CALCULATE NUMBER OF AUXILIARY VALUES
      NAUX=NCHDVL-5
      ITERPU = 1
      IOUTU = IOUT
      IF (NOPRCH.EQ.1) THEN
        ITERPU = 99
        IOUTU = -1
      ENDIF
C
C2------TEST ITMP
C2A-----IF ITMP<0 THEN REUSE DATA FROM LAST STRESS PERIOD
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,7)
    7    FORMAT(1X,/1X,'REUSING NON-PARAMETER SPECIFIED-HEAD DATA FROM',
     1     ' LAST STRESS PERIOD')
      ELSE
         NNPCHD=ITMP
      END IF
C
C3------IF THERE ARE NEW NON-PARAMETER CHDS, READ THEM
      MXACTC=IPCBEG-1
      IF(ITMP.GT.0) THEN
         IF(NNPCHD.GT.MXACTC) THEN
            WRITE(IOUT,99) NNPCHD,MXACTC
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE CHD CELLS (',I6,
     1                     ') IS GREATER THAN MXACTC(',I6,')')
            STOP
         END IF
         CALL ULSTRD(NNPCHD,CHDS,1,NCHDVL,MXCHD,0,IN,IOUT,
     1    'CHD NO.   LAYER   ROW   COL    START HEAD        END HEAD',
     2     CHDAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,4,5,ITERPU)
      END IF
      NCHDS=NNPCHD
C
Cx------IF THERE ARE ACTIVE CHD PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('CHD')
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'CHD',IOUTU,'CHD',CHDS,NCHDVL,MXCHD,NCHDVL,
     1             MXACTC,NCHDS,4,5,
     2    'CHD NO.   LAYER   ROW   COL    START HEAD        END HEAD',
     3            CHDAUX,5,NAUX)
   30    CONTINUE
      END IF
C
C4------PRINT # OF SPECIFIED-HEAD CELLS THIS STRESS PERIOD
      WRITE(IOUT,1) NCHDS
    1 FORMAT(1X,//1X,I6,' TIME-VARIANT SPECIFIED-HEAD CELLS')
C
C5------SET IBOUND NEGATIVE AT SPECIFIED-HEAD CELLS.
      DO 250 II=1,NCHDS
      IL=CHDS(1,II)
      IR=CHDS(2,II)
      IC=CHDS(3,II)
      IF(IBOUND(IC,IR,IL).GT.0) IBOUND(IC,IR,IL)=-IBOUND(IC,IR,IL)
      IF(IBOUND(IC,IR,IL).EQ.0) THEN
         WRITE(IOUT,6) IL,IR,IC
    6    FORMAT(1X,'CELL (',I3,',',I5,',',I5,') IS NO FLOW (IBOUND=0)',/
     1      1X,'NO-FLOW CELLS CANNOT BE CONVERTED TO SPECIFIED HEAD')
         STOP
      END IF
  250 CONTINUE
C
C8------RETURN
      RETURN
      END
      SUBROUTINE GWF1CHD6AD(NCHDS,MXCHD,CHDS,HNEW,HOLD,PERLEN,PERTIM,
     1            NCOL,NROW,NLAY,NCHDVL,IOUT)
C
C-----VERSION 11JAN2000 GWF1CHD6FM
C     ******************************************************************
C     COMPUTE HEAD FOR TIME STEP AT EACH TIME-VARIANT SPECIFIED HEAD
C     CELL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,DZERO,HB
      DIMENSION CHDS(NCHDVL,MXCHD),HNEW(NCOL,NROW,NLAY),
     1          HOLD(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
      DZERO=0.
C
C1------IF NCHDS<=0 THEN THERE ARE NO TIME VARIANT SPECIFIED-HEAD CELLS.
C1------RETURN.
      IF(NCHDS.LE.0) RETURN
C
C6------INITIALIZE HNEW TO 0 AT SPECIFIED-HEAD CELLS.
      DO 50 L=1,NCHDS
      IL=CHDS(1,L)
      IR=CHDS(2,L)
      IC=CHDS(3,L)
      HNEW(IC,IR,IL)=DZERO
   50 CONTINUE
C
C2------COMPUTE PROPORTION OF STRESS PERIOD TO CENTER OF THIS TIME STEP
      IF (PERLEN.EQ.0.0) THEN
        FRAC=1.0
      ELSE
        FRAC=PERTIM/PERLEN
      ENDIF
C
C2------PROCESS EACH ENTRY IN THE SPECIFIED-HEAD CELL LIST (CHDS)
      DO 100 L=1,NCHDS
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY
      IL=CHDS(1,L)
      IR=CHDS(2,L)
      IC=CHDS(3,L)
C
      IF (PERLEN.EQ.0.0 .AND. CHDS(4,L).NE.CHDS(5,L)) THEN
        WRITE(IOUT,200)IL,IR,IC
 200    FORMAT(/,' ***WARNING***  FOR CHD CELL (',I3,',',I5,',',I5,
     &'), START HEAD AND END HEAD DIFFER',/,
     &' FOR A STRESS PERIOD OF ZERO LENGTH --',/,
     &' USING ENDING HEAD AS CONSTANT HEAD',
     &' (GWF1CHD6AD)',/)
      ENDIF
C5------COMPUTE HEAD AT CELL BY LINEAR INTERPOLATION.
      HB=CHDS(4,L)+(CHDS(5,L)-CHDS(4,L))*FRAC
C
C6------UPDATE THE APPROPRIATE HNEW VALUE
      HNEW(IC,IR,IL)=HNEW(IC,IR,IL)+HB
      HOLD(IC,IR,IL)=HNEW(IC,IR,IL)
  100 CONTINUE
C
C7------RETURN
      RETURN
      END
