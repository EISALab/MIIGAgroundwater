C     Last change:  ERB  10 Jul 2002   12:26 pm
      SUBROUTINE GWF1WEL6AL(ISUM,LCWELL,MXWELL,NWELLS,IN,IOUT,IWELCB,
     1        NWELVL,IWELAL,IFREFM,NPWEL,IPWBEG,NNPWEL,NOPRWL)
C
C     VERSION 11JAN2000 GWF1WEL6AL
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /WELCOM/WELAUX(5)
      CHARACTER*16 WELAUX
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE AND INITIALIZE NWELLS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'WEL6 -- WELL PACKAGE, VERSION 6, 1/11/2000',
     1' INPUT READ FROM UNIT ',I4)
      NWELLS=0
      NNPWEL=0
C
C2------READ MAXIMUM NUMBER OF WELLS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPWEL,MXPW)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTW
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
      IF(IWELCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      IWELAL=0
      NAUX=0
      NOPRWL=0
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CBCALLOCATE' .OR.
     1   LINE(ISTART:ISTOP).EQ.'CBC') THEN
         IWELAL=1
         WRITE(IOUT,11)
   11    FORMAT(1X,'MEMORY IS ALLOCATED FOR CELL-BY-CELL BUDGET TERMS')
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            WELAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) WELAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         NOPRWL = 1
         GO TO 10
      END IF
      NWELVL=4+NAUX+IWELAL
C
C4------ALLOCATE SPACE IN THE RX ARRAY FOR THE WELL DATA.
      IPWBEG=MXACTW+1
      MXWELL=MXACTW+MXPW
      ISOLD=ISUM
      LCWELL=ISUM
      ISUM=ISUM+NWELVL*MXWELL
      IF(MXACTW.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'Deactivating the Well Package because MXACTW=0')
         IN=0
      END IF
C
C5------PRINT NUMBER OF SPACES IN RX ARRAY USED BY WELL PACKAGE.
      ISP=ISUM-ISOLD
      WRITE(IOUT,14) ISP
   14 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY WEL')
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF1WEL6RQ(IN,IOUT,NWELVL,IWELAL,NCOL,NROW,NLAY,NPWEL,
     1            WELL,IPWBEG,MXWELL,IFREFM,ITERP,INAMLOC,NOPRWL)
C
C-----VERSION 20011107 GWF1WEL6RQ
C     ******************************************************************
C     READ WELL PARAMETERS
C     ******************************************************************
C     Modified 11/7/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION WELL(NWELVL,MXWELL)
      COMMON /WELCOM/WELAUX(5)
      CHARACTER*16 WELAUX
C     ------------------------------------------------------------------
C
      ITERPU=ITERP
      IF (NOPRWL.EQ.1) ITERPU=99
C-------READ NAMED PARAMETERS
      IF(ITERP.EQ.1) WRITE(IOUT,3) NPWEL
    3 FORMAT(1X,//1X,I5,' Well parameters')
      IF(NPWEL.GT.0) THEN
        NAUX=NWELVL-4-IWELAL
        LSTSUM=IPWBEG
        DO 20 K=1,NPWEL
          LSTBEG=LSTSUM
C---------READ AND STORE LIST PARAMETER DEFINITION INFORMATION
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'WEL','Q',ITERP,
     &                   NUMINST,INAMLOC)
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
C-----------READ AND PRINT A LIST OF CELLS
            CALL ULSTRD(NLST,WELL,LB,NWELVL,MXWELL,IWELAL,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,ITERPU)
            LB=LB+NLST
   10     CONTINUE
   20   CONTINUE
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF1WEL6RP(WELL,NWELLS,MXWELL,IN,IOUT,NWELVL,IWELAL,
     &                      IFREFM,NCOL,NROW,NLAY,NNPWEL,NPWEL,IPWBEG,
     &                      NOPRWL)
C
C     VERSION 11JAN2000 GWF1WEL6RP
C     ******************************************************************
C     READ NEW WELL LOCATIONS AND STRESS RATES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION WELL(NWELVL,MXWELL)
      COMMON /WELCOM/WELAUX(5)
      CHARACTER*16 WELAUX
C     ------------------------------------------------------------------
C
C1----READ NUMBER OF WELLS (OR FLAG SAYING REUSE WELL DATA).
C1----AND NUMBER OF PARAMETERS
      IF(NPWEL.GT.0) THEN
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
C------Calculate some constants.
      NAUX=NWELVL-4-IWELAL
      ITERPU = 1
      IOUTU = IOUT
      IF (NOPRWL.EQ.1) THEN
        ITERPU = 99
        IOUTU = -1
      ENDIF
C
C1A-----IF ITMP LESS THAN ZERO REUSE NON-PARAMETER DATA. PRINT MESSAGE.
C1A-----IF ITMP=>0, SET NUMBER OF NON-PARAMETER WELLS EQUAL TO ITMP.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING NON-PARAMETER WELLS FROM LAST STRESS PERIOD')
      ELSE
         NNPWEL=ITMP
      END IF
C
C1B-----IF THERE ARE NEW NON-PARAMETER WELLS, READ THEM.
      MXACTW=IPWBEG-1
      IF(ITMP.GT.0) THEN
         IF(NNPWEL.GT.MXACTW) THEN
            WRITE(IOUT,99) NNPWEL,MXACTW
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE WELLS (',I6,
     1                     ') IS GREATER THAN MXACTW(',I6,')')
            STOP
         END IF
         CALL ULSTRD(NNPWEL,WELL,1,NWELVL,MXWELL,IWELAL,IN,IOUT,
     1            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     2             WELAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,ITERPU)
      END IF
      NWELLS=NNPWEL
C
C1C-----IF THERE ARE ACTIVE WELL PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('Q')
      NREAD=NWELVL-IWELAL
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'WEL',IOUTU,'Q',WELL,NWELVL,MXWELL,NREAD,
     1                MXACTW,NWELLS,4,4,
     2            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     3            WELAUX,5,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF WELLS IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NWELLS
  101 FORMAT(1X,/1X,I6,' WELLS')
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF1WEL6FM(NWELLS,MXWELL,RHS,WELL,IBOUND,
     1        NCOL,NROW,NLAY,NWELVL)
C
C-----VERSION 11JAN2000 GWF1WEL6FM
C
C     ******************************************************************
C     SUBTRACT Q FROM RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION RHS(NCOL,NROW,NLAY),WELL(NWELVL,MXWELL),
     1            IBOUND(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C1------IF NUMBER OF WELLS <= 0 THEN RETURN.
      IF(NWELLS.LE.0) RETURN
C
C2------PROCESS EACH WELL IN THE WELL LIST.
      DO 100 L=1,NWELLS
      IR=WELL(2,L)
      IC=WELL(3,L)
      IL=WELL(1,L)
      Q=WELL(4,L)
C
C2A-----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C2B-----IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
  100 CONTINUE
C
C3------RETURN
      RETURN
      END
      SUBROUTINE GWF1WEL6BD(NWELLS,MXWELL,VBNM,VBVL,MSUM,WELL,IBOUND,
     1        DELT,NCOL,NROW,NLAY,KSTP,KPER,IWELCB,ICBCFL,BUFF,IOUT,
     2        PERTIM,TOTIM,NWELVL,IWELAL,IAUXSV)
C-----VERSION 05JUNE2000 GWF1WEL6BD
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR WELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /WELCOM/WELAUX(5)
      CHARACTER*16 WELAUX
      CHARACTER*16 VBNM(MSUM),TEXT
      DIMENSION VBVL(4,MSUM),WELL(NWELVL,MXWELL),IBOUND(NCOL,NROW,NLAY),
     1          BUFF(NCOL,NROW,NLAY)
      DOUBLE PRECISION RATIN,RATOUT,QQ
      DATA TEXT /'           WELLS'/
C     ------------------------------------------------------------------
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IWELCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IWELCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NWELVL-4-IWELAL
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,WELAUX,IWELCB,NCOL,NROW,NLAY,
     1          NWELLS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C4------IF THERE ARE NO WELLS, DO NOT ACCUMULATE FLOW.
      IF(NWELLS.EQ.0) GO TO 200
C
C5------LOOP THROUGH EACH WELL CALCULATING FLOW.
      DO 100 L=1,NWELLS
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING WELL.
      IR=WELL(2,L)
      IC=WELL(3,L)
      IL=WELL(1,L)
      Q=ZERO
C
C5B-----IF THE CELL IS NO-FLOW OR CONSTANT_HEAD, IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 99
C
C5C-----GET FLOW RATE FROM WELL LIST.
      Q=WELL(4,L)
      QQ=Q
C
C5D-----PRINT FLOW RATE IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
         WRITE(IOUT,62) L,IL,IR,IC,Q
   62    FORMAT(1X,'WELL ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   RATE ',1PG15.6)
         IBDLBL=1
      END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+Q
C
C5F-----SEE IF FLOW IS POSITIVE OR NEGATIVE.
      IF(Q) 90,99,80
C
C5G-----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
   80 RATIN=RATIN+QQ
      GO TO 99
C
C5H-----FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
   90 RATOUT=RATOUT-QQ
C
C5I-----IF CELL-BY-CELL FLOWS ARE BEING SAVED AS A LIST, WRITE FLOW.
C5I-----OR IF RETURNING THE FLOW IN THE WELL ARRAY, COPY FLOW TO WELL.
   99 IF(IBD.EQ.2) CALL UBDSVB(IWELCB,NCOL,NROW,IC,IR,IL,Q,
     1                  WELL(1,L),NWELVL,NAUX,5,IBOUND,NLAY)
      IF(IWELAL.NE.0) WELL(NWELVL,L)=Q
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IWELCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C
C9------RETURN
      RETURN
      END
