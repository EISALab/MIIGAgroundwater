C     Last change:  ERB  10 Jul 2002    9:39 am
C  GAGE5 Gaging Stations
C  1/99
C
C GAGE5AL ALLOCATE SPACE FOR GAGING STATIONS
C
C     ******************************************************************
      SUBROUTINE GAGE5AL(INGAGE,ISUMIR,LSGAGE,NUMGAGE,IOUT,IUNITSFR,
     *    IUNITLAK,LKACC7,LCSTAG,LSLAKE,ICSTRM,NSTRM,NLAKES)
C
C     ******************************************************************
      IF(INGAGE.LE.0) THEN
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
      READ(INGAGE,*) NUMGAGE
      IF(NUMGAGE.LE.0) THEN
         WRITE(IOUT,1)
    1    FORMAT(1X,' NUMGAGE=0, SO GAGE IS BEING TURNED OFF')
         INGAGE=0
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
C
C     ARRAY IS SEGMENT (or LAKE) NUMBER, REACH NUMBER, UNIT#
      LSGAGE=ISUMIR
      ISUMIR=ISUMIR+NUMGAGE*3
      ISP=NUMGAGE*3
      IF (IUNITSFR.LE.0) THEN
         ICSTRM=1
         NSTRM=1
      END IF
      IF (IUNITLAK.LE.0) THEN
         LKACC7=1
         LCSTAG=1
         LSLAKE=1
         NLAKES=1
      END IF
C
      WRITE(IOUT,101) ISP
  101 FORMAT(1X,I10,' ELEMENTS IN IX ARRAY ARE USED BY GAGE')
C
      RETURN
      END
C
C
C  GAGE5RP READ GAGING STATION INPUT FILE
C
C     ******************************************************************
C
      SUBROUTINE GAGE5RP(IGGLST,NUMGAGE,IOUT,INGAGE)
C
C     ******************************************************************
C
C     READ GAGING STATION LOCATIONS
C     ******************************************************************
C     IGGLST ARRAY IS SEGMENT (or LAKE) NUMBER, REACH NUMBER (or OUTTYPE
C     for LAKE), and UNIT#
C
      DIMENSION IGGLST(3,NUMGAGE)
C
C     ******************************************************************
C
      IF (NUMGAGE.GT.1.OR.NUMGAGE.LT.1) WRITE (IOUT,140) NUMGAGE
      IF (NUMGAGE.EQ.1) WRITE (IOUT,141) NUMGAGE
C INITIALIZE GAGE COUNTERS
         NSG=0
         NLG=0
C READ THE FIRST RECORD OF LIST
      DO 135 IOB=1,NUMGAGE
         READ(INGAGE,*) IGGLST(1,IOB)
         BACKSPACE INGAGE
         IF (IGGLST(1,IOB).GT.0) THEN
C           for stream:
            NSG=NSG+1
            READ(INGAGE,*) IGGLST(1,IOB),IGGLST(2,IOB),IGGLST(3,IOB)
         ELSE
            IF(IGGLST(1,IOB).EQ.0) THEN
               WRITE(IOUT,170)
               STOP
            ELSE
C              for lake:
               NLG=NLG+1
               READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB)
C              check for negative unit number, which designates OUTTYPE is read
               IF (IGGLST(3,IOB).LT.0) THEN
                BACKSPACE INGAGE
                READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB),IGGLST(2,IOB)
               ELSE
                 IGGLST(2,IOB)=0
               END IF
            END IF
         END IF
C
  135 CONTINUE
C
C PRINT STREAM GAGES
      IF (NSG.GT.0) THEN
        WRITE (IOUT,*) 'Stream Gages:'
        WRITE (IOUT,150)
        DO 136 IOB=1,NUMGAGE
          IF (IGGLST(1,IOB).GT.0) THEN
            WRITE(IOUT,'(4I8,13X,A40)') IOB,IGGLST(1,IOB),
     *                                 IGGLST(2,IOB),IGGLST(3,IOB)
          END IF
  136   CONTINUE
      END IF
C
C PRINT LAKE GAGES
      IF (NLG.GT.0) THEN
        WRITE (IOUT,*) 'Lake Gages:'
        WRITE (IOUT,155)
        DO 137 IOB=1,NUMGAGE
          IF (IGGLST(1,IOB).LT.0) THEN
            IF (IGGLST(3,IOB).LT.0) THEN
              WRITE(IOUT,'(4I8)') IOB,IGGLST(1,IOB),
     *                  IGGLST(3,IOB),IGGLST(2,IOB)
            ELSE
              WRITE(IOUT,'(3I8)') IOB,IGGLST(1,IOB),IGGLST(3,IOB)
            END IF
          END IF
  137   CONTINUE
      END IF
      WRITE (IOUT,180)
C
  140 FORMAT(///I4,' GAGING STATIONS WERE SPECIFIED.',/5X,'(Lakes are ',
     *'identified by a negative value of the Lake Number)',/5X,'RECORDS'
     1,' WILL BE WRITTEN TO SEPARATE OUTPUT FILES REPRESENTED BY ',
     2'FOLLOWING UNIT NUMBERS:',/)
  141 FORMAT(///I4,' GAGING STATION WAS SPECIFIED.',/5X,'(Lakes are ',
     *'identified by a negative value of the Lake Number)',/5X,'RECORDS'
     1,' WILL BE WRITTEN TO SEPARATE OUTPUT FILE REPRESENTED BY ',
     2'FOLLOWING UNIT NUMBER:')
  150 FORMAT('  GAGE #   SEGMENT   REACH   UNIT')
  155 FORMAT('  GAGE #    LAKE     UNIT   OUTTYPE')
  170 FORMAT(/'*** ERROR *** Expected non-zero value for segment no.'/
     * 25X,'EXECUTION STOPPING')
 180  FORMAT(///)
      RETURN
      END
C
C
C GAGE5I GAGING STATIONS--WRITE HEADER LINES TO OUTPUT FILES
C                       --DETERMINE & SAVE CROSS-REFERENCE INDEX
C                       --RECORD INITIAL CONDITIONS FOR LAKE GAGES
C
C     ******************************************************************
C
      SUBROUTINE GAGE5I(IGGLST,NUMGAGE,IOUT,IUNITGWT,STAGES,CLAKE,
     *                  NLAKES,ISTRM,NSTRM,DUM,NSOL,VOL)
C
C     ******************************************************************
C
      CHARACTER*1 A
      CHARACTER*2 B
      CHARACTER*7 CONCNAME
      CHARACTER*9 DCTSNAME
      CHARACTER*10 DCCMNAME
      CHARACTER*1256  LFRMAT
C TEMPORARY ARRAYS
      ALLOCATABLE CONCNAME(:),DCTSNAME(:),DCCMNAME(:),DUMMY(:,:)
      DIMENSION IGGLST(3,NUMGAGE),VOL(NLAKES)
      DIMENSION STAGES(NLAKES),CLAKE(NLAKES,NSOL),ISTRM(5,NSTRM)
C ALLOCATE TEMPORARY ARRAYS
      ALLOCATE(CONCNAME(NSOL),DCTSNAME(NSOL),DCCMNAME(NSOL),
     *DUMMY(NLAKES,NSOL))
C
C     ******************************************************************
C
      DUMMY=0.0
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG.GT.0) THEN
C---Stream gage; save stream reach index; write header lines
            IG2=IGGLST(2,IOG)
            DO 20 IRCH=1,NSTRM
               IF (ISTRM(4,IRCH).EQ.IG.AND.ISTRM(5,IRCH).EQ.IG2) THEN
C---              Convert reach no. from segment list to master list
                  IGGLST(1,IOG)=IRCH
                  GO TO 30
               END IF
 20         CONTINUE
            WRITE (IOUT,100) IOG,IG3
            GO TO 10
 30         CONTINUE
            IF (IGGLST(1,IOG).GT.0) THEN
               II=IGGLST(1,IOG)
               WRITE (IG3,200) IOG,ISTRM(1,II),ISTRM(2,II),ISTRM(3,II),
     *                         ISTRM(4,II),ISTRM(5,II)
               IF (IUNITGWT.LE.0) THEN
                  WRITE (IG3,300)
               ELSE
                  IF (NSOL.EQ.1) WRITE (IG3,310)
                  IF (NSOL.GT.1) WRITE (IG3,312) NSOL
               END IF
            END IF
         ELSE
C---Lake gage; write header lines; write initial conditions
            LK=-IG
            IF (LK.GT.NLAKES) THEN
               WRITE (IOUT,105) IOG,IG3
               GO TO 10
            ELSE
               WRITE (IG3,210) IOG,LK
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(2,IOG))
                   CASE (0)
                     WRITE (IG3,305)
                     WRITE (IG3,400) DUM,STAGES(LK),VOL(LK)
                   CASE (1)
                     WRITE (IG3,306)
                     WRITE (IG3,401) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *DUM,DUM,DUM,DUM,DUM,DUM,DUM
                   CASE (2)
                     WRITE (IG3,307)
                     WRITE (IG3,402) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *DUM
                   CASE (3)
                     WRITE (IG3,308)
                     WRITE (IG3,403) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                 END SELECT
               ELSE
C              TRANSPORT ON
C                Prepare array of header names for multiple constituents
                 DFLAG=0
                 IF(IGGLST(2,IOG).EQ.2.OR.IGGLST(2,IOG).EQ.3) DFLAG=1
                 DO 1000 ISOL=1,NSOL
                   IF (ISOL.LT.10) THEN
                     WRITE(A,'(I1)') ISOL
                     CONCNAME(ISOL)='Conc'//'_0'//A
                     IF(DFLAG.EQ.1) THEN
                       DCTSNAME(ISOL)='D-C'//'_0'//A//'-TS'
                       DCCMNAME(ISOL)='D-C'//'_0'//A//'-Cum'
                     END IF
                   ELSE IF (ISOL.GT.9.AND.ISOL.LT.100) THEN
                     WRITE(B,'(I2)') ISOL
                     CONCNAME(ISOL)='Conc'//'_'//B
                     IF(DFLAG.EQ.1) THEN
                       DCTSNAME(ISOL)='D-C'//'_'//B//'-TS'
                       DCCMNAME(ISOL)='D-C'//'_'//B//'-Cum'
                     END IF
                   ELSE
                     WRITE(IOUT,*) '***ERROR***  NSOL TOO BIG'
                     STOP
                   END IF
 1000            CONTINUE
C                GET OUTTYPE
                 SELECT CASE (IGGLST(2,IOG))
                 CASE(0)
                   WRITE (LFRMAT,315) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,425) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL)
                 CASE(1)
                   WRITE (LFRMAT,316) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,426) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                 CASE(2)
                   WRITE (LFRMAT,317) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL),
     * (DCTSNAME(ISOL),ISOL=1,NSOL),(DCCMNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,427) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL)
                 CASE(3)
                   WRITE (LFRMAT,318) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL),
     * (DCTSNAME(ISOL),ISOL=1,NSOL),(DCCMNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,428) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL)
                 END SELECT
               END IF
            END IF
         END IF
 10   CONTINUE
C
 100  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' STREAM REACH',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 105  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' LAKE',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 200  FORMAT (1X,'"GAGE No.',I3,':  K,I,J Coord. = ',I3,',',I3,',',I3,
     *   ';  STREAM SEGMENT = ',I3,';  REACH = ',I3,' "')
 210  FORMAT (1X,'"GAGE No.',I3,':  Lake No. = ',I3,' "')
 300  FORMAT (1X,'"DATA:  Time',7X,'Stage',7X,'Width',8X,'Flow"')
 305  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',4X,'Volume "')
 306  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',2X,'Volume',5X,'Precip.',
     1 5x,'Evap.',5x,'Runoff',4x,'GW-Inflw',3x,'GW-Outflw',2x,'SW-Inflw'
     2 ,3x,'SW-Outflw',x,'Withdrawal',1x,'Lake-Inflx',x,'Total-Cond "')
 307  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',2X,'Volume',
     * 5x,'Del-H-TS',3x,'Del-V-TS',2x,'Del-H-Cum',2x,'Del-V-Cum "')
 308  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',2X,'Volume',5X,'Precip.',
     1 5x,'Evap.',5x,'Runoff',4x,'GW-Inflw',3x,'GW-Outflw',2x,'SW-Inflw'
     2 ,3x,'SW-Outflw',x,'Withdrawal',1x,'Lake-Inflx',x,'Total-Cond ',
     * 2x,'Del-H-TS',3x,'Del-V-TS',2x,'Del-H-Cum',2x,'Del-V-Cum "')
 310  FORMAT (1X,'"DATA:  Time',7X,'Stage',7X,'Width',8X,'Flow',
     *           '    Concentration"')
 312  FORMAT (1X,'"DATA:  Time',7X,'Stage',8X,'Flow    Concentration ',
     *   'of ',I3,' Solutes "')
 315  FORMAT ('( 1X,''"DATA:  Time'',6X,''Stage(H)'',2X,''Volume'',2X,'
     *,I2,'A12, '' "'')')
 316  FORMAT ('( 1X,''"DATA:  Time'',6X,''Stage(H)'',2X,''Volume'',2X,'
     *,I2,'A12,5X,''Precip'',5x,''Evap.'',5x,''Runoff'',4x,''GW-Inflw'',
     *3x,''GW-Outflw'',2x,''SW-Inflw'',3x,''SW-Outflw'',x,''Withdrawal''
     *,x,''Lake-Inflx'',x,''Total-Cond "'')')
 317  FORMAT ('( 1X,''"DATA:  Time'',6X,''Stage(H)'',2X,''Volume'',2X,'
     *,I2,'A12,4x,''Del-H-TS'',4x,''Del-V-TS'',0x,',I2,'A12,3x,
     *''Del-H-Cum'',3x,''Del-V-Cum'',0x,',I2,'A12,'' "'')')
 318  FORMAT ('( 1X,''"DATA:  Time'',6X,''Stage(H)'',2X,''Volume'',2X,'
     *,I2,'A12,5X,''Precip'',5x,''Evap.'',5x,''Runoff'',4x,''GW-Inflw'',
     *3x,''GW-Outflw'',2x,''SW-Inflw'',3x,''SW-Outflw'',x,''Withdrawal''
     *,x,''Lake-Inflx'',x,''Total-Cond'',3x,''Del-H-TS'',4x,''Del-V-TS''
     *,0x,',I2,'A12,3x,
     *''Del-H-Cum'',3x,''Del-V-Cum'',0x,',I2,'A12,'' "'')')
 400  FORMAT (4X,1PE11.3,0PF11.3,1PE11.3)
 401  FORMAT (4X,1PE11.3,0PF11.3,1P11E11.3)
 402  FORMAT (4X,1PE11.3,0PF11.3,1P5E11.3)
 403  FORMAT (4X,1PE11.3,0PF11.3,1P15E11.3)
 425  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X))')
 426  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X),
     *10E11.3)')
 427  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X),
     *E11.3,X,E11.3,X,',I3,'(E11.3,1X),E11.3,X,E11.3,X,
     *',I3,'(E11.3,1X))')
 428  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X),10E11.3,
     *E11.3,X,E11.3,X,',I3,'(E11.3,1X),E11.3,X,E11.3,X,
     *',I3,'(E11.3,1X))')
C
C  RELEASE MEMORY
      DEALLOCATE(CONCNAME,DCTSNAME,DCCMNAME,DUMMY)
      RETURN
      END
C
C
C GAGE5LO Lake GAGING STATIONS--RECORD DATA (Write output to separate files)
C
C     ******************************************************************
C
      SUBROUTINE GAGE5LO(IGGLST,NUMGAGE,IUNITGWT,STGNEW,CLAKE,NLAKES,
     *                  GAGETM,NSOL,VOL,
     *                  PRECIP,EVAP,RNF,
     *                  GWIN,GWOUT,SURFIN,SURFOT,
     *                  WTHDRW,FLXINL,SUMCNN,
     *                  STGOLD2,VOLOLD,STAGES,VOLINIT,CLKOLD,CLAKINIT)
C
C     ******************************************************************
C
      CHARACTER*1256  LFRMAT
      DIMENSION IGGLST(3,NUMGAGE),VOL(NLAKES)
      DIMENSION STGNEW(NLAKES),CLAKE(NLAKES,NSOL)
      DIMENSION PRECIP(NLAKES),EVAP(NLAKES),RNF(NLAKES),
     * GWIN(NLAKES),GWOUT(NLAKES),SURFIN(NLAKES),SURFOT(NLAKES),
     * WTHDRW(NLAKES),FLXINL(NLAKES),SUMCNN(NLAKES),
     * STGOLD2(NLAKES),VOLOLD(NLAKES),STAGES(NLAKES),VOLINIT(NLAKES),
     * CLKOLD(NLAKES,NSOL),CLAKINIT(NLAKES,NSOL)
      ALLOCATABLE DELCTS(:,:),DELCCUM(:,:)
      ALLOCATE(DELCTS(NLAKES,NSOL),DELCCUM(NLAKES,NSOL))
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG1.GT.0) THEN
            GO TO 10
         ELSE
C---Lake gage: write time, stage, volume, concentration of each solute
            LK=-IG1
            IF (LK.GT.NLAKES) THEN
               GO TO 10
            ELSE
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(2,IOG))
                 CASE (0)
                   WRITE (IG3,300) GAGETM,STGNEW(LK),VOL(LK)
                 CASE (1)
                   WRITE (IG3,401) GAGETM,STGNEW(LK),VOL(LK),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK)
                 CASE (2)
                   WRITE (IG3,402) GAGETM,STGNEW(LK),VOL(LK),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK)
                 CASE (3)
                   WRITE (IG3,403) GAGETM,STGNEW(LK),VOL(LK),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK)
                 END SELECT
               ELSE
C              TRANSPORT ON
                 SELECT CASE (IGGLST(2,IOG))
                 CASE (0)
                   WRITE (LFRMAT,425) NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *                             (CLAKE(LK,ISOL),ISOL=1,NSOL)
                 CASE (1)
                   WRITE (LFRMAT,426) NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *       (CLAKE(LK,ISOL),ISOL=1,NSOL),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK)
                 CASE (2)
                   DO 744 ISOL=1,NSOL
                     DELCTS(LK,ISOL)=CLAKE(LK,ISOL)-CLKOLD(LK,ISOL)
                     DELCCUM(LK,ISOL)=CLAKE(LK,ISOL)-CLAKINIT(LK,ISOL)
  744              CONTINUE
                   WRITE (LFRMAT,427) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *       (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       (DELCTS(LK,ISOL),ISOL=1,NSOL),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK),
     *       (DELCCUM(LK,ISOL),ISOL=1,NSOL)
                 CASE (3)
                   DO 745 ISOL=1,NSOL
                     DELCTS(LK,ISOL)=CLAKE(LK,ISOL)-CLKOLD(LK,ISOL)
                     DELCCUM(LK,ISOL)=CLAKE(LK,ISOL)-CLAKINIT(LK,ISOL)
  745              CONTINUE
                   WRITE (LFRMAT,428) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *       (CLAKE(LK,ISOL),ISOL=1,NSOL),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       (DELCTS(LK,ISOL),ISOL=1,NSOL),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK),
     *       (DELCCUM(LK,ISOL),ISOL=1,NSOL)
                 END SELECT
               END IF
            END IF
         END IF
 10   CONTINUE
C
 300  FORMAT (4X,1PE11.3,0PF11.3,1PE11.3)
 401  FORMAT (4X,1PE11.3,0PF11.3,1P11E11.3)
 402  FORMAT (4X,1PE11.3,0PF11.3,1P5E11.3)
 403  FORMAT (4X,1PE11.3,0PF11.3,1P15E11.3)
 425  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X))')
 426  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X),
     *10E11.3)')
 427  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X),
     *E11.3,X,E11.3,X,',I3,'(E11.3,1X),E11.3,X,E11.3,X,
     *',I3,'(E11.3,1X))')
 428  FORMAT ('(4X,1PE11.3,0PF11.3,1PE11.3,1X,',I3,'(E11.3,1X),10E11.3,
     *E11.3,X,E11.3,X,',I3,'(E11.3,1X),E11.3,X,E11.3,X,
     *',I3,'(E11.3,1X))')
C
C  RELEASE MEMORY
      DEALLOCATE(DELCTS,DELCCUM)
      RETURN
      END
C
C
C GAGE5SO Stream GAGING STATIONS--RECORD DATA (Write output to separate files)
C
C     ******************************************************************
C
      SUBROUTINE GAGE5SO(IGGLST,NUMGAGE,IUNITGWT,STRM,
     *                  NSTRM,GAGETM,NSOL,COUT)
C
C     ******************************************************************
C
      CHARACTER*50  LFRMAT
      DIMENSION IGGLST(3,NUMGAGE)
      DIMENSION STRM(16,NSTRM),COUT(NSTRM,NSOL)
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG1.GT.0) THEN
            II=IGGLST(1,IOG)
               IF (IUNITGWT.LE.0) THEN
                  WRITE (IG3,300) GAGETM,STRM(7,II),STRM(5,II),
     *                            STRM(9,II)
               ELSE
                  WRITE (LFRMAT,435) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(7,II),STRM(5,II),
     *                          STRM(9,II),(COUT(II,ISOL),ISOL=1,NSOL)
               END IF
         ELSE
            GO TO 10
         END IF
 10   CONTINUE
C
 300  FORMAT (4X,1PE11.3,1X,3(E11.3,1X))
 435  FORMAT ('(4X,1PE11.3,1X,3(E11.3,1X),',I3,'(E11.3,1X))')
C
      RETURN
      END
