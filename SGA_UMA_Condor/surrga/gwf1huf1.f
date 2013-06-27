C     Last change:  ERB  26 Jul 2002    1:51 pm
      SUBROUTINE GWF1HUF1AL(ISUM,LCHK,LCVKA,LCSC1,
     &  IN,ITRSS,NCOL,NROW,NLAY,IOUT,IHUFCB,LCWETD,
     &  HDRY,NPER,ISSFLG,LCHGUF,IREWND,
     &  NHUF,NPHUF,LCHUFTHK,LCHKCC,ISUMI,IOHUF,LAYHDT,LCHUFTMP)
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGEOLOGIC UNIT PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER LAYHDT(NLAY),ISSFLG(NPER)
      CHARACTER*14 LAYPRN(5),TYPNAM(2),VKANAM(2),WETNAM(2),
     1            HANNAM
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
      DATA TYPNAM/'      CONFINED','   CONVERTIBLE'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      IREWND=0
      ZERO=0.
C
C1------IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'HUF1 -- HYDROGEOLOGIC-UNIT FLOW PACKAGE, '
     1' VERSION 1.04 ERA, 05/14/2002',/,' INPUT READ FROM UNIT',I3,/)
C
C2------READ FIRST RECORD AND WRITE
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHUFCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHUF,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPHUF,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOHUF,R,IOUT,IN)
      IF(IHUFCB.LT.0) WRITE(IOUT,8)
    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
     1  ' WHEN ICHUFL IS NOT 0')
      IF(IHUFCB.GT.0) WRITE(IOUT,9) IHUFCB
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
      WRITE(IOUT,11) HDRY
   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',1PG13.5)
      if(NHUF.gt.0) write(iout,12) NPHUF
   12 format(1x,'Hydrogeologic-Unit Flow Package Active with ',i3,
     1  ' parameters')
      IF(IOHUF.gt.0) write(iout,15) IOHUF
   15 FORMAT(1X,'HEADS IN HYDROGEOLOGIC UNITS WILL BE SAVED',
     1  ' ON UNIT',I3)
C
C  Allocate space in parameter-information arrays
      CALL UPARARRAL(0,IOUT,LINE,NPHUF)
C
C  Count number of transient and steady-state stress periods and set ITRSS
      ISS=0
      ITR=0
      DO 20 N=1,NPER
      IF(ISSFLG(N).NE.0) THEN
         ISS=1
      ELSE
         ITR=1
      END IF
   20 CONTINUE
      IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
         ITRSS=1
         WRITE(IOUT,3)
    3    FORMAT(1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
         ITRSS=0
         WRITE(IOUT,4)
    4    FORMAT(1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS=-1
         WRITE(IOUT,5)
    5    FORMAT(1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
C
C3------STOP THE SIMULATION IF THERE ARE MORE THAN 200 LAYERS.
   39 IF(NLAY.GT.200) THEN
         WRITE(IOUT,41)
   41    FORMAT(1X,/1X,'YOU HAVE SPECIFIED MORE THAN 200 MODEL LAYERS'/
     1 1X,'SPACE IS RESERVED FOR A MAXIMUM OF 200 LAYERS IN HUF ARRAYS')
         STOP
      END IF
C
C4------READ LTHUF, LAYWT.
      READ(IN,*) (LTHUF(K),K=1,NLAY)
      READ(IN,*) (LAYWT(K),K=1,NLAY)
C
C4A-----PRINT TABLES OF VALUES FOR LTHUF, HGUHANI, HGUVANI
C4B-----BASED ON LTHUF, HUFLAYAVG, HGUHANI, LAYWT, COUNT THE NUMBER OF EACH
C4B-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
C4B-----POINTERS IN LTHUF, HGUHANI, AND LAYWT FOR CONVENIENT ACCESS
C4B-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
      WRITE(IOUT,47)
   47 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,
     1 'LAYER     LTHUF    LAYER TYPE     LAYWT WETTABILITY',
     2 /1X,75('-'))
      NCNVRT=0
      NWETD=0
      DO 50 K=1,NLAY
        IF(LTHUF(K).NE.0) THEN
          NCNVRT=NCNVRT+1
          LTHUF(K)=NCNVRT
        END IF
        IF(LAYWT(K).NE.0) THEN
          IF(LTHUF(K).EQ.0) THEN
            WRITE(IOUT,*)
     1          ' LAYWT is not 0 and LTHUF is 0 for layer:',K
            WRITE(IOUT,*) ' LAYWT must be 0 if LTHUF is 0'
            STOP
          ELSE
            NWETD=NWETD+1
            LAYWT(K)=NWETD
          END IF
        END IF
        LAYPRN(1)=TYPNAM(1)
        IF(LTHUF(K).NE.0) LAYPRN(1)=TYPNAM(2)
        LAYPRN(5)=WETNAM(1)
        IF(LAYWT(K).NE.0) LAYPRN(5)=WETNAM(2)
        WRITE(IOUT,78) K,LTHUF(K),LAYPRN(1),LAYWT(K),LAYPRN(5)
   78   FORMAT(1X,I4,2(I10,A14))
C     SET GLOBAL HEAD-DEPENDENT THICKNESS INDICATOR
      IF (LTHUF(K).NE.0) THEN
        LAYHDT(K)=1
      ELSE
        LAYHDT(K)=0
      ENDIF
   50 CONTINUE
C
C
C5------COMPUTE THE NUMBER OF CELLS IN THE ENTIRE GRID AND IN ONE LAYER.
      NRC=NROW*NCOL
      ISIZ=NRC*NLAY
C
C6------ALLOCATE SPACE FOR ARRAYS.
      ISOLD=ISUM
      ISOLDI=ISUMI
      LCHK=ISUM
      ISUM=ISUM+ISIZ
      LCHKCC=ISUM
      ISUM=ISUM+ISIZ
      LCVKA=ISUM
      ISUM=ISUM+ISIZ
      LCSC1=ISUM
      IF(ITRSS.NE.0) ISUM=ISUM+ISIZ
      LCWETD=ISUM
      ISUM=ISUM+NRC*NWETD
      LCHGUF=ISUMI
      ISUMI=ISUMI+NHUF*5
      LCHUFTHK=ISUM
      ISUM=ISUM+NRC*NHUF*2
      LCHUFTMP=ISUM
      ISUM=ISUM+NRC*NHUF
C
C7------PRINT THE AMOUNT OF SPACE USED BY THE HUF PACKAGE.
      ISP=ISUM-ISOLD
      WRITE(IOUT,101) ISP
  101 FORMAT(1X,/1X,I10,' ELEMENTS IN X ARRAY ARE USED BY HUF')
      ISP=ISUMI-ISOLDI
      WRITE(IOUT,102) ISP
  102 FORMAT(1X,I10,' ELEMENTS IN IX ARRAY ARE USED BY HUF')
C
C8------RETURN.
      RETURN
      END
C=======================================================================
      SUBROUTINE GWF1HUF1RQ(
     1 IN,NCOL,NROW,NLAY,IOUT,WETDRY,WETFCT,IWETIT,IHDWET,
     2 IHGUFLG,ITERP,NHUF,NPHUF,HUFTHK,ITRSS)
C
C     ******************************************************************
C     READ DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*4 PTYP
      CHARACTER*10 HGUNAM,TMPNAM,CTMP1
      CHARACTER*14 LAYPRN,VKANAM(2),HANNAM
      CHARACTER*24 ANAME(8)
      CHARACTER*200 LINE
      REAL HUFTHK
      INTEGER ITRSS
C
      DIMENSION WETDRY(NCOL,NROW,NLAY),
     1  HUFTHK(NCOL,NROW,NHUF,2),IHGUFLG(5,NHUF),IFLG(5)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      COMMON /HUFCOMC/HGUNAM(200)
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        SPECIFIC STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(8) /'        WETDRY PARAMETER'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA HANNAM/'      VARIABLE'/
C     ------------------------------------------------------------------
      ZERO=0.
C
C---Read rewetting information
      IWDFLG=0
      KLAYFLG=0
      DO 10 K=1,NLAY
        IF(LAYWT(K).NE.0) IWDFLG=IWDFLG+1
        IF(LTHUF(K).NE.0) KLAYFLG=1
   10 CONTINUE
      IF(IWDFLG.EQ.0) THEN
         WRITE(IOUT,11)
   11    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
      ELSE
         WRITE(IOUT,12) IWDFLG
   12    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
         IWDFLG=1
         READ(IN,*) WETFCT,IWETIT,IHDWET
         IF(IWETIT.LE.0) IWETIT=1
         WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
         WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
         WRITE(IOUT,*) ' IHDWET=',IHDWET
      END IF
C
C2H-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
C2H-----(LAYWT NOT 0).
      DO 300 K=1,NLAY
      IF(LAYWT(K).NE.0) THEN
         CALL U2DREL(WETDRY(1,1,LAYWT(K)),ANAME(8),NROW,NCOL,K,IN,
     1            IOUT)
      END IF
  300 CONTINUE
      
      WRITE(IOUT,47)
   47 FORMAT(
     1 //1X,'HUF1 -- HYDROGEOLOGIC-UNIT FLOW PACKAGE',
     4 /1X,75('-'))
C
C-------READ HYDROGEOLOGIC-UNIT GEOMETRY
      Call GWF1HUF1GEOMRP(IN,NCOL,NROW,IOUT,NHUF,HUFTHK)
C
C---Read HANI and VANI values for each named unit
      DO 100 NU=1,NHUF
        READ(IN,'(A)') LINE
C  Get the name of the new unit
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Find the unit name in the list
        TMPNAM=LINE(ISTART:ISTOP)
        CALL UPCASE(TMPNAM)
        IF(TMPNAM.EQ.'ALL') THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HANITMP,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,VANITMP,IOUT,IN)
          DO 150 NNU=1,NHUF
            HGUHANI(NNU)=HANITMP
            HGUVANI(NNU)=VANITMP
  150     CONTINUE
          GOTO 101
        ENDIF
        IU=0
        DO 200 NNU=1,NHUF
          CTMP1=HGUNAM(NNU)
          CALL UPCASE(CTMP1)
          IF(TMPNAM.EQ.CTMP1) THEN
            IU=NNU
            WRITE(IOUT,38) TMPNAM,IU
   38       FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)      
            GO TO 201
          END IF
  200   CONTINUE
  201   CONTINUE
        IF(IU.EQ.0) THEN
          WRITE(IOUT,41) TMPNAM
   41     FORMAT('UNIT ',A10,'NOT FOUND (STOP EXECUTION)')
          STOP
        ENDIF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HGUHANI(IU),IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HGUVANI(IU),IOUT,IN)
  100 CONTINUE
  101 CONTINUE


      WRITE(IOUT,48)
   48 FORMAT(
     2 //3X,'INTERPRETATION OF UNIT FLAGS:',/1X,
     3 '    UNIT      HANI           VK/VANI',
     4 /1X,75('-'))
      DO 210 K=1,NHUF
        LAYPRN=VKANAM(1)
        IF(HGUVANI(K).NE.0) LAYPRN=VKANAM(2)
        IF(HGUHANI(K).LE.0) THEN
          WRITE(IOUT,79) HGUNAM(K),HANNAM,LAYPRN
        ELSE
          WRITE(IOUT,80) HGUNAM(K),HGUHANI(K),LAYPRN
        END IF
   79   FORMAT(1X,A10,2A14)
   80   FORMAT(1X,A10,G14.7,A14)
  210 CONTINUE

C
C-------READ NAMED PARAMETERS
      NPVK=0
      NPVANI=0
      NPSS=0
      NPSY=0
      IF(NPHUF.GT.0) THEN
         DO 20 K=1,NPHUF
         CALL GWF1HUF1PARRP(IN,IOUT,N,PTYP,ITERP,NHUF)
         IF(PTYP.EQ.'HK') THEN
            CONTINUE
         ELSE IF(PTYP.EQ.'HANI') THEN
            CONTINUE
         ELSE IF(PTYP.EQ.'VK') THEN
            NPVK=1
         ELSE IF(PTYP.EQ.'VANI') THEN
            NPVANI=1
         ELSE IF(PTYP.EQ.'SS') THEN
            NPSS=1
         ELSE IF(PTYP.EQ.'SY') THEN
            NPSY=1
         ELSE
            WRITE(IOUT,*) ' Invalid parameter type for HUF Package'
            STOP
         END IF
C  Make the parameter global
         IACTIVE(N)=-1
   20    CONTINUE
      END IF
      DO 40 K=1,NHUF
C        IF(HGUVANI(K).EQ.0.AND.NPVANI.NE.0) THEN
C          WRITE(IOUT,*) ' VANI parameters can only be used ',
C     &      'if HGUVANI(HGU) is set to 1 in the HUF Package (STOP)'
C          STOP
C        ENDIF
   40 CONTINUE
      IF(ITRSS.NE.0.AND.NPSS.EQ.0.AND.NPSY.EQ.0) THEN
        WRITE(IOUT,*) 'Simulation is transient and no storage ',
     &    'parameters are defined in the HUF Package (STOP)'
        STOP
      ENDIF
      WRITE(IOUT,*) 'ITRSS',ITRSS
      IF(ITRSS.EQ.0.AND.(NPSS.NE.0.OR.NPSY.NE.0)) THEN
        WRITE(IOUT,*) 'Simulation is steady state and storage ',
     &    'parameters are defined in the HUF Package (STOP)'
        STOP
      ENDIF
      IF(ITRSS.NE.0.AND.KLAYFLG.NE.0.AND.
     &       (NPSS.EQ.0.OR.NPSY.EQ.0)) THEN
        WRITE(IOUT,*) 'Simulation is transient and has convertible ',
     &    'layers and only one storage parameter is defined in the HUF',
     &    ' Package (STOP)'
        STOP
      ENDIF
C
C---Read PRINTCODE
      DO 390 NU=1,NHUF
        DO 390 I=1,5
          IHGUFLG(I,NU)=0
  390 CONTINUE
  399 CONTINUE
      READ(IN,'(A)',END=400) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      TMPNAM=LINE(ISTART:ISTOP)
      CALL UPCASE(TMPNAM)
      IF(TMPNAM.NE.'PRINT') GOTO 400
      WRITE(IOUT,'(/,A)') 'Reading PRINTCODE information'
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Find the unit name in the list
      TMPNAM=LINE(ISTART:ISTOP)
      CALL UPCASE(TMPNAM)
      IF(TMPNAM.EQ.'ALL') THEN
        IU=-1
      ELSE
        IU=0
        DO 410 NU=1,NHUF
          CTMP1=HGUNAM(NU)
          CALL UPCASE(CTMP1)
          IF(TMPNAM.EQ.CTMP1) THEN
            IU=NU
            WRITE(IOUT,438) TMPNAM,IU
  438       FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)      
            GO TO 411
          END IF
  410   CONTINUE
      ENDIF
  411 CONTINUE
      IF(IU.EQ.0) THEN
        WRITE(IOUT,440) TMPNAM
  440   FORMAT('UNIT ',A10,'NOT FOUND (STOP EXECUTION)')
        STOP
      ENDIF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICODE,R,IOUT,IN)
C---Reset flags
      DO 445 I=1,5
        IFLG(I)=0
  445 CONTINUE
  450 CONTINUE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      TMPNAM=LINE(ISTART:ISTOP)
      IF(TMPNAM.EQ.'') GOTO 455
      IF(TMPNAM.EQ.'ALL') THEN
        IFLG(1)=ICODE
        IFLG(2)=ICODE
        IFLG(3)=ICODE
        IF(ITRSS.NE.0) THEN
          IFLG(4)=ICODE
          IFLG(5)=ICODE
        ENDIF
        GOTO 455
      ELSEIF(TMPNAM.EQ.'HK') THEN
        IFLG(1)=ICODE
      ELSEIF(TMPNAM.EQ.'HANI') THEN
        IFLG(2)=ICODE
      ELSEIF(TMPNAM.EQ.'VK') THEN
        IFLG(3)=ICODE
      ELSEIF(TMPNAM.EQ.'SS'.AND.ITRSS.NE.0)THEN
        IFLG(4)=ICODE
      ELSEIF(TMPNAM.EQ.'SY'.AND.ITRSS.NE.0) THEN
        IFLG(5)=ICODE
      ENDIF
      GOTO 450
  455 CONTINUE
      IF(IU.EQ.-1) THEN
        DO 460 NU=1,NHUF
          DO 465 I=1,5
            IHGUFLG(I,NU)=IFLG(I)
  465     CONTINUE
  460   CONTINUE
        GOTO 400
      ELSE
        DO 470 I=1,5
          IHGUFLG(I,IU)=IFLG(I)
  470   CONTINUE
        GOTO 399
      ENDIF
  400 CONTINUE
      WRITE(IOUT,442) 
  442 FORMAT(//,'PRINTCODE FLAGS ARE SET AS FOLLOWS',/,
     &  '   UNIT       HK   HANI   VK    SS    SY',/,
     &  '------------------------------------------')
      DO 480 NU=1,NHUF
        WRITE(IOUT,444) HGUNAM(NU),(IHGUFLG(I,NU),I=1,5)
  444 FORMAT(A10,5I6)
  480 CONTINUE

C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE GWF1HUF1GEOMRP(IN,NCOL,NROW,IOUT,NHUF,HUFTHK)
C
C     ******************************************************************
C     Read and prepare HYDROGEOLOGIC-UNIT GEOMETRY.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      REAL HUFTHK
      DIMENSION HUFTHK(NCOL,NROW,NHUF,2)
      CHARACTER*200 LINE
      CHARACTER*24 ANAME
      CHARACTER*10 HGUNAM
      COMMON /HUFCOMC/HGUNAM(200)
C     ------------------------------------------------------------------
C
C-----Read the hydrogeologic-unit names and arrays
      DO 100 M=1,NHUF
C  Read a line describing a hydrogeologic unit
        READ (IN,'(A)') LINE
C  Get the name of the new unit
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Add new unit name into list
        HGUNAM(M)=LINE(ISTART:ISTOP)
C  Read top elevation of unit
        ANAME=' TOP ELEVATN: '//HGUNAM(M)
        CALL U2DREL(HUFTHK(1,1,M,1),ANAME,NROW,NCOL,0,IN,IOUT)
C  Read thickness of unit
        ANAME='   THICKNESS: '//HGUNAM(M)
        CALL U2DREL(HUFTHK(1,1,M,2),ANAME,NROW,NCOL,0,IN,IOUT)
  100 CONTINUE
C
      RETURN
      END
c======================================================================
      SUBROUTINE GWF1HUF1PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
C
C     ******************************************************************
C     Read and store array parameter definition information for HUF package
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PTYP
      CHARACTER*200 LINE
      CHARACTER*10 PN,CTMP1,CTMP2,HGUNAM
      COMMON /HUFCOMC/HGUNAM(200)
C     ------------------------------------------------------------------
C
      ILFLG=1
C  Read a parameter definition line and decode the parameter name, type,
C  and value
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      PN=LINE(ISTART:ISTOP)
      CTMP1=PN
      CALL UPCASE(CTMP1)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
C
C  Look for the parameter name in the parameter list
      DO 10 NP=1,MXPAR
          CTMP2=PARNAM(NP)
          CALL UPCASE(CTMP2)
          IF(CTMP1.EQ.CTMP2) THEN
C
C  If found, determine if it is an illegal duplicate or if it was
C  predefined.
              IF(PARTYP(NP).NE.' ' .AND. IDEFPAR.EQ.0) THEN
C  Illegal duplicate
                  WRITE(IOUT,*) ' Duplicate parameter name'
                  STOP
              END IF
C  Parameter was predefined -- leave its value alone (i.e. ignore PV).
              GO TO 100
          ELSE IF(PARNAM(NP).EQ.' ') THEN
C  Parameter was not found in the list, so it is a new definition.
C  Put values in the list.
              PARNAM(NP)=PN
              B(NP)=PV
              IPSUM=IPSUM+1
              GO TO 100
          END IF
10    CONTINUE
C  Too many parameters
      WRITE(IOUT,11)
   11 FORMAT(1X,'The number of parameters has exceeded the maximum')
      STOP
C
C  Parse the rest of the parameter definition.
  100 PARTYP(NP)=PTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NDHUF,R,IOUT,IN)
      IF(IPLOC(1,NP).EQ.0) THEN
         ICLSUM=ICLSUM+1
         IPLOC(1,NP)=ICLSUM
         ICLSUM=ICLSUM+NDHUF-1
         IPLOC(2,NP)=ICLSUM
      END IF
      IACTIVE(NP)=0
C
      IF(IPLOC(2,NP).GT.MXCLST) THEN
          WRITE(IOUT,117) IPLOC(2,NP),MXCLST
  117     FORMAT(1X,I5,
     1 ' CLUSTERS WERE SPECIFIED, BUT THERE IS SPACE FOR ONLY',I5)
           WRITE(IOUT,*) NP,NDHUF
           WRITE(IOUT,'(A)') PARNAM(NP)
           WRITE(IOUT,'(2I10)') IPLOC
          STOP
      END IF
      WRITE(IOUT,121) PARNAM(NP),PARTYP(NP),NDHUF
  121 FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A,' UNITS:',I4)
      WRITE(IOUT,122) PV
  122 FORMAT(1X,'The parameter value from the package file is:',1PG13.5)
      IF(B(NP).NE.PV) THEN
         IF(ITERP.EQ.1) THEN
            WRITE(IOUT,123) B(NP)
  123  FORMAT(1X,'This parameter value has been replaced by the',
     1  ' value from the',/1X,'Sensitivity Process file:',1PG13.5)
         ELSE
            WRITE(IOUT,124)B(NP)
  124  FORMAT(1X,'This parameter value has been replaced by the',
     1  ' value estimated by',/1X,'the Parameter Estimation Process:',
     2   1PG13.5)
         END IF
      END IF
C
C  Read clusters
      DO 200 I=IPLOC(1,NP),IPLOC(2,NP)
          READ(IN,'(A)') LINE
          LLOC=1
C
C  Find hydrogeologic-unit number
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
          PN=LINE(ISTART:ISTOP)
          CTMP1=PN
          CALL UPCASE(CTMP1)
C
C  Look for the unit name in the list of unit names
          DO 220 NU=1,NHUF
            CTMP2=HGUNAM(NU)
            CALL UPCASE(CTMP2)
            IF(CTMP1.EQ.CTMP2) THEN
              IPCLST(1,I)=NU
              WRITE(IOUT,38) CTMP1,NU
   38         FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)      
              GO TO 221
            END IF
  220     CONTINUE
  221     CONTINUE
c
c Parse multiplication and zone array information
      CALL URWORD(LINE,LLOC,IM1,IM2,0,N,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,IZ1,IZ2,0,N,R,IOUT,IN)
      DO 30 J=5,14
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(J,I),R,-1,IN)
      IF(IPCLST(J,I).EQ.0) THEN
         IPCLST(4,I)=J-1
         GO TO 32
      END IF
   30 CONTINUE
      IPCLST(4,I)=14
   32 IF(ILFLG.NE.0) THEN
         WRITE(IOUT,36) IPCLST(1,I),LINE(IM1:IM2),LINE(IZ1:IZ2)
   36    FORMAT(1X,'               LAYER:',I3,'   MULTIPLIER:',A,
     2      '   ZONE:',A)
      ELSE
         WRITE(IOUT,37) LINE(IM1:IM2),LINE(IZ1:IZ2)
   37    FORMAT(1X,'               MULTIPLIER:',A,'   ZONE:',A)
      END IF
C
C  Find the multiplier array number
      CTMP1=LINE(IM1:IM2)
      CALL UPCASE(CTMP1)
      IF(CTMP1.EQ.'NONE') THEN
         IPCLST(2,I)=0
      ELSE
         DO 40 J=1,MXMLT
         CTMP2=MLTNAM(J)
         CALL UPCASE(CTMP2)
         IF(CTMP1.EQ.CTMP2) GO TO 45
   40    CONTINUE
         WRITE(IOUT,'(A)') ' Multiplier array has not been defined'
         STOP
   45    IPCLST(2,I)=J
      END IF
C
C  Find the zone array number
      CTMP1=LINE(IZ1:IZ2)
      CALL UPCASE(CTMP1)
      IF(CTMP1.EQ.'ALL') THEN
         IPCLST(3,I)=0
      ELSE
         IF(IPCLST(4,I).EQ.4) THEN
            WRITE(IOUT,47)
   47       FORMAT(
     1      1X,'There were no zone values specified in the cluster',/
     2      1X,'At least one zone must be specified')
            STOP
         END IF
         WRITE(IOUT,48) (IPCLST(J,I),J=5,IPCLST(4,I))
   48    FORMAT(1X,'               ZONE VALUES:',10I5)
         DO 50 J=1,MXZON
         CTMP2=ZONNAM(J)
         CALL UPCASE(CTMP2)
         IF(CTMP1.EQ.CTMP2) GO TO 55
   50    CONTINUE
         WRITE(IOUT,'(A)') ' Zone array has not been defined'
         STOP
   55    IPCLST(3,I)=J
      END IF
  200 CONTINUE
C
      RETURN
      END
c======================================================================
      SUBROUTINE GWF1HUF1SP(
     1 IBOUND,HNEW,CR,CC,CV,DELR,DELC,BOTM,HK,VKA,SC1,
     2 ITRSS,NCOL,NROW,NLAY,IOUT,WETDRY,NHUF,NBOTM,RMLT,IZON,NMLTAR,
     3 NZONAR,HUFTHK,HKCC,HDRY,KITER,KSTP,KPER,IHGUFLG,HUFTMP,IWETIT,
     4 IHDWET,WETFCT)
C
C     ******************************************************************
C     SUBSTITUTE AND PREPARE DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
      CHARACTER*4 PTYPE(6)
      CHARACTER*10 HGUNAM
      CHARACTER*24 ANAME(6)
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),
     1    CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     2    CV(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     3    BOTM(NCOL,NROW,0:NBOTM),HK(NCOL,NROW,NLAY),
     4    VKA(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),
     6    WETDRY(NCOL,NROW,NLAY),RMLT(NCOL,NROW,NMLTAR),
     7    IZON(NCOL,NROW,NZONAR),HUFTHK(NCOL,NROW,NHUF,2),
     8    HKCC(NCOL,NROW,NLAY),HUFHK(200),HUFHANI(200),
     9    HUFVK(200),HUFSS(200),IHGUFLG(5,NHUF),HUFSY(200),
     &    HUFTMP(NCOL,NROW,NHUF)
C
      INCLUDE 'param.inc'
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      COMMON /HUFCOMC/HGUNAM(200)
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /'        SPECIFIC STORAGE'/
      DATA ANAME(5) /'          SPECIFIC YIELD'/
      DATA ANAME(6) /' HORIZ. TO VERTICAL ANI.'/
      DATA PTYPE(1) /'HK'/
      DATA PTYPE(2) /'HANI'/
      DATA PTYPE(3) /'VK'/
      DATA PTYPE(4) /'SS'/
      DATA PTYPE(5) /'SY'/
      DATA PTYPE(6) /'VANI'/

C     ------------------------------------------------------------------
C
C Check for cells that GO DRY/REWET
      DO 5 K=1,NLAY
        CALL SGWF1HUF1WETCHK(HNEW,IBOUND,CR,CC,HK,DELR,DELC,BOTM,
     1   NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,
     2   WETFCT,IWETIT,IHDWET,HDRY,HKCC)
    5 CONTINUE
C Zero out arrays
      DO 30 J=1,NCOL
        DO 20 I=1,NROW
          DO 10 K=1,NLAY
            HK(J,I,K)=0.
            HKCC(J,I,K)=0.
            VKA(J,I,K)=0.
            IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &         KSTP.EQ.0) SC1(J,I,K)=0.
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C2------DEFINE DATA FOR NAMED PARAMETERS.
C
C Loop through rows and columns
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C Zero out arrays
        DO 110 NU=1,NHUF
          HUFHK(NU)=0.
          HUFVK(NU)=0.
          HUFHANI(NU)=0.
          HUFSS(NU)=0.
          HUFSY(NU)=0.
  110   CONTINUE
C
C---Populate HGU arrays depending on parameter type
        CALL SGWF1HUF1POP(HUFHK,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL SGWF1HUF1POP(HUFHANI,'HANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL SGWF1HUF1POP(HUFVK,'VK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL SGWF1HUF1POP(HUFVK,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        IF(ITRSS.NE.0) THEN
          CALL SGWF1HUF1POP(HUFSS,'SS  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
          CALL SGWF1HUF1POP(HUFSY,'SY  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        ENDIF
C
C---Populate HANI and VANI from input file if not already defined by
c     a parameter
        DO 120 NU=1,NHUF
          IF(HGUVANI(NU).GT.0..AND.HUFVK(NU).EQ.0.)
     &          HUFVK(NU)=HGUVANI(NU)
          IF(HGUHANI(NU).GT.0..AND.HUFHANI(NU).EQ.0.) 
     &          HUFHANI(NU)=HGUHANI(NU)
  120   CONTINUE
C
C---Populate MODEL arrays
        DO 130 NU=1,NHUF
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(ABS(THCKU).LT.1E-4) GOTO 130
          BOTU=TOPU-THCKU
C-----Determine which layer(s) unit applies to
          CALL SGWF1HUF1HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     2                        HNEW,IBOUND,KT,KB,IFLG)
C-----Skip unit if thickness is zero
          IF(IFLG.EQ.1) GOTO 130
C-----Populate arrays
          CALL SGWF1HUF1HK(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,
     2                     KB,HK,HKCC,HUFHK,HUFHANI,NHUF,NU,HNEW)
C
          CALL SGWF1HUF1VKA(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     2                      VKA,HNEW,IBOUND,HUFHK,HUFVK,NHUF,NU)
          IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &       KSTP.EQ.0) THEN
            TOPU = HUFTHK(J,I,NU,1)
            BOTU = TOPU - THCKU
            CALL SGWF1HUF1SC1(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,
     &                        BOTU,SC1,HUFSS,KT,KB,NHUF,NU)
          ENDIF
  130   CONTINUE
  100 CONTINUE
C
      IF(KITER.EQ.0) THEN
C-----Print unit arrays depending on flags
        DO 200 NU=1,NHUF
          DO 210 IP=1,5
            IF(IHGUFLG(IP,NU).GT.0) THEN
              IF(IP.EQ.3.AND.HGUVANI(NU).GT.0) THEN
                IA=6
              ELSE
                IA=IP
              ENDIF
C-----Populate HUFHK as a temporary holding array
              DO 220 J=1,NCOL
                DO 220 I=1,NROW
                  HUFHK(NU)=0
                  CALL SGWF1HUF1POP(HUFHK,PTYPE(IP),NCOL,NROW,NHUF,I,J,
     &                   HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
                  IF(IA.EQ.6)
     &            CALL SGWF1HUF1POP(HUFHK,PTYPE(IA),NCOL,NROW,NHUF,I,J,
     &                   HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
C-----Transfer to HUFTMP
                  HUFTMP(J,I,NU)=HUFHK(NU)
                  IF(IA.EQ.6.AND.HUFHK(NU).EQ.0.)
     &              HUFTMP(J,I,NU)=HGUVANI(NU)
                  IF(IA.EQ.2.AND.HGUHANI(NU).GT.0..AND.HUFHK(NU).EQ.0.) 
     &              HUFTMP(J,I,NU)=HGUHANI(NU)
  220         CONTINUE
C-----Print HUFTMP
              WRITE(IOUT,305) ANAME(IA),HGUNAM(NU)
  305         FORMAT(//A24,' FOR UNIT ',A10)
              CALL ULAPRWC(HUFTMP(1,1,NU),NCOL,NROW,0,IOUT,
     &                     IHGUFLG(IP,NU),ANAME(IA))
            ENDIF
  210     CONTINUE
  200   CONTINUE
      ENDIF
C
C3------PREPARE AND CHECK HUF DATA.
      CALL SGWF1HUF1N(HNEW,IBOUND,CR,CC,CV,HK,VKA,DELR,
     1       DELC,NCOL,NROW,NLAY,IOUT,WETDRY,BOTM,NBOTM,SC1,ITRSS,HKCC,
     2       HDRY,KITER,KSTP,KPER,IHDWET,WETFCT)
C
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1POP(
     1  HUFARRAY,PTYPE,NCOL,NROW,NHUF,I,J,HUFTHK,IZON,NZONAR,RMLT,
     2  NMLTAR,NNU,IOUT)
C
C     ******************************************************************
C     Populate HUF arrays.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*4 PTYPE
      DIMENSION HUFARRAY(200),HUFTHK(NCOL,NROW,NHUF,2),
     1    IZON(NCOL,NROW,NZONAR),RMLT(NCOL,NROW,NMLTAR)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
C Loop through parameters
      DO 250 NP=1,MXPAR
        IF(PARTYP(NP).EQ.PTYPE) THEN
C Loop through units that apply to this parameter
          DO 300 ND=IPLOC(1,NP),IPLOC(2,NP)
            NU=IPCLST(1,ND)
            IF(NNU.GT.0.AND.NNU.NE.NU) GOTO 300
            NM=IPCLST(2,ND)
            NZ=IPCLST(3,ND)
C
C First, skip this unit if thickness if zero
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            BOTU=TOPU-THCKU
            RMLT0=1.
            IF(NZ.GT.0) THEN
              RMLT0=0.
              DO 310 JJ=5,IPCLST(4,ND)
                IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ND)) THEN
                  IF(NM.GT.0) THEN
                    RMLT0=RMLT(J,I,NM)
                  ELSE
                    RMLT0=1.
                  ENDIF
                END IF
  310         CONTINUE
            ELSEIF(NM.GT.0) THEN
              RMLT0=RMLT(J,I,NM)
            ENDIF
            THCKU=RMLT0*THCKU
            IF(THCKU.LE.0) GOTO 300
C
C---Populate HUF array
          IF(PTYPE.EQ.'VANI'.AND.RMLT0.NE.0..AND.HUFARRAY(NU).NE.0) THEN
            WRITE(IOUT,100)
  100       FORMAT(//,'Additive VANI parameters not allowed! ',
     &       'STOP EXECUTION(SGWF1HUF1POP)')
            STOP
          ENDIF
          HUFARRAY(NU)=HUFARRAY(NU)+RMLT0*B(NP)
  300     CONTINUE
        ENDIF
  250 CONTINUE
C
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1HK(
     1  NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,KB,HK,
     2  HKCC,HUFHK,HUFHANI,NHUF,NU,HNEW)
C
C     ******************************************************************
C     Substitute for HK parameters.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HNEW
      DIMENSION HK(NCOL,NROW,NLAY),HKCC(NCOL,NROW,NLAY),
     &  HUFHK(NHUF),BOTM(NCOL,NROW,0:NBOTM),HUFHANI(NHUF),
     &  HNEW(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)

      IF(KT.EQ.KB) THEN
        HKCR=HUFHK(NU)*(TOPU-BOTU)
        HK(J,I,KT)=HK(J,I,KT)+HKCR
        HKCC(J,I,KT)=HKCC(J,I,KT)+HUFHANI(NU)*HKCR
      ELSE
        DO 300 KL=KT,KB
          BOTL = BOTM(J,I,LBOTM(KL))
          TOPL = BOTM(J,I,LBOTM(KL)-1)
C---Adjust top elevation for water-table layers
          IF(LTHUF(KL).NE.0.AND.HNEW(J,I,KL).LT.TOPL)
     &       TOPL=HNEW(J,I,KL)
          IF(KL.EQ.KT) THEN
            THCK=TOPU-BOTL
          ELSEIF(KL.EQ.KB) THEN
            THCK=TOPL-BOTU
          ELSE
            THCK=TOPL-BOTL
          ENDIF
C---Check for small or negative thickness
          IF(THCK.LT.1.E-4) GOTO 300
          HKCR=HUFHK(NU)*THCK
          HK(J,I,KL)=HK(J,I,KL)+HKCR
          HKCC(J,I,KL)=HKCC(J,I,KL)+HUFHANI(NU)*HKCR
  300   CONTINUE
      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1VKA(
     1 NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,VKA,HNEW,IBOUND,HUFHK,
     2 HUFVK,NHUF,NU)
C
C     ******************************************************************
C     Substitute for VKA parameters.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HNEW
      REAL TOP1,TOP2,TOP3,RMID1,RMID2
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     1  VKA(NCOL,NROW,NLAY),HUFHK(NHUF),HUFVK(NHUF),
     2  IBOUND(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)

      CALL SGWF1HUF1VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     2  HNEW,IBOUND,KT,KB,IFLG)

      IF(IFLG.EQ.1) RETURN

      IF(KB.EQ.NLAY) KB=KB-1
        DO 300 KL=KT,KB
          IF(IBOUND(J,I,KL).EQ.0 .OR. IBOUND(J,I,KL+1).EQ.0) GOTO 300
          TOP1=BOTM(J,I,LBOTM(KL)-1)
          TOP2=BOTM(J,I,LBOTM(KL))
          TOP3=BOTM(J,I,LBOTM(KL)+1)
C---Adjust top elevation for water-table layers
          IF(LTHUF(KL).NE.0.AND.HNEW(J,I,KL).LT.TOP1)
     &       TOP1=HNEW(J,I,KL)
          RMID1=(TOP1+TOP2)/2.
          RMID2=(TOP2+TOP3)/2.
C---If layer below is unconfined, it does not contribute
          IF(LTHUF(KL+1).NE.0.AND.HNEW(J,I,KL+1).LT.TOP2)
     &       RMID2=TOP2
          IF(RMID1.GT.TOPU) RMID1=TOPU
          IF(BOTU.GT.RMID2) RMID2=BOTU
          THCK=RMID1-RMID2
C---Check to see if negative thickness
          IF(THCK.LE.0.0) GOTO 300
          IF(HGUVANI(NU).EQ.0.) THEN
            VKA(J,I,KL)=VKA(J,I,KL)+THCK/HUFVK(NU)
          ELSE
            VKA(J,I,KL)=VKA(J,I,KL)+THCK*HUFVK(NU)/HUFHK(NU)
          ENDIF
  300   CONTINUE
C      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     2  HNEW,IBOUND,KT,KB,IFLG)
C
C     ******************************************************************
C     Search for top and bottom layer the unit applies to.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     1  IBOUND(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
C Reset IFLG
      IFLG=1
C
C Loop through layers to determine where unit applies
C
C First, search for top
      DO 100 KT=1,NLAY-1
        IF(IBOUND(J,I,KT).EQ.0.OR.IBOUND(J,I,KT+1).EQ.0) GOTO 100
        TOP1=BOTM(J,I,LBOTM(KT)-1)
C---Adjust top of model layer for unconfined layer
        IF(LTHUF(KT).NE.0.AND.HNEW(J,I,KT).LT.TOP1) TOP1=HNEW(J,I,KT)
        TOP2=BOTM(J,I,LBOTM(KT))
        TOP3=BOTM(J,I,LBOTM(KT)+1)
        RMID1=(TOP1+TOP2)/2.
        RMID2=(TOP2+TOP3)/2.
C---If unit bottom is above the middle of the top layer, return
C        IF(KT.EQ.1.AND.TOPU.GT.RMID1.AND.BOTU.GE.RMID1) THEN
        IF(TOPU.GT.RMID1.AND.BOTU.GE.RMID1) THEN
          RETURN
        ENDIF
C---If unit top is above middle of top layer, exit loop
C        IF(KT.EQ.1.AND.TOPU.GT.RMID1.AND.BOTU.LT.RMID1) GOTO 110
        IF(TOPU.GT.RMID1.AND.BOTU.LT.RMID1) GOTO 110
C---If unit top is between the middle of this layer and the middle of the
C     next layer, exit loop
        IF(TOPU.LE.RMID1.AND.TOPU.GE.RMID2) GOTO 110
  100 CONTINUE
      RETURN
  110 CONTINUE
C Now search for bottom
      DO 200 KKB=KT,NLAY-1
        TOP1=BOTM(J,I,LBOTM(KKB)-1)
C---Adjust top of model layer for unconfined layer
        IF(LTHUF(KKB).NE.0.AND.HNEW(J,I,KKB).LT.TOP1) TOP1=HNEW(J,I,KKB)
        TOP2=BOTM(J,I,LBOTM(KKB))
        TOP3=BOTM(J,I,LBOTM(KKB)+1)
        RMID1=(TOP1+TOP2)/2.
        RMID2=(TOP2+TOP3)/2.
C---If unit bottom is between the middle of this layer and the middle of
C     the next layer, return
        IF(BOTU.LE.RMID1.AND.BOTU.GE.RMID2) THEN
          KB=KKB
          IFLG=0
          RETURN
        ENDIF
C---If entire unit is below the middle of the bottom layer, return
        IF(KKB.EQ.NLAY-1.AND.TOPU.LE.RMID2.AND.BOTU.LT.RMID2) THEN
          RETURN
        ENDIF
C---If unit bottom is below the middle of the bottom layer, return
        IF(KKB.EQ.NLAY-1.AND.BOTU.LT.RMID2) THEN
          KB=NLAY
          IFLG=0
          RETURN
        ENDIF
  200 CONTINUE
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF1HUF1HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     2  HNEW,IBOUND,KT,KB,IFLG)
C
C     ******************************************************************
C     Search for top and bottom layer the unit applies to.
C     Values for IFLG:
C       IFLG = 0, Unit successfully found
C       IFLG = 1, Unit not found
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     1  IBOUND(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
C Reset IFLG
      IFLG=1
C
C Loop through layers to determine where unit applies
C
C First, search for top
      DO 100 KT=1,NLAY
        IF(IBOUND(J,I,KT).EQ.0) GOTO 100
        TOP=BOTM(J,I,LBOTM(KT)-1)
C---Adjust top of model layer for unconfined layer
        IF(LTHUF(KT).NE.0.AND.HNEW(J,I,KT).LT.TOP) TOP=HNEW(J,I,KT)
C---If unit top is in this layer, exit loop
        IF(TOPU.LE.TOP.AND.TOPU.GT.BOTM(J,I,LBOTM(KT))) GOTO 110
C---If unit top is above model top, adjust unit top elevation, exit loop
        IF(TOPU.GT.TOP) THEN
          TOPU=TOP
          GOTO 110
        ENDIF
  100 CONTINUE
      RETURN
  110 CONTINUE
C---If unit top has been adjusted to below unit bottom, return
      IF(TOPU.LE.BOTU) THEN
        RETURN
      ENDIF
C
C Now search for bottom
      DO 200 KKB=KT,NLAY
        IF(IBOUND(J,I,KKB).EQ.0) GOTO 200
        TOP=BOTM(J,I,LBOTM(KKB)-1)
C---Adjust top of model layer for unconfined layer
        IF(LTHUF(KKB).NE.0.AND.HNEW(J,I,KKB).LT.TOP) 
     &    TOP=HNEW(J,I,KKB)
C---If unit bottom is in this layer, set KB=KKB, return
        IF(BOTU.LE.TOP.AND.BOTU.GE.BOTM(J,I,LBOTM(KKB))) THEN
          KB=KKB
          IFLG=0
          RETURN
        ENDIF
C---If top of model layer has been adjusted for unconfined unit and
C    unit bottom is in the gap, set KB=KKB-1, return
        IF(BOTU.GT.TOP) THEN
          KB=KKB-1
          BOTU=BOTM(J,I,LBOTM(KKB)-1)
          IFLG=0
          RETURN
        ENDIF
C---If unit bottom is below model bottom, set KB=NLAY, return
        IF(KKB.EQ.NLAY.AND.BOTU.LT.BOTM(J,I,LBOTM(KKB))) THEN
          KB=KKB
          BOTU=BOTM(J,I,LBOTM(KKB))
          IFLG=0
          RETURN
        ENDIF
  200 CONTINUE
C---If KB has not been found due to inactive cells, search again from bottom
      DO 300 KKB=NLAY,1,-1
        IF(IBOUND(J,I,KKB).EQ.0) GOTO 300
        KB = KKB
        IFLG = 0
        BOTU = BOTM(J,I,LBOTM(KKB))
        RETURN
  300 CONTINUE
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF1HUF1SC1(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        SC1,HUFSS,KT,KB,NHUF,NU)
C
C     ******************************************************************
C     Substitute for SS parameters.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),HUFSS(NHUF),
     1  SC1(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)

      IF(KT.EQ.KB) THEN
        SC1(J,I,KT)=SC1(J,I,KT)+HUFSS(NU)*(TOPU-BOTU)
      ELSE
        DO 300 KL=KT,KB
          IF(KL.EQ.KT) THEN
            THCK=TOPU-BOTM(J,I,LBOTM(KL))
          ELSEIF(KL.EQ.KB) THEN
            THCK=BOTM(J,I,LBOTM(KL)-1)-BOTU
          ELSE
            THCK=BOTM(J,I,LBOTM(KL)-1)-BOTM(J,I,LBOTM(KL))
          ENDIF
          IF(ABS(THCK).LT.1.E-4) GOTO 300
          SC1(J,I,KL)=SC1(J,I,KL)+HUFSS(NU)*THCK
  300   CONTINUE
      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1N(HNEW,IBOUND,CR,CC,CV,HK,VKA,DELR,DELC,NCOL,
     1  NROW,NLAY,IOUT,WETDRY,BOTM,NBOTM,SC1,ITRSS,HKCC,HDRY,KITER,KSTP,
     2  KPER,IHDWET,WETFCT)
C
C     ******************************************************************
C     INITIALIZE AND CHECK HUF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION HNEW,HCNV
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     1    CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     2    CV(NCOL,NROW,NLAY),HK(NCOL,NROW,NLAY),
     3    VKA(NCOL,NROW,NLAY),
     4    DELR(NCOL),DELC(NROW),WETDRY(NCOL,NROW,NLAY),
     5    BOTM(NCOL,NROW,0:NBOTM), SC1(NCOL,NROW,NLAY),
     6    HKCC(NCOL,NROW,NLAY)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
      ZERO=0.
      HCNV=888.88
C
C-------CONVERT CELL TO NO FLOW IF CELL THICKNESS IS 0.
      DO 30 K=1,NLAY
      KB=LBOTM(K)
      KT=KB-1
      DO 30 I=1,NROW
      DO 30 J=1,NCOL
      IF(IBOUND(J,I,K).GT.0) THEN
         THICK=BOTM(J,I,KT)-BOTM(J,I,KB)
         IF(THICK.LE.ZERO) THEN
            IBOUND(J,I,K)=0
            HNEW(J,I,K)=HCNV
            IF(LAYWT(K).NE.0) WETDRY(J,I,LAYWT(K))=ZERO
            WRITE(IOUT,25) K,I,J
   25       FORMAT(1X,
     1  'Converting cell to no flow due to 0 thickness (Layer,row,col)',
     2     I4,',',I4,',',I4)
         END IF
      END IF
   30 CONTINUE
C
C1------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO
C1------TRANSMISSIVE PARAMETER.
      DO 60 K=1,NLAY
      IF(LAYWT(K).NE.0) THEN
C
C2------WETTING IS ACTIVE.
         DO 40 I=1,NROW
         DO 40 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0) GO TO 40
C
C2A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
         IF(HK(J,I,K).NE.ZERO) GO TO 40
C
C2C-----ALL TRANSMISSIVE TERMS ARE ALL 0, SO CONVERT CELL TO NO FLOW.
         IBOUND(J,I,K)=0
         HNEW(J,I,K)=HCNV
         WETDRY(J,I,LAYWT(K))=ZERO
         WRITE(IOUT,43) K,I,J
   40    CONTINUE
C
      ELSE
C
C3------WETTING IS INACTIVE
         DO 50 I=1,NROW
         DO 50 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0) GO TO 50
C
C3A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
         IF(HK(J,I,K).NE.ZERO) GO TO 50
C
C3B-----CHECK VERTICAL HYDRAULIC CONDUCTIVITY AND CONFINING BED
C3B-----VERTICAL HYDRAULIC CONDUCTIVITY.
         IF(NLAY.GT.1) THEN
            IF(VKA(J,I,K).NE.ZERO) GOTO 50
         END IF
C
C3C-----ALL TRANSMISSIVE TERMS ARE 0, SO CONVERT CELL TO NO FLOW.
         IBOUND(J,I,K)=0
         HNEW(J,I,K)=HCNV
         WRITE(IOUT,43) K,I,J
   43    FORMAT(1X,'NODE (LAYER,ROW,COL)',3I4,
     1 ' ELIMINATED BECAUSE ALL HYDRAULIC CONDUCTIVITIES TO NODE ARE 0')
   50    CONTINUE
      END IF
   60 CONTINUE
C

C
C4------CALCULATE HOR. CONDUCTANCE(CR AND CC) FOR CONSTANT T LAYERS.
      DO 70 K=1,NLAY
C-------CONVERT HK TO HYDRAULIC CONDUCTVITY AND SC1 TO SPECIFIC STORAGE
        DO 71 I=1,NROW
        DO 71 J=1,NCOL
          IF(IBOUND(J,I,K).NE.0) THEN
              TOP=BOTM(J,I,LBOTM(K)-1)
              BOT=BOTM(J,I,LBOTM(K))
              IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &           KSTP.EQ.0) SC1(J,I,K)=SC1(J,I,K)*DELR(J)*DELC(I)
              IF(LTHUF(K).NE.0.AND.HNEW(J,I,K).LT.TOP) 
     &            TOP=HNEW(J,I,K)
              HK(J,I,K)=HK(J,I,K)/(TOP-BOT)
              HKCC(J,I,K)=HKCC(J,I,K)/(TOP-BOT)
          ENDIF
   71   CONTINUE
      KK=K
C
C6------COMPUTE HORIZONTAL BRANCH CONDUCTANCES FROM CELL HYDRAULIC
C6------CONDUCTIVITY, SATURATED THICKNESS, AND GRID DIMENSIONS.
         CALL SGWF1HUF1HHARM(CR,CC,HK,IBOUND,DELR,DELC,K,NCOL,NROW,
     1         NLAY,HKCC)
   70 CONTINUE
C
C5------CALCULATE VERTICAL CONDUCTANCE
      IF(NLAY.GT.1) THEN
        DO 10 K=1,NLAY-1
        DO 11 I=1,NROW
        DO 11 J=1,NCOL
          IF(IBOUND(J,I,K).NE.0.AND.VKA(J,I,K).GT.0)  THEN
            CV(J,I,K)=DELR(J)*DELC(I)/VKA(J,I,K)
          ELSE
            CV(J,I,K)=0.
          ENDIF
   11    CONTINUE
   10    CONTINUE
      END IF
C
C7------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1WETCHK(HNEW,IBOUND,CR,CC,HK,DELR,DELC,BOTM,
     1 NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,
     2 WETFCT,IWETIT,IHDWET,HDRY,HKCC)
C     ******************************************************************
C     CHECK FOR CELLS THAT GO DRY/REWET
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,HHD,BBOT,TTOP
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     1 CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY), HK(NCOL,NROW,NLAY),
     2 DELR(NCOL), DELC(NROW),
     3 BOTM(NCOL,NROW,0:NBOTM),WETDRY(NCOL,NROW,NLAY),
     4 HKCC(NCOL,NROW,NLAY)
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
C1------INITIALIZE DATA.
      ZERO=0.
      NCNVRT=0
      IHDCNV=0
C
C2------IF LAYER IS WETTABLE CONVERT DRY CELLS TO WET WHEN APPROPRIATE.
      ITFLG=1
      IF(LAYWT(K).NE.0) ITFLG=MOD(KITER,IWETIT)
      IF(ITFLG.EQ.0) CALL SGWF1HUF1WET(HNEW,IBOUND,BOTM,NBOTM,K,KITER,
     1      KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,WETFCT,IHDWET,
     2      IHDCNV,NCNVRT,ICNVRT,JCNVRT,ACNVRT)
C
C3------LOOP THROUGH EACH CELL, AND CALCULATE SATURATED THICKNESS.
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C3A-----SET STAURATED THICKNESS=0. FOR DRY CELLS.
      IF(IBOUND(J,I,K).EQ.0) THEN
         CC(J,I,K)=ZERO
      ELSE
C
C3B-----CALCULATE SATURATED THICKNESS FOR A WET CELL.
         BBOT=BOTM(J,I,LBOTM(K))
         TTOP=BOTM(J,I,LBOTM(K)-1)
         IF(LTHUF(K).NE.0) THEN
            HHD=HNEW(J,I,K)
            IF(HHD.LT.TTOP) TTOP=HHD
         END IF
         THCK=TTOP-BBOT
         CC(J,I,K)=THCK
C
C
C3C-----WHEN SATURATED THICKNESS <= 0, PRINT A MESSAGE AND SET
C3C-----HNEW=HDRY, SATURATED THICKNESS=0.0, AND IBOUND=0.
         IF(THCK.LE.ZERO) THEN
            CALL SGWF1HUF1WDMSG(1,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     1             IOUT,KITER,J,I,K,KSTP,KPER)
            HNEW(J,I,K)=HDRY
            CC(J,I,K)=ZERO
            IF(IBOUND(J,I,K).LT.0) THEN
               WRITE(IOUT,151)
  151          FORMAT(1X,/1X,'CONSTANT-HEAD CELL WENT DRY',
     1          ' -- SIMULATION ABORTED')
                    write(IOUT,*) TTOP, BBOT, THCK
               WRITE(IOUT,152) K,I,J,KITER,KSTP,KPER
  152          FORMAT(1X,'LAYER=',I2,'   ROW=',I3,'   COLUMN=',I3,
     1    '   ITERATION=',I3,'   TIME STEP=',I3,'   STRESS PERIOD=',I3)
               STOP
            END IF
            IBOUND(J,I,K)=0
         END IF
      END IF
  200 CONTINUE
C
C4------PRINT ANY REMAINING CELL CONVERSIONS NOT YET PRINTED.
      CALL SGWF1HUF1WDMSG(0,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     1             IOUT,KITER,J,I,K,KSTP,KPER)
C
C5------CHANGE IBOUND VALUE FOR CELLS THAT CONVERTED TO WET THIS
C5------ITERATION FROM 30000 to 1.
      IF(LAYWT(K).NE.0) THEN
         DO 205 I=1,NROW
         DO 205 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.30000) IBOUND(J,I,K)=1
  205    CONTINUE
      END IF
C
C7------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE GWF1HUF1AD(IBOUND,HOLD,BOTM,WETDRY,ISS,NCOL,NROW,NLAY,
     1            NBOTM)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     SET HOLD TO BOTM WHENEVER A WETTABLE CELL IS DRY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),HOLD(NCOL,NROW,NLAY),
     1          BOTM(NCOL,NROW,0:NBOTM),WETDRY(NCOL,NROW,NLAY)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
C
C1------RETURN IF STEADY STATE.
      IF(ISS.NE.0) RETURN
C
C2------LOOP THROUGH ALL LAYERS TO SET HOLD=BOT IF A WETTABLE CELL IS DRY
      ZERO=0.
      DO 100 K=1,NLAY
C
C2A-----SKIP LAYERS THAT CANNOT CONVERT BETWEEN WET AND DRY
      IF(LAYWT(K).EQ.0) GO TO 100
      DO 90 I=1,NROW
      DO 90 J=1,NCOL
C
C2B-----SKIP CELLS THAT ARE CURRENTLY WET OR ARE NOT WETTABLE
      IF(IBOUND(J,I,K).NE.0) GO TO 90
      IF(WETDRY(J,I,LAYWT(K)).EQ.ZERO) GO TO 90
C
C2C-----SET HOLD=BOT
      HOLD(J,I,K)=BOTM(J,I,LBOTM(K))
   90 CONTINUE
  100 CONTINUE
C
C3-----RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF1HUF1WET(HNEW,IBOUND,BOTM,NBOTM,K,KITER,KSTP,KPER,
     1      NCOL,NROW,NLAY,IOUT,WETDRY,WETFCT,IHDWET,IHDCNV,
     2      NCNVRT,ICNVRT,JCNVRT,ACNVRT)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     CONVERT DRY CELLS TO WET.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     1          BOTM(NCOL,NROW,0:NBOTM),WETDRY(NCOL,NROW,NLAY)
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
      ZERO=0.0
C
C1------LOOP THROUGH ALL CELLS.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY AND IF IT IS WETTABLE, CONTINUE CHECKING TO SEE
C2------IF IT SHOULD BECOME WET.
      IF(IBOUND(J,I,K).EQ.0 .AND. WETDRY(J,I,LAYWT(K)).NE.ZERO) THEN
C
C3------CALCULATE WETTING ELEVATION.
         WD=WETDRY(J,I,LAYWT(K))
         IF(WD.LT.ZERO) WD=-WD
         TURNON=BOTM(J,I,LBOTM(K))+WD
C
C4------CHECK HEAD IN CELL BELOW TO SEE IF WETTING ELEVATION HAS BEEN
C4------REACHED.
         IF(K.NE.NLAY) THEN
            HTMP=HNEW(J,I,K+1)
            IF(IBOUND(J,I,K+1).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
         END IF
C
C5------CHECK HEAD IN ADJACENT HORIZONTAL CELLS TO SEE IF WETTING
C5------ELEVATION HAS BEEN REACHED.
         IF(WETDRY(J,I,LAYWT(K)).GT.ZERO) THEN
            IF(J.NE.1) THEN
               HTMP=HNEW(J-1,I,K)
               IF(IBOUND(J-1,I,K).GT.0 .AND. IBOUND(J-1,I,K).NE.30000.
     1                       AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(J.NE.NCOL) THEN
               HTMP=HNEW(J+1,I,K)
               IF(IBOUND(J+1,I,K).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(I.NE.1) THEN
               HTMP=HNEW(J,I-1,K)
               IF(IBOUND(J,I-1,K).GT.0 .AND. IBOUND(J,I-1,K).NE.30000.
     1                       AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(I.NE.NROW) THEN
               HTMP=HNEW(J,I+1,K)
               IF(IBOUND(J,I+1,K).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
            END IF
         END IF
C
C6------WETTING ELEVATION HAS NOT BEEN REACHED, SO CELL REMAINS DRY.
         GO TO 100
C
C7------CELL BECOMES WET.  PRINT MESSAGE, SET INITIAL HEAD, AND SET
C7------IBOUND.
   50    CALL SGWF1HUF1WDMSG(2,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     1             IOUT,KITER,J,I,K,KSTP,KPER)
C
C7A-----USE EQUATION 3A IF IHDWET=0; ISE EQUATION 3B IF IHDWET IS NOT 0.
         IF(IHDWET.EQ.0) THEN
            HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+
     1                        WETFCT*(HTMP-BOTM(J,I,LBOTM(K)))
         ELSE
            HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+WETFCT*WD
         END IF
         IBOUND(J,I,K)=30000
      END IF
  100 CONTINUE
C
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1WDMSG(ICODE,NCNVRT,ICNVRT,JCNVRT,ACNVRT,
     1             IHDCNV,IOUT,KITER,J,I,K,KSTP,KPER)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     PRINT MESSAGE WHEN CELLS CONVERT BETWEEN WET AND DRY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C1------KEEP TRACK OF CELL CONVERSIONS.
      IF(ICODE.GT.0) THEN
         NCNVRT=NCNVRT+1
         ICNVRT(NCNVRT)=I
         JCNVRT(NCNVRT)=J
         IF(ICODE.EQ.1) THEN
            ACNVRT(NCNVRT)='DRY'
         ELSE
            ACNVRT(NCNVRT)='WET'
         END IF
      END IF
C
C2------PRINT A LINE OF DATA IF 5 CONVERSIONS HAVE OCCURRED OR IF ICODE
C2------INDICATES THAT A PARTIAL LINE SHOULD BE PRINTED.
      IF(NCNVRT.EQ.5 .OR. (ICODE.EQ.0 .AND. NCNVRT.GT.0)) THEN
         IF(IHDCNV.EQ.0) WRITE(IOUT,17) KITER,K,KSTP,KPER
   17    FORMAT(1X,/1X,'CELL CONVERSIONS FOR ITER.=',I3,'  LAYER=',
     1       I3,'  STEP=',I3,'  PERIOD=',I3,'   (ROW,COL)')
         IHDCNV=1
         WRITE(IOUT,18) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
   18    FORMAT(1X,3X,5(A,'(',I3,',',I3,')   '))
         NCNVRT=0
      END IF
C
C3------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1HHARM(CR,CC,HK,IBOUND,DELR,DELC,K,NCOL,NROW,
     1         NLAY,HKCC)
C
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK
C     CONDUCTANCES (DISTANCE WEIGHTED HARMONIC MEAN OF TRANSMISSIVITY).
C     CELL THICKNESS IS IN CC UPON ENTRY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     1          HK(NCOL,NROW,NLAY),
     2          IBOUND(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     3          HKCC(NCOL,NROW,NLAY)
C
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
C     ------------------------------------------------------------------
      ZERO=0.
      TWO=2.
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY OR HK=0., SET CONDUCTANCE EQUAL TO 0 AND GO ON
C2------TO NEXT CELL.
      IF(IBOUND(J,I,K).EQ.0 .OR. HK(J,I,K).EQ.ZERO) THEN
         CR(J,I,K)=ZERO
         CC(J,I,K)=ZERO
      ELSE
C
C3------CELL IS WET -- CALCULATE TRANSMISSIVITY OF CELL.
         T1R=HK(J,I,K)*CC(J,I,K)
         T1C=HKCC(J,I,K)*CC(J,I,K)
C3A-----IF THIS IS NOT THE LAST COLUMN (RIGHTMOST), CALCULATE
C3A-----BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
         IF(J.NE.NCOL) THEN
            IF(IBOUND(J+1,I,K).NE.0) THEN
               T2=HK(J+1,I,K)*CC(J+1,I,K)
               CR(J,I,K)=TWO*T2*T1R*DELC(I)/(T1R*DELR(J+1)+T2*DELR(J))
            ELSE
               CR(J,I,K)=ZERO
            END IF
         ELSE
C3B-----IF THIS IS THE LAST COLUMN, SET BRANCH CONDUCTANCE=0.
            CR(J,I,K)=ZERO
         END IF
C
C3C-----IF THIS IS NOT THE LAST ROW (FRONTMOST) THEN CALCULATE
C3C-----BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
         IF(I.NE.NROW) THEN
            IF(IBOUND(J,I+1,K).NE.0) THEN
               T2=HKCC(J,I+1,K)*CC(J,I+1,K)
               CC(J,I,K)=TWO*T2*T1C*DELR(J)/(T1C*DELC(I+1)+T2*DELC(I))
            ELSE
               CC(J,I,K)=ZERO
            END IF
         ELSE
C3D-----IF THIS IS THE LAST ROW, SET BRANCH CONDUCTANCE=0.
            CC(J,I,K)=ZERO
         END IF
      END IF
  100 CONTINUE
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE GWF1HUF1FM(HCOF,RHS,HOLD,SC1,HNEW,IBOUND,CR,CC,CV,HK,
     1    VKA,BOTM,DELR,DELC,DELT,ITRSS,ISS,NCOL,NROW,NLAY,IOUT,WETDRY,
     2    NBOTM,NHUF,RMLT,IZON,NMLTAR,NZONAR,HUFTHK,HKCC,HDRY,KITER,
     3    KSTP,KPER,HUFTMP,IHGUFLG,IWETIT,IHDWET,WETFCT)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
C
      DIMENSION HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),
     1    HOLD(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),
     2    IBOUND(NCOL,NROW,NLAY),CR(NCOL,NROW,NLAY),
     3    CC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),HK(NCOL,NROW,NLAY),
     4    VKA(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),DELR(NCOL),
     5    DELC(NROW),WETDRY(NCOL,NROW,NLAY),
     6    RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     7    HUFTHK(NCOL,NROW,NHUF,2),HKCC(NCOL,NROW,NLAY),
     8    IHGUFLG(5,NHUF),HUFTMP(NCOL,NROW,NHUF)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
      ONE=1.
C
C1------IF ANY LAYER IS CONVERTIBLE REPOPULATE ARRAYS AND CALCULATE
C          BRANCH CONDUCTANCES
      KLAYFLG=0
      DO 100 K=1,NLAY
        IF(LTHUF(K).NE.0) KLAYFLG=1
  100 CONTINUE
      IF (KLAYFLG.NE.0) CALL GWF1HUF1SP(
     1 IBOUND,HNEW,CR,CC,CV,DELR,DELC,BOTM,HK,VKA,SC1,
     2 ITRSS,NCOL,NROW,NLAY,IOUT,WETDRY,NHUF,NBOTM,RMLT,IZON,
     3 NMLTAR,NZONAR,HUFTHK,HKCC,HDRY,KITER,KSTP,KPER,IHGUFLG,HUFTMP,
     4 IWETIT,IHDWET,WETFCT)

C
C2------IF THE STRESS PERIOD IS TRANSIENT, ADD STORAGE TO HCOF AND RHS
C
      IF(ISS.EQ.0) THEN
         TLED=ONE/DELT
         DO 200 K=1,NLAY
C
C3------SEE IF THIS LAYER IS CONVERTIBLE OR NON-CONVERTIBLE.
         IF(LTHUF(K).EQ.0) THEN
C4------NON-CONVERTIBLE LAYER, SO USE PRIMARY STORAGE
            DO 140 I=1,NROW
            DO 140 J=1,NCOL
              IF(IBOUND(J,I,K).LE.0) GO TO 140
              RHO=SC1(J,I,K)*TLED
              HCOF(J,I,K)=HCOF(J,I,K)-RHO
              RHS(J,I,K)=RHS(J,I,K)-RHO*HOLD(J,I,K)
  140       CONTINUE
         ELSE
C
C5------A CONVERTIBLE LAYER, SO CHECK OLD AND NEW HEADS TO DETERMINE
C5------WHEN TO USE PRIMARY AND SECONDARY STORAGE
            DO 180 I=1,NROW
            DO 180 J=1,NCOL
C
C5A-----IF THE CELL IS EXTERNAL THEN SKIP IT.
              IF(IBOUND(J,I,K).LE.0) GO TO 180
              TOP=BOTM(J,I,LBOTM(K)-1)
              BOT=BOTM(J,I,LBOTM(K))
              HO=HOLD(J,I,K)
              HN=HNEW(J,I,K)
              CRHS=0.
              CHCOF=0.
              IF(HO.GT.TOP.AND.HN.GT.TOP) THEN
                CHCOF=SC1(J,I,K)*TLED
                CRHS=SC1(J,I,K)*HO*TLED
              ELSE
C---------------Compute SC1 Component
                IF(HO.GT.TOP) THEN
                  CRHS=SC1(J,I,K)*(HO-TOP)*TLED
                ELSEIF(HN.GT.TOP) THEN
                  CHCOF=SC1(J,I,K)*TLED
                  CRHS=SC1(J,I,K)*TOP*TLED
                ENDIF
C---------------Compute SC2 Component
                CALL SGWF1HUF1SC2(0,J,I,TOP,BOT,HN,HO,TLED,CHCOF,CRHS,
     &                        HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,
     &                        NMLTAR,DELR(J)*DELC(I))
              ENDIF
C
C5D-----ADD STORAGE TERMS TO RHS AND HCOF.
              HCOF(J,I,K)=HCOF(J,I,K) - CHCOF
              RHS(J,I,K) = RHS(J,I,K) - CRHS
C
  180       CONTINUE
         END IF
C
  200    CONTINUE
      END IF
C
C6------FOR EACH LAYER DETERMINE IF CORRECTION TERMS ARE NEEDED FOR
C6------FLOW DOWN INTO PARTIALLY SATURATED LAYERS.
      DO 300 K=1,NLAY
C
C7------SEE IF CORRECTION IS NEEDED FOR LEAKAGE FROM ABOVE.
      IF(LTHUF(K).NE.0 .AND. K.NE.1) THEN
C
C7A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
         DO 220 I=1,NROW
         DO 220 J=1,NCOL
C
C7B-----IF THE CELL IS EXTERNAL(IBOUND<=0) THEN SKIP IT.
         IF(IBOUND(J,I,K).LE.0) GO TO 220
         HTMP=HNEW(J,I,K)
C
C7C-----IF HEAD IS ABOVE TOP THEN CORRECTION NOT NEEDED
         TOP=BOTM(J,I,LBOTM(K)-1)
         IF(HTMP.GE.TOP) GO TO 220
C
C7D-----WITH HEAD BELOW TOP ADD CORRECTION TERMS TO RHS.
         RHS(J,I,K)=RHS(J,I,K) + CV(J,I,K-1)*(TOP-HTMP)
  220    CONTINUE
      END IF
C
C8------SEE IF THIS LAYER MAY NEED CORRECTION FOR LEAKAGE TO BELOW.
      IF(K.EQ.NLAY) GO TO 300
      IF(LTHUF(K+1).NE.0) THEN
C
C8A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
         DO 280 I=1,NROW
         DO 280 J=1,NCOL
C
C8B-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.
           IF(IBOUND(J,I,K).LE.0 .OR. IBOUND(J,I,K+1).LE.0) GO TO 280
C
C8C-----IF HEAD IN THE LOWER CELL IS LESS THAN TOP ADD CORRECTION
C8C-----TERM TO RHS.
           HTMP=HNEW(J,I,K+1)
           TOP=BOTM(J,I,LBOTM(K+1)-1)
           IF(HTMP.LT.TOP) RHS(J,I,K)=RHS(J,I,K)- CV(J,I,K)*(TOP-HTMP)
  280    CONTINUE
      END IF
C
  300 CONTINUE
C
C9------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF1HUF1SC2(IFLG,J,I,TOP,BOT,HN,HO,TLED,CHCOF,CRHS,
     &                    HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,
     &                    NMLTAR,AREA)
C
C     ******************************************************************
C     Compute contributions to HCOF and RHS for convertible cell
C     Enter subroutine when HO and/or HN are below TOP
C     Values for IFLG:
C       IFLG = 0, Calculate contributions to HCOF and RHS
C       IFLG = 1, Calculate contributions to flow within cell
C       IFLG = 2, Calculate contributions to sensitivity calculations
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      REAL TOPU, BOTU, THCKU, TOP, BOT, CHCOF, CRHS, AREA
      INTEGER IFLG
      DIMENSION HUFTHK(NCOL,NROW,NHUF,2),IZON(NCOL,NROW,NZONAR),
     &  RMLT(NCOL,NROW,NMLTAR)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
C      
C-----Loop through parameters
      DO 100 NP=1,MXPAR
        IF(PARTYP(NP).EQ.'SY') THEN
          BNP=B(NP)*AREA*TLED
C---------Loop through units for this parameter to determine if they apply
C         to this layer
          DO 200 ND=IPLOC(1,NP),IPLOC(2,NP)
            NU=IPCLST(1,ND)
            NM=IPCLST(2,ND)
            NZ=IPCLST(3,ND)
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            BOTU=TOPU-THCKU
C-----------Skip this unit if it is not present in this layer
            IF(TOPU.GT.TOP.AND.BOTU.GE.TOP) GOTO 200
            IF(TOPU.LE.BOT.AND.BOTU.LT.BOT) GOTO 200
            IF(TOPU.GT.TOP) TOPU=TOP
            IF(BOTU.LT.BOT) BOTU=BOT
            RMLT0=1.
            IF(NZ.GT.0) THEN
              RMLT0=0.
C-------------Loop through zones if applicable
              DO 310 JJ=5,IPCLST(4,ND)
                IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ND)) THEN
                  IF(NM.GT.0) THEN
                    RMLT0=RMLT(J,I,NM)
                  ELSE
                    RMLT0=1.
                  ENDIF
                END IF
  310         CONTINUE
            ELSEIF(NM.GT.0) THEN
              RMLT0=RMLT(J,I,NM)
            ENDIF
C-----------Skip this unit if it does not apply to this cell
            IF(RMLT0.LE.0) GOTO 200
C-----------Compute contributions for this unit to flow in layer
            IF(IFLG.LT.2) THEN
              IF(HO.GT.TOP) THEN
C-------------Layer converts, water table is coming down
                IF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C---------------New head is in this unit
                  CHCOF=RMLT0*BNP
                  CRHS=CRHS+RMLT0*BNP*TOPU
                  IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                ELSEIF(HN.LT.BOTU) THEN
C---------------New head is below this unit
                  CRHS=CRHS+RMLT0*BNP*(TOPU-BOTU)
                ENDIF
              ELSEIF(HN.GT.TOP) THEN
C-------------Layer converts, water table is going up
                IF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C---------------Old head is in this unit
                  CRHS=CRHS+RMLT0*BNP*(HO-TOPU)
                ELSEIF(HO.LT.BOTU) THEN
C---------------Old head is below this unit
                  CRHS=CRHS+RMLT0*BNP*(BOTU-TOPU)
                ENDIF
              ELSEIF(HO.LT.TOP.AND.HN.LT.TOP) THEN
C-------------Layer does not convert, just use SC2
                IF(HO.GT.HN) THEN
C---------------Water table is coming down
                  IF(HO.LT.TOPU.AND.HO.GT.BOTU .AND.
     &               HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------Old and new heads are both in this unit
                    CHCOF=RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*HO
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C-----------------Old head is in this unit
                    CRHS=CRHS+RMLT0*BNP*(HO-BOTU)
                  ELSEIF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------New head is in this unit
                    CHCOF=RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*TOPU
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.GT.TOPU.AND.HN.LT.BOTU) THEN
C-----------------Old head is above and new head is below this unit
                    CRHS=CRHS+RMLT0*BNP*(TOPU-BOTU)
                  ENDIF
                ELSE
C---------------Water table is going up
                  IF(HO.LT.TOPU.AND.HO.GT.BOTU .AND.
     &               HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------Old and new heads are both in this unit
                    CHCOF=RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*HO
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C-----------------Old head is in this unit
                    CRHS=CRHS+RMLT0*BNP*(HO-TOPU)
                  ELSEIF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------New head is in this unit
                    CHCOF=RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*BOTU
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.LT.BOTU.AND.HN.GT.TOPU) THEN
C-----------------Old head is below and new head is abov this unit
                    CRHS=CRHS+RMLT0*BNP*(BOTU-TOPU)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(IFLG.EQ.2) THEN
              IF(HO.LE.TOPU.AND.HO.GT.BOTU) THEN
                CRHS=CRHS+RMLT0*B(NP)*AREA
                RETURN
              ENDIF
            ENDIF
  200     CONTINUE
        ENDIF
  100 CONTINUE
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF1HUF1S(VBNM,VBVL,MSUM,HNEW,IBOUND,HOLD,SC1,
     1   BOTM,DELT,ISS,NCOL,NROW,NLAY,KSTP,KPER,IHUFCB,
     2   ICGHFL,BUFF,IOUT,PERTIM,TOTIM,NBOTM,HUFTHK,NHUF,IZON,NZONAR,
     3   RMLT,NMLTAR,DELR,DELC)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR HUF.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 VBNM(MSUM),TEXT
      DOUBLE PRECISION HNEW,STOIN,STOUT,SSTRG
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     1   HOLD(NCOL,NROW,NLAY),SC1(NCOL,NROW,NLAY),VBVL(4,MSUM),
     2   BOTM(NCOL,NROW,0:NBOTM),BUFF(NCOL,NROW,NLAY),
     3   HUFTHK(NCOL,NROW,NHUF,2),IZON(NCOL,NROW,NZONAR),
     4   RMLT(NCOL,NROW,NMLTAR),DELR(NCOL),DELC(NROW)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
      DATA TEXT /'         STORAGE'/
C     ------------------------------------------------------------------
C
C2------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
C
C1------SKIP STORAGE BUDGET CALCULATIONS IF STEADY STATE.
      IF(ISS.NE.0) GOTO 400
      ONE=1.
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(IHUFCB.GT.0) IBD=ICGHFL
C
C4------CLEAR BUFFER.
      DO 210 K=1,NLAY
      DO 210 I=1,NROW
      DO 210 J=1,NCOL
        BUFF(J,I,K)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO 300 K=1,NLAY
        LC=LTHUF(K)
        IF(LC.NE.0) KT=KT+1
        DO 300 I=1,NROW
        DO 300 J=1,NCOL
C
C6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
          IF(IBOUND(J,I,K).LE.0) GO TO 300
          HN=HNEW(J,I,K)
          HO=HOLD(J,I,K)
          STRG=0.
C
C7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
          IF(LC.EQ.0) GO TO 285
          TOP=BOTM(J,I,LBOTM(K)-1)
          BOT=BOTM(J,I,LBOTM(K))
          IF(HO.GT.TOP.AND.HN.GT.TOP) GOTO 285
C
C7A----TWO STORAGE CAPACITIES.
C---------------Compute SC1 Component
          IF(HO.GT.TOP) THEN
            STRG=SC1(J,I,K)*(HO-TOP)*TLED
          ELSEIF(HN.GT.TOP) THEN
            STRG=SC1(J,I,K)*TLED*(TOP-HN)
          ENDIF
C---------------Compute SC2 Component
          CALL SGWF1HUF1SC2(1,J,I,TOP,BOT,HN,HO,TLED,CHCOF,STRG,HUFTHK,
     &           NCOL,NROW,NHUF,IZON,NZONAR,RMLT,NMLTAR,DELR(J)*DELC(I))
C      STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
          GO TO 288
C
C7B----ONE STORAGE CAPACITY.
  285     RHO=SC1(J,I,K)*TLED
          STRG=RHO*HO-RHO*HN
 
C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
  288     BUFF(J,I,K)=STRG
          SSTRG=STRG
          IF(STRG) 292,300,294
  292     STOUT=STOUT-SSTRG
          GO TO 300
  294     STOIN=STOIN+SSTRG
C
  300 CONTINUE
C
C9-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,IHUFCB,
     1            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C10-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C11----RETURN.
      RETURN
      END
c======================================================================
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
      SUBROUTINE SGWF1HUF1B(HNEW,IBOUND,CR,CC,CV,BOTM,NCOL,NROW,NLAY,
     1      KSTP,KPER,IHUFCB,BUFF,IOUT,ICGHFL,DELT,PERTIM,TOTIM,
     2      IDIR,IBDRET,ICHFLG,IC1,IC2,IR1,IR2,IL1,IL2,NBOTM)
C
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HNEW,HD
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     2     CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     3     BUFF(NCOL,NROW,NLAY)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     1 /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      ZERO=0.
      IBD=0
      IF(IHUFCB.GT.0) IBD=ICGHFL
      IF(IBD.EQ.0 .AND. IBDRET.EQ.0) RETURN
C
C2------SET THE SUBREGION EQUAL TO THE ENTIRE GRID IF VALUES ARE BEING
C2------SAVED IN A FILE.
      IF(IBD.NE.0) THEN
         K1=1
         K2=NLAY
         I1=1
         I2=NROW
         J1=1
         J2=NCOL
      END IF
C
C3------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS COLUMNS, GO TO
C3------STEP 4.  IF ONLY 1 COLUMN, RETURN.
      IF(IDIR.NE.1) GO TO 405
      IF(NCOL.EQ.1) RETURN
C
C3A-----CALCULATE FLOW ACROSS COLUMNS (THROUGH RIGHT FACE).  IF NOT
C3A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1-1
         IF(J1.LT.1) J1=1
         J2=IC2
      END IF
      DO 310 K=K1,K2
      DO 310 I=I1,I2
      DO 310 J=J1,J2
      BUFF(J,I,K)=ZERO
  310 CONTINUE
C
C3B-----FOR EACH CELL CALCULATE FLOW THRU RIGHT FACE & STORE IN BUFFER.
      IF(J2.EQ.NCOL) J2=J2-1
      DO 400 K=K1,K2
      DO 400 I=I1,I2
      DO 400 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J+1,I,K).LE.0)) GO TO 400
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J+1,I,K).EQ.0)) GO TO 400
      END IF
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      BUFF(J,I,K)=HDIFF*CR(J,I,K)
  400 CONTINUE
C
C3C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(1),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(1),IHUFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C4------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS ROWS, GO TO
C4------STEP 5.  IF ONLY 1 ROW, RETURN.
  405 IF(IDIR.NE.2) GO TO 505
      IF(NROW.EQ.1) RETURN
C
C4A-----CALCULATE FLOW ACROSS ROWS (THROUGH FRONT FACE).  IF NOT SAVING
C4A-----IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1-1
         IF(I1.LT.1) I1=1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 410 K=K1,K2
      DO 410 I=I1,I2
      DO 410 J=J1,J2
      BUFF(J,I,K)=ZERO
  410 CONTINUE
C
C4B-----FOR EACH CELL CALCULATE FLOW THRU FRONT FACE & STORE IN BUFFER.
      IF(I2.EQ.NROW) I2=I2-1
      DO 500 K=K1,K2
      DO 500 I=I1,I2
      DO 500 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I+1,K).LE.0)) GO TO 500
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I+1,K).EQ.0)) GO TO 500
      END IF
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      BUFF(J,I,K)=HDIFF*CC(J,I,K)
  500 CONTINUE
C
C4C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(2),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(2),IHUFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C5------DIRECTION OF CALCULATION IS ACROSS LAYERS BY ELIMINATION.  IF
C5------ONLY 1 LAYER, RETURN.
  505 IF(NLAY.EQ.1) RETURN
C
C5A-----CALCULATE FLOW ACROSS LAYERS (THROUGH LOWER FACE).  IF NOT
C5A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1-1
         IF(K1.LT.1) K1=1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 510 K=K1,K2
      DO 510 I=I1,I2
      DO 510 J=J1,J2
      BUFF(J,I,K)=ZERO
  510 CONTINUE
C
C5B-----FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER.
      IF(K2.EQ.NLAY) K2=K2-1
      DO 600 K=1,K2
      IF(K.LT.K1) GO TO 600
      DO 590 I=I1,I2
      DO 590 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0)) GO TO 590
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I,K+1).EQ.0)) GO TO 590
      END IF
      HD=HNEW(J,I,K+1)
      IF(LTHUF(K+1).EQ.0) GO TO 580
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  580 HDIFF=HNEW(J,I,K)-HD
      BUFF(J,I,K)=HDIFF*CV(J,I,K)
  590 CONTINUE
  600 CONTINUE
C
C5C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     1   CALL UBUDSV(KSTP,KPER,TEXT(3),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT(3),IHUFCB,BUFF,NCOL,NROW,
     1     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
      END
c======================================================================
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
      SUBROUTINE SGWF1HUF1F(VBNM,VBVL,MSUM,HNEW,IBOUND,CR,CC,CV,BOTM,
     1         DELT,NCOL,NROW,NLAY,KSTP,KPER,IHUFCB,BUFF,IOUT,ICBCFL,
     2         PERTIM,TOTIM,NBOTM,ICHFLG)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 VBNM(MSUM),TEXT
      DOUBLE PRECISION HNEW,HD,CHIN,CHOUT,XX1,XX2,XX3,XX4,XX5,XX6
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     2     CV(NCOL,NROW,NLAY), VBVL(4,MSUM),
     3     BOTM(NCOL,NROW,0:NBOTM),BUFF(NCOL,NROW,NLAY)
C
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD=0
      IF(IHUFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IHUFCB.GT.0) IBD=ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
      CHIN=ZERO
      CHOUT=ZERO
      IBDLBL=0
C
C3------CLEAR BUFFER.
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      BUFF(J,I,K)=ZERO
5     CONTINUE
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF(IBD.EQ.2) THEN
         NCH=0
         DO 7 K=1,NLAY
         DO 7 I=1,NROW
         DO 7 J=1,NCOL
         IF(IBOUND(J,I,K).LT.0) NCH=NCH+1
7        CONTINUE
         CALL UBDSV2(KSTP,KPER,TEXT,IHUFCB,NCOL,NROW,NLAY,
     1          NCH,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 K=1,NLAY
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
      IF (IBOUND(J,I,K).GE.0)GO TO 200
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
      CHCH1=ZERO
      CHCH2=ZERO
      CHCH3=ZERO
      CHCH4=ZERO
      CHCH5=ZERO
      CHCH6=ZERO
C
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
      CHCH1=HDIFF*CR(J-1,I,K)
      IF(IBOUND(J-1,I,K).LT.0) GO TO 30
      X1=CHCH1
      XX1=X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
      IF (X1) 10,30,20
   10 CHOUT=CHOUT-XX1
      GO TO 30
   20 CHIN=CHIN+XX1
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      CHCH2=HDIFF*CR(J,I,K)
      IF(IBOUND(J+1,I,K).LT.0) GO TO 60
      X2=CHCH2
      XX2=X2
      IF(X2)40,60,50
   40 CHOUT=CHOUT-XX2
      GO TO 60
   50 CHIN=CHIN+XX2
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF (IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
      CHCH3=HDIFF*CC(J,I-1,K)
      IF(IBOUND(J,I-1,K).LT.0) GO TO 90
      X3=CHCH3
      XX3=X3
      IF(X3) 70,90,80
   70 CHOUT=CHOUT-XX3
      GO TO 90
   80 CHIN=CHIN+XX3
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      CHCH4=HDIFF*CC(J,I,K)
      IF(IBOUND(J,I+1,K).LT.0) GO TO 120
      X4=CHCH4
      XX4=X4
      IF (X4) 100,120,110
  100 CHOUT=CHOUT-XX4
      GO TO 120
  110 CHIN=CHIN+XX4
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF (IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(LTHUF(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) HD=TOP
  122 HDIFF=HD-HNEW(J,I,K-1)
      CHCH5=HDIFF*CV(J,I,K-1)
      IF(IBOUND(J,I,K-1).LT.0) GO TO 150
      X5=CHCH5
      XX5=X5
      IF(X5) 130,150,140
  130 CHOUT=CHOUT-XX5
      GO TO 150
  140 CHIN=CHIN+XX5
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LTHUF(K+1).EQ.0) GO TO 152
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  152 HDIFF=HNEW(J,I,K)-HD
      CHCH6=HDIFF*CV(J,I,K)
      IF(IBOUND(J,I,K+1).LT.0) GO TO 180
      X6=CHCH6
      XX6=X6
      IF(X6) 160,180,170
  160 CHOUT=CHOUT-XX6
      GO TO 180
  170 CHIN=CHIN+XX6
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
 180  RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
      BUFF(J,I,K)=RATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,899) TEXT,KPER,KSTP
  899    FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
         WRITE(IOUT,900) K,I,J,RATE
  900    FORMAT(1X,'LAYER',I3,'   ROW',I4,'   COL',I4,
     1       '   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
      IF(IBD.EQ.2) CALL UBDSVA(IHUFCB,NCOL,NROW,J,I,K,RATE,IBOUND,NLAY)
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                   IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN=CHIN
      COUT=CHOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+CIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+COUT*DELT
      VBVL(3,MSUM)=CIN
      VBVL(4,MSUM)=COUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C18-----RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE GWF1HUF1OT(IOHUF,HNEW,IHEDFM,IBOUND,NHUF,NCOL,NROW,
     &                  NLAY,HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &                  IZON,NZONAR,KSTP,KPER,ISA,ICNVG,IOUT,HNOFLO,
     &                  CHEDFM,LBHDSV,PERTIM,TOTIM,HNWHGU)
C
C     ******************************************************************
C     PRINT AND RECORD HEADS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    DELR(NCOL),DELC(NROW),CV(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),RMLT(NCOL,NROW,NMLTAR),
     &    IZON(NCOL,NROW,NZONAR)
      CHARACTER*20 CHEDFM
      CHARACTER*10 HGUNAM
      COMMON /HUFCOMC/HGUNAM(200)
C
      DATA TEXT /'     HEAD IN HGU'/
C     ------------------------------------------------------------------
C
C
      IF(ISA.EQ.0.OR.ICNVG.EQ.0) THEN
        IF(ISA.EQ.0) THEN
          WRITE(IOUT,9) KSTP,KPER
    9     FORMAT(1X,/11X,'NO FLOW EQUATION TO SOLVE IN TIME STEP',I3,
     1      ' OF STRESS PERIOD',I3,/1X,'ALL HEADS ARE 0.0')
        END IF
C
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
        IF(ICNVG.EQ.0) THEN
          WRITE(IOUT,17) KSTP,KPER
   17     FORMAT(1X,/11X,'****FAILED TO CONVERGE IN TIME STEP',I3,
     1      ' OF STRESS PERIOD',I3,'****')
        END IF
      RETURN
      ENDIF
C
C-------CALCULATE HEADS WITHIN UNITS
        CALL SGWF1HUF1OT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &               HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &               IZON,NZONAR,HNOFLO,IOUT)

      IFIRST=1
      DO 100 NU=1,NHUF
C
C-------CALL ULAPRS OR ULAPRW TO PRINT HEAD.
        WRITE(IOUT,72) KSTP,KPER,NU,HGUNAM(NU)
   72   FORMAT('1',
     &    /2X,'HEAD AT END OF TIME STEP',I3,' IN STRESS PERIOD',I3,
     &    /2X,'IN HYDROGEOLOGIC UNIT',I3,' WITH NAME ',A10,
     &    /2X,71('-'))
        IF(IHEDFM.LT.0) CALL ULAPRS(HNWHGU(1,1,NU),TEXT,KSTP,KPER,
     1               NCOL,NROW,0,-IHEDFM,IOUT)
        IF(IHEDFM.GE.0) CALL ULAPRW(HNWHGU(1,1,NU),TEXT,KSTP,KPER,
     1               NCOL,NROW,0,IHEDFM,IOUT)
C
C5------FOR EACH UNIT: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IOHUF,KSTP,KPER
   74     FORMAT(1X,/1X,'HEAD IN HYDROGEOLOGIC UNITS WILL BE SAVED ',
     1    'ON UNIT ',I4,' AT END OF TIME STEP',I3,', STRESS PERIOD',I3)
        IF(IFIRST.EQ.1) IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
          CALL ULASAV(HNWHGU(1,1,NU),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,NU,IOHUF)
        ELSE
          CALL ULASV2(HNWHGU(1,1,NU),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,NU,IOHUF,CHEDFM,1,IBOUND(1,1,1))
        END IF
  100 CONTINUE
C
C6------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF1HUF1OT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,RMLT,NMLTAR,
     &                   IZON,NZONAR,HNOFLO,IOUT)
C
C     ******************************************************************
C     CALCULATE HEADS WITHIN A GIVEN HYDROGEOLOGIC UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HNEW,HDIFF,QN
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    DELR(NCOL),DELC(NROW),CV(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),RMLT(NCOL,NROW,NMLTAR),
     &    IZON(NCOL,NROW,NZONAR),HUFHK(200),HUFVK(200),
     &    INDX(200)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------

C
C     SORT HYDROGEOLOGIC UNITS
      CALL SGWF1HUF1IND(NCOL,NROW,NHUF,HUFTHK,1,1,INDX)
C     CHECK SORTING TO CORRECT FOR ZERO-THICKNESS UNITS
      DO 50 NU=1,NHUF
        IF(HUFTHK(1,1,INDX(NU),2).EQ.0) THEN
          DO 60 I=1,NROW
          DO 60 J=1,NCOL
            IF(HUFTHK(J,I,INDX(NU),2).GT.0) THEN
              IF(NU.GT.1 .AND. HUFTHK(J,I,INDX(NU),1).LT.
     &           HUFTHK(J,I,INDX(NU-1),1)) THEN
                NNU=INDX(NU)
                INDX(NU)=INDX(NU-1)
                INDX(NU-1)=NNU
              ELSEIF(NU.LT.NHUF .AND. HUFTHK(J,I,INDX(NU),1).GT.
     &               HUFTHK(J,I,INDX(NU+1),1)) THEN
                NNU=INDX(NU)
                INDX(NU)=INDX(NU+1)
                INDX(NU+1)=NNU
              ENDIF
            ENDIF
   60     CONTINUE
        ENDIF
   50 CONTINUE
C
C-----LOOP THROUGH ROWS AND COLUMNS
      DO 100 I=1,NROW
      DO 200 J=1,NCOL
C Zero out arrays
        DO 210 NU=1,NHUF
          HUFHK(NU)=0.
          HUFVK(NU)=0.
  210   CONTINUE
C
C---Populate HGU arrays depending on parameter type
        CALL SGWF1HUF1POP(HUFHK,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL SGWF1HUF1POP(HUFVK,'VK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
        CALL SGWF1HUF1POP(HUFVK,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,0,IOUT)
C
C---Populate VANI from input file if not already defined by
c     a parameter
        DO 220 NU=1,NHUF
          IF(HGUVANI(NU).GT.0..AND.HUFVK(NU).EQ.0.)
     &          HUFVK(NU)=HGUVANI(NU)
  220   CONTINUE


        DRDC=DELR(J)*DELC(I)
        IFRST=0
        DO 300 NNU=NHUF,1,-1
          NU=INDX(NNU)
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(THCKU.EQ.0) THEN
            IF(IFRST.EQ.0) THEN
              HNWHGU(J,I,NU)=HNOFLO
            ELSE
              IF(NNU.GT.1) HNWHGU(J,I,INDX(NNU-1)) = HNWHGU(J,I,NU)
            ENDIF
            GOTO 300
          ENDIF
          BOTU=TOPU-THCKU
          RMIDU=TOPU-0.5*THCKU
          CALL SGWF1HUF1VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     2                 HNEW,IBOUND,KT,KB,IFLG)
C-------UNIT ABOVE/BELOW MODEL
          IF(IFLG.EQ.1) THEN
            HNWHGU(J,I,NU)=HNOFLO
            GOTO 300
          ENDIF
          IF(HGUVANI(NU).EQ.0.) THEN
            VK=HUFVK(NU)
          ELSE
            VK=HUFHK(NU)/HUFVK(NU)
          ENDIF
C
          IF(KT.EQ.KB) THEN
            IF(KT.LT.NLAY) THEN
              HDIFF=HNEW(J,I,KT)-HNEW(J,I,KT+1)
              CVKT=CV(J,I,KT)
            ELSE
              HDIFF=HNEW(J,I,KT-1)-HNEW(J,I,KT)
              CVKT=CV(J,I,KT-1)
            ENDIF
            RMIDKT=(BOTM(J,I,LBOTM(KT)-1)+BOTM(J,I,LBOTM(KT)))/2.
            QN=HDIFF*CVKT/DRDC
            IF(IFRST.EQ.0) THEN
              HNWHGU(J,I,NU) = HNEW(J,I,KT)-QN*(RMIDKT-RMIDU)/VK
              IF(NNU.GT.1) HNWHGU(J,I,INDX(NNU-1)) = 
     &                        HNEW(J,I,KT)-QN*(RMIDKT-BOTU)/VK
              IFRST=1
            ELSE
              HNWHGU(J,I,NU) = HNWHGU(J,I,NU)-QN*0.5*THCKU/VK
              IF(NNU.GT.1) HNWHGU(J,I,INDX(NNU-1)) = 
     &                        HNWHGU(J,I,NU)-QN*0.5*THCKU/VK
            ENDIF
          ELSE
C---------FIRST, PROPOGATE HEAD FROM MIDDLE OF CELL TO MIDDLE OF UNIT
            DO 400 KL=KT,KB
              TOPKL=BOTM(J,I,LBOTM(KL)-1)
              BOTKL=BOTM(J,I,LBOTM(KL))
              RMIDKL=(TOPKL+BOTKL)/2.
              IF(RMIDU.LT.TOPKL.AND.RMIDU.GE.BOTKL) THEN
                IF(RMIDU.GT.RMIDKL) THEN
                  IF(KL.EQ.1) THEN
                    HDIFF=HNEW(J,I,KL)-HNEW(J,I,KL+1)
                    CVKL=CV(J,I,KL)
                  ELSE
                    HDIFF=HNEW(J,I,KL-1)-HNEW(J,I,KL)
                    CVKL=CV(J,I,KL-1)
                  ENDIF
                ELSE
                  IF(KL.LT.NLAY) THEN
                    HDIFF=HNEW(J,I,KL)-HNEW(J,I,KL+1)
                    CVKL=CV(J,I,KL)
                  ELSE
                    HDIFF=HNEW(J,I,KL-1)-HNEW(J,I,KL)
                    CVKL=CV(J,I,KL-1)
                  ENDIF
                ENDIF
              QN=HDIFF*CVKL/DRDC
              HNWHGU(J,I,NU) = HNEW(J,I,KL)+QN*(RMIDU-RMIDKL)/VK
              GOTO 410
            ENDIF
  400       CONTINUE
C---------NOW, PROPOGATE CELL TO BOTTOM OF UNIT
  410       IF(NNU.GT.1) THEN
              TOPKB=BOTM(J,I,LBOTM(KB)-1)
              BOTKB=BOTM(J,I,LBOTM(KB))
              RMIDKB=(TOPKB+BOTKB)/2.
              IF(KB.LT.NLAY) THEN
                HDIFF=HNEW(J,I,KB)-HNEW(J,I,KB+1)
                CVKB=CV(J,I,KB)
              ELSE
                HDIFF=HNEW(J,I,KB-1)-HNEW(J,I,KB)
                CVKB=CV(J,I,KB-1)
              ENDIF
              QN=HDIFF*CVKB/DRDC
              HNWHGU(J,I,INDX(NNU-1)) = HNEW(J,I,KB)-QN*(RMIDKB-BOTU)/VK
              IF(IFRST.EQ.0) IFRST=1
            ENDIF
          ENDIF
  300   CONTINUE
        DO 350 NU=1,NHUF
          IF(HUFTHK(J,I,NU,2).EQ.0.0) HNWHGU(J,I,NU) = HNOFLO
  350   CONTINUE
  200 CONTINUE
  100 CONTINUE
C
C6------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF1HUF1IND(NCOL,NROW,NHUF,HUFTHK,JJ,II,INDX)
C
C     ******************************************************************
C     INDEX HYDROGEOLOGIC UNITS FOR A GIVEN ROW/COLUMN LOCATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER NHUF,indx(200),M,NSTACK
      REAL HUFTHK(NCOL,NROW,NHUF,2)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a
      do 11 j=1,NHUF
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=NHUF
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=HUFTHK(JJ,II,indxt,1)
          do 12 i=j-1,l,-1
            if(HUFTHK(JJ,II,indx(i),1).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=l-1
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)

        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(HUFTHK(JJ,II,indx(l),1).gt.HUFTHK(JJ,II,indx(ir),1))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(HUFTHK(JJ,II,indx(l+1),1).gt.HUFTHK(JJ,II,indx(ir),1))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(HUFTHK(JJ,II,indx(l),1).gt.HUFTHK(JJ,II,indx(l+1),1))then
          itemp=indx(l)
          indx(l)=indx(l+1)
          indx(l+1)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l+1)
        a=HUFTHK(JJ,II,indxt,1)

3       continue
          i=i+1
        if(HUFTHK(JJ,II,indx(i),1).lt.a)goto 3
4       continue
          j=j-1
        if(HUFTHK(JJ,II,indx(j),1).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l+1)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END