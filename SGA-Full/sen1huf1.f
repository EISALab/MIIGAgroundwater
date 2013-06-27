C     Version 1.04
C     Last change:  ERB  10 Jul 2002    9:04 am
      SUBROUTINE SEN1HUF1FM(H,NCOL,NROW,NLAY,PID,HK,HKCC,DELR,
     &                     DELC,IBOUND,RHS,CV,BOTM,NBOTM,HUFTHK,
     &                     NHUF,IP,IZON,NZONAR,RMLT,NMLTAR,IUHFBP,
     &                     HFBP,MXACTFB,NHFB,HOLD,DELT,ISS,IOUT)
C     ******************************************************************
C      CALCULATE MATRIX DERIVATIVES AND MULTIPLY BY HEADS AS NEEDED.  
C      ADD RESULTING CONTRIBUTION TO RHS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CV,  DELC, DELR, C, DELT, HH, R0, RHS, RMLT, RMLT0, TH0,
     &     ZERO, ONE, BOTM , MID1 , MID2, TH0L, TH1L
      INTEGER I, IBM, IBOUND, IBP, IND, ISS, J, K, LT, IZON, NCOL, NLAY,
     &        NRC, NROW
      CHARACTER*4 PID
      DOUBLE PRECISION H(NCOL*NROW*NLAY) , HP
      DIMENSION DELR(NCOL), DELC(NROW),
     & RHS(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     & CV(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     & HK(NCOL,NROW,NLAY), HUFTHK(NCOL,NROW,NHUF,2),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     & HFBP(7,MXACTFB),HOLD(NCOL*NROW*NLAY),HKCC(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      ONE = 1.0
      NRC = NROW*NCOL
C
      DO 140 ICL = IPLOC(1,IP),IPLOC(2,IP)
        NU = IPCLST(1,ICL)
        NZ = IPCLST(3,ICL)
        NM = IPCLST(2,ICL)
C-----HORIZONTAL CONDUCTANCES
        IF (PID.EQ.'HK  '.OR.PID.EQ.'HANI') THEN
          DO 70 I = 1, NROW
            DO 60 J = 1, NCOL
              DO 55 K=1,NLAY
                LT=LTHUF(K)
                IF (IBOUND(J,I,K).EQ.0) GOTO 55
                IND = J + NCOL*(I-1) + NRC*(K-1)
                HO = H(IND)
                R0 = ZERO
                RMLT0 = ONE
                IF (NZ.GT.0) THEN
                  RMLT0=0.
                  DO 30 JJ = 5,IPCLST(4,ICL)
                    IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ICL)) THEN
                      IF(NM.GT.0) THEN
                        RMLT0=RMLT(J,I,NM)
                      ELSE
                        RMLT0=1.
                      ENDIF
                    END IF
   30             CONTINUE
                ELSEIF(NM.GT.0) THEN
                  RMLT0=RMLT(J,I,NM)
                ENDIF

C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
                TOP0=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.H(IND).LT.TOP0) TOP0=H(IND)
                BOT0=BOTM(J,I,LBOTM(K))
                CALL SSEN1HUF1THK(TOP0,BOT0,
     1                          HUFTHK(J,I,NU,1),
     2                          HUFTHK(J,I,NU,2),TH0)
C-------CR
                IF (J.NE.NCOL) THEN
                  IF (PID.NE.'HANI'.AND.IBOUND(J+1,I,K).NE.0) THEN
                    TOP1C=BOTM(J+1,I,LBOTM(K)-1)
                    IF(LTHUF(K).NE.0.AND.H(IND+1).LT.TOP1C)
     &                  TOP1C=H(IND+1)
                    BOT1C=BOTM(J+1,I,LBOTM(K))
                    CALL SSEN1HUF1THK(TOP1C,BOT1C,
     1                          HUFTHK(J+1,I,NU,1),
     2                          HUFTHK(J+1,I,NU,2),TH1C)
                    IF(TH0.EQ.0..AND.TH1C.EQ.0.) GOTO 54
                    CALL SSEN1HUF1CH(CO,TH0,TH1C,HP,I,J,K,'CR',RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
                  IF (IUHFBP.GT.0 .AND. CO.NE.0.)
     &                CALL SSEN1HFB6MD(C,'CR',CO,DELC,DELR,HFBP,I,J,K,
     &                                 MXACTFB,NCOL,NHFB,NROW,
     &                                 TH0L,TH1L)
                    HH = HO - HP
                    R0 = R0 + CO*HH
                    RHS(J+1,I,K) = RHS(J+1,I,K) - CO*HH
                  ENDIF
                ENDIF
C-------CC
   54           IF (I.EQ.NROW) GOTO 50
                IF (IBOUND(J,I+1,K).EQ.0) GOTO 50
                  TOP1R=BOTM(J,I+1,LBOTM(K)-1)
                  IF(LTHUF(K).NE.0.AND.H(IND+NCOL).LT.TOP1R)
     &                TOP1R=H(IND+NCOL)
                  BOT1R=BOTM(J,I+1,LBOTM(K))
                  CALL SSEN1HUF1THK(TOP1R,BOT1R,
     1                          HUFTHK(J,I+1,NU,1),
     2                          HUFTHK(J,I+1,NU,2),TH1R)
                  IF(TH0.EQ.0..AND.TH1R.EQ.0.) GOTO 50
                IF (PID.EQ.'HK  ') THEN
                  CALL SSEN1HUF1CH(CO,TH0,TH1R,HP,I,J,K,'CC',RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
                ELSEIF(PID.EQ.'HANI') THEN
                  CALL SSEN1HUF1CHN(CO,TH0,TH1R,HP,I,J,K,RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
                ENDIF
                IF (IUHFBP.GT.0 .AND. CO.NE.0.)
     &              CALL SSEN1HFB6MD(C,'CC',CO,DELC,DELR,HFBP,I,J,K,
     &                               MXACTFB,NCOL,NHFB,NROW,TH0L,
     &                               TH1L)
                  IF (CO.EQ.ZERO) GOTO 50
                HH = HO - HP
                R0 = R0 + CO*HH
                RHS(J,I+1,K) = RHS(J,I+1,K) - CO*HH
   50           RHS(J,I,K) = RHS(J,I,K) + R0
   55         CONTINUE
   60       CONTINUE
   70     CONTINUE
        ENDIF
C--End Horizontal
C-------CV
        IF (PID.EQ.'VK'.OR.PID.EQ.'VANI'.OR.
     &      (PID.EQ.'HK'.AND.HGUVANI(NU).NE.0)) THEN
          DO 90 I = 1, NROW
            DO 80 J = 1, NCOL
              RMLT0=ONE
              IF (NZ.GT.0) THEN
                RMLT0=0.
                DO 31 JJ = 5,IPCLST(4,ICL)
                  IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ICL)) THEN
                    IF(NM.GT.0) THEN
                      RMLT0=RMLT(J,I,NM)
                    ELSE
                      RMLT0=1.
                    ENDIF
                  END IF
   31           CONTINUE
              ELSEIF(NM.GT.0) THEN
                RMLT0=RMLT(J,I,NM)
              ENDIF
              IF(RMLT0.EQ.0.) GOTO 80
              DO 85 K=1,NLAY-1
                IND = J + NCOL*(I-1) + NRC*(K-1)
                TOP1=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.H(IND).LT.TOP1) TOP1=H(IND)
                BOT1=BOTM(J,I,LBOTM(K))
                TOP2=BOTM(J,I,LBOTM(K))
                BOT2=BOTM(J,I,LBOTM(K)+1)
                MID1=0.5*(TOP1+BOT1)
                MID2=0.5*(TOP2+BOT2)
                IF(LTHUF(K+1).NE.0.AND.H(IND+NRC).LT.TOP2) 
     &              MID2 = TOP2
                CALL SSEN1HUF1THK(MID1,MID2,
     1                  HUFTHK(J,I,NU,1),HUFTHK(J,I,NU,2),THK1)
                IF(THK1.EQ.0.) GOTO 85
                CALL SSEN1HUF1CV(PID,CO,THK1,IBP,IBM,NCOL,NROW,NLAY,CV,
     &                        DELR,DELC,J,I,K,IBOUND,IP,NU,NZ,IZON,
     &                        NZONAR,RMLT0,RMLT,NMLTAR,IOUT,HUFTHK,NHUF)
                IF (CO.EQ.ZERO) GOTO 85
                HH = ZERO
                IF (K.LT.NLAY .AND. IBP.NE.0) HH = H(IND) - H(IND+NRC)
                IF (K.LT.NLAY .AND. IBP.NE.0) THEN
                  RHS(J,I,K) = RHS(J,I,K) + CO*HH
                  RHS(J,I,K+1) = RHS(J,I,K+1) - CO*HH
C               ACCOUNT FOR UNCONFINED LAYER UNDERLYING ACTIVE LAYER
                  IF (LTHUF(K+1).NE.0 .AND.H(IND+NRC).LT.BOT1) THEN
                    RHS(J,I,K) = RHS(J,I,K) - CO*(BOT1-H(IND+NRC))
                    RHS(J,I,K+1) = RHS(J,I,K+1) + CO*(BOT1-H(IND+NRC))
                  ENDIF
                ENDIF
   85         CONTINUE
   80       CONTINUE
   90     CONTINUE
        ENDIF
C-----S
        IF (PID.EQ.'SS  ' .OR. PID.EQ.'SY  ') THEN
          IF (ISS.NE.0) GOTO 140
          DO 130 I = 1, NROW
            DO 120 J = 1, NCOL
              RMLT0=1.0
              IF (NZ.GT.0) THEN
                RMLT0=0.
                DO 32 JJ = 5,IPCLST(4,ICL)
                  IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ICL)) THEN
                    IF(NM.GT.0) THEN
                      RMLT0=RMLT(J,I,NM)
                    ELSE
                      RMLT0=1.
                    ENDIF
                  END IF
   32           CONTINUE
              ELSEIF(NM.GT.0) THEN
                RMLT0=RMLT(J,I,NM)
              ENDIF
              IF(RMLT0.EQ.0) GOTO 120
              DO 110 K = 1, NLAY
                IF (IBOUND(J,I,K).LT.1) GOTO 110
                LT=LTHUF(K)
                IND = J + NCOL*(I-1) + NRC*(K-1)
                HO = H(IND)
                SHO = HOLD(IND)
                TOP = BOTM(J,I,LBOTM(K)-1)
                BOT = BOTM(J,I,LBOTM(K))
                IF (LT.NE.0) THEN
                  IF (PID.EQ.'SS  ' .AND. HO.LT.TOP .AND. SHO.LT.TOP)
     &              GOTO 110
                  IF (PID.EQ.'SY  ' .AND. HO.GE.TOP .AND. SHO.GE.TOP)
     &              GOTO 110
                ELSEIF(LT.EQ.0 .AND. PID.EQ.'SY  ') THEN
                  GOTO 110
                ENDIF
                TOPU = HUFTHK(J,I,NU,1)
                THCKU = HUFTHK(J,I,NU,2)
                BOTU = TOPU - THCKU
                CALL SSEN1HUF1THK(TOP,BOT,TOPU,THCKU,TH0)
                IF(ABS(TH0).LT.1E-6) GOTO 110
                IF (PID.EQ.'SS  ') THEN
                  CO = RMLT0*TH0*DELR(J)*DELC(I)/DELT
                  IF (LT.NE.0) THEN
                    IF (SHO.GE.TOP .AND. HO.LT.TOP) THEN
                      HO = TOP
                    ELSEIF (SHO.LT.TOP .AND. HO.GE.TOP) THEN
                      SHO = TOP
                    ENDIF
                  ENDIF
                  RHS(J,I,K) = RHS(J,I,K) - CO*(SHO-HO)
                ELSEIF(PID.EQ.'SY  ') THEN
                  CO = RMLT0*DELR(J)*DELC(I)/DELT
                  CALL SEN1HUF1SC2(TOP,BOT,TOPU,BOTU,HO,SHO,CRHS,CO)
                  RHS(J,I,K) = RHS(J,I,K) - CRHS
                ENDIF
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
        ENDIF
  140 CONTINUE
  150 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1HUF1UN(ISS,DELT,NCOL,NROW,NLAY,SOLD,HNEW,
     &                  SNEW,DELR,DELC,IBOUND,RHS,SC1,CR,CC,KITER,
     &                  HK,HKCC,BOTM,NBOTM,HOLD,CV,HUFTHK,NHUF,IZON,
     &                  NZONAR,RMLT,NMLTAR)
C     ******************************************************************
C     COMPUTE SENSITIVITY-EQUATION RHS TERMS FOR UNCONFINED AQUIFERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, CC, CR, CV, DELC, DELR, DELT, HOLD, RHS, SC1, SCC,
     &     HK
      INTEGER I, IBOUND, J, K, KITER, KT, LT, NCOL, NLAY,
     &        NROW, NBOTM
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      DIMENSION HOLD(NCOL,NROW,NLAY), SOLD(NCOL,NROW,NLAY),
     &          DELR(NCOL), DELC(NROW), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM), 
     &          CC(NCOL,NROW,NLAY), CR(NCOL,NROW,NLAY),
     &          SC1(NCOL,NROW,NLAY),HK(NCOL,NROW,NLAY),
     &          HKCC(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          HUFTHK(NCOL,NROW,NHUF,2), IZON(NCOL,NROW,NZONAR),
     &          RMLT(NCOL,NROW,NMLTAR)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
C
C-------TERMS FOR UNCONFINED AQUIFERS
      IF (KITER.GT.1) THEN
        DO 60 K = 1, NLAY
          LT = LTHUF(K)
          IF (LT.NE.0) THEN
            CALL SSEN1HUF1NL(HNEW,SNEW,NCOL,NROW,NLAY,HK,HKCC,DELR,DELC,
     &                       IBOUND,RHS,BOTM,NBOTM,CR,CC,CV,K,LT,NHUF,
     &                       HUFTHK,RMLT,IZON,NMLTAR,NZONAR)
          ENDIF
          IF (K.GT.1 .AND. LT.NE.0) THEN
            DO 50 I = 1, NROW
              DO 40 J = 1, NCOL
                IF (IBOUND(J,I,K).NE.0 .AND. IBOUND(J,I,K-1).NE.0
     &              .AND. HNEW(J,I,K).LT.BOTM(J,I,LBOTM(K)-1)) THEN
                  RHS(J,I,K-1) = RHS(J,I,K-1)+CV(J,I,K-1)*SNEW(J,I,K)
                  RHS(J,I,K) = RHS(J,I,K)-CV(J,I,K-1)*SNEW(J,I,K)
                ENDIF
   40         CONTINUE
   50       CONTINUE
          ENDIF
   60   CONTINUE
      ENDIF
C
C-------B MATRIX TIMES SOLUTION FROM LAST TIME STEP FOR SENSITIVITY-
C-------EQUATION SENSITIVITIES
      IF (ISS.EQ.0) THEN
        KT = 0
        DO 110 K = 1, NLAY
          IF (LTHUF(K).EQ.0) THEN
            DO 80 I = 1, NROW
              DO 70 J = 1, NCOL
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SC1(J,I,K)/DELT
   70         CONTINUE
   80       CONTINUE
          ELSE
            KT = KT + 1
            DO 100 I = 1, NROW
              DO 90 J = 1, NCOL
                IF(IBOUND(J,I,K).EQ.0) GOTO 90
                SCC = SC1(J,I,K)
                HO=HOLD(J,I,K)
                TOP=BOTM(J,I,LBOTM(K)-1)
                BOT=BOTM(J,I,LBOTM(K))
                IF (HO.LT.TOP) THEN
                    SCC = 0.0
                    CALL SGWF1HUF1SC2(2,J,I,TOP,BOT,1.0,HO,1.0,DUM,
     &                    SCC,HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,
     &                    NMLTAR,DELR(J)*DELC(I))
                ENDIF
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SCC/DELT
   90         CONTINUE
  100       CONTINUE
          ENDIF
  110   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1NL(HN,SN,NC,NR,NL,HK,HKCC,DELR,DELC,IBOUND,
     &  RHS,BOTM,NBOTM,CR,CC,CV,K,LT,NHUF,HUFTHK,RMLT,IZON,NMLTAR,
     &  NZONAR)
C     ******************************************************************
C     ADD NONLINEAR TERMS FOR SENSITIVITY EQUATION CALCULATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BO, BOTM, BP, CC, CO, COCC, COCR, CR, D1CC, D1CR, D2CC, D2CR, 
     &     DELC, DELR,RHS, HK, THO, THI1, THJ1, TOPO, TOPI1, TOPJ1,
     &     ZERO, MID
      INTEGER I, IBOUND, IND, J, K, LT, NC, NC1, NL, NR, NR1,
     &        NRC
      DOUBLE PRECISION SN(NC,NR,NL),AO,AI1,AJ1,HN(NC,NR,NL),
     &  HO,HI1,HJ1,HP
      DIMENSION CR(NC,NR,NL), CC(NC,NR,NL), HK(NC,NR,NL),
     &  DELR(NC), DELC(NR), RHS(NC,NR,NL), IBOUND(NC,NR,NL), 
     &  BOTM(NC,NR,0:NBOTM),HKCC(NC,NR,NL),HUFTHK(NC,NR,NHUF,2),
     &  HUFTMP(200), CV(NC,NR,NL), RMLT(NC,NR,NMLTAR),
     &  IZON(NC,NR,NZONAR)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      NRC = NR*NC
      NR1 = NR - 1
      NC1 = NC - 1
C
C      CR & CC
C
      DO 20 I = 1, NR
        DO 10 J = 1, NC
          IBDO = IBOUND(J,I,K)
          IF(IBDO.EQ.0) GOTO 10
          HO = HN(J,I,K)
          TOPO = BOTM(J,I,LBOTM(K)-1)
          BO = BOTM(J,I,LBOTM(K))
          THO = HO - BO
          TRO = HK(J,I,K) * THO
          TCO = HKCC(J,I,K) * THO
          AO = SN(J,I,K)
          NUO = 0
          HSING = HO
          IF(TOPO.GT.HO) CALL SSEN1HUF1HDFND(HUFTHK,NC,NR,NHUF,
     &                                       HSING,I,J,NUO)
          IF(NUO.NE.0) CALL SSEN1HUF1HKFND(NC,NR,NHUF,HUFTHK,IZON,
     &                                     NZONAR,RMLT,NMLTAR,IOUT,
     &                                     I,J,NUO,HKO,HKCCO)
          IF(J.LT.NC) THEN
            IBDJ1 = IBOUND(J+1,I,K)
          ELSE
            IBDJ1 = 0
          ENDIF
          IF(I.LT.NR) THEN
            IBDI1 = IBOUND(J,I+1,K)
          ELSE
            IBDI1 = 0
          ENDIF
          HI1 = ZERO
          TOPI1 = ZERO
          IF(IBDI1.NE.0) THEN
            HI1 = HN(J,I+1,K)
            TOPI1 = BOTM(J,I+1,LBOTM(K)-1)
            BI1 = BOTM(J,I+1,LBOTM(K))
            THI1 = HI1 - BI1
            TCI1 = HKCC(J,I+1,K) * THI1
            AI1 = SN(J,I+1,K)
            NUI1 = 0
            HSING = HI1
            IF(TOPI1.GT.HI1) CALL SSEN1HUF1HDFND(HUFTHK,NC,NR,NHUF,
     &                                           HSING,I+1,J,NUI1)
            IF(NUI1.NE.0) CALL SSEN1HUF1HKFND(NC,NR,NHUF,HUFTHK,IZON,
     &                                        NZONAR,RMLT,NMLTAR,IOUT,
     &                                        I+1,J,NUI1,HKI1,HKCCI1)
          ENDIF
          HJ1 = ZERO
          TOPJ1 = ZERO
          IF(IBDJ1.NE.0) THEN
            HJ1 = HN(J+1,I,K)
            TOPJ1 = BOTM(J+1,I,LBOTM(K)-1)
            BJ1 = BOTM(J+1,I,LBOTM(K))
            THJ1 = HJ1 - BJ1
            TRJ1 = HK(J+1,I,K) * THJ1
            AJ1 = SN(J+1,I,K)
            NUJ1 = 0
            HSING = HJ1
            IF(TOPJ1.GT.HJ1) CALL SSEN1HUF1HDFND(HUFTHK,NC,NR,NHUF,
     &                                           HSING,I,J+1,NUJ1)
            IF(NUJ1.NE.0) CALL SSEN1HUF1HKFND(NC,NR,NHUF,HUFTHK,IZON,
     &                                        NZONAR,RMLT,NMLTAR,IOUT,
     &                                        I,J+1,NUJ1,HKJ1,HKCCJ1)
          ENDIF
C-------MATRIX DERIVATIVES
          COCR = ZERO
          COCC = ZERO
          IF(J.LT.NC .AND. IBDJ1.GT.0) THEN
            D1CR = ZERO
            D2CR = ZERO
            IF (TOPO.GT.HO)
     &        D1CR = (CR(J,I,K)**2)*DELR(J)*HKO/
     &               (DELC(I)*2.*TRO**2.)
            IF (TOPJ1.GT.HJ1)
     &        D2CR = (CR(J,I,K)**2)*DELR(J+1)*HKJ1/
     &               (DELC(I)*2.*TRJ1**2.)
            COCR = D1CR*AO + D2CR*AJ1
            RHS(J,I,K) = RHS(J,I,K) - COCR*(HJ1-HO)
            RHS(J+1,I,K) = RHS(J+1,I,K) - COCR*(HO-HJ1)
          ENDIF
          IF(I.LT.NR .AND. IBDI1.GT.0) THEN
            D1CC = ZERO
            D2CC = ZERO
            IF (TOPO.GT.HO)
     &          D1CC = (CC(J,I,K)**2)*DELC(I)*HKCCO/
     &                 (2.*DELR(J)*TCO**2.)
            IF (TOPI1.GT.HI1)
     &          D2CC = (CC(J,I,K)**2)*DELC(I+1)*HKCCI1/
     &                 (2.*DELR(J)*TCI1**2.)
            COCC = D1CC*AO + D2CC*AI1
            RHS(J,I,K) = RHS(J,I,K) - COCC*(HI1-HO)
            RHS(J,I+1,K) = RHS(J,I+1,K) - COCC*(HO-HI1)
          ENDIF
   10   CONTINUE
   20 CONTINUE
C
C      CV
C
      IF (NL.GT.1.AND.K.LT.NL) THEN
        DO 60 J = 1, NC
          DO 50 I = 1, NR
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J,I,K+1).EQ.0) GOTO 50
            HO = HN(J,I,K)
            HP = HN(J,I,K+1)
            TOP1 = ZERO
            IF (LT.NE.0) TOP1 = BOTM(J,I,LBOTM(K)-1)
            IF (TOP1.LT.HO) GOTO 50
            AO = SN(J,I,K)
            BO = BOTM(J,I,LBOTM(K))
            MID = 0.5*(HO + BO)
C-------FIRST, FIND UNIT THAT LAYER MID POINT IS IN
            CALL SSEN1HUF1HDFND(HUFTHK,NC,NR,NHUF,MID,I,J,NNU)
C-------UNIT NOT FOUND, SKIP CALCULATIONS
            IF(NNU.EQ.0) GOTO 50
   80       CONTINUE
C-------NOW FIND THE KV OF THE UNIT
            HUFTMP(NNU)=0.0
            CALL SGWF1HUF1POP(HUFTMP,'VK  ',NC,NR,NHUF,I,J,HUFTHK,
     &                        IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
            CALL SGWF1HUF1POP(HUFTMP,'VANI',NC,NR,NHUF,I,J,HUFTHK,
     &                        IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
            IF(HGUVANI(NNU).EQ.0.) THEN
              VK1=HUFTMP(NNU)
            ELSE
              IF(HUFTMP(NNU).EQ.0.) THEN
                VK1 = HGUVANI(NNU)
              ELSE
                VK1 = HUFTMP(NNU)
              ENDIF
              HUFTMP(NNU)=0.0
              CALL SGWF1HUF1POP(HUFTMP,'HK  ',NC,NR,NHUF,I,J,HUFTHK,
     &                          IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
              VK1=HUFTMP(NNU)/VK1
            ENDIF
C-------MATRIX DERIVATIVES
            D1CV=0.
            IF (TOP1.GT.HO)
     &          D1CV = -(CV(J,I,K)**2)/(DELC(I)*2.*DELR(J)*VK1)
C-------MULTIPLY BY DERIVATIVES FROM LAST ITERATION
            CO = ZERO
            CO = D1CV*AO
C-------MULTIPLY BY HEAD VECTOR AND ADD TO RHS
            RHS(J,I,K) = RHS(J,I,K) - CO*(HP-HO)
            RHS(J,I,K+1) = RHS(J,I,K+1) - CO*(HO-HP)
   50     CONTINUE
   60   CONTINUE
      ENDIF
      RETURN
      END
c======================================================================
      SUBROUTINE SEN1HUF1SC2(TOP,BOT,TOPU,BOTU,HN,HO,CRHS,CO)
C
C     ******************************************************************
C     Compute contributions to RHS for sensitivities for convertible cell
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL TOPU, BOTU, THCKU, TOP, BOT, CHCOF, CRHS, AREA
C
      CRHS = 0.0

      IF(TOPU.GT.TOP) TOPU=TOP
      IF(BOTU.LT.BOT) BOTU=BOT
C-----Compute contributions for this unit to flow in layer
      IF(HO.GE.TOP) THEN
C-------Layer converts, water table is coming down
        IF(HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C---------New head is in this unit
          CRHS=CRHS+CO*(TOPU-HN)
        ELSEIF(HN.LE.BOTU) THEN
C---------New head is below this unit
          CRHS=CRHS+CO*(TOPU-BOTU)
        ENDIF
      ELSEIF(HN.GE.TOP) THEN
C-------Layer converts, water table is going up
        IF(HO.LE.TOPU.AND.HO.GE.BOTU) THEN
C---------Old head is in this unit
          CRHS=CRHS+CO*(HO-TOPU)
        ELSEIF(HO.LE.BOTU) THEN
C---------Old head is below this unit
          CRHS=CRHS+CO*(BOTU-TOPU)
        ENDIF
      ELSEIF(HO.LE.TOP.AND.HN.LE.TOP) THEN
C-------Layer does not convert, just use SC2
        IF(HO.GE.HN) THEN
C---------Water table is coming down
          IF(HO.LE.TOPU.AND.HO.GE.BOTU .AND.
     &       HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C------------Old and new heads are both in this unit
            CRHS=CRHS+CO*(HO-HN)
          ELSEIF(HO.LE.TOPU.AND.HO.GE.BOTU) THEN
C-----------Old head is in this unit
            CRHS=CRHS+CO*(HO-BOTU)
          ELSEIF(HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C-----------New head is in this unit
            CRHS=CRHS+CO*(TOPU-HN)
          ELSEIF(HO.GE.TOPU.AND.HN.LE.BOTU) THEN
C-----------Old head is above and new head is below this unit
            CRHS=CRHS+CO*(TOPU-BOTU)
          ENDIF
        ELSE
C---------Water table is going up
          IF(HO.LE.TOPU.AND.HO.GE.BOTU .AND.
     &       HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C-----------Old and new heads are both in this unit
            CRHS=CRHS+CO*(HO-HN)
          ELSEIF(HO.LE.TOPU.AND.HO.GE.BOTU) THEN
C-----------Old head is in this unit
            CRHS=CRHS+CO*(HO-TOPU)
          ELSEIF(HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C-----------New head is in this unit
            CRHS=CRHS+CO*(BOTU-HN)
          ELSEIF(HO.LE.BOTU.AND.HN.GE.TOPU) THEN
C-----------Old head is below and new head is above this unit
            CRHS=CRHS+CO*(BOTU-TOPU)
          ENDIF
        ENDIF
      ENDIF
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF1HKFND(NCOL,NROW,NHUF,HUFTHK,IZON,NZONAR,
     &  RMLT,NMLTAR,IOUT,I,J,NNU,HKCR,HKCC)
C
C     ******************************************************************
C     Find the horizontal hydraulic conductivity of a hydrogeologic unit.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION RMLT(NCOL,NROW,NMLTAR),
     &          IZON(NCOL,NROW,NZONAR), HUFTMP(200),
     &          HUFTHK(NCOL,NROW,NHUF,2)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
      HUFTMP(NNU)=0.0
      CALL SGWF1HUF1POP(HUFTMP,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                  IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
      HKCR = HUFTMP(NNU)
      HUFTMP(NNU)=0.0
      CALL SGWF1HUF1POP(HUFTMP,'HANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                  IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
      IF(HGUHANI(NNU).GT.0..AND.HUFTMP(NNU).EQ.0.) THEN
        HANI = HGUHANI(NNU)
      ELSE
        HANI = HUFTMP(NNU)
      ENDIF
      HKCC = HKCR * HANI
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF1HDFND(HUFTHK,NCOL,NROW,NHUF,HD,I,J,NNU)
C
C     ******************************************************************
C     Find the hydrogeologic unit that an elevation is in.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL HUFTHK(NCOL,NROW,NHUF,2)
C
C-------FIRST, FIND UNIT THAT HEAD IS IN
      NNU = 0
      DO 70 NU = 1,NHUF
        TOPU=HUFTHK(J,I,NU,1)
        THCKU=HUFTHK(J,I,NU,2)
        IF(ABS(THCKU).LT.1E-4) GOTO 70
        BOTU=TOPU-THCKU
        IF(BOTU.LT.HD .AND. HD.LT.TOPU) THEN
          NNU = NU
          GOTO 80
        ENDIF
   70 CONTINUE
C
C4------RETURN
   80 RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF1THK(TOP,BOT,TOPU,THKU,THCK)
C
C     ******************************************************************
C     Determine contributing thicknesses of hydrogeologic units.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C      DOUBLE PRECISION HNEW
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C
C
      TOPL=TOP
      BOTU=TOPU-THKU
      IF(TOPU.LE.BOT.OR.BOTU.GE.TOPL) THEN
cc        THCK=0
        THCK=0.0    ! changed 5/29/02 - erb
      ELSE
        TOP1=TOPU
        BOT1=BOTU
        IF(TOPU.GT.TOPL) TOP1=TOPL
        IF(BOTU.LT.BOT) BOT1=BOT
        THCK=TOP1-BOT1
      ENDIF
      IF(ABS(THCK).LT.1E-4) THCK=0.
C
C4------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1CH(CO,TH0,TH1,HP,I,J,K,CHAR,RMLT0,
     &                  HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,NZ,
     &                  NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,
     &                  HUFTHK,NHUF,NU)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1, TOP, U, V, ZERO
      INTEGER I, II, IJ, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*2 CHAR
      DIMENSION HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL), 
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),HUFTMP(200),
     & HUFTHK(NCOL,NROW,NHUF,2)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      C = ZERO
      DR = DELR(J)
      DC = DELC(I)
      II = 0
      IJ = 0
      AN0 = 1.0
      AN1 = 1.0
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      IF (CHAR.EQ.'CR') IJ = 1
      IF (CHAR.EQ.'CC') II = 1
      RMLT1 = 1.
      IF (NZ.GT.0) THEN
        RMLT1=0.
        DO 30 JJ = 5,IPCLST(4,ICL)
          IF(IZON(J+IJ,I+II,NZ).EQ.IPCLST(JJ,ICL)) THEN
            IF(NM.GT.0) THEN
              RMLT1=RMLT(J+IJ,I+II,NM)
            ELSE
              RMLT1=1.
            ENDIF
          END IF
   30   CONTINUE
      ELSEIF(NM.GT.0) THEN
        RMLT1=RMLT(J+IJ,I+II,NM)
      ENDIF
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      IF (CHAR.EQ.'CR') THEN
        HP = H(IND+1)
        FAC = 2.*DC
        D0 = DR
        D1 = DELR(J+1)
        HK0=HK(J,I,K)
        HK1=HK(J+IJ,I+II,K)
      ELSEIF (CHAR.EQ.'CC') THEN
        HP = H(IND+NCOL)
        FAC = 2.*DR
        D0 = DC
        D1 = DELC(I+1)
        HK0=HKCC(J,I,K)
        HK1=HKCC(J+IJ,I+II,K)
        HUFTMP(NU) = 0.0
        CALL SGWF1HUF1POP(HUFTMP,'HANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
        IF(HGUHANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.) THEN
          AN0 = HGUHANI(NU)
        ELSE
          AN0 = HUFTMP(NU)
        ENDIF
        HUFTMP(NU) = 0.0
        CALL SGWF1HUF1POP(HUFTMP,'HANI',NCOL,NROW,NHUF,I+II,J+IJ,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
        IF(HGUHANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.) THEN
          AN1 = HGUHANI(NU)
        ELSE
          AN1 = HUFTMP(NU)
        ENDIF
      ENDIF
      H0=H(IND)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J+IJ,I+II)
      TH1L=TOP1L-BOT(J+IJ,I+II)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J+IJ,I+II)
      T0 = HK0*TH0L
      T1 = HK1*TH1L
      DT0 = RMLT0*AN0*TH0
      DT1 = RMLT1*AN1*TH1
C U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C CONDUCTANCE TERM DIVIDED BY WHAT IS ALREADY IN FAC.
C DU AND DV ARE THEIR DERIVATIVES WITH RESPECT TO THE PARAMETER.
C UOV IS U DIVIDED BY V (U OVER V).
      U = T0*T1
      V = T0*D1 + T1*D0
      DU = T0*DT1 + T1*DT0
      DV = D1*DT0 + D0*DT1
      CO=0.          
C-----CHANGE VALUE TO MACHINE ZERO -- ASK STEVE
      IF(ABS(V).GT.1E-24) THEN
        C = FAC*U/V
        CO = FAC*(1./V**2)*(V*DU-U*DV)   
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1CHN(CO,TH0,TH1,HP,I,J,K,RMLT0,
     &  HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,NZ,
     &  NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,HUFTHK,NHUF,NU)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO HORIZONTAL ANISOTROPY, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1, TOP, U, V, ZERO
      INTEGER I, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      DIMENSION DELC(NROW), DELR(NCOL), HK(NCOL,NROW,NLAY),
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR), HUFTMP(200),
     & HUFTHK(NCOL,NROW,NHUF,2)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      C = ZERO
      DR = DELR(J)
      DC = DELC(I)
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      RMLT1 = 1.
      IF (NZ.GT.0) THEN
        RMLT1=0.
        DO 30 JJ = 5,IPCLST(4,ICL)
          IF(IZON(J,I+1,NZ).EQ.IPCLST(JJ,ICL)) THEN
            IF(NM.GT.0) THEN
              RMLT1=RMLT(J,I+1,NM)
            ELSE
              RMLT1=1.
            ENDIF
          END IF
   30   CONTINUE
      ELSEIF(NM.GT.0) THEN
        RMLT1=RMLT(J,I+1,NM)
      ENDIF
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      H0=H(IND)
      HP = H(IND+NCOL)
      FAC = 2.*DR
      D0 = DC
      D1 = DELC(I+1)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J,I+1)
      TH1L=TOP1L-BOT(J,I+1)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J,I+1)
      T0 = HKCC(J,I,K)*TH0L
      T1 = HKCC(J,I+1,K)*TH1L
C--Get horizontal hydraulic conductivity of this unit
      CALL SSEN1HUF1HKFND(NCOL,NROW,NHUF,HUFTHK,IZON,
     &                    NZONAR,RMLT,NMLTAR,IOUT,
     &                    I,J,NU,HKU0,HKCCUO)
      CALL SSEN1HUF1HKFND(NCOL,NROW,NHUF,HUFTHK,IZON,
     &                    NZONAR,RMLT,NMLTAR,IOUT,
     &                    I+1,J,NU,HKU1,HKCCU1)
      DT0 = HKU0*RMLT0*TH0
      DT1 = HKU1*RMLT1*TH1
C U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C CONDUCTANCE TERM DIVIDED BY WHAT IS ALREADY IN FAC.
C DU AND DV ARE THEIR DERIVATIVES WITH RESPECT TO THE PARAMETER.
C UOV IS U DIVIDED BY V (U OVER V).
      U = T0*T1
      V = T0*D1 + T1*D0
      DU = T0*DT1 + T1*DT0
      DV = D1*DT0 + D0*DT1
      CO=0.          
C-----CHANGE VALUE TO MACHINE ZERO -- ASK STEVE
      IF(ABS(V).GT.1E-24) THEN
        C = FAC*U/V
        CO = FAC*(1./V**2)*(V*DU-U*DV)   
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1CV(PID,CO,THK1,IBP,IBM,NCOL,NROW,NLAY,CV,
     &                    DELR,DELC,J,I,K,IBOUND,IP,NU,NZ,IZON,
     &                    NZONAR,RMLT0,RMLT,NMLTAR,IOUT,HUFTHK,NHUF)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE VERTICAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CV, DELC, DELR, ZERO
      INTEGER I, IBM, IBOUND, IBP, J, K, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*4 PID
      DIMENSION DELC(NROW), DELR(NCOL), RMLT(NCOL,NROW,NMLTAR),
     &          IBOUND(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          IZON(NCOL,NROW,NZONAR), HUFTMP(200),
     &          HUFTHK(NCOL,NROW,NHUF,2)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      IBP = 0
      IBM = 0
      CO = ZERO
      IF(K.GT.1) IBM=IBOUND(J,I,K-1)
      IBP=IBOUND(J,I,K+1)
      IF (IBOUND(J,I,K).EQ.0 .OR. IBP.EQ.0) RETURN
      IF(PID.EQ.'VK') THEN
C---Vertical hydraulic conductivity
C-----First, get (additive) KV for this unit
        HUFTMP(NU)=0.0
        CALL SGWF1HUF1POP(HUFTMP,'VK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
        COD = DELR(J)*DELC(I)*(HUFTMP(NU))**2.
        CO = (CV(J,I,K)**2)*RMLT0*THK1/COD
      ELSE
C---Vertical Anisotropy
        IF(PID.EQ.'VANI') THEN
C-----First, get (additive) KH for this unit
          HUFTMP(NU)=0.0
          CALL SGWF1HUF1POP(HUFTMP,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          COD=DELR(J)*DELC(I)*(HUFTMP(NU)/(RMLT0*B(IP)))**2.
          CO=-(CV(J,I,K)**2)*THK1*RMLT0*(HUFTMP(NU)/
     &          (RMLT0*B(IP))**2)/COD
C---Horizontal hydraulic conductivity
        ELSEIF(PID.EQ.'HK') THEN
C-----First, get VANI for this unit
          HUFTMP(NU)=0.0
          CALL SGWF1HUF1POP(HUFTMP,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          IF(HGUVANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.)
     &          HUFTMP(NU) = HGUVANI(NU)
          COD = DELR(J)*DELC(I)*(RMLT0*B(IP)/(HUFTMP(NU)))**2.
          CO = ((CV(J,I,K)**2)*THK1*RMLT0/(HUFTMP(NU)))/COD
        ENDIF
      ENDIF

      RETURN
      END


