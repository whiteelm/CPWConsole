        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 20 12:49:44 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE STEP__genmod
          INTERFACE 
            SUBROUTINE STEP(X,Y,F,NEQN,H,EPS,WT,START,HOLD,K,KOLD,CRASH,&
     &PHI,P,YP,PSI,ALPHA,BETA,SIG,V,W,G,PHASE1,NS,NORND)
              INTEGER(KIND=4) :: NEQN
              REAL(KIND=8) :: X
              REAL(KIND=8) :: Y(NEQN)
              EXTERNAL F
              REAL(KIND=8) :: H
              REAL(KIND=8) :: EPS
              REAL(KIND=8) :: WT(NEQN)
              LOGICAL(KIND=4) :: START
              REAL(KIND=8) :: HOLD
              INTEGER(KIND=4) :: K
              INTEGER(KIND=4) :: KOLD
              LOGICAL(KIND=4) :: CRASH
              REAL(KIND=8) :: PHI(NEQN,16)
              REAL(KIND=8) :: P(NEQN)
              REAL(KIND=8) :: YP(NEQN)
              REAL(KIND=8) :: PSI(12)
              REAL(KIND=8) :: ALPHA(12)
              REAL(KIND=8) :: BETA(12)
              REAL(KIND=8) :: SIG(13)
              REAL(KIND=8) :: V(12)
              REAL(KIND=8) :: W(12)
              REAL(KIND=8) :: G(13)
              LOGICAL(KIND=4) :: PHASE1
              INTEGER(KIND=4) :: NS
              LOGICAL(KIND=4) :: NORND
            END SUBROUTINE STEP
          END INTERFACE 
        END MODULE STEP__genmod
