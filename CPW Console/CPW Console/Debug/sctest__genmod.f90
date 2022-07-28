        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 27 10:59:09 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SCTEST__genmod
          INTERFACE 
            SUBROUTINE SCTEST(ERREST,N,C,Z,WC,W,BETAM,NPTSQ,QWORK)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: ERREST
              COMPLEX(KIND=8) :: C
              COMPLEX(KIND=8) :: Z(N)
              COMPLEX(KIND=8) :: WC
              COMPLEX(KIND=8) :: W(N)
              REAL(KIND=8) :: BETAM(N)
              INTEGER(KIND=4) :: NPTSQ
              REAL(KIND=8) :: QWORK(1)
            END SUBROUTINE SCTEST
          END INTERFACE 
        END MODULE SCTEST__genmod
