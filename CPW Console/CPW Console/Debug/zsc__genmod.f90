        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 27 10:59:09 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ZSC__genmod
          INTERFACE 
            FUNCTION ZSC(WW,IGUESS,ZINIT,Z0,W0,K0,EPS,IER,N,C,Z,WC,W,   &
     &BETAM,NPTSQ,QWORK)
              INTEGER(KIND=4) :: N
              COMPLEX(KIND=8) :: WW
              INTEGER(KIND=4) :: IGUESS
              COMPLEX(KIND=8) :: ZINIT
              COMPLEX(KIND=8) :: Z0
              COMPLEX(KIND=8) :: W0
              INTEGER(KIND=4) :: K0
              REAL(KIND=8) :: EPS
              INTEGER(KIND=4) :: IER
              COMPLEX(KIND=8) :: C
              COMPLEX(KIND=8) :: Z(N)
              COMPLEX(KIND=8) :: WC
              COMPLEX(KIND=8) :: W(N)
              REAL(KIND=8) :: BETAM(N)
              INTEGER(KIND=4) :: NPTSQ
              REAL(KIND=8) :: QWORK(1)
              COMPLEX(KIND=8) :: ZSC
            END FUNCTION ZSC
          END INTERFACE 
        END MODULE ZSC__genmod
