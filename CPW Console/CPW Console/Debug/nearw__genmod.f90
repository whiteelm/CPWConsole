        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 27 10:59:09 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NEARW__genmod
          INTERFACE 
            SUBROUTINE NEARW(WW,ZN,WN,KN,N,Z,WC,W,BETAM)
              INTEGER(KIND=4) :: N
              COMPLEX(KIND=8) :: WW
              COMPLEX(KIND=8) :: ZN
              COMPLEX(KIND=8) :: WN
              INTEGER(KIND=4) :: KN
              COMPLEX(KIND=8) :: Z(N)
              COMPLEX(KIND=8) :: WC
              COMPLEX(KIND=8) :: W(N)
              REAL(KIND=8) :: BETAM(N)
            END SUBROUTINE NEARW
          END INTERFACE 
        END MODULE NEARW__genmod
