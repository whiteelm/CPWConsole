        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 27 10:59:09 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ZPROD__genmod
          INTERFACE 
            FUNCTION ZPROD(ZZ,KS,N,Z,BETAM)
              INTEGER(KIND=4) :: N
              COMPLEX(KIND=8) :: ZZ
              INTEGER(KIND=4) :: KS
              COMPLEX(KIND=8) :: Z(N)
              REAL(KIND=8) :: BETAM(N)
              COMPLEX(KIND=8) :: ZPROD
            END FUNCTION ZPROD
          END INTERFACE 
        END MODULE ZPROD__genmod
