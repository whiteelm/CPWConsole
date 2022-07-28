        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 27 10:59:09 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WSC__genmod
          INTERFACE 
            FUNCTION WSC(ZZ,KZZ,Z0,W0,K0,N,C,Z,BETAM,NPTSQ,QWORK)
              INTEGER(KIND=4) :: N
              COMPLEX(KIND=8) :: ZZ
              INTEGER(KIND=4) :: KZZ
              COMPLEX(KIND=8) :: Z0
              COMPLEX(KIND=8) :: W0
              INTEGER(KIND=4) :: K0
              COMPLEX(KIND=8) :: C
              COMPLEX(KIND=8) :: Z(N)
              REAL(KIND=8) :: BETAM(N)
              INTEGER(KIND=4) :: NPTSQ
              REAL(KIND=8) :: QWORK(1)
              COMPLEX(KIND=8) :: WSC
            END FUNCTION WSC
          END INTERFACE 
        END MODULE WSC__genmod
