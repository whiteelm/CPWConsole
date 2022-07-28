        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 20 12:49:44 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DGEFA__genmod
          INTERFACE 
            SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
              INTEGER(KIND=4) :: LDA
              REAL(KIND=8) :: A(LDA,1)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IPVT(1)
              INTEGER(KIND=4) :: INFO
            END SUBROUTINE DGEFA
          END INTERFACE 
        END MODULE DGEFA__genmod
