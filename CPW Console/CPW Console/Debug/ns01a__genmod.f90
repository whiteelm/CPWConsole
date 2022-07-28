        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 20 12:49:44 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NS01A__genmod
          INTERFACE 
            SUBROUTINE NS01A(N,X,F,AJINV,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W,&
     &CALFUN)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(1)
              REAL(KIND=8) :: F(1)
              REAL(KIND=8) :: AJINV(82,82)
              REAL(KIND=8) :: DSTEP
              REAL(KIND=8) :: DMAX
              REAL(KIND=8) :: ACC
              INTEGER(KIND=4) :: MAXFUN
              INTEGER(KIND=4) :: IPRINT
              REAL(KIND=8) :: W(1)
              EXTERNAL CALFUN
            END SUBROUTINE NS01A
          END INTERFACE 
        END MODULE NS01A__genmod
