C                                                                       NROO  10

C     ..................................................................NROO  20

C                                                                       NROO  30

C        SUBROUTINE NROOT                                               NROO  40

C                                                                       NROO  50

C        PURPOSE                                                        NROO  60

C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL NONSYMMETRIC NROO  70

C           MATRIX OF THE FORM B-INVERSE TIMES A.  THIS SUBROUTINE IS   NROO  80

C           NORMALLY CALLED BY SUBROUTINE CANOR IN PERFORMING A         NROO  90

C           CANONICAL CORRELATION ANALYSIS.                             NROO 100

C                                                                       NROO 110

C        USAGE                                                          NROO 120

C           CALL NROOT (M,A,B,XL,X)                                     NROO 130

C                                                                       NROO 140

C        DESCRIPTION OF PARAMETERS                                      NROO 150

C           M  - ORDER OF SQUARE MATRICES A, B, AND X.                  NROO 160

C           A  - INPUT MATRIX (M X M).                                  NROO 170

C           B  - INPUT MATRIX (M X M).                                  NROO 180

C           XL - OUTPUT VECTOR OF LENGTH M CONTAINING EIGENVALUES OF    NROO 190

C                B-INVERSE TIMES A.                                     NROO 200

C           X  - OUTPUT MATRIX (M X M) CONTAINING EIGENVECTORS COLUMN-  NROO 210

C                WISE.                                                  NROO 220

C                                                                       NROO 230

C        REMARKS                                                        NROO 240

C           NONE                                                        NROO 250

C                                                                       NROO 260

C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  NROO 270

C           EIGEN                                                       NROO 280

C                                                                       NROO 290

C        METHOD                                                         NROO 300

C           REFER TO W. W. COOLEY AND P. R. LOHNES, 'MULTIVARIATE PRO-  NROO 310

C           CEDURES FOR THE BEHAVIORAL SCIENCES', JOHN WILEY AND SONS,  NROO 320

C           1962, CHAPTER 3.                                            NROO 330

C                                                                       NROO 340

C     ..................................................................NROO 350

C                                                                       NROO 360

      SUBROUTINE NROOT (M,A,B,XL,X)                                     NROO 370

      DIMENSION A(1),B(1),XL(1),X(1)                                    NROO 380
	
C                                                                       NROO 390

C        ...............................................................NROO 400

C                                                                       NROO 410

C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  NROO 420

C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      NROO 430

C        STATEMENT WHICH FOLLOWS.                                       NROO 440

C                                                                       NROO 450

      DOUBLE PRECISION A,B,XL,X,SUMV                                    NROO 460

C                                                                       NROO 470

C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS    NROO 480

C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS      NROO 490

C        ROUTINE.                                                       NROO 500

C                                                                       NROO 510

C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO      NROO 520

C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTSNROO 530

C        110 AND 175 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT 110    NROO 540

C        MUST BE CHANGED TO DABS.                                       NROO 550

C                                                                       NROO 560

C        ...............................................................NROO 570

C                                                                       NROO 580

C     COMPUTE EIGENVALUES AND EIGENVECTORS OF B                         NROO 590

C                                                                       NROO 600

      K=1                                                               NROO 610

      DO 100 J=2,M                                                      NROO 620

      L=M*(J-1)                                                         NROO 630

      DO 100 I=1,J                                                      NROO 640

      L=L+1                                                             NROO 650

      K=K+1                                                             NROO 660

  100 B(K)=B(L)                                                         NROO 670

C                                                                       NROO 680

C        THE MATRIX B IS A REAL SYMMETRIC MATRIX.                       NROO 690

C                                                                       NROO 700

      MV=0                                                              NROO 710


      CALL EIGEN (B,X,M,MV)                                             NROO 720

C                                                                       NROO 730

C     FORM RECIPROCALS OF SQUARE ROOT OF EIGENVALUES.  THE RESULTS      NROO 740

C     ARE PREMULTIPLIED BY THE ASSOCIATED EIGENVECTORS.                 NROO 750

C                                                                       NROO 760

      L=0                                                               NROO 770

      DO 110 J=1,M                                                      NROO 780

      L=L+J                                                             NROO 790

  110 XL(J)=1.0/ DSQRT( DABS(B(L)))                                     NROO 800

      K=0                                                               NROO 810

      DO 115 J=1,M                                                      NROO 820

      DO 115 I=1,M                                                      NROO 830

      K=K+1                                                             NROO 840

  115 B(K)=X(K)*XL(J)                                                   NROO 850

C                                                                       NROO 860

C     FORM (B**(-1/2))PRIME * A * (B**(-1/2))                           NROO 870

C                                                                       NROO 880

      DO 120 I=1,M                                                      NROO 890

      N2=0                                                              NROO 900

      DO 120 J=1,M                                                      NROO 910

      N1=M*(I-1)                                                        NROO 920

      L=M*(J-1)+I                                                       NROO 930

      X(L)=0.0                                                          NROO 940

      DO 120 K=1,M                                                      NROO 950

      N1=N1+1                                                           NROO 960

      N2=N2+1                                                           NROO 970

  120 X(L)=X(L)+B(N1)*A(N2)                                             NROO 980

      L=0                                                               NROO 990

      DO 130 J=1,M                                                      NROO1000

      DO 130 I=1,J                                                      NROO1010

      N1=I-M                                                            NROO1020

      N2=M*(J-1)                                                        NROO1030

      L=L+1                                                             NROO1040

      A(L)=0.0                                                          NROO1050

      DO 130 K=1,M                                                      NROO1060

      N1=N1+M                                                           NROO1070

      N2=N2+1                                                           NROO1080

  130 A(L)=A(L)+X(N1)*B(N2)                                             NROO1090

C                                                                       NROO1100

C     COMPUTE EIGENVALUES AND EIGENVECTORS OF A                         NROO1110

C                                                                       NROO1120

      CALL EIGEN (A,X,M,MV)                                             NROO1130

      L=0                                                               NROO1140

      DO 140 I=1,M                                                      NROO1150

      L=L+I                                                             NROO1160

  140 XL(I)=A(L)                                                        NROO1170

C                                                                       NROO1180

C     COMPUTE THE NORMALIZED EIGENVECTORS                               NROO1190

C                                                                       NROO1200

      DO 150 I=1,M                                                      NROO1210

      N2=0                                                              NROO1220

      DO 150 J=1,M                                                      NROO1230

      N1=I-M                                                            NROO1240

      L=M*(J-1)+I                                                       NROO1250

      A(L)=0.0                                                          NROO1260

      DO 150 K=1,M                                                      NROO1270

      N1=N1+M                                                           NROO1280

      N2=N2+1                                                           NROO1290

  150 A(L)=A(L)+B(N1)*X(N2)                                             NROO1300

      L=0                                                               NROO1310

      K=0                                                               NROO1320

      DO 180 J=1,M                                                      NROO1330

      SUMV=0.0                                                          NROO1340

      DO 170 I=1,M                                                      NROO1350

      L=L+1                                                             NROO1360

  170 SUMV=SUMV+A(L)*A(L)                                               NROO1370

  175 SUMV= DSQRT(SUMV)                                                  NROO1380

      DO 180 I=1,M                                                      NROO1390

      K=K+1                                                             NROO1400

  180 X(K)=A(K)/SUMV                                                    NROO1410

      RETURN                                                            NROO1420

      END                                                               NROO1430
