C                                                                       EIGE  10

C     ..................................................................EIGE  20

C                                                                       EIGE  30

C        SUBROUTINE EIGEN                                               EIGE  40

C                                                                       EIGE  50

C        PURPOSE                                                        EIGE  60

C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC    EIGE  70

C           MATRIX                                                      EIGE  80

C                                                                       EIGE  90

C        USAGE                                                          EIGE 100

C           CALL EIGEN(A,R,N,MV)                                        EIGE 110

C                                                                       EIGE 120

C        DESCRIPTION OF PARAMETERS                                      EIGE 130

C           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.  EIGE 140

C               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF      EIGE 150

C               MATRIX A IN DESCENDING ORDER.                           EIGE 160

C           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,    EIGE 170

C               IN SAME SEQUENCE AS EIGENVALUES)                        EIGE 180

C           N - ORDER OF MATRICES A AND R                               EIGE 190

C           MV- INPUT CODE                                              EIGE 200

C                   0   COMPUTE EIGENVALUES AND EIGENVECTORS            EIGE 210

C                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE         EIGE 220

C                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING    EIGE 230

C                       SEQUENCE)                                       EIGE 240

C                                                                       EIGE 250

C        REMARKS                                                        EIGE 260

C           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)   EIGE 270

C           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R         EIGE 280

C                                                                       EIGE 290

C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  EIGE 300

C           NONE                                                        EIGE 310

C                                                                       EIGE 320

C        METHOD                                                         EIGE 330

C           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED     EIGE 340

C           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICALEIGE 350

C           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND    EIGE 360

C           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7   EIGE 370

C                                                                       EIGE 380

C     ..................................................................EIGE 390

C                                                                       EIGE 400

      SUBROUTINE EIGEN(A,R,N,MV)                                        EIGE 410

      DIMENSION A(1),R(1)                                               EIGE 420
C                                                                       EIGE 430

C        ...............................................................EIGE 440

C                                                                       EIGE 450

C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  EIGE 460

C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      EIGE 470

C        STATEMENT WHICH FOLLOWS.                                       EIGE 480

C                                                                       EIGE 490

      DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,         EIGE 500

     1                 COSX2,SINCS,RANGE                                EIGE 510

C                                                                       EIGE 520

C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS    EIGE 530

C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS      EIGE 540

C        ROUTINE.                                                       EIGE 550

C                                                                       EIGE 560

C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO      EIGE 570

C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTSEIGE 580

C        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT EIGE 590

C        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD EIGE 600

C        BE CHANGED TO 1.0D-12.                                         EIGE 610

C                                                                       EIGE 620

C        ...............................................................EIGE 630

C                                                                       EIGE 640

C        GENERATE IDENTITY MATRIX                                       EIGE 650

C                                                                       EIGE 660

    5 RANGE=1.0D-12                                                     EIGE 670

      IF(MV-1) 10,25,10                                                 EIGE 680

   10 IQ=-N                                                             EIGE 690

      DO 20 J=1,N                                                       EIGE 700

      IQ=IQ+N                                                           EIGE 710

      DO 20 I=1,N                                                       EIGE 720

      IJ=IQ+I                                                           EIGE 730

      R(IJ)=0.0                                                         EIGE 740

      IF(I-J) 20,15,20                                                  EIGE 750

   15 R(IJ)=1.0                                                         EIGE 760

   20 CONTINUE                                                          EIGE 770

C                                                                       EIGE 780

C        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)             EIGE 790

C                                                                       EIGE 800

   25 ANORM=0.0                                                         EIGE 810

      DO 35 I=1,N                                                       EIGE 820

      DO 35 J=I,N                                                       EIGE 830

      IF(I-J) 30,35,30                                                  EIGE 840

   30 IA=I+(J*J-J)/2                                                    EIGE 850

      ANORM=ANORM+A(IA)*A(IA)                                           EIGE 860

   35 CONTINUE                                                          EIGE 870

      IF(ANORM) 165,165,40                                              EIGE 880

   40 ANORM=1.414*DSQRT(ANORM)                                          EIGE 890

      ANRMX=ANORM*RANGE/FLOAT(N)                                        EIGE 900

C                                                                       EIGE 910

C        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR               EIGE 920

C                                                                       EIGE 930

      IND=0                                                             EIGE 940

      THR=ANORM                                                         EIGE 950

   45 THR=THR/FLOAT(N)                                                  EIGE 960

   50 L=1                                                               EIGE 970

   55 M=L+1                                                             EIGE 980

C                                                                       EIGE 990

C        COMPUTE SIN AND COS                                            EIGE1000

C                                                                       EIGE1010

   60 MQ=(M*M-M)/2                                                      EIGE1020

      LQ=(L*L-L)/2                                                      EIGE1030

      LM=L+MQ                                                           EIGE1040

   62 IF( DABS(A(LM))-THR) 130,65,65                                    EIGE1050

   65 IND=1                                                             EIGE1060

      LL=L+LQ                                                           EIGE1070

      MM=M+MQ                                                           EIGE1080

      X=0.5*(A(LL)-A(MM))                                               EIGE1090

   68 Y=-A(LM)/ DSQRT(A(LM)*A(LM)+X*X)                                  EIGE1100

      IF(X) 70,75,75                                                    EIGE1110

   70 Y=-Y                                                              EIGE1120

   75 SINX=Y/ DSQRT(2.0*(1.0+( DSQRT(1.0-Y*Y))))                        EIGE1130

      SINX2=SINX*SINX                                                   EIGE1140

   78 COSX= DSQRT(1.0-SINX2)                                            EIGE1150

      COSX2=COSX*COSX                                                   EIGE1160

      SINCS =SINX*COSX                                                  EIGE1170

C                                                                       EIGE1180

C        ROTATE L AND M COLUMNS                                         EIGE1190

C                                                                       EIGE1200

      ILQ=N*(L-1)                                                       EIGE1210

      IMQ=N*(M-1)                                                       EIGE1220

      DO 125 I=1,N                                                      EIGE1230

      IQ=(I*I-I)/2                                                      EIGE1240

      IF(I-L) 80,115,80                                                 EIGE1250

   80 IF(I-M) 85,115,90                                                 EIGE1260

   85 IM=I+MQ                                                           EIGE1270

      GO TO 95                                                          EIGE1280

   90 IM=M+IQ                                                           EIGE1290

   95 IF(I-L) 100,105,105                                               EIGE1300

  100 IL=I+LQ                                                           EIGE1310

      GO TO 110                                                         EIGE1320

  105 IL=L+IQ                                                           EIGE1330

  110 X=A(IL)*COSX-A(IM)*SINX                                           EIGE1340

      A(IM)=A(IL)*SINX+A(IM)*COSX                                       EIGE1350

      A(IL)=X                                                           EIGE1360

  115 IF(MV-1) 120,125,120                                              EIGE1370

  120 ILR=ILQ+I                                                         EIGE1380

      IMR=IMQ+I                                                         EIGE1390

      X=R(ILR)*COSX-R(IMR)*SINX                                         EIGE1400

      R(IMR)=R(ILR)*SINX+R(IMR)*COSX                                    EIGE1410

      R(ILR)=X                                                          EIGE1420

  125 CONTINUE                                                          EIGE1430

      X=2.0*A(LM)*SINCS                                                 EIGE1440

      Y=A(LL)*COSX2+A(MM)*SINX2-X                                       EIGE1450

      X=A(LL)*SINX2+A(MM)*COSX2+X                                       EIGE1460

      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)                     EIGE1470

      A(LL)=Y                                                           EIGE1480

      A(MM)=X                                                           EIGE1490

C                                                                       EIGE1500

C        TESTS FOR COMPLETION                                           EIGE1510

C                                                                       EIGE1520

C        TEST FOR M = LAST COLUMN                                       EIGE1530

C                                                                       EIGE1540

  130 IF(M-N) 135,140,135                                               EIGE1550

  135 M=M+1                                                             EIGE1560

      GO TO 60                                                          EIGE1570

C                                                                       EIGE1580

C        TEST FOR L = SECOND FROM LAST COLUMN                           EIGE1590

C                                                                       EIGE1600

  140 IF(L-(N-1)) 145,150,145                                           EIGE1610

  145 L=L+1                                                             EIGE1620

      GO TO 55                                                          EIGE1630

  150 IF(IND-1) 160,155,160                                             EIGE1640

  155 IND=0                                                             EIGE1650

      GO TO 50                                                          EIGE1660

C                                                                       EIGE1670

C        COMPARE THRESHOLD WITH FINAL NORM                              EIGE1680

C                                                                       EIGE1690

  160 IF(THR-ANRMX) 165,165,45                                          EIGE1700

C                                                                       EIGE1710

C        SORT EIGENVALUES AND EIGENVECTORS                              EIGE1720

C                                                                       EIGE1730

  165 IQ=-N                                                             EIGE1740

      DO 185 I=1,N                                                      EIGE1750

      IQ=IQ+N                                                           EIGE1760

      LL=I+(I*I-I)/2                                                    EIGE1770

      JQ=N*(I-2)                                                        EIGE1780

      DO 185 J=I,N                                                      EIGE1790

      JQ=JQ+N                                                           EIGE1800

      MM=J+(J*J-J)/2                                                    EIGE1810

      IF(A(LL)-A(MM)) 170,185,185                                       EIGE1820

  170 X=A(LL)                                                           EIGE1830

      A(LL)=A(MM)                                                       EIGE1840

      A(MM)=X                                                           EIGE1850

      IF(MV-1) 175,185,175                                              EIGE1860

  175 DO 180 K=1,N                                                      EIGE1870

      ILR=IQ+K                                                          EIGE1880

      IMR=JQ+K                                                          EIGE1890

      X=R(ILR)                                                          EIGE1900

      R(ILR)=R(IMR)                                                     EIGE1910

  180 R(IMR)=X                                                          EIGE1920

  185 CONTINUE                                                          EIGE1930

      RETURN                                                            EIGE1940

      END                                                               EIGE1950
