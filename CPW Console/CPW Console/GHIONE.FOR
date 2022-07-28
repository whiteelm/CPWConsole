        SUBROUTINE GHIONE(X,C,N,M) 
      real*8 X(1), C(1), xi,sigma,          
     *       F(361),G1(361),U1(361),sumF,sumG,
     *       pi,co,pf1,pf2,pf3,pf4,d,xx
       pi=3.14159265359
      do 1 j=1,N
      do 1 i=1,N
        sumF=0.d0
        sumG=0.d0

            do 2 k=1,M
             co = dcos((2.*k-1.)*pi/(2.*M))
             xi    = (x(2*j+1)+ x(2*j))/2. + (x(2*j+1)- x(2*j))*co/2.
             sigma = (x(2*j)+ x(2*j-1))/2. + (x(2*j)- x(2*j-1))*co/2.
         pf1=1.
         do 11 mm=1,2*j-1
11       pf1=pf1*dsqrt(xi-x(mm))
             pf2=1.
             do 12 mm=2*j+2,2*N+2
12           pf2=pf2*dsqrt(x(mm)-xi)
         pf3=1.
         if(j.ne.1)then
         do 13 mm=1,2*j-2
13       pf3=pf3*dsqrt(sigma-x(mm))
         endif
              pf4=1.
              do 14 mm=2*j+1,2*N+2
14            pf4=pf4*dsqrt(x(mm)-sigma)

         sumF=sumF+   xi**(i-1)/(pf1*pf2)
2        sumG=sumG+sigma**(i-1)/(pf3*pf4)
        ij=(j-1)*N+i
        F(ij) =((-1)**(N+1-j)*pi/M)*sumF
1       G1(ij)=((-1)**(N+2-j)*pi/M)*sumG
        CALL DMINV(G1,N,D)
          do3 i=1,N
          do3 j=1,N
        xx=0.
        ij=(j-1)*N+i
            U1(ij)=0.
            if(i.eq.j)U1(ij)=1.
            if(i+1.eq.j)U1(ij)=-1.
          do4 k=1,N
        ik=(k-1)*N+i
        kj=(j-1)*N+k
4       xx=xx+G1(ik)*F(kj)
3       C(ij)=xx

          do7 i=1,N              
          do7 j=1,N
        ij=(j-1)*N+i
7       F(ij)=C(ij)

        do5 i=1,N
        do5 j=1,N
            xx=0.
            ij=(j-1)*N+i
         do6 k=1,N
             ik=(k-1)*N+i
             kj=(j-1)*N+k
6       xx=xx+U1(ik)*F(kj)
5       C(ij)=-xx
        return
        end