      subroutine qinit(n,betam,nptsq,qwork)

      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension qwork(1),betam(n)
      iscr = nptsq*(2*n+2) + 1
      do 1 k = 1,n
        inodes = nptsq*(k-1) + 1
        iwts = nptsq*(n+k) + 1
    1   if (betam(k).gt.-1.d0) call gaussj(nptsq,0.d0,betam(k),
     &    qwork(iscr),qwork(inodes),qwork(iwts))
      inodes = nptsq*n + 1
      iwts = nptsq*(2*n+1) + 1
      call gaussj(nptsq,0.d0,0.d0,
     &  qwork(iscr),qwork(inodes),qwork(iwts))
      return
      end


c*******************************************************************
c* scsolv                                     primary subroutine  **
c*******************************************************************
c
      subroutine scsolv(iprint,iguess,tol,errest,n,c,z,wc,
     &   w,betam,nptsq,qwork)
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
!
      common /param1/ kfix(82),krat(82),ncomp,nptsq2,c2,
     &  qwork2(2004),betam2(82),z2(82),wc2,w2(82)
      dimension z(n),w(n),betam(n),qwork(1)
      dimension ajinv(82,82),scr(10000),fval(82),y(82)
      external scfun
      nm = n-1
      call check(tol,n,w,betam)
      ncomp = 0
      do 1 k = 2,nm
        if (betam(k).gt.-1.d0) goto 1
        ncomp = ncomp + 1
        kfix(ncomp) = k - 1
        if (ncomp.eq.1) kfix(ncomp) = 1
    1 continue
      if (ncomp.gt.0) goto 2
      ncomp = 1
      kfix(ncomp) = 1
    2 continue
      neq = 2*ncomp
      do 3 k = 1,nm
        if (neq.eq.n-1) goto 4
        if (betam(k).le.-1.d0.or.betam(k+1).le.-1.d0) goto 3
        neq = neq + 1
        krat(neq) = k
    3 continue
    4 z(n) = (1.d0,0.d0)
      if (iguess.ne.0) goto 11
      do 5 k = 1,nm
    5   y(k) = 0.d0
      goto 12
   11 continue
      do 9 k = 1,nm
        km = k-1
        if (km.eq.0) km = n
        tmp1 = dimag(log(z(k+1)/z(k)))
        if (tmp1.lt.0.d0) tmp1 = tmp1 + 2.d0 * acos(-1.d0)
        tmp2 = dimag(log(z(k)/z(km)))
        if (tmp2.lt.0.d0) tmp2 = tmp2 + 2.d0 * acos(-1.d0)
    9   y(k) = log(tmp2) - log(tmp1)
   12 continue
      dstep = 1.d-6
      dmax = 20.d0
      maxfun = (n-1) * 15
      nptsq2 = nptsq
      wc2 = wc
      do 6 k = 1,n
        z2(k) = z(k)
        betam2(k) = betam(k)
    6   w2(k) = w(k)
      nwdim = nptsq * (2*n+3)
      do 7 i = 1,nwdim
    7 qwork2(i) = qwork(i)
      call ns01a(nm,y,fval,ajinv,dstep,dmax,tol,maxfun,
     &  iprint,scr,scfun)
c
c copy output data from /param1/:
      c = c2
      do 8 k = 1,nm
    8   z(k) = z2(k)
c
c print results and test accuracy:
      if (iprint.ge.0) call scoutp(n,c,z,wc,w,betam,nptsq)
      call sctest(errest,n,c,z,wc,w,betam,nptsq,qwork)
c      if (iprint.ge.-1) write (6,201) errest
  201 format (' errest:',e12.4/)
      return
c
      end


c*******************************************************************
c* wsc                                        primary subroutine  **
c*******************************************************************
c
      function wsc(zz,kzz,z0,w0,k0,n,c,z,betam,nptsq,qwork)
c
c computes forward map w(zz)
c
c calling sequence:
c
c   zz     point in the disk at which w(zz) is desired (input)
c
c   kzz    k if zz = z(k) for some k, otherwise 0 (input)
c
c   z0     nearby point in the disk at which w(z0) is known and
c          finite (input)
c
c   w0     w(z0)  (input)
c
c   k0     k if z0 = z(k) for some k, otherwise 0 (input)
c
c   n,c,z,betam,nptsq,qwork     as in scsolv (input)
c
c convenient values of z0, w0, and k0 for most applications can be
c supplied by subroutine nearz.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),betam(n),qwork(1)
c
      wsc = w0 + c * zquad(z0,k0,zz,kzz,n,z,betam,nptsq,qwork)
c
      return
      end


c*******************************************************************
c* zsc                                        primary subroutine  **
c*******************************************************************
c
      function zsc(ww,iguess,zinit,z0,w0,k0,eps,ier,n,c,
     &  z,wc,w,betam,nptsq,qwork)
c
c computes inverse map z(ww) by newton iteration
c
c calling sequence:
c
c   ww     point in the polygon at which z(ww) is desired (input)
c
c   iguess (input)
c          .eq.1 - initial guess is supplied as parameter zinit
c          .ne.1 - get initial guess from program ode (slower).
c                  for this the segment wc-ww must lie within
c                  the polygon.
c
c   zinit  initial guess if iguess.eq.1, otherwise ignored (input).
c          may not be a prevertex z(k)
c
c   z0     point in the disk near z(ww) at which w(z0) is known and
c          finite (input).
c
c   w0     w(z0)  (input).  the line segment from w0 to ww must
c          lie entirely within the closed polygon.
c
c   k0     k if z0 = z(k) for some k, otherwise 0 (input)
c
c   eps    desired accuracy in answer z(ww)  (input)
c
c   ier    error flag (input and output).
c          on input, give ier.ne.0 to suppress error messages.
c          on output, ier.ne.0 indicates unsuccessful computation --
c          try again with a better initial guess.
c
c   n,c,z,wc,w,betam,nptsq,qwork     as in scsolv (input)
c
c convenient values of z0, w0, and k0 for some applications can be
c supplied by subroutine nearw.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension scr(10000),iscr(82), y(2)
      dimension z(n),w(n),betam(n),qwork(1)
      external zfode
      logical odecal
      common /param2/ cdwdt,z2(82),betam2(82),n2

!      dimension scr(142),iscr(5)
!      dimension z(n),w(n),betam(n),qwork(1)
!      external zfode
!      logical odecal
!      common /param2/ cdwdt,z2(20),betam2(20),n2
c
      odecal = .false.
      if (iguess.ne.1) goto 1
      zi = zinit
      goto 3
c
c get initial guess zi from program ode:
    1 n2 = n
      do 2 k = 1,n
        z2(k) = z(k)
    2   betam2(k) = betam(k)
      zi = (0.d0,0.d0)
      t = 0.d0
      iflag = -1
      relerr = 0.d0
      abserr = 1.d-4
      cdwdt = (ww-wc)/c
      y(1) = real(zi)
      y(2) = imag(zi)
      call ode(zfode,2,y,t,1.d0,relerr,abserr,iflag,scr,iscr)
      zi = cmplx(y(1), y(2))
      if (iflag.ne.2.and.ier.eq.0) write (6,201) iflag
      odecal = .true.
c
c refine answer by newton iteration:
    3 continue
      do 4 iter = 1,10
        zfnwt = ww - wsc(zi,0,z0,w0,k0,n,c,z,betam,nptsq,qwork)
        zi = zi + zfnwt/(c*zprod(zi,0,n,z,betam))
        if (abs(zi).ge.1.1d0) zi = .5d0 * zi/abs(zi)
        if (abs(zfnwt).lt.eps) goto 5
    4   continue
      if (.not.odecal) goto 1
      if (ier.eq.0) write (6,202)
      ier = 1
    5 zsc = zi
c
  201 format (/' *** nonstandard return from ode in zsc: iflag =',i2/)
  202 format (/' *** possible error in zsc: no convergence in 10'/
     &      '     iterations.  may need a better initial guess zinit')
      return
      end


c*******************************************************************
c* zfode                             subordinate(zsc) subroutine  **
c*******************************************************************
c
      subroutine zfode(t,zz,zdzdt)
c
c computes the function zdzdt needed by ode in zsc.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      common /param2/ cdwdt,z(82),betam(82),n
!	common /param2/ cdwdt,z(20),betam(20),n
c
      zdzdt = cdwdt / zprod(zz,0,n,z,betam)
c
      return
      end


c*******************************************************************
c* check                          subordinate(scsolv) subroutine  **
c*******************************************************************
c
      subroutine check(eps,n,w,betam)
c
c checks geometry of the problem to make sure it is a form usable
c by scsolv.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension w(n),betam(n)
c
      sum = 0.d0
      do 1 k = 1,n
    1   sum = sum + betam(k)
      if (abs(sum+2.d0).lt.eps) goto 2
      write (6,301)
    2 if (betam(1).gt.-1.d0) goto 3
      write (6,302)
      stop
    3 if (betam(n).gt.-1.d0) goto 4
      write (6,303)
      stop
    4 if (abs(betam(n-1)).gt.eps) goto 5
      write (6,304)
      write (6,306)
    5 if (abs(betam(n-1)-1.d0).gt.eps) goto 6
      write (6,305)
      write (6,306)
      stop
    6 do 7 k = 2,n
        if (betam(k).le.-1.d0.or.betam(k-1).le.-1.d0) goto 7
        if (abs(w(k)-w(k-1)).le.eps) goto 8
    7   continue
      if (abs(w(1)-w(n)).gt.eps) goto 9
    8 write (6,307)
      stop
    9 if (n.ge.3) goto 10
      write (6,309)
      stop
   10 if (n.le.82) goto 11
!   10 if (n.le.20) goto 11
      write (6,310)
      stop
   11 continue
      return
c
  301 format (/' *** error in check: angles do not add up to 2'/)
  302 format (/' *** error in check: w(1) must be finite'/)
  303 format (/' *** error in check: w(n) must be finite'/)
  304 format (/' *** warning in check: w(n-1) not determined'/)
  305 format (/' *** error in check: w(n-1) not determined')
  306 format (/'   renumber vertices so that betam(n-1) is not 0 or 1')
  307 format (/' *** error in check: two adjacent vertices are equal'/)
  309 format (/' *** error in check: n must be no less than 3'/)
  310 format (/' *** error in check: n must be no more than 20'/)
      end


c*******************************************************************
c* yztran                         subordinate(scsolv) subroutine  **
c*******************************************************************
c
      subroutine yztran(n,y,z)
c
c transforms y(k) to z(k).  see comments in subroutine scsolv.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension y(n),z(n)
      nm = n - 1
      pi = acos(-1.d0)
c
      dth = 1.d0
      thsum = dth
      do 1 k = 1,nm
        dth = dth / exp(y(k))
    1   thsum = thsum + dth
c
      dth = 2.d0 * pi / thsum
      thsum = dth
      z(1) = dcmplx(cos(dth),sin(dth))
      do 2 k = 2,nm
        dth = dth / exp(y(k-1))
        thsum = thsum + dth
    2   z(k) = dcmplx(cos(thsum),sin(thsum))
c
      return
      end


c*******************************************************************
c* scfun                          subordinate(scsolv) subroutine  **
c*******************************************************************
      subroutine scfun(ndim,y,fval)
c
c this is the function whose zero must be found in scsolv.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension fval(ndim),y(ndim)
      common /param1/ kfix(82),krat(82),ncomp,nptsq,c,
     &  qwork(2004),betam(82),z(82),wc,w(82)

!      common /param1/ kfix(20),krat(20),ncomp,nptsq,c,
!	&  qwork(460),betam(20),z(20),wc,w(20)
      n = ndim+1
c
c transform y(k) to z(k):
      call yztran(n,y,z)
c
c set up: compute integral from 0 to z(n):
      wdenom = zquad((0.d0,0.d0),0,z(n),n,n,z,betam,nptsq,qwork)
      c = (w(n)-wc) / wdenom
c
c case 1: w(k) and w(k+1) finite:
c (compute integral along chord z(k)-z(k+1)):
      nfirst = 2*ncomp + 1
      if (nfirst.gt.n-1) goto 11
      do 10 neq = nfirst,ndim
        kl = krat(neq)
        kr = kl+1
        zint = zquad(z(kl),kl,z(kr),kr,n,z,betam,nptsq,qwork)
        fval(neq) = abs(w(kr)-w(kl)) - abs(c*zint)
   10 continue
c
c case 2: w(k+1) infinite:
c (compute contour integral along radius 0-z(k)):
   11 do 20 nvert = 1,ncomp
        kr = kfix(nvert)
        zint = zquad((0.d0,0.d0),0,z(kr),kr,n,z,betam,nptsq,qwork)
        zfval = w(kr) - wc - c*zint
        fval(2*nvert-1) = dreal(zfval)
        fval(2*nvert) = dimag(zfval)
   20 continue
      return
c
      end


c*******************************************************************
c* scoutp                         subordinate(scsolv) subroutine  **
c*******************************************************************
c
      subroutine scoutp(n,c,z,wc,w,betam,nptsq)
c
c prints a table of k, w(k), th(k), betam(k), and z(k).
c also prints the constants n, nptsq, wc, c.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),w(n),betam(n)
c
c      write (6,102) n, nptsq
c      pi = acos(-1.d0)
c      do 1 k = 1,n
c        thdpi = dimag(log(z(k))) / pi
c        if (thdpi.le.0.d0) thdpi = thdpi + 2.d0
c        if (betam(k).gt.-1.d0) write (6,103) k,w(k),thdpi,betam(k),z(k)
c    1   if (betam(k).le.-1.d0) write (6,104) k,thdpi,betam(k),z(k)
c      write (6,105) wc,c
c      return
c
  102 format (/' parameters defining map:',15x,'(n =',
     &  i3,')',6x,'(nptsq =',i3,')'//
     &  '  k',10x,'w(k)',10x,'th(k)/pi',5x,'betam(k)',
     &  13x,'z(k)'/
     &  ' ---',9x,'----',10x,'--------',5x,'--------',
     &  13x,'----'/)
  103 format (i3,'    (',f6.3,',',f6.3,')',f14.8,f12.5,
     &  3x,'(',f11.8,',',f11.8,')')
  104 format (i3,'        infinity   ',f14.8,f12.5,
     &  3x,'(',f11.8,',',f11.8,')')
  105 format (/' wc = (',e15.8,',',e15.8,')'/
     &          '  c = (',e15.8,',',e15.8,')'/)
      end


c*******************************************************************
c* sctest                         subordinate(scsolv) subroutine  **
c*******************************************************************
c
      subroutine sctest(errest,n,c,z,wc,w,betam,nptsq,qwork)
c
c tests the computed map for accuracy.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),w(n),betam(n),qwork(1)
c
c test length of radii:
      errest = 0.d0
      do 10 k = 2,n
        if (betam(k).gt.-1.d0) rade = abs(wc -
     &    wsc((0.d0,0.d0),0,z(k),w(k),k,n,c,z,betam,nptsq,qwork))
        if (betam(k).le.-1.d0) rade = abs(wsc((.1d0,.1d0),0,
     &    z(k-1),w(k-1),k-1,n,c,z,betam,nptsq,qwork)
     &    - wsc((.1d0,.1d0),0,z(k+1),w(k+1),k+1,
     &    n,c,z,betam,nptsq,qwork))
        errest = max(errest,rade)
   10   continue
      return
      end


c*******************************************************************
c* zquad                                      primary subroutine  **
c*******************************************************************
c
      function zquad(za,ka,zb,kb,n,z,betam,nptsq,qwork)
c
c computes the complex line integral of zprod from za to zb along a
c straight line segment within the unit disk.  function zquad1 is
c called twice, once for each half of this integral.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),betam(n),qwork(1)
c
      if (abs(za).gt.1.1d0.or.abs(zb).gt.1.1d0) write (6,301)
  301 format (/' *** warning in zquad: z outside the disk')
c
      zmid = (za + zb) / 2.d0
      zquad = zquad1(za,zmid,ka,n,z,betam,nptsq,qwork)
     &  - zquad1(zb,zmid,kb,n,z,betam,nptsq,qwork)
      return
      end


c*******************************************************************
c* zquad1                          subordinate(zquad) subroutine  **
c*******************************************************************
c
      function zquad1(za,zb,ka,n,z,betam,nptsq,qwork)
c
c computes the complex line integral of zprod from za to zb along a
c straight line segment within the unit disk.  compound one-sided
c gauss-jacobi quadrature is used, using function dist to determine
c the distance to the nearest singularity z(k).
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),betam(n),qwork(1)
*/*/*
      data resprm /1.d0/
c
c check for zero-length integrand:
      if (abs(za-zb).gt.0.d0) goto 1
      zquad1 = (0.d0,0.d0)
      return
c
c step 1: one-sided gauss-jacobi quadrature for left endpoint:
    1 r = min(1.d0,dist(za,ka,n,z)*resprm/abs(za-zb))
      zaa = za + r*(zb-za)
      zquad1 = zqsum(za,zaa,ka,n,z,betam,nptsq,qwork)
c
c step 2: adjoin intervals of pure gaussian quadrature if necessary:
   10 if (r.eq.1.d0) return
      r = min(1.d0,dist(zaa,0,n,z)*resprm/abs(zaa-zb))
      zbb = zaa + r*(zb-zaa)
      zquad1 = zquad1 + zqsum(zaa,zbb,0,n,z,betam,nptsq,qwork)
      zaa = zbb
      goto 10
      end


c*******************************************************************
c* dist                            subordinate(zquad) subroutine  **
c*******************************************************************
c
      function dist(zz,ks,n,z)
c
c determines the distance from zz to the nearest singularity z(k)
c other than z(ks).
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n)
c
      dist = 99.d0
      do 1 k = 1,n
        if (k.eq.ks) goto 1
        dist = min(dist,abs(zz-z(k)))
    1   continue
      return
      end


c*******************************************************************
c* zqsum                           subordinate(zquad) subroutine  **
c*******************************************************************
c
      function zqsum(za,zb,ka,n,z,betam,nptsq,qwork)
c
c computes the integral of zprod from za to zb by applying a
c one-sided gauss-jacobi formula with possible singularity at za.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),betam(n),qwork(1)
c
      zs = (0.d0,0.d0)
      zh = (zb-za) / 2.d0
      zc = (za+zb) / 2.d0
      k = ka
      if (k.eq.0) k = n+1
      iwt1 = nptsq*(k-1) + 1
      iwt2 = iwt1 + nptsq - 1
      ioffst = nptsq*(n+1)
      do 1 i = iwt1,iwt2
    1   zs = zs + qwork(ioffst+i)*zprod(zc+zh*qwork(i),ka,n,z,betam)
      zqsum = zs*zh
      if (abs(zh).ne.0.d0.and.k.ne.n+1)
     &  zqsum = zqsum*abs(zh)**betam(k)
      return
      end


c*******************************************************************
c* zprod                           subordinate(zquad) subroutine  **
c*******************************************************************
c
      function zprod(zz,ks,n,z,betam)
c
c computes the integrand
c
c           n
c         prod  (1-zz/z(k))**betam(k)  ,
c          k=1
c
c taking argument only (not modulus) for term k = ks.
c
c *** note -- in practice this is the innermost subroutine
c *** in scpack calculations.  the complex log calculation below
c *** may account for as much as half of the total execution time.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),betam(n)
*/*/* new line:
      common /logcnt/ ncount,ncnt1
c
      zsum = (0.d0,0.d0)
      do 1 k = 1,n
        ztmp = (1.d0,0.d0) - zz/z(k)
        if (k.eq.ks) ztmp = ztmp / abs(ztmp)
    1   zsum = zsum + betam(k)*log(ztmp)
      zprod = exp(zsum)
*/*/* new line:
      ncount = ncount + n
      return
      end


c*******************************************************************
c* rprod                                      primary subroutine  **
c*******************************************************************
c
      function rprod(zz,n,z,betam)
c
c computes the absolute value of the integrand
c
c           n
c         prod  (1-zz/z(k))**betam(k)
c          k=1
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),betam(n)
c
      sum = 0.d0
      do 1 k = 1,n
        ztmp = (1.d0,0.d0) - zz/z(k)
    1   sum = sum + betam(k)*log(abs(ztmp))
      rprod = exp(sum)
      return
      end


c*******************************************************************
c* nearz                                      primary subroutine  **
c*******************************************************************
c
      subroutine nearz(zz,zn,wn,kn,n,z,wc,w,betam)
c
c returns information associated with the nearest prevertex z(k)
c to the point zz, or with 0 if 0 is closer than any z(k).
c zn = prevertex position, wn = w(zn), kn = prevertex no. (0 to n)
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),w(n),betam(n)
c
      dist = abs(zz)
      kn = 0
      zn = (0.d0,0.d0)
      wn = wc
      if (dist.le..5d0) return
      do 1 k = 1,n
        if (betam(k).le.-1.d0) goto 1
        distzk = abs(zz-z(k))
        if (distzk.ge.dist) goto 1
        dist = distzk
        kn = k
    1   continue
      if (kn.eq.0) return
      zn = z(kn)
      wn = w(kn)
      return
      end


c*******************************************************************
c* nearw                                      primary subroutine  **
c*******************************************************************
c
      subroutine nearw(ww,zn,wn,kn,n,z,wc,w,betam)
c
c returns information associated with the nearest vertex w(k)
c to the point ww, or with wc if wc is closer than any w(k).
c zn = prevertex position, wn = w(zn), kn = vertex no. (0 to n)
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension z(n),w(n),betam(n)
c
      dist = abs(ww-wc)
      kn = 0
      zn = (0.d0,0.d0)
      wn = wc
      do 1 k = 1,n
        if (betam(k).le.-1.d0) goto 1
        distwk = abs(ww-w(k))
        if (distwk.ge.dist) goto 1
        dist = distwk
        kn = k
    1   continue
      if (kn.eq.0) return
      zn = z(kn)
      wn = w(kn)
      return
      end


c*******************************************************************
c* angles                                     primary subroutine  **
c*******************************************************************
c
      subroutine angles(n,w,betam)
c
c computes external angles -pi*betam(k) from knowledge of
c the vertices w(k).  an angle betam(k) is computed for each
c k for which w(k-1), w(k), and w(k+1) are finite.
c to get this information across any vertices at infinity
c should be signaled by the value w(k) = (99.,99.) on input.
c
      implicit double precision (a-b,d-h,o-v,x-y)
      implicit complex*16(c,w,z)
      dimension w(n),betam(n)
      c9 = (99.d0,99.d0)
c
      pi = acos(-1.d0)
      do 1 k = 1,n
        km = mod(k+n-2,n)+1
        kp = mod(k,n)+1
        if (w(km).eq.c9.or.w(k).eq.c9.or.w(kp).eq.c9) goto 1
        betam(k) = dimag(log((w(km)-w(k))/(w(kp)-w(k))))/pi - 1.d0
        if (betam(k).le.-1.d0) betam(k) = betam(k) + 2.d0
    1   continue
      return
      end

*/*/* two new subroutines:

      subroutine count0
      common /logcnt/ ncount,ncnt1
      ncount = 0
      ncnt1 = 0
      write (6,1)
    1 format (' ------- log counter set to zero')
      return
      end

      subroutine count
      common /logcnt/ ncount,ncnt1
      ncdiff = ncount - ncnt1
      write (6,2) ncdiff,ncount
    2 format (' ------- no. logs: since last count',i7,',   total',i8)
      ncnt1 = ncount
      return
      end


