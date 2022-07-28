      function aKK1(ak)
      real*8 a, a1, b, b1, a0, a01, aKK1, ak
      a=1.d0; a1=1.d0;
      b=dsqrt(1.d0-ak*ak)
      b1=ak
      do while (((a-b)+(a1-b1)) > 1.e-9)
          a0=(a+b)/2.d0
          a01=(a1+b1)/2.d0
          b=dsqrt(a*b)
          b1=dsqrt(a1*b1)
          a=a0
          a1=a01
      end do
      aKK1=(a1+b1)/(a+b)
      end