      subroutine refor(aC,bC,n)
! Удаление "лишних" (чётных) строк и столбцов, соответств. "лишним" электродам 
      real*8 aC(1), bC(1)
	do 1 i=1,n
	do 1 j=1,n
1	bC((j-1)*n + i) = aC((2*j-2)*(2*n-1) + (2*i-1))
      return
	end