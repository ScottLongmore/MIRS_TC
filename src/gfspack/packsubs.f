      subroutine maxmin (par,i1,i2,parmax,parmin)
c     This routine finds the max and min of the one-dimensional
c     array par. If the max is equal to the min, then the max is set
c     to a value a little higher than the min.
c
      dimension par(*)
c
      parmax = -1.0e+10
      parmin =  1.0e+10
c
      do 10 m = i1,i2
         if (par(m) .gt. parmax) parmax = par(m)
         if (par(m) .lt. parmin) parmin = par(m)
   10 continue
c
      if (parmax .eq. parmin) parmax = parmin + 0.1*abs(parmin) + 1.0
c
      return
      end
      subroutine tstcod (par,i1,i2,parmax,parmin,bsub,smpy,cod)
c
      character code*2
      character*2 cod(*)
      dimension par(*)
c
      bsub = -parmin
      rix = 32**2 - 1
      rax = amax1 (parmax + bsub,0.)
      smpy = rax / rix
      scap = 1. / smpy
c
      do 10 m = i1,i2
         k = m - i1 + 1
         iz = nint ((par(m) + bsub) * scap)
c
         call encod (iz,code)
         cod(k) = code
   10 continue
c
      return
      end
      subroutine encod (iz,code)
c     hp version
c
      character*(*) code
      dimension idgt(2)
c
      ibase = 32
      idgt(2) = iz / ibase
      idgt(1) = iz - idgt(2) * ibase
c
      do 10 i = 1,2
         j = 3 - i
         if (idgt(i) .le. 9) then
            code(j:j) = char (idgt(i) + ichar ('0'))
         else
            code(j:j) = char (idgt(i) + (ichar ('A') - 10))
         end if
   10 continue
c
      return
      end
      integer function idecod (code)
c     hp version
c
      character*(*) code
      character dgtb*(31)
      parameter (dgtb = '123456789ABCDEFGHIJKLMNOPQRSTUV')
c
      idecod = index (dgtb,code(1:1)) * 32
     1  + index (dgtb,code(2:2))
c
      return
      end
