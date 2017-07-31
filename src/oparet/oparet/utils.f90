      ! $Id: utils.f90,v 0.1 2012/05/XX 12:00:00 longmore Exp $
 
      module utils
        implicit none

        ! Specify needed constants
        integer,private :: nday(12) = (/0,31,59,90,120,151,181,212,243,273,304,334/)
        integer,private :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

        contains

        subroutine jdayi(julday,iyear,imon,iday)
          ! This routine calculates the month (imon) and day (iday)
          ! from the Julian day (julday) and year (iyear).
          ! The appropriate correction is made for leap year.
          integer, intent(in) :: julday, iyear
          integer, intent(inout) :: imon, iday

          integer :: nsum(13),mxjul
          integer :: i
  
            ! Specify the number of days in each month
            !ndmon =(/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

            ! Correct for leap year
          if (mod(iyear,4) .eq. 0) then 
            ndmon(2)=29
          end if
  
            ! Check for illegal input
          if (mod(iyear,4) .eq. 0) then
            mxjul = 366
          else
            mxjul = 365
          end if
  
          if (julday .lt. 1 .or. julday .gt. mxjul) then
            imon = -1
            iday = -1
            return
          endif

            !Calculate the month and day
          nsum(1) = 0
          do i=1,12
            nsum(i+1) = nsum(i) + ndmon(i)
          end do 

          do i=2,13
            if (julday .le. nsum(i)) then
              imon = i-1
              exit 
            end if
          end do 

          iday = julday - nsum(imon)

          return

        end subroutine jdayi

        subroutine julian(imon,iday,iyear,julday)
          ! This routine calculates the Julian day (julday) from
          ! the month (imon), day (iday), and year (iyear). The
          ! appropriate correction is made for leap year.
          integer, intent(in) :: imon, iday, iyear
          integer, intent(out) :: julday
          integer :: i

          ! Correct for leap year
          if (mod(iyear,4) .eq. 0) ndmon(2)=29

          ! Check for illegal input
          if (imon .lt. 1 .or. imon .gt. 12) then
            julday=-1
            return
          end if

          if (iday .lt. 1 .or. iday .gt. ndmon(imon)) then
            julday=-1
            return
          end if

          ! Calculate the Julian day
          julday = iday
          if (imon .gt. 1) then
            do i=2,imon
              julday = julday + ndmon(i-1)
            end do
          end if

          return

        end subroutine julian

        subroutine llintsp(f1,slon1,slat1,dlon1,dlat1,im1,jm1,i1,j1, & 
                           fsp,rlon,rlat,izp,ierr)
        ! This routine linearly interpolates f1 on an evenly spaced
        ! lat,lon grid to obtain fsp at the single point (rlon,rlat). 
        ! The subroutine arguments are defined as follows:
 
        ! f1(im1,jm1):  Contains the function to be interpolated. The
        !               first index represents longitude (increasing
        !               eastward) and the second index represents 
        !               latitude (increasing northward).
 
        ! fsp:          The interpolated value of f1.
 
        ! slon1         The first longitude of f1 (deg E positive)
        ! slat1         The first latitude  of f1 (deg N positive)
        ! dlon1         The longitude increment of f1 (deg)
        ! dlat1         The latitude  increment of f1 (deg)
        ! rlon,rlat     The longitude,latitude of the point to interpolate f1.
 
        ! im1,jm1       The dimensions of f1
 
        ! i1,j1         The number of longitude,latitude points of f1
        !               to use in the interpolation
 
        ! izp           Zonal Periodic flag: 
        !                   =1 when f1 is periodic in the zonal direction
        !                      (normally used only when f1 spans 360 deg
        !                      of longitude, starting at 0)
        !                   =0 if not periodic
 
        ! ierr          Error flag: =0 for normal return
        !                           =1 if fsp is outside of the f1 domain
        !                              (non fatal)
        !                           =2 if indices i1,j1 exceed
        !                              dimension of f1 (fatal) 
        !                           =3 if dlon or dlat .le. 0.0 (fatal) 
 
          real, intent(in) :: f1(im1,jm1)
          real, intent(inout) :: fsp
          real, intent(in) :: slon1, slat1
          real, intent(in) :: dlon1, dlat1
          real, intent(inout) :: rlon,rlat
          integer, intent(in) :: im1, jm1
          integer, intent(in) :: i1,j1
          integer, intent(in) :: izp
          integer, intent(inout) ::ierr

          real :: rlonn1,rlonx1,rlatn1,rlatx1
          integer :: i00,j00
          real :: f00,f01,f10,f11
          real :: rlon00,rlat00,dx0,dx1,dy,x,y,a,b,c,d

          ! XXX Needs to be synced/moved to cons.f90
          real, parameter ::  pi   = 3.14159265
          real, parameter ::  dtr  = pi/180.0
          real, parameter ::  erad = 6371.0
          real, parameter ::  adtr = erad*dtr

        ! Initialize error flag
          ierr=0
         
        ! Check indices and dlat,dlon
          if (i1 .gt. im1  .or.  j1 .gt. jm1) then
            ierr=2
            return
          end if
 
          if (dlat1 .le. 0.0 .or. dlon1 .le. 0.0) then
            ierr=3
            return
          end if
          

 
        ! Calculate min and max long,lat of f1 domain
          rlonn1 = slon1
          rlonx1 = slon1 + dlon1*float(i1-1)
          rlatn1 = slat1
          rlatx1 = slat1 + dlat1*float(j1-1)
 
 
        ! Check if fsp point is outside of f1 domain.
        ! If yes, move the fsp point to the nearest point in the
        ! f1 domain and set error flag. 
 
          if (izp .ne. 1) then
            ! Adjust fsp longitude for case without zonal periodicity
            if (rlon .gt. rlonx1) then
              rlon = rlonx1
              ierr=1
            end if
 
            if (rlon .lt. rlonn1) then
              rlon = rlonn1
              ierr=1
            end if
          else
           ! Zonal periodic case
             if (rlon .ge. 360.0) rlon=rlon-360.0
          end if
 
          if (rlat .gt. rlatx1) then
            rlat = rlatx1
            ierr=1
          end if
 
          if (rlat .lt. rlatn1) then
            rlat = rlatn1
            ierr=1
          end if
 
        ! Find the indices of the f1 point closest to,
        ! but with lon,lat less than the fsp point.
          i00 = 1 + int( (rlon-rlonn1)/dlon1 )
          j00 = 1 + int( (rlat-rlatn1)/dlat1 )
          if (i00 .lt.    1)    i00=   1
          if (izp .ne. 1) then
            if (i00 .gt. i1-1) i00=i1-1
          end if
          if (j00 .lt.    1)    j00=   1
          if (j00 .gt. j1-1)    j00=j1-1
 
        ! Define the four f1 values to be used in the linear
        ! interpolation.
          f00 = f1(i00  ,j00  )
          f01 = f1(i00  ,j00+1)
 
          if (izp .eq. 1 .and. i00 .eq. i1) then
            f10 = f1(    1,j00  )
            f11 = f1(    1,j00+1)
          else
            f10 = f1(i00+1,j00  )
            f11 = f1(i00+1,j00+1)
          endif
 
        ! Calculate the lon,lat of the point i00,j00
          rlon00 = rlonn1 + dlon1*float(i00-1)
          rlat00 = rlatn1 + dlat1*float(j00-1)
 
        ! Calculate the x,y distances between the four f1 points
        ! where x,y = 0,0 at i00,j00
          dx0 = dlon1*adtr*cos( dtr*(rlat00        ) )
          dx1 = dlon1*adtr*cos( dtr*(rlat00 + dlat1) )
          dy  = dlat1*adtr
 
        ! Calculate the x,y coordinates of the current f2 point
          x = adtr*(rlon-rlon00)*cos(dtr*rlat)
          y = adtr*(rlat-rlat00)

        ! Calculate the coefficients for the linear interpolation
          a = f00
          b = (f10-f00)/dx0
          c = (f01-f00)/dy
          d = (f11 - f00 - b*dx1 - c*dy)/(dx1*dy)
 
        ! Perform interpolation 
          fsp = a + b*x + c*y + d*x*y
 
          return

        end subroutine llintsp

        subroutine llintp(f1,slon1,slat1,dlon1,dlat1,im1,jm1,i1,j1, &
                          f2,slon2,slat2,dlon2,dlat2,im2,jm2,i2,j2, &
                          izp,ierr)
        ! This routine linearly interpolates f1 on an evenly spaced
        ! lat,lon grid to obtain f2 on a different lat,lon grid. 
        ! The subroutine arguments are defined as follows:

        ! f1(im1,jm1):  Contains the function to be interpolated. The
        !               first index represents longitude (increasing
        !               eastward) and the second index represents 
        !               latitude (increasing northward).

        ! f2(im2,jm2):  The interpolated function with indices defined 
        !               the same as for f1.

        ! slon1         The first longitude of f1 (deg E positive)
        ! slat1         The first latitude  of f1 (deg N positive)
        ! dlon1         The longitude increment of f1 (deg)
        ! dlat1         The latitude  increment of f1 (deg)
        ! slon2         The first longitude of f2 (deg E positive)
        ! slat2         The first latitude  of f2 (deg N positive)
        ! dlon2         The longitude increment of f2 (deg)
        ! dlat2         The latitude  increment of f2 (deg)

        ! im1,jm1       The dimensions of f1
        ! im2,jm2       The dimensions of f2

        ! i1,j1         The number of longitude,latitude points of f1
        !               to use in the interpolation
        ! i2,j2         The number of longitude,latitude points of f2
        !               to interpolate

        ! izp           Zonal Periodic flag: 
        !                  =1 when f1 is periodic in the zonal direction
        !                     (normally used only when f1 spans 360 deg
        !                     of longitue)
        !                  =0 if not periodic

        ! ierr          Error flag: =0 for normal return
        !                  =1 if f2 domain exceeds f1 domain
        !                     (non fatal)
        !                  =2 if indices i1,j1 or i2,j2 exceed
        !                     dimension of f1 or f2 (fatal) 
        !                  =3 if dlon or dlat .le. 0.0 (fatal) 

          real, intent (inout) :: f1(im1,jm1),f2(im2,jm2)
          real, intent(in) :: slon1, slat1, slon2, slat2
          real, intent(in) :: dlon1, dlat1, dlon2, dlat2
          integer, intent(in) :: im1, jm1, im2, jm2
          integer, intent(in) :: i1,j1,i2,j2
          integer, intent(in) :: izp
          integer, intent(inout) :: ierr

          real :: rlon,rlat,rlonn1,rlonx1,rlatn1,rlatx1
          integer :: i00,j00
          real :: f00,f01,f10,f11
          real :: rlon00,rlat00,dx0,dx1,dy,x,y,a,b,c,d
          integer :: i,j

          ! XXX Needs to be synced/moved to cons.f90
          real, parameter ::  pi   = 3.14159265
          real, parameter ::  dtr  = pi/180.0
          real, parameter ::  erad = 6371.0
          real, parameter ::  adtr = erad*dtr

        ! Initialize error flag
          ierr=0

        ! Check indices and dlat,dlon
          if (i1 .gt. im1  .or.  j1 .gt. jm1  .or. &
              i2 .gt. im2  .or.  j2 .gt. jm2)  then
            ierr=2
            return
          end if

          if (dlat1 .le. 0.0 .or. dlon1 .le. 0.0 .or. &
              dlat2 .le. 0.0 .or. dlon2 .le. 0.0) then
            ierr=3
            return
          end if

        ! Calculate min and max long,lat of f1 domain
          rlonn1 = slon1
          rlonx1 = slon1 + dlon1*float(i1-1)
          rlatn1 = slat1
          rlatx1 = slat1 + dlat1*float(j1-1)

        ! Start loop for f2 points
          do j=1,j2

            do  i=1,i2

              rlon = slon2 + dlon2*float(i-1)
              rlat = slat2 + dlat2*float(j-1)

              ! Check if current f2 point is outside of f1 domain.
              ! If yes, move the f2 point to the nearest point in the
              ! f1 domain and set error flag. 

              if (izp .ne. 1) then
                ! Adjust f2 longitude for case without zonal periodicity
                if (rlon .gt. rlonx1) then
                  rlon = rlonx1
                  ierr=1
                end if

                if (rlon .lt. rlonn1) then
                  rlon = rlonn1
                  ierr=1
                end if

              else
                ! Zonal periodic case
                if (rlon .ge. 360.0) rlon=rlon-360.0
              end if

              if (rlat .gt. rlatx1) then
                rlat = rlatx1
                ierr=1
              end if

              if (rlat .lt. rlatn1) then
                rlat = rlatn1
                ierr=1
              end if

              ! Find the indices of the f1 point closest to,
              ! but with lon,lat less than the current f2 point.
              i00 = 1 + int( (rlon-rlonn1)/dlon1 )
              j00 = 1 + int( (rlat-rlatn1)/dlat1 )
              if (i00 .lt.    1)    i00=   1
              if (izp .ne. 1) then
                 if (i00 .gt. i1-1) i00=i1-1
              end if
              if (j00 .lt.    1)    j00=   1
              if (j00 .gt. j1-1)    j00=j1-1

              ! Define the four f1 values to be used in the linear
              ! interpolation.
              f00 = f1(i00  ,j00  )
              f01 = f1(i00  ,j00+1)

              if (izp .eq. 1 .and. i00 .eq. i1) then
                f10 = f1(    1,j00  )
                f11 = f1(    1,j00+1)
              else
                f10 = f1(i00+1,j00  )
                f11 = f1(i00+1,j00+1)
              end if

              ! Calculate the lon,lat of the point i00,j00
              rlon00 = rlonn1 + dlon1*float(i00-1)
              rlat00 = rlatn1 + dlat1*float(j00-1)

              ! Calculate the x,y distances between the four f1 points
              ! where x,y = 0,0 at i00,j00
              dx0 = dlon1*adtr*cos( dtr*(rlat00        ) )
              dx1 = dlon1*adtr*cos( dtr*(rlat00 + dlat1) )
              dy  = dlat1*adtr

              ! Calculate the x,y coordinates of the current f2 point
              x = adtr*(rlon-rlon00)*cos(dtr*rlat)
              y = adtr*(rlat-rlat00)

              ! Calculate the coefficients for the linear interpolation
              a = f00
              b = (f10-f00)/dx0
              c = (f01-f00)/dy
              d = (f11 - f00 - b*dx1 - c*dy)/(dx1*dy)

              ! Perform interpolation and then go to the next f2 point
              f2(i,j) = a + b*x + c*y + d*x*y

             end do

          end do

          return

        end subroutine llintp

        subroutine maxmin (par,i1,i2,parmax,parmin)
        ! This routine finds the max and min of the one-dimensional
        ! array par. If the max is equal to the min, then the max is set
        ! to a value a little higher than the min.

          real, intent(in) :: par(:)
          real , intent(inout) :: parmax,parmin
          integer , intent(in) :: i1,i2
          integer :: m

          parmax = -1.0e+10
          parmin =  1.0e+10

          do m = i1,i2
            if (par(m) .gt. parmax) parmax = par(m)
            if (par(m) .lt. parmin) parmin = par(m)
          end do

          if (parmax .eq. parmin) parmax = parmin + 0.1*abs(parmin) + 1.0

          return

        end subroutine maxmin

        subroutine tstcod (par,i1,i2,parmax,parmin,bsub,smpy,cod)
          real, intent(in) :: par(:)
          real, intent(in) :: parmin,parmax
          integer, intent(in) :: i1,i2
          real, intent(inout) :: bsub, smpy
          character (len=2), intent(inout) :: cod(:)

          character (len=2) :: code
          integer :: rix
          real :: rax, scap
          integer :: m, k, iz

          bsub = -parmin
          rix = 32**2 - 1

          rax = amax1 (parmax + bsub,0.)
          smpy = rax / rix
          scap = 1. / smpy

          do m = i1,i2
            k = m - i1 + 1
            iz = nint ((par(m) + bsub) * scap)

            call encod (iz,code)
            cod(k) = code
          end do

          return
        end subroutine tstcod

        subroutine encod (iz,code)
        ! hp version
          integer, intent(in) :: iz
          character (len=*), intent(inout) :: code
  
          integer :: idgt(2), ibase 
          integer :: i,j
  
          ibase = 32
          idgt(2) = iz / ibase
          idgt(1) = iz - idgt(2) * ibase

          do i = 1,2
            j = 3 - i
            if (idgt(i) .le. 9) then
              code(j:j) = char (idgt(i) + ichar ('0'))
            else
              code(j:j) = char (idgt(i) + (ichar ('A') - 10))
            end if
          end do

          return

        end subroutine encod

        integer function idecod (code)
        ! hp version

          character (len=*), intent(in) :: code

          character (len=31), parameter :: dgtb = & 
            '123456789ABCDEFGHIJKLMNOPQRSTUV'

          idecod = index (dgtb,code(1:1)) * 32  + index (dgtb,code(2:2))

          return

        end function idecod

        subroutine pstcal(z1,z2,t1,t2,p1,ps,ts)
        ! This routine calculates the surface pressure (psfc) 
        ! from thermodynamic variables near the surface.

        ! level 1 = level closest to the surface (usually 1000 mb)
        ! level 2 = next level up (usually 850 or 925 mb)

        ! input: p1 = pressure (Pa) 
        !        zi = geopotential height (m) (i=1,2)
        !        ti = temperature (K)         (i=1,2)
        ! 
        ! output: ps = surface pressure (Pa)
        !         ts = surface temperature (K)
          real, intent(in) :: z1,z2,t1,t2,p1
          real, intent(inout) :: ps, ts
          real :: gamma

          ! Specify physical constants (mks units) 
          ! XXX Needs to be synced/moved to cons.f90
          real, parameter :: rd = 287.0
          real, parameter :: g  = 9.81

          ! Calculate the lapse rate
          gamma = -(t2-t1)/(z2-z1)

          ! Assume constant lapse rate atmosphere
          if (gamma .gt. 0.0) then
            ps = p1*( (t1/(t1-gamma*z1))**(g/(rd*gamma)) )
            ts = t1 + gamma*z1
          ! Assume isothermal atmosphere
          else
           ps = p1*exp(g*z1/(rd*t1))
           ts = t1
          end if

          return

        end subroutine pstcal

        subroutine smooth (fld,temp,idim,jdim,idm,jdm,izp)
        ! -------------------------------------------------------------------
        ! --- smooths field in zonal and meridional directions 
        ! --- smoothing function from Shapiro (Shapiro, Ralph,1975: 
        ! --- Linear Filtering, Mathematics of Computation, Vol 29, No. 132,
        ! --- p. 1094-1097 
        ! --- 3 point filter
        ! --- 01/30/96  ---  Fiona Horsfall
        ! -------------------------------------------------------------------
        !     Modified 2/7/96 by MDM
        !     This version modified 8/99 to include temp array in argument list
        !     izp = 1 if field is zonally periodic with no overlapping point
        !         = 0 if not periodic
        ! -------------------------------------------------------------------
          integer, intent(in) :: idim,jdim,idm,jdm,izp
          real, intent(inout) :: temp(idim,jdim),fld(idim,jdim)

          real :: wt1,wt2
          integer :: i,j,im1,ip1,jm1,jp1

        ! --- set weights 
          wt1=.5
          wt2=.25

        ! --- apply filter zonally

          do j=1,jdm
            do i=1,idm
              im1=i-1
              ip1=i+1
              if (i.eq.1) then
                if (izp .eq. 1) then
                  im1=idm
                else
                  im1=1
                end if
              end if

              if (i.eq.idm) then
                if (izp .eq. 1) then
                  ip1=1
                else
                 ip1=idm
                end if
              end if

              temp(i,j)=fld(i,j)*wt1+(fld(ip1,j)+fld(im1,j))*wt2

            end do
          end do

        ! --- apply filter meridionally

          do i=1,idm
            do j=1,jdm
              jm1=j-1
              jp1=j+1

              if (j.eq.  1) jm1=1
              if (j.eq.jdm) jp1=jdm

              fld(i,j)=temp(i,j)*wt1+(temp(i,jp1)+temp(i,jm1))*wt2
            end do
          end do

          return

        end subroutine smooth

        subroutine stndz(p,z,t,theta)
        ! This routine calculates the standard height z (m) from the
        ! pressure p (mb). The temperature t (K) and potential temperature
        ! theta (K) at p are also calculated.
          real, intent(in) :: p
          real, intent(inout) :: z,t,theta

          ! XXX Needs to be synced/moved to cons.f90
          real, parameter :: g   = 9.81
          real, parameter :: r   = 287.05
          real, parameter :: cp  = 1004.0
          real, parameter :: b   = 0.0065
          real, parameter :: p0  = 1013.25
          real, parameter :: t0  = 288.15
          real, parameter :: p00 = 1000.0
          real, parameter :: p1  = 226.32
          real, parameter :: t1  = 216.65
          real, parameter :: z1  = 11000.0
          real, parameter :: cap = r/cp
          real, parameter :: a   = r*b/g

          real, parameter :: z2  = 20000.0
          real, parameter :: b2  = -0.0010
          real, parameter :: p2  = 54.75
          real, parameter :: t2  = t1
          real, parameter :: a2  = r*b2/g

          if (p .ge. p1) then
            z = (t0/b)*(1.0 - (p/p0)**a)
            t = t0 - b*z
          elseif (p .lt. p1 .and. p .ge. p2) then
            z = z1 + (r*t1/g)*alog(p1/p)
            t = t1
          else
            z = z2 + (t2/b2)*(1.0 - (p/p2)**a2)
            t = t2 - b2*(z-z2)
          end if

          theta = t*( (p00/p)**cap )
          return

        end subroutine stndz

        subroutine tdiff(iy2,im2,id2,it2,iy1,im1,id1,it1,idelt)
        ! This routine calculates the number of hours (delt) between
        ! two date/times.  Note: Times are in hours
          integer, intent(in) :: iy2,im2,id2,it2,iy1,im1,id1,it1 
          integer, intent(inout) :: idelt

          integer iry,ity1,ity2,i

        ! Calculate reference year
          iry = iy1-2
          if (iy2 .lt. iry) iry=iy2-2

        ! Calculate the number of hours from 00 Jan. 1 of the reference year
          ity1 = 0
          do i=iry,iy1-1
             if (mod(i,4) .eq. 0) then
                ity1 = ity1 + 24*366
             else
                ity1 = ity1 + 24*365
             endif
          end do 

          ity2 = 0
          do i=iry,iy2-1
            if (mod(i,4) .eq. 0) then
              ity2 = ity2 + 24*366
            else
              ity2 = ity2 + 24*365
            endif
          end do

          ity1 = ity1 + 24*nday(im1)
          if ((mod(iy1,4) .eq. 0) .and. im1 .gt. 2) ity1=ity1+24

          ity2 = ity2 + 24*nday(im2)
          if ((mod(iy2,4) .eq. 0) .and. im2 .gt. 2) ity2=ity2+24

          ity1 = ity1 + 24*id1 + it1
          ity2 = ity2 + 24*id2 + it2

          idelt = ity2 - ity1

          return

        end subroutine tdiff

        subroutine ctor(x,y,r,theta)
        ! This routine converts from Cartesion coordinates
        ! to radial coordinates, where theta is in
        ! degrees measured counter-clockwise from
        ! the +x-axis.
          real, intent(in) :: x,y
          real, intent(inout) :: r,theta
 
          ! XXX Needs to be synced/moved to cons.f90
          real, parameter :: rtd = 57.296

          r = sqrt(x*x + y*y)

          if (r .le. 0.0) then
            theta = 0.0
            return
          end if

          theta = rtd*acos(x/r)
          if (y .lt. 0.0) theta = 360.0 - theta

          return

        end subroutine ctor

        subroutine rtoc(r,theta,x,y)
        ! This routine converts from radial coordinates
        ! to Cartesian coordinates, where theta is in
        ! degrees measured counter-clockwise from
        ! the +x-axis.
          real, intent(in) :: r,theta
          real, intent(inout) :: x,y

          ! XXX These params need to eventually go to cons.f90
          real, parameter :: rtd = 57.296
  
          x = r*cos(theta/rtd)
          y = r*sin(theta/rtd)

          return

        end subroutine rtoc

        subroutine ucase(chr,len)
        ! This routine converts all letters in the character char 
        ! of length len to upper case

          integer, intent(in) :: len
          character(len=len), intent(inout) :: chr

          character (len=1)::ucl(26)= &
            (/'A','B','C','D','E','F','G','H','I','J','K','L','M', &
              'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/)
          character (len=1)::lcl(26)= &
            (/'a','b','c','d','e','f','g','h','i','j','k','l','m', &
              'n','o','p','q','r','s','t','u','v','w','x','y','z'/)
          integer :: m,k

          do m=1,len
            do k=1,26
              if (chr(m:m) .eq. lcl(k)) then 
                chr(m:m)=ucl(k) 
              endif
            end do
          end do

          return

        end subroutine ucase


      end module utils
