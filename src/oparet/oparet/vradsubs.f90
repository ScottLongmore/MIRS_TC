      ! $Id: vradsubs.f90,v 0.1 2012/05/XX 12:00:00 longmore Exp $
 
      module vradsubs 
        use io
        implicit none

        contains

        subroutine readvarcoeffs(flcoef,varcoeff)
        
        ! Subroutine to read coefficients from file
        ! for single variable
        ! from io.f90
        !   lucoef   - unit number of input file
        !   ncoefs - number of coefficients for each var 
        ! Input:
        !   flcoeaf   - input coef file
        ! Output: 
        ! one array of coefficients

        character(len=25), intent(in) :: flcoef 
        integer :: istat, i, open_status, io_status
        real, dimension(ncoefs) :: varcoeff
        real :: coef
        open(unit=lucoef,file=flcoef,status='OLD',iostat=open_status)

        do i = 1,ncoefs
            read(lucoef,101,err=920,end = 910,iostat=io_status) &
                    varcoeff(i) 
        enddo
        
  101   format(f18.7)
      
        close(lucoef)
 
        istat=0
        return

        910 continue
        istat=1
        close(lucoef)
        return

        920  continue
        istat=2
        close(lucoef)
        return

        end subroutine readvarcoeffs

        subroutine readcoeffs(cvmx,cpmn,cr34,cr50,cr64)

        ! Subroutine to read coefficients from file
        ! from io.f90
        !   lucoef   - unit number of input file
        !   ncoefs - number of coefficients for each var
        !    fl_coeff_cvmx
        !    fl_coeff_cpmn
        !    fl_coeff_cr34
        !    fl_coeff_cr50
        !    fl_coeff_cr64  - input coefs fnames; 
        !                    described in io.f90
        !                    read from config file
        ! Input:
        ! Output: 
        ! 5 arrays of coefficients:cvmx, cpmn, cr34,cr50,cr64

        integer  :: io_status,open_status, istat
        real,dimension(ncoefs),intent(inout) :: cvmx,cpmn,cr34,cr50,cr64

        call readvarcoeffs(fl_coeff_cvmx,cvmx)
        call readvarcoeffs(fl_coeff_cpmn,cpmn)
        call readvarcoeffs(fl_coeff_cr34,cr34)
        call readvarcoeffs(fl_coeff_cr50,cr50)
        call readvarcoeffs(fl_coeff_cr64,cr64)

        istat=0
        return

        910 continue
        istat=1
        close(lucoef)
        return

        920  continue
        istat=2
        close(lucoef)
        return
        
        end subroutine readcoeffs
        

        subroutine vradp(preds,slat,slon,spd,head,vmaxop,ias, &
                        pvmx,ppmin,rm,x,pr34,pr50,pr64)
        !***********************************************************
        ! This subroutine was modified April 2005, by CIRA
        ! with the new predictors and coefficients, 
        ! developed using global data from 1999-2004,
        ! for estimating the MSW (pvmx), MSLP (ppmin), and wind radii.  
        ! Changes are denoted by *** prior to and after changes.
        !***********************************************************
        ! This routine estimates the storm maximum wind
        ! and the radii of 34,50 and 64 kt winds from AMSU
        ! analysis parameters.
        !
        ! Input:
        !   preds - Array of length 18 containing predictors from the 
        !           AMSU analysis (see below)
        !   slat - Storm latitude (deg N)
        !   slon - Storm longitude (deg W positive)
        !   spd  - storm translational speed
        !   head - storm heading (deg clockwise from north)
        !   vmaxop - Operational estimate of max winds (kt)
        !   ias  - Set ias=1 to skip asymmetric wind radii estimation
        !          Set ias=0 to include asymmetric winds 
        !
        ! Output 
        !   pvmx - predicted maximum surface winds (kt)
        !   ppmin- predicted minimum sea-level pressure (hPa) 
        !   pr34 - radius (nm) of 34 kt winds
        !   pr50 - radius (nm) of 50 kt winds
        !   pr64 - radius (nm) of 64 kt winds
        !   rm   - radius of max winds (nm) from vortex model fit
        !   x    - Decay exponent from vortex model fit

        !   Notes: 
        !      The 18 input AMSU predictors are as follows: 
        !      1. analyzed pressure (hPa) at r,z=0
        !      2. r=600 to r=0 pressure drop (hPa) at z=0 km  
        !      3. r=600 to r=0 pressure drop (hPa) at z=3 km
        !      4. r=0 Max temperature anomaly (C)
        !      5. height (km) of r=0 max temp. anomaly
        !      6. swath spacing (km)
        !      7. max wind (kt) at z=0 
        !      8. radius (km) of z=0 max wind
        !      9. max wind (kt) at z=3 km
        !      10. radius (km) of z=3 max wind
        !      11. 0-250 km avg. wind (kt) at z=0 km
        !      12. 0-250 km avg. wind (kt) at z=3 km
        !      13. 0-250 km avg. wind (kt) at z=5 km
        !      14. 250-500 km avg. wind (kt) at z=0 km
        !      15. 250-500 km avg. wind (kt) at z=3 km
        !      16. 250-500 km avg. wind (kt) at z=5 km
        !      17. r=0 to r=100 km avg. CLW
        !      18. Percent CLW r=0 to r=300 exceeding 0.5

        !      vmaxop and lat are included as possible predictors
        !      for wind radii. These are predictors 19. and 20. 
 
        !      (vmaxop should not be used to predict pvmx or ppmin)

        !      Four additional predictors are added to the pool. 
        !      Predictor 21 = tmax^2 (i.e., predictor 4 from above squared).
        !      Predictor 22 = tmax*clwave (i.e., predictors 4*17).
        !      Predictor 23 = clwave^2 (i.e., predictor 17 squared).
        !      Predictor 24 = the pressure at r=600km (i.e., predictors 1+2
        !      from above), which is not AMSU-derived, rather it is acquired
        !      from the NCEP GFS model and is used to derive pmin and dp0.

        !      pr34,50,64 are 1-D arrays of dimension 6, where
        !      pr(1) - NE radius 
        !      pr(2) - SE radius
        !      pr(3) - SW radius
        !      pr(4) - SW radius
        !      pr(5) - azimuthal mean radius
        !      pr(6) - azimuthal mean radius from vortex fit
          use cons

          ! nct is modified to accomodate the 4 additional predictors, 21-24
          integer, parameter  :: nco = 18
          integer, parameter  :: nct=nco+6

          real, intent(inout) :: preds(nco)
          real, intent(in) :: slat, slon, spd, head, vmaxop
          integer, intent(in) :: ias
          real, intent(inout) :: pvmx, ppmin
          real, intent(inout) :: pr34(6),pr50(6),pr64(6)
          real, intent(inout) :: rm, x

          real :: dummy(4)
          real :: predt(nct)
          real :: rb34, rb50, rb64

          integer :: i,k
        
          !character(len=120), intent(inout) :: flcoef 
          !integer, intent(inout) :: lucoef, io_status,open_status
          !real, dimension(0:nct) :: cvmx,cpmn,cr34,cr50,cr64
          real, allocatable :: cvmx(:),cpmn(:),cr34(:),cr50(:),cr64(:)

          allocate(cvmx(0:ncoefs-1))
          allocate(cpmn(0:ncoefs-1))
          allocate(cr34(0:ncoefs-1))
          allocate(cr50(0:ncoefs-1))
          allocate(cr64(0:ncoefs-1))

          ! Modified predictor layout to include predictors 21-24
          ! (i.e., tmax^2, tmax*clwave, clwave^2, and p600, respectively)
          ! Coefficients also were modified to be based on 1999-2004 data.
          ! 0        1        2        3        4        5
          !          6        7        8        9       10
          !          11       12       13       14       15
          !          16       17       18       19       20
          !          21       22       23       24
 
          !call readcoeffs(lucoef,flcoef,cvmx,cpmn,cr34,cr50,cr64)
          call readcoeffs(cvmx,cpmn,cr34,cr50,cr64)


!!!           ! Coefficients based upon 1999-2004 global data
!!!           real, parameter :: cvmx(0:nct) = (/ & 
!!!                 13.12557, 0.00000,-4.33459, 6.48789, 6.28701, &
!!!                  0.00000, 0.13380, 0.00000, 0.00000, 0.49635, &
!!!                 -0.02713, 0.00000, 0.00000, 1.72608, 1.85672, &
!!!                 -2.48450, 0.00000,19.84888,-0.26614, 0.00000, &
!!!                  0.00000,-0.51428, 0.00000, 0.00000, 0.00000  &
!!!                 /)
!!! 
!!!           real, parameter :: cpmn(0:nct) = (/ &
!!!                 15.17819, 0.00000,-0.04260, 0.06316, 0.07395, & 
!!!                  0.00000, 0.00153, 0.00000, 0.00000, 0.00537, &
!!!                 -0.00029, 0.00000, 0.00000, 0.01681, 0.01753, &
!!!                 -0.02156, 0.00000, 0.20820,-0.00209, 0.00000, & 
!!!                  0.00000,-0.00655, 0.00000, 0.00000, -0.01145 &
!!!                 /)
!!! 
!!!           real, parameter :: cr34(0:nct) = (/ &
!!!                 1734.869,-1.67423, 3.46238, 0.00000,10.41016, &
!!!                  0.00000, 0.38938,-0.67877, 0.09078, 0.00000, & 
!!!                  0.00000, 0.00000, 0.00000, 0.00000, 0.00000, & 
!!!                  0.00000, 0.00000,-39.6419, 0.30049, 0.00000, & 
!!!                  0.00000, 0.00000,-8.81319,30.62560, 0.00000  &
!!!                 /)
!!! 
!!!           real, parameter :: cr50(0:nct) = (/ &
!!!                 -5.12785, 0.00000, 2.56301, 0.00000, 6.85088, &
!!!                  0.00000, 0.00000, 0.89570, 0.06395, 0.00000, & 
!!!                  0.00000,-5.54525,13.59224,-13.0449, 0.00000, & 
!!!                  0.00000, 0.00000,-18.6103, 0.27442, 0.49101, & 
!!!                  0.00000, 0.00000, 0.00000, 0.00000, 0.00000  &
!!!                 /)
!!! 
!!!           real, parameter :: cr64(0:nct) = (/ &
!!!                 -18.4571, 0.00000, 0.00000, 0.00000, 0.00000, &
!!!                  0.00000, 0.37589, 2.10560, 0.00000,-2.52543, &
!!!                  0.00000,-4.11899, 6.62236, 1.75455, 0.00000, &
!!!                  0.00000,-1.94906, 0.00000, 0.15221, 0.00000, &
!!!                  0.00000, 0.45719, 0.00000, 0.00000, 0.00000  &
!!!                 /)
           
          !     Coefficients based upon 1999-2001 data
          !      data cvmx /14.036, 0.000,-2.171, 0.000, 2.917, 0.000,
          !     +                   0.190, 0.000, 0.000, 0.000,-0.028,
          !     +                   0.000, 0.000, 4.250, 0.581, 0.000,
          !     +                   0.000,20.050,-0.206, 0.000, 0.000/
          !
          !      data cpmn /450.28, 0.564, 1.808, 0.000,-2.737, 0.000,
          !     +                  -0.145, 0.000, 0.000, 0.000, 0.020,
          !     +                   0.000, 0.000,-2.588,-0.371, 0.000,
          !     +                   0.000,-12.77, 0.103, 0.000, 0.000/
          !
          !      data cr34 /2406.1,-2.409, 4.482,-10.39, 0.000, 0.000,
          !     +                   0.000, 0.000, 0.000, 0.000, 0.000,
          !     +                   0.000, 0.000, 0.000, 0.000, 0.000,
          !     +                   2.493, 0.000, 0.590, 1.676, 0.891/
          !
          !     +                   0.000, 8.043,-13.62, 0.000, 0.000,
          !     +                   0.000, 0.000, 0.000, 0.000, 0.990/
          !
          !      data cr64 /-48.57, 0.000, 0.000, 0.000, 6.286, 0.000,
          !     +                   0.427, 0.000, 0.000, 0.000, 0.000,
          !     +                   0.000, 0.000, 0.000, 0.000, 0.000,
          !     +                   0.000, 0.000, 0.270, 0.000, 0.217/
          !
          !     Coefficients based upon 1999-2000 data
          !     data cvmx /26.781, 0.000, 0.000,-2.315, 0.000, 0.000,
          !    +                   0.000, 0.000, 0.000, 0.000,-0.038,
          !    +                   0.000,-5.158,10.261, 1.169, 0.000,
          !    +                  -1.477,11.319, 0.000, 0.000, 0.000/
          !
          !     data cpmn /1005.5, 0.000, 0.000, 0.000, 0.000, 0.000,
          !    +                   0.000, 0.000, 0.000, 0.000, 0.024,
          !    +                   0.000, 3.453,-6.190,-0.909, 0.000,
          !    +                   1.236,-8.436, 0.000, 0.298, 0.000/
          !
          !     data cr34 /-85.57, 0.000, 0.000,-0.658, 0.000, 0.000,
          !    +                   0.000, 0.000, 0.000, 0.000, 0.000,
          !    +                   0.000, 1.597, 0.000, 0.000, 0.000,
          !    +                   0.000,32.184, 0.000, 3.616, 0.873/
          !
          !     data cr50 /-72.25, 0.000, 0.000, 3.269, 0.000, 0.000,
          !    +                   0.000, 0.000, 0.000, 0.000, 0.000,
          !    +                   0.000,-0.890, 0.000, 0.000, 0.000,
          !    +                   0.000,19.278, 0.000, 1.609, 0.842/
          !
          !     data cr64 /-22.97, 0.000, 0.000, 4.087, 0.000, 0.000,
          !    +                   0.000, 0.000, 0.000, 0.000, 0.000,
          !    +                   0.000,-1.647, 0.000, 0.000, 0.000,
          !    +                   0.000, 8.009, 0.000, 0.231, 0.418/

          ! Initialize wind radii to zero
          do  i=1,6
            pr34(i) = 0.0
            pr50(i) = 0.0
            pr64(i) = 0.0
          end do 

          ! Copy preds array and append it with latitude and vmaxop
          do i=1,nco
            predt(i) = preds(i)
          end do

          predt(nco+1) = abs(slat)
          predt(nco+2) = vmaxop

          ! Create predictors 21 (tmax^2), 22 (tmax*clwave),
          ! 23 (clwave^2), and 24 (p600=pmin+dp0)
          predt(nco+3) = preds(4) * preds(4)
          predt(nco+4) = preds(4) * preds(17)
          predt(nco+5) = preds(17) * preds(17)
          predt(nco+6) = preds(1) + preds(2)

          ! Predict  max wind, minimum pressure, and azimuthally averaged
          ! 34, 50 and 64 kt wind radii
          pvmx   = cvmx(0)
          ppmin  = cpmn(0)
          rb34   = cr34(0)
          rb50   = cr50(0)
          rb64   = cr64(0)

          do k=1,nct
            pvmx  = pvmx  + predt(k)*cvmx(k)
            ppmin = ppmin + predt(k)*cpmn(k)
            rb34  = rb34  + predt(k)*cr34(k)
            rb50  = rb50  + predt(k)*cr50(k)
            rb64  = rb64  + predt(k)*cr64(k)
          end do

          ! Modified prediction of MSLP (ppmin) is of ln(1050-MSLP),
          ! so need to calculate MSLP (ppmin) from that
          ppmin = 1050. - exp(ppmin)

          if (rb34 .lt. 0.0) rb34=0.0
          if (rb50 .lt. 0.0) rb50=0.0
          if (rb64 .lt. 0.0) rb64=0.0

          pr34(5) = rb34
          pr50(5) = rb50
          pr64(5) = rb64

          if (ias .eq. 1) return

          ! Calcuate asymmetric wind radii
          call wrasym(rb34,rb50,rb64,vmaxop,spd,head,rm,x, &
                      pr34,pr50,pr64,slat)

          ! Switch assymetry around for southern hemisphere
          ! J. Knaff - January 10, 2003
          ! if (slat.lt.0.0) then
          !   do i=1,4
          !      dummy(i)=pr34(i)
          !   enddo
          !   pr34(1)=dummy(4)
          !   pr34(4)=dummy(1)
          !   pr34(2)=dummy(3)
          !   pr34(3)=dummy(2)
          !   do i=1,4
          !      dummy(i)=pr50(i)
          !   enddo
          !   pr50(1)=dummy(4)
          !   pr50(4)=dummy(1)
          !   pr50(2)=dummy(3)
          !   pr50(3)=dummy(2)
          !   do i=1,4
          !      dummy(i)=pr64(i)
          !   enddo
          !   pr64(1)=dummy(4)
          !   pr64(4)=dummy(1)
          !   pr64(2)=dummy(3)
          !   pr64(3)=dummy(2)
          ! end if

          return

        end subroutine vradp

        subroutine wrasym(rb34,rb50,rb64,vmx,spd,head,rm,x,pr34,pr50, &
                          pr64, slat)
        ! The routine calculates the wind radii in 4 quadrants relative
        ! to the storm center (NE,SE,SW,NW) given the azimuthally
        ! average wind radii (rb34,rb50,rb64) the max winds (vmx),
        ! and the speed and heading of the storm motion (spd,head).
        !
        ! An idealized Rankine vortex with a wave number one asymmetry
        ! is fitted to the mean wind radii to give the wind radii in
        ! each qaudrant. The parameters of the Rankine vortex
        ! (maximum wind radius rm and decay exponent x) are also returned.
        !
        ! Input:  rb34 - azimuthally averaged 34 kt wind radius
        !         rb50 - azimuthally averaged 50 kt wind radius
        !         rb64 - azimuthally averaged 64 kt wind radius
        !         vmx  - maximum wind (kt)
        !         spd  - storm speed of motion (kt)
        !         head - storm heading (deg clockwise from N)
        !         slat - storm latitude (degrees)
        !
        ! Output: pr34(4) - array with 34 kt wind radii (nm)  NE,SE,SW,NW of center
        !         pr50(4) - array with 50 kt wind radii (nm)  NE,SE,SW,NW of center
        !         pr64(4) - array with 64 kt wind radii (nm)  NE,SE,SW,NW of center
        !         rm     - radius of max winds (nm) from vortex model fit
        !         x      - Decay exponent from vortex model fit
          use cons 

          real, intent(in) :: rb34, rb50, rb64, vmx, spd, head, slat
          real, intent(inout) :: pr34(6),pr50(6),pr64(6)
          real, intent(inout) :: rm, x

          ! Internal work array
          real :: cf(0:125,0:125)

          real :: al1, al2, a, vmxa, vmx2a, ac34, ac50, ac64, theta0
          real :: rasf, wttc, x0, dx, rm0, dr, aclow,  wtval, rmc, srm 
          real :: xc, sx
          real :: s34, s50, s64
          real :: rmt, xt, cmin, xi 
          integer :: imin, jmin
          real :: tb34, tb50, tb64
          real :: fb34, fb50, fb64
          real :: wttc34, wttc50, wttc64
          real :: w34, w50, w64
          real :: hemfac, q, theta
          integer :: nx, nr, iprt, i, k, j

          ! Set weights for climatology penalty terms for x, rm
          al1 = 0.1
          al2 = 0.1

          ! Specify angle for adjusting max winds relative to the direction
          ! 90 deg to the right of the direction of motion
          theta0 = 0.0

          ! Specify weight for adjusting the asymmetry factor
          rasf = 0.6

          ! Specify weight for case when wind threshold is too close
          ! to vmx, or set wttc=-1.0 to calculate wttc based upon azimuthal
          ! distance covered by each wind radii.
          wttc = -1.0

          ! Specify search interval for x,rm
          x0 = 0.01
          dx = 0.01
          nx = 125

          rm0 = 5.0
          dr  = 1.0
          nr  = 125

          ! Initialize output variables to zero
          do  k=1,4
            pr34(k) = 0.0
            pr50(k) = 0.0
            pr64(k) = 0.0
          end do

          x  = 0.0
          rm = 0.0

          ! Calculate asymmetry factor from spd
          if (spd .le. 0.0) then
            a = 0.0
          else
            a = rasf*1.5*(spd**0.63)
          end if

          vmxa  = vmx - a
          vmx2a = vmx - 2.0*a

          ! Find azimuth covered by each wind radius
          if (a .le. 0.0) then
            ac34 = 360.0
            ac50 = 360.0
            ac64 = 360.0
          else

            if (vmx2a .ge. 34.0) then
              ac34 = 360.0
            else
              if (vmx .le. 34.0) then
               ac34 = 0.0
              else
               ac34 = 2.0*rtd*acos(1.0 - (vmx-34.0)/a)
              end if
            end if

            if (vmx2a .ge. 50.0) then
              ac50 = 360.0
            else
              if (vmx .le. 50.0) then
                ac50 = 0.0
              else
                ac50 = 2.0*rtd*acos(1.0 - (vmx-50.0)/a)
              end if
            end if

            if (vmx2a .ge. 64.0) then
              ac64 = 360.0
            else
              if (vmx .le. 64.0) then
                ac64 = 0.0
              else
                ac64 = 2.0*rtd*acos(1.0 - (vmx-64.0)/a)
              end if
            end if

          end if

          ! Set wttc variables
          aclow = 180.0
          wtval = 0.1

          if (wttc .lt. 0.0) then
            if (ac34 .lt. aclow) then
              wttc34 = wtval
            else if (ac34 .ge. 360.0) then
              wttc34 = 1.0
            else
              wttc34 = wtval + (1.0-wtval)*(ac34-aclow)/(360.0-aclow)
            endif

            if (ac50 .lt. aclow) then
              wttc50 = wtval
            else if (ac50 .ge. 360.0) then
              wttc50 = 1.0
            else
              wttc50 = wtval + (1.0-wtval)*(ac50-aclow)/(360.0-aclow)
            end if

            if (ac64 .lt. aclow) then
              wttc64 = wtval
            elseif (ac64 .ge. 360.0) then
              wttc64 = 1.0
            else
              wttc64 = wtval + (1.0-wtval)*(ac64-aclow)/(360.0-aclow)
            end if
          else
            wttc34 = wttc
            wttc50 = wttc
            wttc64 = wttc
          end if

          ! Check maximum wind and set values accordingly.
          if (vmx .lt. 34.0) then
            return
          else if (vmx .ge. 34.0 .and. vmx .lt. 50.0) then
            w34 = wttc34
            w50 = 0.0
            w64 = 0.0
          else if (vmx .gt. 50.0 .and. vmx .lt. 64.0) then
            w34 = wttc34
            w50 = wttc50
            w64 = 0.0
          else
            w34 = wttc34
            w50 = wttc50
            w64 = wttc64
          end if

          ! Calculate climatological rm,x and their standard deviations
          ! from empirical formulas
          rmc = 54.0 - .27*vmx
          if (rmc .lt. 18.0) rmc = 18.0

          srm = 33.0 - .21*vmx
          if (srm .lt.  6.0) srm = 6.0

          xc = .42 + .0025*vmx
          sx = .10

          ! Specify wind radii standard deviations (indep. of vmax)
          s34 = 43.0
          s50 = 30.0
          s64 = 22.0

          !     write(6,810) rmc,srm,xc,sx,ac34,ac50,ac64,w34,w50,w64,a
          ! 810 format(' rm mean,std: ',f5.1,1x,f5.1,/, &
          !            '  x mean,std: ',f5.3,1x,f5.3,/, &
          !            ' ac34,50,64:  ',f5.1,1x,f5.1,1x,f5.1,/, &
          !            ' wt34,50,64:  ',f5.3,1x,f5.3,1x,f5.3,/, &
          !            ' asym factor: ',f5.1)

          ! Start search loop for x,rm

          do i=0,nx
            do j=0,nr
              rmt = rm0 + float(j)*dr
              xt  = x0  + float(i)*dx

              ! Calculate mean radii for current values of x,rm
              call rbar(vmx,34.0,a,rmt,xt,tb34)
              call rbar(vmx,50.0,a,rmt,xt,tb50)
              call rbar(vmx,64.0,a,rmt,xt,tb64)

              ! Calculate cost function
              cf(i,j) = w34*((tb34-rb34)/s34)**2 + &
                        w50*((tb50-rb50)/s50)**2 + &
                        w64*((tb64-rb64)/s64)**2 + &
                        al1*((xt  -xc  )/sx )**2 + &
                        al2*((rmt -rmc )/srm)**2 
            end do
          end do


          iprt = 0
          if (iprt .eq. 1) then
            ! Print cost function
            write(6,300)
            300 format(/,' COST FUNCTION')

            do  j=nr,0,-1
              write(6,310) j,(cf(i,j),i=0,nx)
              310 format(1x,i2,1x,11(f4.1,1x))
            end do
            write(6,320) (i,i=0,nx)
            320 format(4x,11(1x,i2,2x))
          end if

          ! Find cost function minimum
          cmin = 1.0e+10
          do j=0,nr
            do i=0,nx
              if (cf(i,j) .lt. cmin) then
                imin = i
                jmin = j
                cmin = cf(i,j)
              endif
            end do
          end do

          x = x0 + dx*float(imin)
          rm = rm0+ dr*float(jmin)

          ! Calculate best fit mean radii
          call rbar(vmx,34.0,a,rm,x,fb34)
          call rbar(vmx,50.0,a,rm,x,fb50)
          call rbar(vmx,64.0,a,rm,x,fb64)

          ! write(6,200) fb34,fb50,fb64
          ! 200 format('  Fit 34,50,64 kt wind radii:  ',3(f5.0,1x))

          ! Put fit to mean radii in element 6 of pr arrays
          pr34(6) =fb34
          pr50(6) =fb50
          pr64(6) =fb64

          ! Calculate wind radii in each quadrant
          xi = 1.0/x

          hemfac=1.0
          if (slat.lt.0.0)then  ! a knaff change for SH.
            hemfac=-1.0
          endif

          do k=1,4
            q = 45.0 + 90.0*(float(k-1))
            theta = dtr*(head + hemfac*90.0 - q -theta0)

            pr34(k) = rm*( (vmx-a)/(34.0-a*cos(theta)) )**xi
            if (pr34(k) .lt. rm) pr34(k) = 0.0

            pr50(k) = rm*( (vmx-a)/(50.0-a*cos(theta)) )**xi
            if (pr50(k) .lt. rm) pr50(k) = 0.0

            pr64(k) = rm*( (vmx-a)/(64.0-a*cos(theta)) )**xi
            if (pr64(k) .lt. rm) pr64(k) = 0.0
          end do

          return

        end subroutine wrasym

        subroutine rbar(vm,v,a,rm,x,rb)
        ! This routine calculates the azimuthally averaged wind radii 
        ! (rb) for wind speed v, max wind vm, asymmetry factor a, radius
        ! of max wind rm, and Rankine vortex factor x.  
          use cons 
  
          real, intent(in) :: vm, v, a, rm, x
          real, intent(inout) :: rb
  
          real :: xi, dt, theta, fac
          integer :: nt
          integer :: i, ncount
          integer :: ierr
   
          ! Check for illegal values of x,rm
          if (x .le. 0.0 .or. rm .le. 0.0) then
            write(6,100) 
            100 format('WARNING: Illegal values of x or rm input to routine rbar')
            ierr=102
            call exit(ierr)   
          end if
 
          ! Check for wind threshold greater than max wind
          if (v .gt. vm) then
             rb = 0.0
             return
          end if
  
          xi = 1.0/x
          nt = 72
          dt = 360.0/float(nt)
          rb = 0.0
   
          ! Set up new counter to only average azimuths with wind radii > 0
          ncount = 0
  
          do i=1,nt
            theta = dtr*dt*float(i)
            fac = (vm-a)/(v-a*cos(theta))
            if (fac .lt. 1.0) then
              fac = 0.0
            else
              fac = fac**xi
              ! Increment new counter for azimuths with wind radii > 0	    
              ncount = ncount + 1
            end if
   
            rb = rb + fac
          end do
  
          ! Check that ncount isn't 0 and modify calculation of rb
          ! so that it's only averaged over azimuths with wind radii > 0      
          if (ncount .le. 0) then
            rb = rm*(1.1)
          else 
             rb = rm*rb/float(ncount)
          end if
  
          !  rb = rm*rb/float(nt)      
   
          return
  
        end subroutine rbar
  
      end module vradsubs 