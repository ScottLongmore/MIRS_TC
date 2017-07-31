MODULE mreg_verify
!==============================================================================
! This module contains the subroutines which check the results of the
! regression.
!==============================================================================

USE mreg_read
USE mreg_regress
USE utils_stat

IMPLICIT NONE
SAVE

REAL, ALLOCATABLE, DIMENSION(:) :: y
REAL, ALLOCATABLE, DIMENSION(:) :: yhat

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE verify_wintercept
! This subroutine checks the multiple linear regression created from the
! non-standarized predictors and using an intercept.
!..............................................................................
INTEGER :: count
REAL :: bias
REAL :: rmse
REAL :: mae
REAL, ALLOCATABLE, DIMENSION(:,:) :: x
!..............................................................................

! Loop over input files and fill data arrays
DO l=1,npr_and
  string=TRIM(datadir)//TRIM(veriffile(l))
  OPEN(UNIT=lunum,FILE=string,STATUS='OLD')

! Count the number of observations for each of the variables. My Fortran 90 book
! says the way I'm going to use the READ statement, with the END keyword, is not
! the best. I'll keep it around for now, but will look to improve it sometime.
  READ(lunum,*)
  READ(lunum,*)
  count=0
  DO n=1,mxobs
    READ(lunum,*,END=500)
    count=count+1
  ENDDO
500 CONTINUE 

! Rewind data and read file
  ALLOCATE(x(count,npr_or))
  ALLOCATE(y(count))
  REWIND(UNIT=lunum) 
  READ(lunum,*)
  READ(lunum,*)
  DO n=1,count
    READ(lunum,*) y(n),(x(n,m),m=1,npr_or)
    IF(l==2) THEN
      y(n)=1050.0-EXP(y(n))
    ENDIF
  ENDDO
  CLOSE(UNIT=lunum)

! Pass through y and x arrays and count number of missing data.
  nmiss=0
  DO n=1,nobs(l)
    IF(ABS(y(n)-rmiss) <= EPSILON(1.0)) THEN
      nmiss=nmiss+1
    ENDIF
    DO m=1,npr_or
      IF(ABS(x(n,m)-rmiss) <= EPSILON(1.0)) THEN
        nmiss=nmiss+1
      ENDIF
    ENDDO
  ENDDO
  WRITE(*,510) TRIM(veriffile(l)),nmiss

! Compute the regression estimate, yhat.
  ALLOCATE(yhat(count))
  DO n=1,count
    yhat(n)=xcoef(npr_or+1,l)
    DO m=1,npr_or
      yhat(n)=yhat(n)+xcoef(m,l)*x(n,m)*weight(m,l)
    ENDDO
    IF(l==2) THEN
      yhat(n)=1050.0-EXP(yhat(n))
    ENDIF
  ENDDO

! Check the quality of the regression estimate
  nmiss=0
  CALL bias_rmse_mae(yhat,y,count,bias,rmse,mae,nmiss)
  WRITE(*,*) TRIM(veriffile(l)),count-nmiss,bias,rmse,mae

! Write out predicted and actual values to file.
  OPEN(UNIT=22,FILE=TRIM(veriffile(l))//'.compare',STATUS='REPLACE')
  DO n=1,count
    WRITE(22,*) n,yhat(n),y(n)
  ENDDO 
  CLOSE(UNIT=22)

  DEALLOCATE(x)
  DEALLOCATE(y)
  DEALLOCATE(yhat)
  
! Write coefficients to a file
  OPEN(UNIT=22,FILE=TRIM(datafile(l))//'.coef',STATUS='REPLACE')
  WRITE(22,*) xcoef(npr_or+1,l)*weight(npr_or+1,l)
  DO m=1,npr_or
    WRITE(22,*) xcoef(m,l)*weight(m,l)
  ENDDO
  CLOSE(UNIT=22)

ENDDO

510 FORMAT('Regression verification file ',A15,' has ',I4,' missing predictors &
            and ATCF values of predictands.')

END SUBROUTINE verify_wintercept
!------------------------------------------------------------------------------
SUBROUTINE verify_atcf
! The wind radii validated in verify_wintercept are not the radii sent to the
! ATCF. They correspond to oparet's rb34, rb50, and rb64. These radii are sent
! to subroutine wrasym for further refinement and then to the ATCF via the
! *.AFX files. This subroutine will verify the wind radii as they would be 
! sent to the ATCF.  I'll go back to Galina's original files before I ran them
! through gc_to_as.x.  I'll apply the coefficients to the predictors and the
! winds will pass through wrasym before being validated. Hopefully it will be
! the case that the coefficients from the best fit of rb34, rb50, and rb64 to 
! the best track data also produce a good result of the final r34, r50, and 
! r64.

INTEGER :: nlines
INTEGER :: ivmax
INTEGER :: ipmin 
INTEGER :: n34atcf
INTEGER :: n50atcf
INTEGER :: n64atcf
INTEGER :: n34oper
INTEGER :: n50oper
INTEGER :: n64oper
INTEGER :: n34both
INTEGER :: n50both
INTEGER :: n64both
INTEGER :: numrad
INTEGER :: nmiss

REAL, ALLOCATABLE, DIMENSION(:,:) :: pred
REAL, ALLOCATABLE, DIMENSION(:) :: vmax
REAL, ALLOCATABLE, DIMENSION(:) :: pmin
REAL, ALLOCATABLE, DIMENSION(:) :: rb34
REAL, ALLOCATABLE, DIMENSION(:) :: rb50
REAL, ALLOCATABLE, DIMENSION(:) :: rb64
REAL, ALLOCATABLE, DIMENSION(:) :: pr34
REAL, ALLOCATABLE, DIMENSION(:) :: pr50
REAL, ALLOCATABLE, DIMENSION(:) :: pr64
REAL, ALLOCATABLE, DIMENSION(:,:) :: pr34n
REAL, ALLOCATABLE, DIMENSION(:,:) :: pr50n
REAL, ALLOCATABLE, DIMENSION(:,:) :: pr64n
REAL, ALLOCATABLE, DIMENSION(:) :: r34atcf
REAL, ALLOCATABLE, DIMENSION(:) :: r50atcf
REAL, ALLOCATABLE, DIMENSION(:) :: r64atcf
REAL, ALLOCATABLE, DIMENSION(:) :: spd
REAL, ALLOCATABLE, DIMENSION(:) :: heading
LOGICAL, ALLOCATABLE, DIMENSION(:) :: atcf_c  !Valid system spd heading

REAL :: rm
REAL :: x
REAL :: bias
REAL :: rmse
REAL :: mae

TYPE best_track
  CHARACTER(LEN=2), ALLOCATABLE, DIMENSION(:) :: basin
  CHARACTER(LEN=2), ALLOCATABLE, DIMENSION(:) :: cy
  CHARACTER(LEN=10), ALLOCATABLE, DIMENSION(:) :: datime
  REAL, ALLOCATABLE, DIMENSION(:) :: lat
  REAL, ALLOCATABLE, DIMENSION(:) :: lon
  REAL, ALLOCATABLE, DIMENSION(:) :: vmax
  REAL, ALLOCATABLE, DIMENSION(:) :: pmin
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: r34
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: r50
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: r64
  CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: name
END TYPE best_track

TYPE(best_track) :: bt

!..............................................................................
! Opening print statement
WRITE(*,*)
WRITE(*,*) 'This portion of the verification is for the winds only. The winds &
&used here are after the call to wrasym, and are what would be sent to the &
&f-deck'

! Open Galina's file and count the number of lines. My Fortran 90 book
! says the way I'm going to use the READ statement, with the END keyword, is not
! the best. I'll keep it around for now, but will look to improve it sometime.
OPEN(UNIT=lunum, FILE='/home/dostalek/data/mult_reg/verif.txt', &
     STATUS='OLD')
nlines=0
DO n=1,mxobs
  READ(lunum,*,END=500)
  nlines=nlines+1
ENDDO
500 CONTINUE
print*, nlines,' lines in verif.txt'

! Allocate arrays
ALLOCATE(pred(nlines,npr_or))
ALLOCATE(bt%basin(nlines))
ALLOCATE(bt%cy(nlines))
ALLOCATE(bt%datime(nlines))
ALLOCATE(bt%lat(nlines))
ALLOCATE(bt%lon(nlines))
ALLOCATE(bt%vmax(nlines))
ALLOCATE(bt%pmin(nlines))
ALLOCATE(bt%r34(nlines,4))
ALLOCATE(bt%r50(nlines,4))
ALLOCATE(bt%r64(nlines,4))
ALLOCATE(vmax(nlines))
ALLOCATE(pmin(nlines))
ALLOCATE(rb34(nlines))
ALLOCATE(rb50(nlines))
ALLOCATE(rb64(nlines))
ALLOCATE(pr34(6))
ALLOCATE(pr50(6))
ALLOCATE(pr64(6))
ALLOCATE(pr34n(nlines,6))
ALLOCATE(pr50n(nlines,6))
ALLOCATE(pr64n(nlines,6))
ALLOCATE(r34atcf(nlines))
ALLOCATE(r50atcf(nlines))
ALLOCATE(r64atcf(nlines))
ALLOCATE(spd(nlines))
ALLOCATE(heading(nlines))
ALLOCATE(atcf_c(nlines))

! Rewind input file and read through, filling arrays
REWIND(UNIT=lunum)
DO n=1,nlines
  READ(lunum,510) bt%basin(n),bt%cy(n),bt%datime(n),bt%lat(n),bt%lon(n),&
  ivmax,ipmin,(bt%r34(n,m),m=1,4),(bt%r50(n,m),m=1,4),(bt%r64(n,m),m=1,4),&
  (pred(n,m),m=1,18),pred(n,19),(pred(n,m),m=21,npr_or),pred(n,20),spd(n),&
  heading(n)
  bt%vmax(n)=REAL(ivmax)
  bt%pmin(n)=ALOG(REAL(1050-ipmin))
ENDDO
CLOSE(UNIT=lunum)

! Compute vmax, pmin, rb34, rb50, rb64 with regression equation
DO n=1,nlines
  vmax(n)=xcoef(npr_or+1,1)
  pmin(n)=xcoef(npr_or+1,2)
  rb34(n)=xcoef(npr_or+1,3)
  rb50(n)=xcoef(npr_or+1,4)
  rb64(n)=xcoef(npr_or+1,5)
  DO m=1,npr_or
    vmax(n)=vmax(n)+pred(n,m)*xcoef(m,1)*weight(m,1)
    pmin(n)=pmin(n)+pred(n,m)*xcoef(m,2)*weight(m,2)
    rb34(n)=rb34(n)+pred(n,m)*xcoef(m,3)*weight(m,3)
    rb50(n)=rb50(n)+pred(n,m)*xcoef(m,4)*weight(m,4)
    rb64(n)=rb64(n)+pred(n,m)*xcoef(m,5)*weight(m,5)
  ENDDO 
  IF(rb34(n) < 0.0) rb34(n)=0.0
  IF(rb50(n) < 0.0) rb50(n)=0.0
  IF(rb64(n) < 0.0) rb64(n)=0.0
  pr34(5)=rb34(n)
  pr50(5)=rb50(n)
  pr64(5)=rb64(n)

! Get final wind radii which go to the ATCF
  CALL  wrasym(rb34(n),rb50(n),rb64(n),pred(n,20),spd(n),heading(n),rm,x,pr34,&
  pr50,pr64,pred(n,19))
  
  DO l=1,6
    pr34n(n,l)=pr34(l)
    pr50n(n,l)=pr50(l)
    pr64n(n,l)=pr64(l)
  ENDDO

ENDDO

! Get validation statistics on the mean radii (pr34n(n,6),pr50n(n,6), and
! pr64n(n,6) as compared to the mean radii from the best track: bt%r34,
! bt%r50, and bt%r64. Depending on the intensity of the cyclone, the radii may
! or may not be present, so check for that.

! Find out how many have observed mean wind radii
n34atcf=0
n50atcf=0
n64atcf=0
DO n=1,nlines

  r34atcf(n)=0.0
  r50atcf(n)=0.0
  r64atcf(n)=0.0

! For observed and predicted radii, check for spd
! and heading not equal to 999.
  atcf_c(n)=.FALSE.
  IF(spd(n) < 999. .AND. heading(n) < 999.) THEN
    atcf_c(n)=.TRUE.
  ENDIF

  numrad=0
  DO m=1,4
    IF(bt%r34(n,m) > 0.0) THEN
      r34atcf(n)=r34atcf(n)+bt%r34(n,m)
      numrad=numrad+1
    ENDIF
  ENDDO
  r34atcf(n)=r34atcf(n)/REAL(numrad)
  IF(r34atcf(n) > 0.0 .AND. atcf_c(n)) n34atcf=n34atcf+1

  numrad=0
  DO m=1,4
    IF(bt%r50(n,m) > 0.0) THEN
      r50atcf(n)=r50atcf(n)+bt%r50(n,m)
      numrad=numrad+1
    ENDIF
  ENDDO
  r50atcf(n)=r50atcf(n)/REAL(numrad)
  IF(r50atcf(n) > 0.0 .AND. atcf_c(n)) n50atcf=n50atcf+1

  numrad=0
  DO m=1,4
    IF(bt%r64(n,m) > 0.0) THEN
      r64atcf(n)=r64atcf(n)+bt%r64(n,m)
      numrad=numrad+1
    ENDIF
  ENDDO
  r64atcf(n)=r64atcf(n)/REAL(numrad)
  IF(r64atcf(n) > 0.0 .AND. atcf_c(n)) n64atcf=n64atcf+1

ENDDO
WRITE(*,*) 'Numbers of valid best-track observations:'
WRITE(*,*) '34-kt radius ',n34atcf
WRITE(*,*) '50-kt radius ',n50atcf
WRITE(*,*) '64-kt radius ',n64atcf
WRITE(*,*)

! Find out how many have predicted mean wind radii
n34oper=0
n50oper=0
n64oper=0
DO n=1,nlines
  IF(pr34n(n,6) > 0.0 .AND. atcf_c(n)) n34oper=n34oper+1
  IF(pr50n(n,6) > 0.0 .AND. atcf_c(n)) n50oper=n50oper+1
  IF(pr64n(n,6) > 0.0 .AND. atcf_c(n)) n64oper=n64oper+1
ENDDO
WRITE(*,*) 'Numbers of valid oparet observations:'
WRITE(*,*) '34-kt radius ',n34oper
WRITE(*,*) '50-kt radius ',n50oper
WRITE(*,*) '64-kt radius ',n64oper
WRITE(*,*)

! Find out how many times have both observed and predicted mean
! wind radii.
n34both=0
n50both=0
n64both=0
DO n=1,nlines
  IF(pr34n(n,6) > 0.0 .AND. r34atcf(n) > 0.0 .AND. atcf_c(n)) n34both=n34both+1
  IF(pr50n(n,6) > 0.0 .AND. r50atcf(n) > 0.0 .AND. atcf_c(n)) n50both=n50both+1
  IF(pr64n(n,6) > 0.0 .AND. r64atcf(n) > 0.0 .AND. atcf_c(n)) n64both=n64both+1
ENDDO
WRITE(*,*) 'Numbers of overlapping observations:'
WRITE(*,*) '34-kt radius ',n34both
WRITE(*,*) '50-kt radius ',n50both
WRITE(*,*) '64-kt radius ',n64both
WRITE(*,*)

WRITE(*,*) 'Statistics for winds from wrasym '
WRITE(*,*) '(Number,bias,rmse,mae)'
! Compare 34-kt winds
ALLOCATE(y(n34both))
ALLOCATE(yhat(n34both))
m=0
DO n=1,nlines
  IF(pr34n(n,6) > 0.0 .AND. r34atcf(n) > 0.0 .AND. atcf_c(n)) THEN
    m=m+1
    y(m)=r34atcf(n)
    yhat(m)=pr34n(n,6)
  ENDIF
ENDDO
nmiss=0
CALL bias_rmse_mae(yhat,y,m,bias,rmse,mae,nmiss)
WRITE(*,*) '34-kt wind: ',m-nmiss,bias,rmse,mae
DEALLOCATE(y)
DEALLOCATE(yhat)

! Compare 50-kt winds
ALLOCATE(y(n50both))
ALLOCATE(yhat(n50both))
m=0
DO n=1,nlines
  IF(pr50n(n,6) > 0.0 .AND. r50atcf(n) > 0.0 .AND. atcf_c(n)) THEN
    m=m+1
    y(m)=r50atcf(n)
    yhat(m)=pr50n(n,6)
  ENDIF
ENDDO
nmiss=0
CALL bias_rmse_mae(yhat,y,m,bias,rmse,mae,nmiss)
WRITE(*,*) '50-kt wind: ',m-nmiss,bias,rmse,mae
DEALLOCATE(y)
DEALLOCATE(yhat)

! Compare 64-kt winds
ALLOCATE(y(n64both))
ALLOCATE(yhat(n64both))
m=0
DO n=1,nlines
  IF(pr64n(n,6) > 0.0 .AND. r64atcf(n) > 0.0 .AND. atcf_c(n)) THEN
    m=m+1
    y(m)=r64atcf(n)
    yhat(m)=pr64n(n,6)
  ENDIF
ENDDO
nmiss=0
CALL bias_rmse_mae(yhat,y,m,bias,rmse,mae,nmiss)
WRITE(*,*) '64-kt wind: ',m-nmiss,bias,rmse,mae
DEALLOCATE(y)
DEALLOCATE(yhat)

! Deallocate arrays
DEALLOCATE(pred)
DEALLOCATE(vmax)
DEALLOCATE(pmin)
DEALLOCATE(rb34)
DEALLOCATE(rb50)
DEALLOCATE(rb64)
DEALLOCATE(pr34)
DEALLOCATE(pr50)
DEALLOCATE(pr64)
DEALLOCATE(pr34n)
DEALLOCATE(pr50n)
DEALLOCATE(pr64n)
DEALLOCATE(r34atcf)
DEALLOCATE(r50atcf)
DEALLOCATE(r64atcf)
DEALLOCATE(spd)
DEALLOCATE(heading)
DEALLOCATE(atcf_c)

DEALLOCATE(bt%basin)
DEALLOCATE(bt%cy)
DEALLOCATE(bt%datime)
DEALLOCATE(bt%lat)
DEALLOCATE(bt%lon)
DEALLOCATE(bt%vmax)
DEALLOCATE(bt%pmin)
DEALLOCATE(bt%r34)
DEALLOCATE(bt%r50)
DEALLOCATE(bt%r64)

! Format statements
510 FORMAT(2X,A2,4X,A2,3X,A10,4X,F7.3,4X,F8.3,5X,I3,4X,I4,6X,4(4X,I3),9X,4(6X,I3),&
           9X,4(6X,I3),203X,F6.1,4(3X,F6.1),4X,F6.1,5X,10(F6.1,4X),2(F6.2,4x),&
           F6.2,3X,F7.3,3X,2(F7.3,3X),1X,F8.3,3(1X,F7.0))

END SUBROUTINE verify_atcf

!..............................................................................
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

          real, intent(in) :: rb34, rb50, rb64, vmx, spd, head, slat
          real, intent(inout) :: pr34(6),pr50(6),pr64(6)
          real, intent(inout) :: rm, x
          real, parameter :: pi=3.14159
          real, parameter :: dtr=0.0174533
          real, parameter :: rtd=180.0/pi

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
!..............................................................................
        subroutine rbar(vm,v,a,rm,x,rb)
        ! This routine calculates the azimuthally averaged wind radii 
        ! (rb) for wind speed v, max wind vm, asymmetry factor a, radius
        ! of max wind rm, and Rankine vortex factor x.  
  
          real, intent(in) :: vm, v, a, rm, x
          real, intent(inout) :: rb
  
          real :: xi, dt, theta, fac
          real, parameter :: dtr=0.0174533
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
!==============================================================================
END MODULE mreg_verify
