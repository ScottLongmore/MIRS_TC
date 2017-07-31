MODULE utils_stat
!==============================================================================
! This module contains subroutines which perform basic statistic
! calculations.
!==============================================================================

IMPLICIT NONE

SAVE

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE astand(nvals,x,xmiss,xstand,xmn,xsdv)

! Given a 1-D array of values, this subroutine returns an array of the
! same length containing the standardized values of the input array.
! Standardization is subtracting off the mean and dividing by the
! standard deviation. The mean and standar deviation are also given.
!
!..............................................................................

INTEGER :: n
INTEGER :: nvals
REAL, DIMENSION(nvals) :: x
REAL, DIMENSION(nvals) :: xstand
REAL :: xmn
REAL :: xsdv
REAL :: xmiss
REAL :: sum

! Compute the mean
xmn=0.0
sum=0.0
DO n=1,nvals
  IF(ABS(x(n)-xmiss) > EPSILON(1.0)) THEN
    xmn=xmn+x(n)
    sum=sum+1.0
  ENDIF
ENDDO 
xmn=xmn/sum

! Compute the standard deviation
xsdv=0.0
sum=0.0
DO n=1,nvals
  IF(ABS(x(n)-xmiss) > EPSILON(1.0)) THEN
    xsdv=xsdv+(x(n)-xmn)**2
    sum=sum+1.0
  ENDIF
ENDDO 
xsdv=SQRT(xsdv/sum)

! Fill in array of standardized values
DO n=1,nvals
  xstand(n)=(x(n)-xmn)/xsdv
ENDDO

END SUBROUTINE astand
!..............................................................................
SUBROUTINE bias_rmse_mae(x,x0,num,bias,rmse,mae,nmiss)
! Given two 1-D arrays, this subroutine compute the bias, rmse, and mae.

INTEGER :: n
INTEGER :: num
INTEGER :: count
INTEGER :: nmiss 
REAL, DIMENSION(num) :: x
REAL, DIMENSION(num) :: x0
REAL :: bias
REAL :: rmse
REAL :: mae

! Compute bias, rmse, and mae
bias=0.0
rmse=0.0
mae=0.0
count=0
DO n=1,num
  IF(x(n) > 0.0) THEN
    bias=bias+x(n)-x0(n)
    rmse=rmse+(x(n)-x0(n))**2
    mae=mae+ABS(x(n)-x0(n))
    count=count+1
  ELSE
    WRITE(*,*) 'Regression produced a negative value ',x(n),' at ',n
    nmiss=nmiss+1
  ENDIF
ENDDO
bias=bias/REAL(count)
rmse=SQRT(rmse/REAL(count))
mae=mae/REAL(count)

END SUBROUTINE bias_rmse_mae

!------------------------------------------------------------------------------
END MODULE utils_stat
