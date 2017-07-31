MODULE mreg_regress
!==============================================================================
! This module contains subroutines for different options for the
! multiple regression, most of which come from the IMSL library.
!==============================================================================

USE mreg_read
USE utils_stat

IMPLICIT NONE
SAVE

REAL,DIMENSION(8) :: amach
REAL, ALLOCATABLE, DIMENSION(:,:) :: weight    !Weight of predictor
REAL, ALLOCATABLE, DIMENSION(:,:) :: xcoef

CONTAINS

!-------------------------------------------------------------------------------
SUBROUTINE stepwise 
! This subroutine acts as an interface to IMSL's rstep subroutine, a stepwise
! multiple linear regression.

REAL, ALLOCATABLE, DIMENSION(:) :: x         !Predictors
REAL, ALLOCATABLE, DIMENSION(:) :: xstand    !Predictors (standardized)
REAL, ALLOCATABLE, DIMENSION(:) :: y         !Predictand
REAL, ALLOCATABLE, DIMENSION(:) :: ystand    !Predictand (standardized)
REAL, ALLOCATABLE, DIMENSION(:,:) :: xystand !All variables (standardized)
REAL :: ymn     !Mean of y
REAL :: ysdv    !Standard deviation of y
REAL, ALLOCATABLE, DIMENSION(:) :: xmn     !Mean of x
REAL, ALLOCATABLE, DIMENSION(:) :: xsdv    !Standard deviation of x
REAL, ALLOCATABLE, DIMENSION(:) :: xymn    !Mean of x and y
REAL, ALLOCATABLE, DIMENSION(:,:) :: cov   !Variance-covariance array

! For corvc
INTEGER :: ido
INTEGER :: nvar
INTEGER :: ldx
INTEGER :: ifrq
INTEGER :: iwt
INTEGER :: mopt
INTEGER :: icopt
INTEGER :: ldcov
INTEGER :: ldincd
INTEGER :: totobs
INTEGER :: totmiss
REAL :: sumwt
INTEGER, DIMENSION(1) :: incd

! For rstep (beyond what was needed for corvc)
INTEGER :: invoke
INTEGER, ALLOCATABLE, DIMENSION(:) :: level
INTEGER :: nforce
INTEGER :: nstep
INTEGER :: istep
REAL :: pin
REAL :: pout
REAL :: tol
INTEGER :: iprint
REAL, ALLOCATABLE, DIMENSION(:) :: scale
REAL, ALLOCATABLE, DIMENSION(:) :: hist
INTEGER :: iend
REAL, DIMENSION(13) :: aov
INTEGER :: ldcoef
REAL, DIMENSION(npr_or+1,5) :: coef
!
!..............................................................................
! ALLOCATE some arrays
ALLOCATE(xcoef(npr_or+1,npr_and))
ALLOCATE(weight(npr_or+1,npr_and))

! Loop over the predictands
DO l=1,npr_and

! Allocate more arrays
  ALLOCATE(y(nobs(l)))
  ALLOCATE(ystand(nobs(l)))
  ALLOCATE(x(nobs(l)))
  ALLOCATE(xstand(nobs(l)))
  ALLOCATE(xystand(nobs(l),npr_or+2))
  ALLOCATE(xmn(npr_or))
  ALLOCATE(xsdv(npr_or))
  ALLOCATE(xymn(npr_or+1))
  ALLOCATE(cov(npr_or+2,npr_or+2))
  ALLOCATE(level(npr_or+2))
  ALLOCATE(scale(npr_or+2))
  ALLOCATE(hist(npr_or+2))

! Standardize predictand
  DO n=1,nobs(l)
    y(n)=pr_and(n,l)
  ENDDO
 
  CALL astand(nobs(l),y,rmiss,ystand,ymn,ysdv)

! Standardize predictors and begin filling in array xystand
  DO m=1,npr_or
    DO n=1,nobs(l)
      x(n)=pr_or(n,m,l)
    ENDDO

    CALL astand(nobs(l),x,rmiss,xstand,xmn(m),xsdv(m))

    DO n=1,nobs(l)
      xystand(n,m)=x(n)
    ENDDO
  ENDDO

! Finish filling in xystand with standardized predictand and weights.
! Subroutine corvc allows also for frequencies, but I won't include those
! at this point. Also for now, weights will be one, but also not used.
  DO n=1,nobs(l)
    xystand(n,npr_or+1)=1.0
    xystand(n,npr_or+2)=y(n) 
  ENDDO

! Assign values for use with subroutine corvc
  ido=0
  nvar=npr_or+2
  ldx=nobs(l)+1
  ifrq=0
  iwt=0
  mopt=0
  icopt=0
  ldcov=npr_or+2
  ldincd=1
  
!  CALL corvc(ido,nobs(l),nvar,xystand,ldx,ifrq,iwt,mopt,icopt,xymn,cov, &
!             ldcov,incd,ldincd,totobs,totmiss,sumwt)

  CALL mxtxf(nobs(l),nvar,xystand,nobs(l),nvar,cov,nvar)

! Assign values for use with subroutine rstep
  invoke=0
  nforce=1
  nstep=-1
  istep=1
  pin=0.05
  pout=2.0*pin
  tol=100.0*AMACH(4)
  iprint=0
  ldcoef=npr_or+1

  DO m=1,npr_or
    level(m)=onoff(m,l)
  ENDDO
  level(npr_or+1)=1
  level(npr_or+2)=-1

! Call to IMSL's rstep for regression 
  CALL rstep(invoke,nvar,cov,ldcov,level,nforce,nstep,istep,nobs(l)+1,pin,pout,tol, &
        iprint,scale,hist,iend,aov,coef,ldcoef,cov,ldcov)

! Save coefficients to array
  DO m=1,npr_or+1
    xcoef(m,l)=coef(m,1)
  ENDDO

! Calculate weight of predictors
  DO m=1,npr_or+1
    IF(level(m) > 0) THEN
      weight(m,l)=REAL(level(m)/level(m))
    ELSE
      weight(m,l)=0.0
    ENDIF
  ENDDO

! Deallocate arrays
  DEALLOCATE(y)
  DEALLOCATE(ystand)
  DEALLOCATE(x)
  DEALLOCATE(xstand)
  DEALLOCATE(xystand)
  DEALLOCATE(xmn)
  DEALLOCATE(xsdv)
  DEALLOCATE(xymn)
  DEALLOCATE(cov)
  DEALLOCATE(level)
  DEALLOCATE(scale)
  DEALLOCATE(hist)

ENDDO

END SUBROUTINE stepwise 
!-------------------------------------------------------------------------------
END MODULE mreg_regress
