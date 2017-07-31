MODULE mreg_read
!==============================================================================
! This module contains the subroutines for reading the input information
! for the program mreg. It also contains the declaration for many
! variables that will be used throughout the program.
!==============================================================================

IMPLICIT NONE
SAVE

CHARACTER(LEN=120) :: datadir   !Directory containing the data
CHARACTER(LEN=120), ALLOCATABLE, DIMENSION(:) :: datafile   !Data file names
CHARACTER(LEN=120), ALLOCATABLE, DIMENSION(:) :: veriffile   !Data file names
CHARACTER(LEN=120) :: string            !Utility character variable
CHARACTER(LEN=6), ALLOCATABLE, DIMENSION(:) :: name_pr_and !Predictand name
CHARACTER(LEN=6), ALLOCATABLE, DIMENSION(:,:) :: name_pr_or  !Predictor names

INTEGER :: nmiss                !Missing value
INTEGER :: npr_and              !Number of predictands
INTEGER :: npr_or               !Number of predictors
INTEGER :: nchar                !Number of characters in a string
INTEGER :: lunum=11             !General logical unit number
INTEGER :: l,m,n                !Indexing variables
INTEGER :: mxobs=10000          !Maximum number of observations
INTEGER :: count                !Generic counting variable
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: onoff !Include predictor or not
INTEGER, ALLOCATABLE, DIMENSION(:) :: nobs    !Number of observations
INTEGER, ALLOCATABLE, DIMENSION(:) :: tot_onoff !Total predictors

REAL, ALLOCATABLE, DIMENSION(:,:) :: pr_and   !Predictands
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: pr_or  !Predictors
REAL :: rmiss                                 !Missing value

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE read_config
!
!..............................................................................

! Open configuration file and read contents
OPEN(UNIT=lunum,FILE='mreg.config',STATUS='OLD')

READ(lunum,*)
READ(lunum,*) npr_and
READ(lunum,*)

READ(lunum,*)
READ(lunum,*) npr_or
READ(lunum,*)

READ(lunum,*)
READ(lunum,'(A120)') datadir
READ(lunum,*)

ALLOCATE(datafile(npr_and))
ALLOCATE(veriffile(npr_and))
READ(lunum,*)
DO l=1,npr_and
  READ(lunum,*) datafile(l),veriffile(l)
ENDDO
READ(lunum,*)

ALLOCATE(onoff(npr_or,npr_and))
ALLOCATE(tot_onoff(npr_and))
READ(lunum,*)
DO m=1,npr_or
  READ(lunum,*) (onoff(m,l),l=1,npr_and)
ENDDO

! Compute total number of "on" predictors
DO l=1,npr_and
  tot_onoff(l)=0
  DO m=1,npr_or
    tot_onoff(l)=tot_onoff(l)+onoff(m,l)
  ENDDO
ENDDO

CLOSE(UNIT=lunum)

END SUBROUTINE read_config
!------------------------------------------------------------------------------
SUBROUTINE read_data
!
!..............................................................................

! Allocate some arrays
ALLOCATE(nobs(npr_and))
ALLOCATE(pr_and(mxobs,npr_and))
ALLOCATE(pr_or(mxobs,npr_or,npr_and))
ALLOCATE(name_pr_and(npr_and))
ALLOCATE(name_pr_or(npr_or,npr_and))

! Loop over input files and fill data arrays
DO l=1,npr_and
  string=TRIM(datadir)//TRIM(datafile(l))
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
500 nobs(l)=count 

! Rewind data and read file
  REWIND(UNIT=lunum) 
  READ(lunum,*) n,rmiss
  READ(lunum,'(25(4X,A6))') name_pr_and(l),(name_pr_or(m,l),m=1,npr_or)
  DO n=1,nobs(l)
    READ(lunum,*) pr_and(n,l),(pr_or(n,m,l),m=1,npr_or)
  ENDDO
  CLOSE(UNIT=lunum)

! Pass through pr_and and pr_or arrays and count number of missing data.
  nmiss=0
  DO n=1,nobs(l)
    IF(ABS(pr_and(n,l)-rmiss) <= EPSILON(1.0)) THEN
      nmiss=nmiss+1
    ENDIF
    DO m=1,npr_or
      IF(ABS(pr_or(n,m,l)-rmiss) <= EPSILON(1.0)) THEN
        nmiss=nmiss+1
      ENDIF
    ENDDO
  ENDDO
  WRITE(*,510) TRIM(datafile(l)),nmiss

ENDDO

510 FORMAT('Regression development file ',A15,' has ',I4,' missing predictors &
            & and ATCF values of predictands.')
WRITE(*,*)

END SUBROUTINE read_data
!------------------------------------------------------------------------------
END MODULE mreg_read
