PROGRAM afdeck
!======================================================================
! afdeck.f90
!
! This program extracts from the *.inp file the following 
! information for a system at the analysis time:
! latitude
! longitude
! direction of movement
! speed of movement
! radius of maximum winds
! intensity
! name
! For the time 12 hours prior to analysis time, the program retrieves
! the following information:
! latitude
! longitude
! intensity
!
! This information is used in the oparet code for real-time processing
! of the MIRS data.
!
! History:
! 10 Apr 2013   Programming begun
! 06 Jun 2013   Changed and added comments to reflect the updated code.
!               In particular, the program no longer reads from the
!               a and f decks, but reads from the *.inp file, which is
!               created from the aa decks.  The name afdeck is 
!               therefore historical.
! 17 Jun 2012   Implemented more structured error handling.
!
!======================================================================

IMPLICIT NONE

INTEGER :: iargc                !For reading from the command line
INTEGER :: n                    !Indexing variable     
INTEGER :: imon0                !Month
INTEGER :: iday0                !Day
INTEGER :: iyear0               !Year
INTEGER :: julday0              !Julian Day
INTEGER :: lunum=11             !Logical unit number
INTEGER :: nfixes               !Number of fixes in the *.inp file
INTEGER :: n0                   !Index of analysis time
INTEGER :: nm12                 !Index of analysis time -12 hrs
INTEGER, ALLOCATABLE, DIMENSION(:) :: idir      !System direction as given in
                                                !the *.inp file.  Will be recalculated
INTEGER, ALLOCATABLE, DIMENSION(:) :: ispd      !System speed as give in the
                                                !*.inp file.  Will be recalculated
INTEGER, ALLOCATABLE, DIMENSION(:) :: irmax     !Radius of maximum wind
INTEGER, ALLOCATABLE, DIMENSION(:) :: ivmax     !System intensity
INTEGER :: ierr                                 !Error flag
INTEGER :: open_status                          !Open status flag
INTEGER :: io_status                            !I/O status flag
CHARACTER(LEN=8) :: system                      !System id
CHARACTER(LEN=4), ALLOCATABLE, DIMENSION(:) :: ctime    !Time of fix
CHARACTER(LEN=8), ALLOCATABLE, DIMENSION(:) :: cdate    !Date of fix
CHARACTER(LEN=10), ALLOCATABLE, DIMENSION(:) :: cname   !System name
CHARACTER(LEN=7) :: cjulday0                            !Julian day
CHARACTER(LEN=12) :: inpfile                            !*.inp file
REAL, ALLOCATABLE, DIMENSION(:) :: rdelt        !Hours from analysis
REAL, ALLOCATABLE, DIMENSION(:) :: rlat         !Latitude of fix
REAL, ALLOCATABLE, DIMENSION(:) :: rlon         !Longitude of fix
REAL :: rlonTemp0, rlonTemp1                    !Temp lon variables for dateline checking
REAL :: dir0                                    !System direction
REAL :: spd0                                    !System speed
REAL :: delx,dely                               !Used in calculating distance
                                        !between analysis location and location at -12 hours
REAL :: dist                            !Distance between analysis location and
                                        !location at -12 hours
LOGICAL :: time0        !Tells whether fix at analysis time is present
LOGICAL :: timem12      !Tells whether fix a -12 hours is present             
! Some necessary constants
REAL, PARAMETER :: pi=4.0*ATAN(1.0)
REAL, PARAMETER :: dtr=pi/180.0
REAL, PARAMETER :: erad=6371.0
REAL, PARAMETER :: kph2kts=1.0/1.852
!======================================================================
! Set error flag to successful completion. If problems exist, ierr will
! be updated
ierr=0

! First read command line arguments
IF(iargc() /= 1) THEN
  WRITE(*,*) 'WARNING: problem with command line'
  ierr=64
  CALL EXIT(ierr)
ENDIF
CALL getarg(1,inpfile)

! Open inpfile
OPEN(UNIT=lunum, FILE=inpfile,STATUS='OLD',IOSTAT=open_status)
IF(open_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not open input file'
  ierr=66
  CALL EXIT(ierr)
ENDIF

READ(lunum,*,IOSTAT=io_status) system
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: problem reading first line of input file'
  ierr=74
  CALL EXIT(ierr)
ENDIF

READ(lunum,*,IOSTAT=io_status) nfixes
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: problem reading second line of input file'
  ierr=74
  CALL EXIT(ierr)
ENDIF

READ(lunum,*,IOSTAT=io_status)
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: problem reading third line of input file'
  ierr=74
  CALL EXIT(ierr)
ENDIF

! Go through fixes and get information at hour 0 and -12 if present.
ALLOCATE(cdate(nfixes))
ALLOCATE(ctime(nfixes))
ALLOCATE(rdelt(nfixes))
ALLOCATE(rlat(nfixes))
ALLOCATE(rlon(nfixes))
ALLOCATE(idir(nfixes))
ALLOCATE(ispd(nfixes))
ALLOCATE(irmax(nfixes))
ALLOCATE(ivmax(nfixes))
ALLOCATE(cname(nfixes))
time0=.FALSE.
timem12=.FALSE.
DO n=1,nfixes
  READ(lunum,*) cdate(n),ctime(n),rdelt(n),rlat(n),rlon(n),idir(n),ispd(n),irmax(n),ivmax(n),cname(n)
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: problem reading fix number ',n
    ierr=74
    CALL EXIT(ierr)
  ENDIF

  IF(NINT(rdelt(n)) == -12) THEN
    nm12=n
    IF(rlon(n) >= 180.0) rlon(n)=rlon(n)-360.0
    timem12=.TRUE.
  ELSEIF(NINT(rdelt(n)) == 0) THEN
    n0=n
    IF(rlon(n) >= 180.0) rlon(n)=rlon(n)-360.0
    time0=.TRUE.
  ENDIF
ENDDO

IF(.NOT. time0 .OR. .NOT. timem12) THEN
  WRITE(*,*) 'INFO: 0-hour or 12-hour fix missing'
  ierr=101
  CALL EXIT(ierr)
ENDIF

! Need to calculate speed and direction of motion. Direction of motion for TCs
! is the meteorological direction the TC is moving towards, not the direction it
! is moving from (like winds).
rlonTemp0=rlon(n0)
rlonTemp1=rlon(nm12)
IF(rlonTemp0 < 0.0) rlonTemp0=rlonTemp0+360.0
IF(rlonTemp1 < 0.0) rlonTemp1=rlonTemp1+360.0

dely=erad*(rlat(n0)-rlat(nm12))*dtr
!delx=erad*COS(((rlat(n0)+rlat(nm12))/2.0)*dtr)*(rlon(n0)-rlon(nm12))*dtr
delx=erad*COS(((rlat(n0)+rlat(nm12))/2.0)*dtr)*(rlonTemp0-rlonTemp1)*dtr
dist=SQRT(delx**2+dely**2)
spd0=(dist/12.0)*kph2kts
dir0=ATAN(dely/delx)/dtr
IF(delx > 0.0) dir0=90.0-dir0
IF(delx < 0.0) dir0=270.0-dir0
IF(ABS(delx) < EPSILON(delx) .AND. dely > 0.0) dir0=360.0
IF(ABS(delx) < EPSILON(delx) .AND. dely < 0.0) dir0=180.0

CLOSE(UNIT=lunum)

! Need also Julian day and time for output
READ(cdate(n0)(1:4),'(I4)') iyear0
READ(cdate(n0)(5:6),'(I2)') imon0
READ(cdate(n0)(7:8),'(I2)') iday0
CALL jday(imon0,iday0,iyear0,julday0)
IF(julday0 == -1) THEN
  WRITE(*,*) 'WARNING: problem with subroutine jday'
  ierr=74
  CALL EXIT(ierr)
ENDIF
julday0=iyear0*1000+julday0
WRITE(cjulday0,'(I7)') julday0

! Make sure the character string system has upper case letters before
! writing out.
CALL uppercase(system)

! Write out information from the best track. 
OPEN(UNIT=lunum, FILE='COORDINATES',STATUS='REPLACE',IOSTAT=open_status)
IF(open_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not open output file'
  ierr=73
  CALL EXIT(ierr)
ENDIF

WRITE(lunum,500,IOSTAT=io_status) '01',cdate(n0),cjulday0//ctime(n0)(1:2),rlat(n0),rlon(n0),rlat(nm12),rlon(nm12),&
NINT(dir0),NINT(spd0),irmax(n0),ivmax(n0),ivmax(nm12),system(1:4)//system(7:8),cname(n0)
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not write to output file'
  ierr=74
  CALL EXIT(ierr)
ENDIF

500 FORMAT(A2,1X,A8,1X,A9,4(F7.1),5(1X,I3.3),1X,A6,2X,A10)
CLOSE(UNIT=lunum)

END PROGRAM afdeck

!-----------------------------------------------------------------------------------------------
SUBROUTINE uppercase(str) 
! This subroutine converts all characters in str to uppercase.
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN OUT) :: str
INTEGER :: i,del

del=IACHAR('a')-IACHAR('A')

DO i=1,LEN_TRIM(str)
  IF (LGE(str(i:i),'a') .AND. LLE(str(i:i),'z')) THEN
    str(i:i)=ACHAR(IACHAR(str(i:i))-DEL)
  ENDIF
ENDDO
END SUBROUTINE uppercase
!-------------------------------------------------------------------------------
SUBROUTINE jday(imon,iday,iyear,julday)
!--------------------------------------------------------------------------------
!     This routine calculates the Julian day (julday) from
!     the month (imon), day (iday), and year (iyear). The
!     appropriate correction is made for leap year.
!-------------------------------------------------------------------------------
IMPLICIT NONE
INTEGER :: i
INTEGER :: imon,iday,iyear,julday
INTEGER, DIMENSION(12) :: ndmon
!
!     Specify the number of days in each month
ndmon(1)  = 31
ndmon(2)  = 28
ndmon(3)  = 31
ndmon(4)  = 30
ndmon(5)  = 31
ndmon(6)  = 30
ndmon(7)  = 31
ndmon(8)  = 31
ndmon(9)  = 30
ndmon(10) = 31
ndmon(11) = 30
ndmon(12) = 31
!
!     Correct for leap year
IF(MOD(iyear,4) == 0) ndmon(2)=29
!
!     Check for illegal input
IF(imon < 1 .OR. imon > 12) THEN
  WRITE(*,*) 'Month must be between 1 and 12'
  julday=-1
  RETURN
ENDIF
!
IF(iday < 1 .OR. iday .GT. ndmon(imon)) then
  WRITE(*,*) 'Day must be between 1 and 28,29,30, or 31'
  julday=-1
  RETURN
ENDIF
!
! Calculate the Julian day
julday = iday
IF(imon > 1) THEN
  DO i=2,imon
    julday = julday + ndmon(i-1)
  ENDDO
ENDIF
!
RETURN
END SUBROUTINE jday
