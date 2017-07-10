PROGRAM satcenter
!======================================================================
! satcenter.f90
!
! This program uses the ATCF decks and the satellite retrievals to 
! determine the time for which the TC comes closest to the center of 
! the satellite swath.  The time and the locations of the storm and the 
! center of the satellite swath, along with the number of retrievals 
! constitute the TIMES file. 
!
! History:
! 25 Mar 2013   Programming begun
! 07 Jun 2013   Added a few comments (JFD)
!
!======================================================================
USE asciidat_routines

IMPLICIT NONE

INTEGER :: luin=11                              !Logical unit number
INTEGER :: iargc                                !For reading command line
INTEGER :: nchar                                !Holding variable
INTEGER :: cyr0                                 !Year at time0
INTEGER :: cjd0                                 !Julian day at time0
INTEGER :: chr0                                 !Hour at time0
INTEGER :: cmm0                                 !Minute at time0
INTEGER :: css0                                 !Second at time0
INTEGER :: cjdm12                               !Julian day at timem12
INTEGER :: chrm12                               !Hour at timem12
INTEGER :: cmmm12                               !Minute at timem12
INTEGER :: cssm12                               !Second at timem12
INTEGER :: n                                    !Indexing variable
INTEGER, ALLOCATABLE, DIMENSION(:) :: syr       !Year of retrieval
INTEGER, ALLOCATABLE, DIMENSION(:) :: smo       !Month of retrieval
INTEGER, ALLOCATABLE, DIMENSION(:) :: sdy       !Day of retrieval
INTEGER, ALLOCATABLE, DIMENSION(:) :: shr       !Hour of retrieval
INTEGER, ALLOCATABLE, DIMENSION(:) :: smm       !Minute of retrieval
INTEGER, ALLOCATABLE, DIMENSION(:) :: sss       !Second of retrieval
INTEGER, ALLOCATABLE, DIMENSION(:) :: sjd       !Julian day of retrieval
INTEGER :: jdmn                                 !Julian day at mindist
INTEGER :: timemn                               !Time at mindist
INTEGER :: ierr                                 !Error flag
INTEGER :: open_status                          !Open status flag
INTEGER :: io_status                            !I/O status flag
REAL :: clat0                                   !Latitude at time0
REAL :: clon0                                   !Longitude at time0
REAL :: clatm12                                 !Latitude at timem12
REAL :: clonm12                                 !Latitude at timem12
REAL :: tclat                                   !Interpolated latitude
REAL :: tclon                                   !Interpolated longitude
REAL :: dlat                                    !Latitude difference
REAL :: dlon                                    !Longitude difference
REAL, ALLOCATABLE, DIMENSION(:) :: sclat        !Latitude at swath center
REAL, ALLOCATABLE, DIMENSION(:) :: sclon        !Longitude at swath center
REAL :: tdiff                                   !Time difference
REAL :: xhold1,xhold2                           !Holding variables
REAL :: dist                                    !Distance
REAL :: mindist                                 !Minimum distance
REAL :: tclatmn                                 !Lat of TC for mindist
REAL :: tclonmn                                 !Lon of TC for mindist
REAL :: retlatmn                                !Lat of retrieval for mindist
REAL :: retlonmn                                !Lon of retrieval for mindist
CHARACTER(LEN=255) :: coortimes
CHARACTER(LEN=120) :: infile
CHARACTER(LEN=8) :: sysid
CHARACTER(LEN=10) :: systime
CHARACTER(LEN=6) :: csat

REAL, PARAMETER :: erad=6371.0                  !Radius of Earth
REAL, PARAMETER :: pi=4.0*ATAN(1.0)             !Pi
REAL, PARAMETER :: dtr=pi/180.0                 !Degrees to radians conversion
!======================================================================
! Set error flag to successful completion. If problems exist, ierr will
! be updated
ierr=0

! Read command line arguments
IF(iargc() /= 3) THEN
  WRITE(*,*) 'WARNING: problem with command line'
  ierr=64
  CALL EXIT(ierr)
ENDIF
CALL getarg(1,sysid)
CALL getarg(2,systime)
CALL getarg(3,csat) 
!nchar=LEN_TRIM(satdir)
!satdir=satdir(1:nchar)//sysid//'/'

! Open COORDINATES file and get the position information at time0 and
! timem12
coortimes='COORDINATES'
OPEN(UNIT=luin, FILE=coortimes,STATUS='OLD',IOSTAT=open_status)
IF(open_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not open input file'
  ierr=66
  CALL EXIT(ierr)
ENDIF

READ(luin,500,IOSTAT=io_status) cyr0,cjd0,chr0,clat0,clon0,clatm12,clonm12
500 FORMAT(12X,I4,I3,I2,4(F7.1))
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: problem reading input file'
  ierr=74
  CALL EXIT(ierr)
ENDIF
CLOSE(UNIT=luin)

! Minutes and seconds at time0 and timem12 are 0
cmm0=0
css0=0
cmmm12=0
cssm12=0

! Read time information of satellite swath
!nchar=LEN_TRIM(satdir)
! Year
infile=sysid//'_'//systime//'_'//csat//'.years'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.years'
CALL rdasciidat(infile,luin)
ALLOCATE(syr(i1num))
DO n=1,i1num
  syr(n)=r1_int(n)
ENDDO

! Month
infile=sysid//'_'//systime//'_'//csat//'.months'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.months'
CALL rdasciidat(infile,luin)
ALLOCATE(smo(i1num))
DO n=1,i1num
  smo(n)=r1_int(n)
ENDDO

! Day
infile=sysid//'_'//systime//'_'//csat//'.days'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.days'
CALL rdasciidat(infile,luin)
ALLOCATE(sdy(i1num))
DO n=1,i1num
  sdy(n)=r1_int(n)
ENDDO

! Hour
infile=sysid//'_'//systime//'_'//csat//'.hours'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.hours'
CALL rdasciidat(infile,luin)
ALLOCATE(shr(i1num))
DO n=1,i1num
  shr(n)=r1_int(n)
ENDDO

! Minute
infile=sysid//'_'//systime//'_'//csat//'.minutes'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.minutes'
CALL rdasciidat(infile,luin)
ALLOCATE(smm(i1num))
DO n=1,i1num
  smm(n)=r1_int(n)
ENDDO

! Second
infile=sysid//'_'//systime//'_'//csat//'.seconds'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.seconds'
CALL rdasciidat(infile,luin)
ALLOCATE(sss(i1num))
DO n=1,i1num
  sss(n)=r1_int(n)
ENDDO

! Read latitude and longitude of satellite swath center
infile=sysid//'_'//systime//'_'//csat//'.scanline_center_lat'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.scanline_center_lat'
CALL rdasciidat(infile,luin)
ALLOCATE(sclat(i1num))
DO n=1,i1num
  sclat(n)=r1_flt(n)
ENDDO

infile=sysid//'_'//systime//'_'//csat//'.scanline_center_lon'
!infile=satdir(1:nchar)//sysid//'_'//systime//'_'//csat//'.scanline_center_lon'
CALL rdasciidat(infile,luin)
ALLOCATE(sclon(i1num))
DO n=1,i1num
  sclon(n)=r1_flt(n)
  IF(sclon(n) >180.0) sclon(n)=sclon(n)-360.0
ENDDO

! Compute Julian day of satellite retrieval
ALLOCATE(sjd(n))
DO n=1,i1num
  CALL jday(smo(n),sdy(n),syr(n),sjd(n))
  IF(sjd(n) == -1) THEN
    WRITE(*,*) 'WARNING: problem with subroutine jday, n=',n
    ierr=74
    CALL EXIT(ierr)
  ENDIF
ENDDO

! Although time0 could be used, I prefer to base the interpolation of TC 
! position based on timem12, which is 12 hours befor time0
CALL subtract_hrs(cyr0,cjd0,chr0,12,cjdm12,chrm12)

! Loop over satellite retrievals to find when the TC is closest to the
! swath center.
mindist=99999.9
DO n=1,i1num
! Time difference (in hours) between timem12 and satellite retrieval
  tdiff=(sjd(n)-cjdm12)*24.0+(shr(n)-chrm12)+(smm(n)-cmmm12)/60.0+(sss(n)-cssm12)/3600.0

! Location of TC at satellite retrieval time. Linear interpolation between
! timem12 and time0
  tclat=clatm12+tdiff*(clat0-clatm12)/12.0
  tclon=clonm12+tdiff*(clon0-clonm12)/12.0

! Calculate distance from TC center to swath center. Use haversine formula
  dlat=(sclat(n)-tclat)*dtr
  dlon=(sclon(n)-tclon)*dtr
  xhold1=SIN(dlat/2.0)**2+COS(sclat(n)*dtr)*COS(tclat*dtr)*SIN(dlon/2.0)**2
  xhold2=2.0*ASIN(MIN(1.0,SQRT(xhold1)))
  dist=erad*xhold2

! Find minimum distance
  IF(dist < mindist) THEN
    mindist=dist
    tclatmn=tclat
    tclonmn=tclon
    retlatmn=sclat(n)
    retlonmn=sclon(n)
    jdmn=syr(n)*1000+sjd(n)
    timemn=shr(n)*10000+smm(n)*100+sss(n)
  ENDIF
ENDDO

! Write information to TIMES file
coortimes='TIMES'
OPEN(UNIT=luin,FILE=coortimes,STATUS='REPLACE',IOSTAT=open_status)
IF(open_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not open output file'
  ierr=73
  CALL EXIT(ierr)
ENDIF
WRITE(luin,510,IOSTAT=io_status) '01',jdmn,timemn,retlatmn,retlonmn,tclatmn,tclonmn
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not write to first line to output file'
  ierr=74
  CALL EXIT(ierr)
ENDIF
WRITE(luin,520,IOSTAT=io_status) i1num
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not write to second line to output file'
  ierr=74
  CALL EXIT(ierr)
ENDIF
510 FORMAT(A2,1X,I7,1X,I6.6,4(1X,F8.2))
520 FORMAT(I6)
CLOSE(UNIT=luin)

END PROGRAM satcenter

!-------------------------------------------------------------------------------
SUBROUTINE subtract_hrs(yeari,jdi,hhi,dif,jdf,hhf)
! This subroutine subtracts a given number of hours (dif) from a given year,
! julian data, and hour.
!
IMPLICIT NONE

INTEGER :: yeari,jdi,hhi,dif	!Input
INTEGER :: jdf,hhf		!Output
INTEGER :: days, hours		!Intermediate values

days=dif/24
hours=MOD(dif,24)

jdf=jdi-days
hhf=hhi-hours
IF(hhf < 0) THEN
  jdf=jdf-1
  hhf=24+hhf
ENDIF 


END SUBROUTINE subtract_hrs
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
