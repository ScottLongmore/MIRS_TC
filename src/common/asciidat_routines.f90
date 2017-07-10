MODULE asciidat_routines
!===============================================================================
! asciidat_routines.f90
!
! This module contains the subroutine(s) which read data from the general ascii 
! format created by Robert D. 
!
! History
! 23 Mar 2012	Programming begun (JFD)
! 10 May 2012	At request of R. DeMaria, the names mirs_routines and rdmirs
! 		were changed to asciidat_routines and rdasciidat, to reflect the
!		fact that the format of the data will contain data other than
!		MIRS.
! 03 May 2013   Included check for allocation at beginning of subroutine. If
!               arrays allocated, deallocate them.
!===============================================================================
IMPLICIT NONE
SAVE

INTEGER, PRIVATE :: i1,i2,i3            !Indexing variables
INTEGER :: rank                !Rank of data array to be read
CHARACTER(LEN=5) :: datatype   !Type of data (float or int)

INTEGER :: i1num,i2num,i3num            !Number of values for each dimension

INTEGER, ALLOCATABLE, DIMENSION(:) :: r1_int								
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: r2_int
INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: r3_int

REAL, ALLOCATABLE, DIMENSION(:) :: r1_flt
REAL, ALLOCATABLE, DIMENSION(:,:) :: r2_flt
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: r3_flt

CONTAINS
!-------------------------------------------------------------------------------
SUBROUTINE rdasciidat(infile,luin)
!
! Written by Jack Dostalek -- CIRA/RAMMB
! This is the subroutine for basic reading of the ascii files. The variables
! are stored in separate files, one variable per file. The input file 'infile',
! with logical unit number luin, is then the file to be read. Its contents are 
! placed in one of the 6 allocatable arrays declared above, depending on the 
! dimension of the data and the type of variable being read.
!
! Variables
! Input 
CHARACTER(LEN=120), INTENT(IN) :: infile        !Name of input file
INTEGER, INTENT(IN) :: luin                     !Logical unit number for infile
INTEGER :: ierr                                 !Error flag
INTEGER :: io_status                            !I/O status flag
INTEGER :: open_status                          !Open status flag

! Set error flag to zero. Nonzero result indicates problem
ierr=0

! If arrays are allocated, deallocate them
IF(ALLOCATED(r1_int)) DEALLOCATE(r1_int)
IF(ALLOCATED(r2_int)) DEALLOCATE(r2_int)
IF(ALLOCATED(r3_int)) DEALLOCATE(r3_int)
IF(ALLOCATED(r1_flt)) DEALLOCATE(r1_flt)
IF(ALLOCATED(r2_flt)) DEALLOCATE(r2_flt)
IF(ALLOCATED(r3_flt)) DEALLOCATE(r3_flt)

! Open infile and read rank of data
OPEN(UNIT=luin,FILE=infile,STATUS='OLD',IOSTAT=open_status)
IF(open_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not open input file'
  ierr=66
  CALL EXIT(ierr)
ENDIF
READ(luin,'(5X,I3)',IOSTAT=io_status) rank
IF(io_status /= 0) THEN
  WRITE(*,*) 'WARNING: could not read rank of ',infile
  ierr=74
  CALL EXIT(ierr)
ENDIF

! Continue according to rank
SELECT CASE(rank)

CASE(1)
! Read value of dimension
  READ(luin,'(11X,I9)',IOSTAT=io_status) i1num
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: could not read i1num of ',infile
    ierr=74
    CALL EXIT(ierr)
  ENDIF
    
! Read type of data (float or int)
  READ(luin,'(5X,A5)',IOSTAT=io_status) datatype
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: could not read datatype of ',infile
    ierr=74
    CALL EXIT(ierr)
  ENDIF
  
! Continue according to datatype
  SELECT CASE(datatype)
    
  CASE('int')
    ALLOCATE(r1_int(i1num))
    DO i1=1,i1num
      READ(luin,'(10X,I9)',IOSTAT=io_status) r1_int(i1)
      IF(io_status /= 0) THEN
        WRITE(*,*) 'WARNING: could not read r1_int data of ',infile
        ierr=74
        CALL EXIT(ierr)
      ENDIF
    ENDDO
    
  CASE('float')
    ALLOCATE(r1_flt(i1num))
    DO i1=1,i1num
      READ(luin,'(10X,F15.5)',IOSTAT=io_status) r1_flt(i1)
      IF(io_status /= 0) THEN
        WRITE(*,*) 'WARNING: could not read r1_flt data of ',infile
        ierr=74
        CALL EXIT(ierr)
      ENDIF
    ENDDO  
  
  END SELECT    

CASE(2)
! Read value of dimensions
  READ(luin,'(11X,I9,1X,I9)',IOSTAT=io_status) i1num,i2num  
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: could not read i1num or i2num of ',infile
    ierr=74
    CALL EXIT(ierr)
  ENDIF
  
! Read type of data (float or int)
  READ(luin,'(5X,A5)',IOSTAT=io_status) datatype
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: could not read datatype of ',infile
    ierr=74
    CALL EXIT(ierr)
  ENDIF
  
! Continue according to datatype
  SELECT CASE(datatype)  
    
  CASE('int')
    ALLOCATE(r2_int(i1num,i2num))
    DO i1=1,i1num
      DO i2=1,i2num
        READ(luin,'(20X,I9)',IOSTAT=io_status) r2_int(i1,i2)
        IF(io_status /= 0) THEN
          WRITE(*,*) 'WARNING: could not read r2_int data of ',infile
          ierr=74
          CALL EXIT(ierr)
        ENDIF
      ENDDO
    ENDDO
    
  CASE('float')
    ALLOCATE(r2_flt(i1num,i2num))
    DO i1=1,i1num
      DO i2=1,i2num        
        READ(luin,'(20X,F15.5)',IOSTAT=io_status) r2_flt(i1,i2)	
        IF(io_status /= 0) THEN
          WRITE(*,*) 'WARNING: could not read r2_flt data of ',infile
          ierr=74
          CALL EXIT(ierr)
        ENDIF
      ENDDO
    ENDDO  
  
  END SELECT    

CASE(3)
! Read value of dimensions
  READ(luin,'(11X,I9,1X,I9,1X,I9)',IOSTAT=io_status) i1num,i2num,i3num
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: could not read i1num, i2num, or i3num of ',infile
    ierr=74
    CALL EXIT(ierr)
  ENDIF
   
! Read type of data (float or int)
  READ(luin,'(5X,A5)',IOSTAT=io_status) datatype
  IF(io_status /= 0) THEN
    WRITE(*,*) 'WARNING: could not read datatype of ',infile
    ierr=74
    CALL EXIT(ierr)
  ENDIF
  
! Continue according to datatype
  SELECT CASE(datatype)
    
  CASE('int')
    ALLOCATE(r3_int(i1num,i2num,i3num))
    DO i1=1,i1num
      DO i2=1,i2num
        DO i3=1,i3num
          READ(luin,'(30X,I9)',IOSTAT=io_status) r3_int(i1,i2,i3)
          IF(io_status /= 0) THEN
            WRITE(*,*) 'WARNING: could not read r3_int data of ',infile
            ierr=74
            CALL EXIT(ierr)
          ENDIF
	ENDDO
      ENDDO
    ENDDO
    
  CASE('float')    
    ALLOCATE(r3_flt(i1num,i2num,i3num))    
    DO i1=1,i1num
      DO i2=1,i2num
        DO i3=1,i3num
          READ(luin,'(30X,F15.5)',IOSTAT=io_status) r3_flt(i1,i2,i3)
          IF(io_status /= 0) THEN
            WRITE(*,*) 'WARNING: could not read r3_flt data of ',infile
            ierr=74
            CALL EXIT(ierr)
          ENDIF
	ENDDO	
      ENDDO
    ENDDO  
  
  END SELECT   

END SELECT

CLOSE(UNIT=luin)

END SUBROUTINE rdasciidat
!===============================================================================
END MODULE asciidat_routines
