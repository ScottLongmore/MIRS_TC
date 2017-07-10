MODULE extract_aid_module
  ! 
  ! This module extracts all of the information from the ATCF 
  ! adeck file.
  !
  ! Three routines
  !
  ! extract_aid(afn,tnm) extracts a specific forecast (fixed 12-hourly,
  !  through 120h)
  !
  ! extract_aidv(afn,tnm,ftimes,fint) extracts a specific forecast, a 
  !  a variable forecast length (ftimes), time interval (fint)version 
  !  of extract_aid
  !
  ! extract_adv(afn) extracts the advisory ("CARQ") information
  !
  ! extract_advt(afn,H) extracts the advisory ("CARQ") information at hour
  !  offset H.  Valid values of H are 0 -6 -12 -18 and -24
  !
  ! POSSIBLE INPUT:
  ! afn    - filename of the advisory/forecast dataset (adeck)
  ! tnm    - TECH NAME see atcf documentation at 
  !          http://www.nrlmry.navy.mil/atcf/docs/new/ ; These are also
  !          commonly provided at the tpc ftp site that provides ATCF
  !          information in real-time as well as archives of past years
  ! ftimes - integer of the number of forecast times desired (hr)
  ! fint   - integer forecast time interval (hr) 
  !
  ! USES: dataio.f dtgutils.f, upcase.f, dataformats.inc, dataioparms.inc 
  !       these codes, and structures were developed at NRLMRY
  ! 
  !      ---- include these ATCF ROUTINES and INCLUDES in the makefile----
  !
  ! WRITTEN BY:  John Knaff, NOAA/NESDIS/RAMMB
  !
  ! LAST MODIFIED: May 13, 2013
  ! Now runs on three different compilers (pgf90, ifort, g95)
  ! added routine extract_advt - JAK, 12/28/2011
  ! fixed a long standing problem with missing names-JAK 5/13/2013
  !******************************************************************

  IMPLICIT NONE
  !
  ! Global variables
  
  ! CARQ variables

  INTEGER              :: nq       ! number of date time groups in the file
  INTEGER, ALLOCATABLE :: qdtg(:)  ! dtg information [yymmddhh]
  REAL,    ALLOCATABLE :: qv(:)    ! contains the advisory intensity [kt]
  REAL,    ALLOCATABLE :: qp(:)    ! contains the advisory pressure [hPa]
  REAL,    ALLOCATABLE :: qroci(:) ! contains the advisory radius of outer 
                                   ! closed isobar [nmi]
  REAL,    ALLOCATABLE :: qpoci(:) ! contains the advisory pressure of outer
                                   ! closed isobar[hPa]
  REAL,    ALLOCATABLE :: qspd(:)  ! storm speed
  REAL,    ALLOCATABLE :: qdir(:)  ! storm direction
  REAL,    ALLOCATABLE :: qrmw(:)  ! the advisory Radius of max winds[nmi}
  REAL,    ALLOCATABLE :: qeyed(:) ! eye diameter (nmi)
  REAL,    ALLOCATABLE :: qlat(:)  ! advisory latitude [degrees] 
  REAL,    ALLOCATABLE :: qlon(:)  ! advisory longitudes [degrees]
  REAL,    ALLOCATABLE :: qr34(:,:),qr50(:,:),qr64(:,:) ! wind radii [nmi]
  CHARACTER (LEN=2), ALLOCATABLE :: qty(:) ! type HU,TY etc
  CHARACTER (LEN=10), ALLOCATABLE :: qname(:) ! storm name
  
  ! Forecast variables

  INTEGER              :: na       ! number of date time groups in the file
  INTEGER              :: naft     ! number of forecast times (parameter in
                                   ! extract_aid that can be changed)
  INTEGER, ALLOCATABLE :: adtg(:)  ! dtg information [yymmddhh]
  INTEGER, ALLOCATABLE :: aift(:,:)! forecast time
  REAL,    ALLOCATABLE :: av(:,:)  ! contains the forecast intensity [kt]
  REAL,    ALLOCATABLE :: ap(:,:)  ! contains the forecast pressure [hPa]
  REAL,    ALLOCATABLE :: alat(:,:)! contains the forecast latitudes 
  REAL,    ALLOCATABLE :: alon(:,:)! contains the forecast longitudes
  REAL,    ALLOCATABLE :: ar34(:,:,:),ar50(:,:,:),ar64(:,:,:) ! forecast 
                                                              !wind radii
  CHARACTER (LEN=2), ALLOCATABLE :: aty(:,:) ! type
  CHARACTER (LEN=10), ALLOCATABLE :: aname(:,:) !storm name

  CONTAINS
    
    LOGICAL FUNCTION extract_adv(afn)
      !
      ! This routine reads the advisory lines of the adeck (or the 
      ! techname CARQ) from the adeck and returns the information
      ! in the global array listed above below CARQ.
      !
      ! Written by John Knaff NOAA/NESDIS
      !
      ! uses :
      ! logical unit 10
      ! dataio.f routines supplied by NRLMRY
      ! dataformats.inc supplied by NRLMRY
      ! dataioparms.inc supplied by NRLMRY
      !
      ! Last modified: July 16,2008, comments.

      IMPLICIT NONE
      
      INCLUDE 'dataformats.inc'
      INCLUDE 'dataioparms.inc'

      
      CHARACTER (LEN=*),INTENT(IN) :: afn  ! a-deck file name
      CHARACTER (LEN =10) :: dtgcur,dtgcheck,dtglast,dtgnext ! date/time
      CHARACTER (LEN = 10):: storm_name      ! storm name
      CHARACTER (LEN =4) tech                !techname
      INTEGER :: luaa, ios, n, ihrs, istat, i, isumchar
      INTEGER :: j, rindex, iquad, istart, k, result
      REAL :: rarray(4)                      !radii array
      LOGICAL :: loop,first

      TYPE ( BIG_AID_DATA) aidsData          ! types given in dataformats.inc
      TYPE ( AID_DATA) aidData, tauData      ! types given in dataformats.inc

      ! Executable statements follow....

      if (allocated(qdtg)) deallocate(qdtg)
      if (allocated(qv)) deallocate(qv)
      if (allocated(qp)) deallocate(qp)
      if (allocated(qroci)) deallocate(qroci)
      if (allocated(qpoci)) deallocate(qpoci)
      if (allocated(qspd)) deallocate(qspd)
      if (allocated(qdir)) deallocate(qdir)
      if (allocated(qrmw)) deallocate(qrmw)
      if (allocated(qeyed)) deallocate(qeyed)
      if (allocated(qlat)) deallocate(qlat)
      if (allocated(qlon)) deallocate(qlon)
      if (allocated(qr34)) deallocate(qr34)
      if (allocated(qr50)) deallocate(qr50)
      if (allocated(qr64)) deallocate(qr64)
      if (allocated(qty)) deallocate(qty)
      if (allocated(qname)) deallocate(qname)

      extract_adv = .false.

      luaa=10
      
      OPEN (luaa, FILE=afn, STATUS='OLD',IOSTAT=ios,ERR=1000)
      
      IF(ios /= 0)then
         print*,'extract_adv:  IOSTATUS = ',ios
         close(luaa)
         return
      END IF

      ! determine the first and last dtg from the a-deck

      backspace(luaa)

      LOOP=.true.
      first=.true.
      dtgcur = '1945010100'
      dtglast= '1945010100'
      n=0
      nq=0

      DO WHILE ( LOOP )
         read ( luaa, '( 8x, a10,6x,a4 )', end=17 ) dtgcheck,tech
         call dtgdif2(dtgcheck,dtgcur,ihrs,istat)
         if (dtglast .ne. dtgcheck) n=n+1
         if ( istat .ne. 0 ) then
            print*, 'extract_adv:  Line ',n,&
                 ' DTG problem in a-deck ',dtgcheck
            close(luaa)
            return
         else if (tech == 'CARQ' .and. first ) then
            dtgcur = dtgcheck
            first = .false.
         else if( ihrs .gt. 2000 ) then
            print*, 'extract_adv:  Line ',n,&
                 ' DTG too far from initial DTG ',dtgcheck
            close(luaa)
            return
         else
            if(tech == 'CARQ')dtglast = dtgcheck
         endif
      END DO
17    rewind ( luaa )

      if (n.eq.0)then
         print*,'extract_adv: input file had zero length'
         close(luaa)
         return
      end if

      call dtgdif2(dtgcur,dtglast,ihrs,istat)

      ! loop through b-deck and cound the number of different date/times

      nq=ihrs/6+1
      n=nq


      allocate (qdtg(n))
      allocate (qv(n))
      allocate (qp(n))
      allocate (qroci(n))
      allocate (qpoci(n))
      allocate (qspd(n))
      allocate (qdir(n))
      allocate (qrmw(n))
      allocate (qeyed(n))
      allocate (qlat(n))
      allocate (qlon(n))
      allocate (qr34(n,4))
      allocate (qr50(n,4))
      allocate (qr64(n,4))
      allocate (qty(n))
      allocate (qname(n))

      ! initialize as missing

      storm_name='    INVEST'
      do i=1,nq
         qdtg(i)=-9999
         qv(i)=-9999.0
         qp(i)=-9999.0
         qroci(i)=-9999.0
         qpoci(i)=-9999.0
         qrmw(i)=-9999.0
         qeyed(i)=-9999.0
         qlat(i)=-9999.0
         qlon(i)=-9999.0
         qty(i)='  '
         qname(i)='          '
         do k=1,4
            qr34(i,k)=-9999.0
            qr50(i,k)=-9999.0
            qr64(i,k)=-9999.0
         end do
      end do

      do i=1,nq

         backspace(luaa) ! this was needed ... don't know why

         call getBigAidDTG ( luaa, dtgcur, aidsData, result )

         read(dtgcur,'(i10)')qdtg(i)

         if ( result .eq. 0 ) goto 18

         call getTech ( aidsData, "CARQ", aidData, result )

         if ( result .eq. 0 ) goto 18

         call getSingleTAU ( aidData, 0, tauData, result )

         if ( result .eq. 0 ) goto 18

         ! set global variable to missing before assignment

         qdtg(i)=-9999
         qv(i)=-9999.00
         qlat(i)=-9999.00
         qlon(i)=-9999.00
         qroci(i)=-9999.00
         qpoci(i)=-9999.00
         qspd(i)=-9999.00
         qdir(i)=-9999.00
         qrmw(i)=-9999.00
         qeyed(i)=-9999.00

         do k=1,4

            qr34(i,k)=-9999.00
            qr50(i,k)=-9999.00
            qr64(i,k)=-9999.00

         enddo

         ! prescribe dtg,bv,blat,blon

         read(tauData%aRecord(1)%DTG,'(i10)')qdtg(i)

         qv(i)=float(tauData%aRecord(1)%vmax)
         qp(i)=float(tauData%aRecord(1)%mslp)
         qroci(i)=float(tauData%aRecord(1)%rrp)
         qpoci(i)=float(tauData%aRecord(1)%radp)
         qspd(i)=float(tauData%aRecord(1)%speed)
         qdir(i)=float(tauData%aRecord(1)%dir)
         qrmw(i)=float(tauData%aRecord(1)%mrd)
         qeyed(i)=float(tauData%aRecord(1)%eye)
         qlat(i)=tauData%aRecord(1)%lat
         qlon(i)=tauData%aRecord(1)%lon

         if (tauData%aRecord(1)%NS == 'S') qlat(i) = qlat(i) * (-1.)
         if (tauData%aRecord(1)%EW == 'W') qlon(i) = 360.0 - qlon(i)

         qty(i)=tauData%aRecord(1)%ty
         qname(i)=tauData%aRecord(1)%stormname
         

         if (qname(i).ne.'         '.and. qname(i) .NE. '    INVEST') THEN
            storm_name=qname(i)
         end if

         ! now prescribe wind radii

         do j= 1,4 ! number of possible wind radii

            istart=0

            if (tauData%aRecord(j)%rad > 0) then

               if (tauData%aRecord(j)%rad ==  34)  istart =  1
               if (tauData%aRecord(j)%rad ==  35)  istart =  1
               if (tauData%aRecord(j)%rad ==  50)  istart =  5
               if (tauData%aRecord(j)%rad ==  64)  istart =  9
               if (tauData%aRecord(j)%rad ==  65)  istart =  9

               ! if (tauData%aRecord(j)%rad == 100)  istart = 13

               rindex=1

               if (tauData%aRecord(j)%windcode == 'NEQ') then

                  do iquad = istart,istart+3

                     rarray(rindex)= float(tauData%aRecord(j)%radii(rindex))
                     rindex=rindex +1

                  end do

               else if (tauData%aRecord(j)%windcode == 'AAA' )then

                  do iquad = istart,istart+3

                     rarray(rindex)= float(tauData%aRecord(j)%radii(1))
                     rindex=rindex+1

                  end do

               endif

               ! clean up data arrays

               do k=1,4

                  tauData%aRecord(j)%radii(k)=0

               end do

               if (istart == 1 ) then

                  do k =1,4

                     if (qv(i).GE.34.0) then

                        qR34(i,k) = rarray(k)

                     else

                        qR34(i,k)=-9999.00

                     endif

                  end do

               else if (istart == 5 )then

                  do k=1,4

                     if (qv(i).GE.50.0) then

                        qR50(i,k) = rarray(k)

                     else

                        qR50(i,k)=-9999.00

                     endif

                  end do

               else if (istart == 9) then

                  do k=1,4

                     if (qv(i).GE.64.0) then

                        qR64(i,k) = rarray(k)

                     else

                        qR64(i,k)=-9999.00

                     endif

                  end do

               end if

            end if

            do k=1,4

               rarray(k)= -9999.00

            end do

         end do

18       continue

         call dtgmod ( dtgcur, 6, dtgnext, result )
         dtgcur = dtgnext

      end do

      do i=1,10
         isumchar=isumchar+iachar(storm_name(i:i))
      end do

      do i=1,nq
         if (isumchar/= 0) then
            qname(i)=storm_name
         else
            qname(i)='IS_MISSING'
         end if

      end do

      extract_adv = .true.

      close(luaa)

      return

1000  continue

      print*,'extract_adv:  error opening ', TRIM(afn)
      close(luaa)

      return

    END FUNCTION extract_adv

!**********************************8
    LOGICAL FUNCTION extract_advt(afn,H)
      !
      ! This routine reads the advisory lines of the adeck (or the 
      ! techname CARQ valid at tau H) from the adeck and returns the 
      ! information  in the global array listed above. 
      !
      ! Written by John Knaff NOAA/NESDIS
      !
      ! uses :
      ! logical unit 10
      ! dataio.f routines supplied by NRLMRY
      ! dataformats.inc supplied by NRLMRY
      ! dataioparms.inc supplied by NRLMRY
      !
      ! Last modified: December 28 ,2011, comments.

      IMPLICIT NONE
      
      INCLUDE 'dataformats.inc'
      INCLUDE 'dataioparms.inc'

      
      CHARACTER (LEN=*),INTENT(IN) :: afn  ! a-deck file name
      CHARACTER (LEN =10) :: dtgcur,dtgcheck,dtglast,dtgnext ! date/time
      CHARACTER (LEN = 10):: storm_name      ! storm name
      CHARACTER (LEN =4) tech                !techname
      INTEGER :: luaa, ios, n, ihrs, istat, i, isumchar
      INTEGER :: j, rindex, iquad, istart, k, result, H
      REAL :: rarray(4)                      !radii array
      LOGICAL :: loop,first

      TYPE ( BIG_AID_DATA) aidsData          ! types given in dataformats.inc
      TYPE ( AID_DATA) aidData, tauData      ! types given in dataformats.inc

      ! Executable statements follow....


      if (allocated(qdtg)) deallocate(qdtg)
      if (allocated(qv)) deallocate(qv)
      if (allocated(qp)) deallocate(qp)
      if (allocated(qroci)) deallocate(qroci)
      if (allocated(qpoci)) deallocate(qpoci)
      if (allocated(qspd)) deallocate(qspd)
      if (allocated(qdir)) deallocate(qdir)
      if (allocated(qrmw)) deallocate(qrmw)
      if (allocated(qeyed)) deallocate(qeyed)
      if (allocated(qlat)) deallocate(qlat)
      if (allocated(qlon)) deallocate(qlon)
      if (allocated(qr34)) deallocate(qr34)
      if (allocated(qr50)) deallocate(qr50)
      if (allocated(qr64)) deallocate(qr64)
      if (allocated(qty)) deallocate(qty)
      if (allocated(qname)) deallocate(qname)

      extract_advt = .false.

      luaa=10
      
      OPEN (luaa, FILE=afn, STATUS='OLD',IOSTAT=ios,ERR=1000)
      
      IF(ios /= 0)then
         print*,'extract_adv:  IOSTATUS = ',ios
         close(luaa)
         return
      END IF

      ! Check the value of H

      IF (H .NE. 0 .and. H .NE. -6 .and. H .NE. -12 .and. H .NE. -18 .and. &
           H .NE. -24) THEN
         print*,'extract_advt:  the value of H is invalid:  H =, ', H
         return
      END IF

      ! determine the first and last dtg from the a-deck

      backspace(luaa)

      LOOP=.true.
      first=.true.
      dtgcur = '1945010100'
      dtglast= '1945010100'
      n=0
      nq=0

      DO WHILE ( LOOP )
         read ( luaa, '( 8x, a10,6x,a4 )', end=17 ) dtgcheck,tech
         call dtgdif2(dtgcheck,dtgcur,ihrs,istat)
         if (dtglast .ne. dtgcheck) n=n+1
         if ( istat .ne. 0 ) then
            print*, 'extract_adv:  Line ',n,&
                 ' DTG problem in a-deck ',dtgcheck
            close(luaa)
            return
         else if (tech == 'CARQ' .and. first ) then
            dtgcur = dtgcheck
            first = .false.
         else if( ihrs .gt. 2000 ) then
            print*, 'extract_adv:  Line ',n,&
                 ' DTG too far from initial DTG ',dtgcheck
            close(luaa)
            return
         else
            if(tech == 'CARQ')dtglast = dtgcheck
         endif
      END DO
17    rewind ( luaa )

      if (n.eq.0)then
         print*,'extract_adv: input file had zero length'
         close(luaa)
         return
      end if

      call dtgdif2(dtgcur,dtglast,ihrs,istat)

      ! loop through b-deck and cound the number of different date/times

      nq=ihrs/6+1
      n=nq


      allocate (qdtg(n))
      allocate (qv(n))
      allocate (qp(n))
      allocate (qroci(n))
      allocate (qpoci(n))
      allocate (qspd(n))
      allocate (qdir(n))
      allocate (qrmw(n))
      allocate (qeyed(n))
      allocate (qlat(n))
      allocate (qlon(n))
      allocate (qr34(n,4))
      allocate (qr50(n,4))
      allocate (qr64(n,4))
      allocate (qty(n))
      allocate (qname(n))

      ! initialize as missing

      storm_name='    INVEST'
      do i=1,nq
         qdtg(i)=-9999
         qv(i)=-9999.0
         qp(i)=-9999.0
         qroci(i)=-9999.0
         qpoci(i)=-9999.0
         qrmw(i)=-9999.0
         qeyed(i)=-9999.0
         qlat(i)=-9999.0
         qlon(i)=-9999.0
         qty(i)='  '
         qname(i)='          '
         do k=1,4
            qr34(i,k)=-9999.0
            qr50(i,k)=-9999.0
            qr64(i,k)=-9999.0
         end do
      end do

      do i=1,nq

         backspace(luaa) ! this was needed ... don't know why

         call getBigAidDTG ( luaa, dtgcur, aidsData, result )

         read(dtgcur,'(i10)')qdtg(i)

         if ( result .eq. 0 ) goto 18

         call getTech ( aidsData, "CARQ", aidData, result )

         if ( result .eq. 0 ) goto 18

         call getSingleTAU ( aidData, H, tauData, result )

         if ( result .eq. 0 ) goto 18

         ! set global variable to missing before assignment

         qdtg(i)=-9999
         qv(i)=-9999.00
         qlat(i)=-9999.00
         qlon(i)=-9999.00
         qroci(i)=-9999.00
         qpoci(i)=-9999.00
         qspd(i)=-9999.00
         qdir(i)=-9999.00
         qrmw(i)=-9999.00
         qeyed(i)=-9999.00

         do k=1,4

            qr34(i,k)=-9999.00
            qr50(i,k)=-9999.00
            qr64(i,k)=-9999.00

         enddo

         ! prescribe dtg,bv,blat,blon

         read(tauData%aRecord(1)%DTG,'(i10)')qdtg(i)

         qv(i)=float(tauData%aRecord(1)%vmax)
         qp(i)=float(tauData%aRecord(1)%mslp)
         qroci(i)=float(tauData%aRecord(1)%rrp)
         qpoci(i)=float(tauData%aRecord(1)%radp)
         qspd(i)=float(tauData%aRecord(1)%speed)
         qdir(i)=float(tauData%aRecord(1)%dir)
         qrmw(i)=float(tauData%aRecord(1)%mrd)
         qeyed(i)=float(tauData%aRecord(1)%eye)
         qlat(i)=tauData%aRecord(1)%lat
         qlon(i)=tauData%aRecord(1)%lon

         if (tauData%aRecord(1)%NS == 'S') qlat(i) = qlat(i) * (-1.)
         if (tauData%aRecord(1)%EW == 'W') qlon(i) = 360.0 - qlon(i)

         qty(i)=tauData%aRecord(1)%ty
         qname(i)=tauData%aRecord(1)%stormname
         

         if (qname(i).ne.'         '.and. qname(i) .NE. '    INVEST') THEN
            storm_name=qname(i)
         end if

         ! now prescribe wind radii

         do j= 1,4 ! number of possible wind radii

            istart=0

            if (tauData%aRecord(j)%rad > 0) then

               if (tauData%aRecord(j)%rad ==  34)  istart =  1
               if (tauData%aRecord(j)%rad ==  35)  istart =  1
               if (tauData%aRecord(j)%rad ==  50)  istart =  5
               if (tauData%aRecord(j)%rad ==  64)  istart =  9
               if (tauData%aRecord(j)%rad ==  65)  istart =  9

               ! if (tauData%aRecord(j)%rad == 100)  istart = 13

               rindex=1

               if (tauData%aRecord(j)%windcode == 'NEQ') then

                  do iquad = istart,istart+3

                     rarray(rindex)= float(tauData%aRecord(j)%radii(rindex))
                     rindex=rindex +1

                  end do

               else if (tauData%aRecord(j)%windcode == 'AAA' )then

                  do iquad = istart,istart+3

                     rarray(rindex)= float(tauData%aRecord(j)%radii(1))
                     rindex=rindex+1

                  end do

               endif

               ! clean up data arrays

               do k=1,4

                  tauData%aRecord(j)%radii(k)=0

               end do

               if (istart == 1 ) then

                  do k =1,4

                     if (qv(i).GE.34.0) then

                        qR34(i,k) = rarray(k)

                     else

                        qR34(i,k)=-9999.00

                     endif

                  end do

               else if (istart == 5 )then

                  do k=1,4

                     if (qv(i).GE.50.0) then

                        qR50(i,k) = rarray(k)

                     else

                        qR50(i,k)=-9999.00

                     endif

                  end do

               else if (istart == 9) then

                  do k=1,4

                     if (qv(i).GE.64.0) then

                        qR64(i,k) = rarray(k)

                     else

                        qR64(i,k)=-9999.00

                     endif

                  end do

               end if

            end if

            do k=1,4

               rarray(k)= -9999.00

            end do

         end do

18       continue

         call dtgmod ( dtgcur, 6, dtgnext, result )
         dtgcur = dtgnext

      end do

      do i=1,10
         isumchar=isumchar+iachar(storm_name(i:i))
      end do

      do i=1,nq
         if (isumchar/= 0) then
            qname(i)=storm_name
         else
            qname(i)='IS_MISSING'
         end if

      end do

      extract_advt = .true.

      close(luaa)

      return

1000  continue

      print*,'extract_adv:  error opening ', TRIM(afn)
      close(luaa)

      return

    END FUNCTION extract_advt

!******************************************************************************

    LOGICAL FUNCTION extract_aid(afn,tnm)
      
      ! This is the original extract_aid which was designed to extract
      ! forecast information at 12-hourly intervals through 120h, as this
      ! is the typical spacing for operational forecasts at TPC and NHC
      !  
      ! A new function extract_aidv now allows the number of forecast times 
      ! and the forecast time interval to be specified.
      !
      ! Written by John Knaff NOAA/NESDIS
      !
      ! uses :
      ! logical unit 10
      ! dataio.f routines supplied by NRLMRY
      ! dataformats.inc supplied by NRLMRY
      ! dataioparms.inc supplied by NRLMRY

      !
      ! Last Modified: July 16,2008 J. Knaff, NOAA/NESDIS/StAR
      ! -Added the parameters FTIMES, and FINT to be constant with the 
      !  logical function extract_aidv.  These parameters could be modified
      !  but that would mirror the function extract_aidv.
      ! -Added global variable naft to return then number of forecast times
      !  keeping with the logic used in extract_aidv
      !

      IMPLICIT NONE
      
      INCLUDE 'dataformats.inc'
      INCLUDE 'dataioparms.inc'

      
      CHARACTER (LEN=*),INTENT(IN) :: afn  ! a-deck file name
      CHARACTER (LEN=4),INTENT(IN) :: tnm  ! tech name (i.e., OFCI etc.)
      CHARACTER (LEN =4):: tech
      CHARACTER (LEN =10) ::dtgcur,dtgcheck,dtglast,dtgnext
      CHARACTER (LEN =10) :: storm_name
      INTEGER :: luaa, ios, n, ihrs, istat, i, it, itp1,itime, isumchar
      INTEGER :: j, rindex, iquad, istart, k, result
      INTEGER, PARAMETER :: FTIMES=10,FINT=12
      REAL :: rarray(4)
      LOGICAL :: loop, first
      TYPE ( BIG_AID_DATA) aidsData
      TYPE ( AID_DATA) aidData, tauData

      ! Executable statements

      if (allocated(adtg)) deallocate(adtg)
      if (allocated(av)) deallocate(av)
      if (allocated(ap)) deallocate(ap)
      if (allocated(alat)) deallocate(alat)
      if (allocated(alon)) deallocate(alon)
      if (allocated(ar34)) deallocate(ar34)
      if (allocated(ar50)) deallocate(ar50)
      if (allocated(ar64)) deallocate(ar64)
      if (allocated(aty)) deallocate(aty)
      if (allocated(aname)) deallocate(aname)
      if (allocated(aift)) deallocate(aift)

      extract_aid = .false.

      naft=FTIMES

      luaa=10
      
      OPEN (luaa, FILE=afn, STATUS='OLD',IOSTAT=ios,ERR=1000)
      
      IF(ios /= 0)then
         print*,'extract_aid:  IOSTATUS = ',ios
         close(luaa)
         return
      END IF
      ! determine the first and last dtg from the a-deck
      
      backspace(luaa)
      
      first=.true.
      LOOP=.true.
      dtgcur = '1945010100'
      dtglast= '1945010100'
      n=0
      na=0

      DO WHILE ( LOOP )
         read ( luaa, '( 8x, a10, 6x, a4 )', end=17 ) dtgcheck,tech
         call dtgdif2(dtgcheck,dtgcur,ihrs,istat)
         if (dtglast .ne. dtgcheck) n=n+1
         if ( istat .ne. 0 ) then
            print*, 'extract_aid:  Line ',n,&
                 ' DTG problem in adeck ',dtgcheck
            close(luaa)
            return
         else if (tech == tnm .and. first) then
            dtgcur = dtgcheck
            first = .false.
         else if( ihrs .gt. 2000 ) then
            print*, 'extract_aid:  Line ',n,&
                 ' DTG too far from initial DTG ',dtgcheck
            close(luaa)
            return
         else
            if (tech == tnm) dtglast = dtgcheck
         endif
      END DO
17    rewind ( luaa )

      if (n.eq.0)then
         print*,'extract_aid: file had zero length'
         close(luaa)
         return
      end if

      if (first)then
         print*,'no records for techname',tnm
         close(luaa)
         return
      end if


      call dtgdif2(dtgcur,dtglast,ihrs,istat)

      ! loop through a-deck and cound the number of different date/times

      na=ihrs/6+1
      n=na

      allocate (adtg(n))
      allocate (aift(n,0:FTIMES))
      allocate (av(n,0:FTIMES))
      allocate (ap(n,0:FTIMES))
      allocate (alat(n,0:FTIMES))
      allocate (alon(n,0:FTIMES))
      allocate (ar34(n,0:FTIMES,4))
      allocate (ar50(n,0:FTIMES,4))
      allocate (ar64(n,0:FTIMES,4))
      allocate (aty(n,0:FTIMES))
      allocate (aname(n,0:FTIMES))

      ! initialize as missing
      do i=1,na
         do j=1,FTIMES
            adtg(i)=-9999
            aift(i,j) = -9999.0
            av(i,j)=-9999.0
            ap(i,j)=-9999.0
            alat(i,j)=-9999.0
            alon(i,j)=-9999.0
            aty(i,j)='  '
            do k=1,4
               ar34(i,j,k)=-9999.0
               ar50(i,j,k)=-9999.0
               ar64(i,j,k)=-9999.0
            end do
         end do
      end do
      storm_name='    INVEST'
      do i=1,na
         backspace(luaa) ! this was needed ... don't know why
         call getBigAidDTG ( luaa, dtgcur, aidsData, result )
         read(dtgcur,'(i10)')adtg(i)
         if ( result .eq. 0 ) goto 18
         call getTech ( aidsData, tnm, aidData, result )
         if ( result .eq. 0 ) goto 18
         do it=0,10
            itp1=it+1
            itime=it*FINT
            aift(i,it)=itime
            call getSingleTAU ( aidData, itime, tauData, result )
            if ( result .eq. 0 ) goto 18
            ! set global variable to missing before assignment
            adtg(i)=-9999
            av(i,it)=-9999.00
            alat(i,it)=-9999.00
            alon(i,it)=-9999.00
            do k=1,4
               ar34(i,it,k)=-9999.00
               ar50(i,it,k)=-9999.00
               ar64(i,it,k)=-9999.00
            enddo
            ! perscribe dtg,bv,blat,blon
            read(tauData%aRecord(1)%DTG,'(i10)')adtg(i)
            av(i,it)=float(tauData%aRecord(1)%vmax)
            ap(i,it)=float(tauData%aRecord(1)%mslp)
            alat(i,it)=tauData%aRecord(1)%lat
            alon(i,it)=tauData%aRecord(1)%lon
            if (tauData%aRecord(1)%NS == 'S') alat(i,it)&
                 = alat(i,it) * (-1.)
            if (tauData%aRecord(1)%EW == 'W') alon(i,it)&
                 = 360.0 - alon(i,it)
            aty(i,it)=tauData%aRecord(1)%ty
            aname(i,it)=tauData%aRecord(1)%stormname
            if (aname(i,it).ne.'         '.and. aname(i,it) &
                 .NE. '    INVEST') THEN
               storm_name=aname(i,it)
            end if
            ! now perscribe wind radii
            do j= 1,3 ! number of possible wind radii
               istart=0
               if (tauData%aRecord(j)%rad > 0) then
                  if (tauData%aRecord(j)%rad ==  34)  istart =  1
                  if (tauData%aRecord(j)%rad ==  35)  istart =  1
                  if (tauData%aRecord(j)%rad ==  50)  istart =  5
                  if (tauData%aRecord(j)%rad ==  64)  istart =  9
                  if (tauData%aRecord(j)%rad ==  65)  istart =  9
                  ! if (tauData%aRecord(j)%rad == 100)  istart = 13
                  rindex=1
                  if (tauData%aRecord(j)%windcode == 'NEQ') then
                     do iquad = istart,istart+3
                        rarray(rindex)= &
                             float(tauData%aRecord(j)%radii(rindex))
                        rindex=rindex +1
                     end do
                  else if (tauData%aRecord(j)%windcode == 'AAA' )then
                     do iquad = istart,istart+3
                        rarray(rindex)= float(tauData%aRecord(j)%radii(1))
                        rindex=rindex+1
                     end do
                  endif
                  ! clean up data arrays
                  do k=1,4
                     tauData%aRecord(j)%radii(k)=0
                  end do
                  !
                  if (istart == 1 ) then
                     do k=1,4
                        if (av(i,it).GE.34.0)then
                           aR34(i,it,k) = rarray(k)
                        else
                           aR34(i,it,k) = -9999.00
                        end if
                     end do
                  else if (istart == 5 )then
                     do k=1,4
                        if (av(i,it).GE.50.0)then
                           aR50(i,it,k) = rarray(k)
                        else
                           aR50(i,it,k) = -9999.00
                        end if
                     end do
                  else if (istart == 9 ) then
                     do k=1,4
                        if (av(i,it).GE.64.0)then
                           aR64(i,it,k) = rarray(k)
                        else
                           aR64(i,it,k) = -9999.00
                        end if
                     end do
                  end if
               end if
               do k=1,4
                  rarray(k)= -9999.00
               end do
            end do
         end do

18       CONTINUE

         call dtgmod ( dtgcur, 6, dtgnext, result )
         dtgcur = dtgnext
      end do

      do i=1,10
         isumchar=isumchar+iachar(storm_name(i:i))
      end do
      if (isumchar/= 0) then
         aname = storm_name
      else
         aname = 'IS_MISSING'
      end if

      extract_aid = .true.
      close(luaa)
      return
1000  continue
      print*,'extract_aid:  error opening ', TRIM(afn)
      close(luaa)
      return

    END FUNCTION extract_aid
 
!******************************************************************************

    LOGICAL FUNCTION extract_aidv(afn,tnm,ftimes,fint)
      !
      ! This routine reads the forecast of the techname in the adeck 
      ! and returns the information in the global array listed above under
      ! Forecasts.  Allows for variable specification of forecast times 
      ! (ftimes), and the forecast time interval (fint)
      !
      ! Written by John Knaff NOAA/NESDIS
      !
      ! uses :
      ! logical unit 10
      ! dataio.f routines supplied by NRLMRY
      ! dataformats.inc supplied by NRLMRY
      ! dataioparms.inc supplied by NRLMRY
      !
      !
      
      IMPLICIT NONE
      
      INCLUDE 'dataformats.inc'
      INCLUDE 'dataioparms.inc'

      
      CHARACTER (LEN=*),INTENT(IN) :: afn  ! a-deck file name
      CHARACTER (LEN=4),INTENT(IN) :: tnm    ! tech name (i.e., OFCI etc.)
      INTEGER,INTENT(IN) :: ftimes           ! number of forecast times
      INTEGER,INTENT(IN) :: fint             ! forecast interval in hours
      CHARACTER (LEN =4):: tech
      CHARACTER (LEN =10) ::dtgcur,dtgcheck,dtglast,dtgnext
      CHARACTER (LEN =10) :: storm_name
      INTEGER :: luaa, ios, n, ihrs, istat, i, it, itp1,itime, isumchar
      INTEGER :: j, rindex, iquad, istart, k, result
      REAL :: rarray(4)
      LOGICAL :: loop, first
      TYPE ( BIG_AID_DATA) aidsData
      TYPE ( AID_DATA) aidData, tauData

      ! Executable statements

      if (allocated(adtg)) deallocate(adtg)
      if (allocated(av)) deallocate(av)
      if (allocated(ap)) deallocate(ap)
      if (allocated(alat)) deallocate(alat)
      if (allocated(alon)) deallocate(alon)
      if (allocated(ar34)) deallocate(ar34)
      if (allocated(ar50)) deallocate(ar50)
      if (allocated(ar64)) deallocate(ar64)
      if (allocated(aty)) deallocate(aty)
      if (allocated(aname)) deallocate(aname)
      if (allocated(aift)) deallocate(aift)

      extract_aidv = .false.

      luaa=10

      naft=FTIMES
      
      OPEN (luaa, FILE=afn, STATUS='OLD',IOSTAT=ios,ERR=1000)
      
      IF(ios /= 0)then
         print*,'extract_aidv:  IOSTATUS = ',ios
         close(luaa)
         return
      END IF
      ! determine the first and last dtg from the a-deck
      
      backspace(luaa)
      
      first=.true.
      LOOP=.true.
      dtgcur = '1945010100'
      dtglast= '1945010100'
      n=0
      na=0

      DO WHILE ( LOOP )
         read ( luaa, '( 8x, a10, 6x, a4 )', end=17 ) dtgcheck,tech
         call dtgdif2(dtgcheck,dtgcur,ihrs,istat)
         if (dtglast .ne. dtgcheck) n=n+1
         if ( istat .ne. 0 ) then
            print*, 'extract_aidv:  Line ',n,&
                 ' DTG problem in adeck ',dtgcheck
            close(luaa)
            return
         else if (tech == tnm .and. first ) then
            dtgcur = dtgcheck
            first = .false.
         else if( ihrs .gt. 2000 ) then
            print*, 'extract_aidv:  Line ',n,&
                 ' DTG too far from initial DTG ',dtgcheck
            close(luaa)
            return
         else
            if (tech == tnm) dtglast = dtgcheck
         endif
      END DO
17    rewind ( luaa )

      if (n.eq.0)then
         print*,'extract_aidv: file had zero length'
         close(luaa)
         return
      end if

      if (first)then
         print*,'no records for techname',tnm
         close(luaa)
         return
      end if


      call dtgdif2(dtgcur,dtglast,ihrs,istat)

      ! loop through a-deck and cound the number of different date/times

      na=ihrs/6+1
      n=na

      allocate (adtg(n))
      allocate (aift(n,0:FTIMES))
      allocate (av(n,0:FTIMES))
      allocate (ap(n,0:FTIMES))
      allocate (alat(n,0:FTIMES))
      allocate (alon(n,0:FTIMES))
      allocate (ar34(n,0:FTIMES,4))
      allocate (ar50(n,0:FTIMES,4))
      allocate (ar64(n,0:FTIMES,4))
      allocate (aty(n,0:FTIMES))
      allocate (aname(n,0:FTIMES))

      ! initialize as missing
      do i=1,na
         adtg(i) = -9999
         do j=1,FTIMES
            aift(i,j) = -9999.0
            av  (i,j) = -9999.0
            ap  (i,j) = -9999.0
            alat(i,j) = -9999.0
            alon(i,j) = -9999.0
            aty(i,j)='  '
            aname(i,j)='           '
            do k=1,4
               ar34(i,j,k) = -9999.0
               ar50(i,j,k) = -9999.0
               ar64(i,j,k) = -9999.0
            end do
         end do
      end do
      storm_name='    INVEST'
      do i=1,na
         backspace(luaa) ! this was needed ... don't know why
         call getBigAidDTG ( luaa, dtgcur, aidsData, result )
         if ( result .eq. 0 ) goto 18
         call getTech ( aidsData, tnm, aidData, result )
         if ( result .eq. 0 ) goto 18
         do it=0,FTIMES
            itp1=it+1
            itime=it*FINT
            aift(i,it)=itime
            call getSingleTAU ( aidData, itime, tauData, result )
            if ( result .eq. 0 ) cycle
            ! set global variable to missing before assignment
            adtg(i)=-9999
            av(i,it)=-9999.00
            alat(i,it)=-9999.00
            alon(i,it)=-9999.00
            do k=1,4
               ar34(i,it,k)=-9999.00
               ar50(i,it,k)=-9999.00
               ar64(i,it,k)=-9999.00
            enddo
            ! prescribe dtg,bv,blat,blon
            read(tauData%aRecord(1)%DTG,'(i10)')adtg(i)
            av(i,it)=float(tauData%aRecord(1)%vmax)
            ap(i,it)=float(tauData%aRecord(1)%mslp)
            alat(i,it)=tauData%aRecord(1)%lat
            alon(i,it)=tauData%aRecord(1)%lon
            if (tauData%aRecord(1)%NS == 'S') alat(i,it)&
                 = alat(i,it) * (-1.)
            if (tauData%aRecord(1)%EW == 'W') alon(i,it)&
                 = 360.0 - alon(i,it)
            aty(i,it)=tauData%aRecord(1)%ty
            aname(i,it)=tauData%aRecord(1)%stormname
            if (aname(i,it).ne.'         '.and. aname(i,it) &
                 .NE. '    INVEST') THEN
               storm_name=aname(i,it)
            end if
            ! now prescribe wind radii
            do j= 1,3 ! number of possible wind radii
               istart=0
               if (tauData%aRecord(j)%rad > 0) then
                  if (tauData%aRecord(j)%rad ==  34)  istart =  1
                  if (tauData%aRecord(j)%rad ==  35)  istart =  1
                  if (tauData%aRecord(j)%rad ==  50)  istart =  5
                  if (tauData%aRecord(j)%rad ==  64)  istart =  9
                  if (tauData%aRecord(j)%rad ==  65)  istart =  9
                  ! if (tauData%aRecord(j)%rad == 100)  istart = 13
                  rindex=1
                  if (tauData%aRecord(j)%windcode == 'NEQ') then
                     do iquad = istart,istart+3
                        rarray(rindex)= &
                             float(tauData%aRecord(j)%radii(rindex))
                        rindex=rindex +1
                     end do
                  else if (tauData%aRecord(j)%windcode == 'AAA' )then
                     do iquad = istart,istart+3
                        rarray(rindex)= float(tauData%aRecord(j)%radii(1))
                        rindex=rindex+1
                     end do
                  endif
                  ! clean up data arrays
                  do k=1,4
                     tauData%aRecord(j)%radii(k)=0
                  end do
                  !
                  if (istart == 1 ) then
                     do k=1,4
                        if (av(i,it).GE.34.0)then
                           aR34(i,it,k) = rarray(k)
                        else
                           aR34(i,it,k) = -9999.00
                        end if
                     end do
                  else if (istart == 5 )then
                     do k=1,4
                        if (av(i,it).GE.50.0)then
                           aR50(i,it,k) = rarray(k)
                        else
                           aR50(i,it,k) = -9999.00
                        end if
                     end do
                  else if (istart == 9 ) then
                     do k=1,4
                        if (av(i,it).GE.64.0)then
                           aR64(i,it,k) = rarray(k)
                        else
                           aR64(i,it,k) = -9999.00
                        end if
                     end do
                  end if
               end if
               do k=1,4
                  rarray(k)= -9999.00
               end do
            end do
         end do
18       continue
         call dtgmod ( dtgcur, 6, dtgnext, result )
         dtgcur = dtgnext
      end do

      do i=1,10
         isumchar=isumchar+iachar(storm_name(i:i))
      end do

      if (isumchar/= 0) then
         aname = storm_name
      else
         aname = 'IS_MISSING'
      end if

      extract_aidv = .true.
      close(luaa)
      return
1000  continue
      print*,'extract_aidv:  error opening ', TRIM(afn)
      close(luaa)
      return

    END FUNCTION extract_aidv
 
  END MODULE extract_aid_module
