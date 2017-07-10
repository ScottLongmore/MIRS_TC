PROGRAM ShortTermTrack_mirs
  !
  ! PREAMBLE: 
  !
  ! Program extracts information from PREVIOUS (-12h) CARQ and official forecast
  ! lines of an adeck around a synoptic time to provide a short term 
  ! latitude, and longitude history for a given storm.  The current date 
  ! and time is also the analysis time for a surface wind analysis and 
  ! determines the analysis time window (typically 3-9 hours). The output 
  ! is meant to be  used to drive a aircraft based surface wind analysis.  
  !
  ! This program uses modules that read the a and f deck formats of the atcf
  ! database.  These may need to be modified as those databases evolve.
  ! 
  ! INPUT: 
  ! cadeck  - adeck filename, provide as an argument (this name is also used
  !           to determine storm identifyer (e.g., al012012)
  ! ccdtg   - current date time, provided as an argument
  ! rmode   - run mode (e.g., CURR or M12H)
  !
  ! OUTPUT:
  ! a file containing a starting time, an analysis time, and an ending time for
  ! the wind analysis followed by the number of lat,lon points that follow.
  !
  ! The naming convention of the output file is storm identifier followed by
  ! .inp (e.g., al012012.inp).   
  ! 
  ! CREDITS: 
  ! Written by J. Knaff NOAA/NESDIS as part of a JHT project starting in
  ! 2011
  !
  !
  ! HISTORY:
  ! 
  ! Added analysis mode to allow track information for both early and late
  ! analysis applications - JAK, January 9, 2012 
  !
  ! Added analysis mode to allow track information for the early case, but
  ! which occurs prior to the synoptic time.  In this process the analysis time
  ! is pushed to the next synoptic time. - JAK, January 11, 2012
  !
  ! Found a feature if the prious OFCL forcast is not available.. Fixed - JAK
  ! August 22, 2012
  !
  ! Made modifications for real-time use for NPP ATMS at NDE.  Removed f-deck
  ! processing, added conditional to only create output file if the -12h and 0h
  ! positions are available, and generalized for use with both NHC a-decks 
  ! (aal, aep, acp) and JTWC a-decks (awp, aio, ash).  IMPORTANT:  changed
  ! input arguments - changed run mode from BEFORE/EARLY/LATE to CURR/M12H - 
  ! A. Schumacher, CIRA, May 30, 2013.
  !
  ! Last Modified: May 30, 2013
  ! 
  !***************************************************************

  ! modules to be used  
  USE extract_aid_module

  IMPLICIT NONE

  INTEGER:: i,j,k,istat,idummy,iargc,iremove
  INTEGER:: iftimes=20,interval=6
  INTEGER :: iexist_00, iexist_m12
  REAL, PARAMETER:: rmiss=-999.000
  INTEGER,PARAMETER::imiss=-999, luout=35
  INTEGER,PARAMETER::iextmiss=-9999
  
  CHARACTER (LEN=255) :: cadeck,cfdeck,coutfn
  CHARACTER (LEN=10)  :: ccdtg,cnewdtg,sname
  CHARACTER (LEN=12)  :: first_t,last_t,current_t,anal_t,prev_t
  CHARACTER (LEN=4)   :: fcst, fcsti, fcstb
  CHARACTER (LEN=4)   :: rmode
  INTEGER:: toffset
  INTEGER:: icdtg
  INTEGER:: iacount,ntrack,ibcount
  INTEGER, DIMENSION(6,8)::atrack=imiss
  CHARACTER(LEN=10) :: snamearr(6)  !Same as sname, but maybe not always
  INTEGER, ALLOCATABLE:: track(:,:)
  INTEGER, ALLOCATABLE:: indx(:)
  REAL, ALLOCATABLE:: dt(:)  
  REAL:: timediff
  LOGICAL:: nofcst
  INTEGER:: ierr


  ! Check the number of arguements, if not equal to 3, stop
  
  IF (iargc() /= 3) THEN
    WRITE(*,*) 'WARNING: problem with command line'
    ierr=64
    CALL EXIT(ierr)
  END IF

  CALL getarg(1,cadeck)
  CALL getarg(2,ccdtg) 
  CALL getarg(3,rmode) 


  ! Make sure argument rmode is valid

  IF (rmode .EQ. 'M12H') THEN
     toffset = -12
  ELSEIF (rmode .EQ. 'CURR') THEN
     toffset = 0
  ELSE
     WRITE(*,*) 'WARNING: rmode must be M12H or CURR'
     ierr=102
     CALL EXIT(ierr)
  ENDIF     


  ! Set default noforcast value
  
  nofcst=.true.
  
  
  ! Determine analysis time string and fcst to use (OFCL for Atl/EPac, JTWC else)

  anal_t=ccdtg//'00'
  
  IF (cadeck(2:3) .EQ. 'al' .OR. cadeck(2:3) .EQ. 'ep' .OR. cadeck(2:3) .EQ. 'cp') THEN
     fcst='OFCL'
     fcsti='OFCI'
     fcstb='BAMM'
  ELSEIF (cadeck(2:3) .EQ. 'wp' .OR. cadeck(2:3) .EQ. 'io' .OR. cadeck(2:3) .EQ. 'sh') THEN
     fcst='JTWC'
     fcsti='JTWI'
     fcstb='MBAM'
  ELSE
     WRITE(*,*) 'WARNING: unrecognized basin string in adeck: ',cadeck(2:3)
     ierr=102
     CALL EXIT(ierr)
  ENDIF
  
  
  ! Determine offset anlaysis time (offset by -12h if rmode = M12H)

  CALL DTGMOD(ccdtg,toffset,cnewdtg,istat)
  ccdtg=cnewdtg
  READ(ccdtg,'(i10.10)')icdtg

  ! create the output file name.

  i=LEN(TRIM(ADJUSTL(cadeck)))
  coutfn=TRIM(ADJUSTL(cadeck))
  coutfn=coutfn(2:i-4)//'.inp'


  ! set forecast counter to 0

  ibcount=0


! Extract CARQ for t = -24h from offset analysis time from a-deck

  IF(extract_advt(cadeck,-24)) THEN
     
     DO i=1,nq

        IF (qdtg(i) == icdtg .AND. qlat(i) >= -90.0)  THEN

           ibcount=ibcount+1
           
           atrack(ibcount,3)=nint(qlat(i)*100.0)
           atrack(ibcount,4)=nint(qlon(i)*100.0)

           atrack(ibcount,5)=NINT(qdir(i))
           atrack(ibcount,6)=NINT(qspd(i))
           atrack(ibcount,7)=NINT(qrmw(i))
           atrack(ibcount,8)=NINT(qv(i))
           snamearr(ibcount)=qname(i)
           
           CALL DTGMOD(ccdtg,-24,cnewdtg,istat)
           READ(cnewdtg,'(i10.10)')idummy

           atrack(ibcount,1)=idummy/100
           atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

        ENDIF

     END DO
  END IF


! Extract CARQ for t = -18h from offset analysis time from a-deck

  IF(extract_advt(cadeck,-18)) THEN
     
     DO i=1,nq

        IF (qdtg(i) == icdtg.AND. qlat(i) >= -90.0)  THEN
           
           ibcount=ibcount+1
           
           atrack(ibcount,3)=nint(qlat(i)*100.0)
           atrack(ibcount,4)=nint(qlon(i)*100.0)

           atrack(ibcount,5)=NINT(qdir(i))
           atrack(ibcount,6)=NINT(qspd(i))
           atrack(ibcount,7)=NINT(qrmw(i))
           atrack(ibcount,8)=NINT(qv(i))
           snamearr(ibcount)=qname(i)
           
           CALL DTGMOD(ccdtg,-18,cnewdtg,istat)
           READ(cnewdtg,'(i10.10)')idummy

           atrack(ibcount,1)=idummy/100
           atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

        ENDIF

     END DO
  END IF


! Extract CARQ for t = -12h from offset analysis time from a-deck
     
  IF(extract_advt(cadeck,-12)) THEN
     
     DO i=1,nq

        IF (qdtg(i) == icdtg .AND. qlat(i) >= -90.0)  THEN
           
           ibcount=ibcount+1
           
           atrack(ibcount,3)=nint(qlat(i)*100.0)
           atrack(ibcount,4)=nint(qlon(i)*100.0)

           atrack(ibcount,5)=NINT(qdir(i))
           atrack(ibcount,6)=NINT(qspd(i))
           atrack(ibcount,7)=NINT(qrmw(i))
           atrack(ibcount,8)=NINT(qv(i))
           snamearr(ibcount)=qname(i)
           
           CALL DTGMOD(ccdtg,-12,cnewdtg,istat)
           READ(cnewdtg,'(i10.10)')idummy

           atrack(ibcount,1)=idummy/100
           atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

        ENDIF

     END DO
  END IF


! Extract CARQ for t = -6h from offset analysis time from a-deck
  
  IF(extract_advt(cadeck,-6)) THEN
     
     DO i=1,nq

        IF (qdtg(i) == icdtg .AND. qlat(i) >= -90.0)  THEN
           
           ibcount=ibcount+1
           
           atrack(ibcount,3)=nint(qlat(i)*100.0)
           atrack(ibcount,4)=nint(qlon(i)*100.0)

           atrack(ibcount,5)=NINT(qdir(i))
           atrack(ibcount,6)=NINT(qspd(i))
           atrack(ibcount,7)=NINT(qrmw(i))
           atrack(ibcount,8)=NINT(qv(i))
           snamearr(ibcount)=qname(i)
           
           CALL DTGMOD(ccdtg,-6,cnewdtg,istat)
           READ(cnewdtg,'(i10.10)')idummy

           atrack(ibcount,1)=idummy/100
           atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

           sname=qname(1)
        ENDIF

     END DO
  END IF


! Extract CARQ for t = 0h from offset analysis time from a-deck

  IF(extract_adv(cadeck)) THEN
     
     DO i=1,nq

        IF (qdtg(i) == icdtg)  THEN
           
           ibcount=ibcount+1
           
           atrack(ibcount,3)=nint(qlat(i)*100.0)
           atrack(ibcount,4)=nint(qlon(i)*100.0)

           atrack(ibcount,5)=NINT(qdir(i))
           atrack(ibcount,6)=NINT(qspd(i))
           atrack(ibcount,7)=NINT(qrmw(i))
           atrack(ibcount,8)=NINT(qv(i))
           snamearr(ibcount)=qname(i)

           atrack(ibcount,1)=icdtg/100
           atrack(ibcount,2)=(icdtg-(atrack(ibcount,1)*100))*100

           sname=qname(1)

        ENDIF

     END DO
     
  END IF


! Extract forecast for t = 12h from offset analysis time from a-deck

  IF (extract_aid(cadeck,fcst)) THEN

     DO i=1,na
     
        IF(adtg(i) == icdtg .AND. ibcount > 0) THEN
           
           ibcount=ibcount+1
           
           atrack(ibcount,3)=nint(alat(i,1)*100.0)
           atrack(ibcount,4)=nint(alon(i,1)*100.0)

           atrack(ibcount,8)=NINT(av(i,1))
           snamearr(ibcount)=sname
              
           CALL DTGMOD(ccdtg,12,cnewdtg,istat)
           READ(cnewdtg,'(i10.10)')idummy
              
           atrack(ibcount,1)=idummy/100
           atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

           nofcst=.false.

        END IF
        
     END DO

  ELSE
     nofcst=.true.
  END IF


! If t = 12h forecast not available, try alternate forecast sources from a-deck

  IF (ibcount  > 0) THEN

     WRITE(first_t,'(i8.8,i4.4)')atrack(1,1),atrack(1,2)
     WRITE(last_t,'(i8.8,i4.4)')atrack(ibcount,1),atrack(ibcount,2)

     ! USE interpolated official forecast since the official is missing.

     IF (atrack(ibcount,3) < -80000 .OR. nofcst) THEN

        IF (extract_aid(cadeck,fcsti)) THEN

           DO i=1,na

              IF(adtg(i) == icdtg .AND. ibcount > 0) THEN
           
                 IF (nofcst) ibcount=ibcount+1

                 atrack(ibcount,3)=nint(alat(i,1)*100.0)
                 atrack(ibcount,4)=nint(alon(i,1)*100.0)

                 atrack(ibcount,8)=NINT(av(i,1))
                 snamearr(ibcount)=sname
              
                 CALL DTGMOD(ccdtg,12,cnewdtg,istat)
                 READ(cnewdtg,'(i10.10)')idummy
              
                 atrack(ibcount,1)=idummy/100
                 atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

                 nofcst=.false.

              END IF
        
           END DO

        ELSE

           nofcst=.true.

        END IF
	
     ENDIF
     

     ! USE BAM medium forecast since the others are missing.

     IF (atrack(ibcount,3) < -80000 .OR. nofcst) THEN

        IF (extract_aidv(cadeck,fcstb,14,12)) THEN

           DO i=1,na

              IF(adtg(i) == icdtg .AND. ibcount > 0) THEN
           
                 IF (nofcst) ibcount=ibcount+1

                 atrack(ibcount,3)=nint(alat(i,1)*100.0)
                 atrack(ibcount,4)=nint(alon(i,1)*100.0)

                 atrack(ibcount,8)=NINT(av(i,1))
                 snamearr(ibcount)=sname
              
                 CALL DTGMOD(ccdtg,12,cnewdtg,istat)
                 READ(cnewdtg,'(i10.10)')idummy
              
                 atrack(ibcount,1)=idummy/100
                 atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100

                 nofcst=.false.

              END IF
        
           END DO

        ELSE

           nofcst=.true.

        END IF

     END IF
     

     !Interpolate forecast point since official, official interpolated, and BAM medium are not available.

     IF (atrack(ibcount,3) < -80000 .OR. nofcst) THEN

        IF (ibcount >= 3) THEN
              
              IF (nofcst) ibcount=ibcount+1

              atrack(ibcount,3)=atrack(ibcount -1,3) + 2.*(atrack(ibcount-1,3) &
                   - atrack(ibcount-2,3))

              atrack(ibcount,4)=atrack(ibcount -1,4) + 2.*(atrack(ibcount-1,4) &
                   - atrack(ibcount-2,4))

              atrack(ibcount,8)=atrack(ibcount -1,8) + 2.*(atrack(ibcount-1,8) &
                   - atrack(ibcount-2,8))

              CALL DTGMOD(ccdtg,12,cnewdtg,istat)
              READ(cnewdtg,'(i10.10)')idummy
              
              atrack(ibcount,1)=idummy/100
              atrack(ibcount,2)=(idummy-(atrack(ibcount,1)*100))*100
     
        ELSE
        
           ibcount = ibcount -1

        END IF

     END IF


     ! Fill track with tracks from a-deck (atrack)

     IF (ALLOCATED(track)) DEALLOCATE(track)
     ALLOCATE(TRACK(ibcount,8))
     IF (ALLOCATED(dt)) DEALLOCATE(dt)
     ALLOCATE(dt(ibcount))
     IF (ALLOCATED(indx)) DEALLOCATE(indx)
     ALLOCATE(indx(ibcount)) 
     
     DO i=1,ibcount
     
                 track(i,1)=atrack(i,1)
                 track(i,2)=atrack(i,2)
                 track(i,3)=atrack(i,3)
                 track(i,4)=atrack(i,4)
                 track(i,5)=atrack(i,5)
                 track(i,6)=atrack(i,6)
                 track(i,7)=atrack(i,7)
                 track(i,8)=atrack(i,8)
     
     END DO

     ntrack=ibcount     


     ! Create offset dates/times

     DO i=1,ntrack
        
        WRITE(current_t,'(i8.8,i4.4)') track(i,1), track(i,2)
        dt(i)=timediff(anal_t,current_t)
        
     END DO


     ! Create offset hour indices

     CALL indexx(ntrack,dt,indx)
     
 
     ! Check to see if the t=0h and t=12h positions were found.  These are the only
     !   two positions needed for NDE NPP TC Products.  
 
     iexist_m12 = 0
     iexist_00 = 0
     
     DO i = 1, ntrack
     
       IF (dt(indx(i)) .EQ. -12.0) THEN
         IF (track(indx(i),3) .GT. iextmiss .AND. track(indx(i),4) .GT. iextmiss .AND. &
             track(indx(i),8) .GT. iextmiss) &
	   iexist_m12 = 1
       ENDIF

       IF (dt(indx(i)) .EQ. 0.0) THEN
         IF (track(indx(i),3) .GT. iextmiss .AND. track(indx(i),4) .GT. iextmiss .AND. &
             track(indx(i),8) .GT. iextmiss) &
	   iexist_00 = 1
       ENDIF

     ENDDO

 
     ! Write output file only if t=0h and t-12h positions are not missing.
     
     IF (iexist_m12 .EQ. 1 .AND. iexist_00 .EQ. 1) THEN

       OPEN(luout,file=coutfn)

       WRITE(luout,'(a8,1x,a8,1x,a4,a10,1x,a10)')coutfn(1:8),anal_t(1:8),&
            anal_t(9:12),sname,rmode
       WRITE(luout,'(i5,1x,"Number of Track Points")')ntrack
       WRITE(luout,'("YEARDATE",2x,"TIME","   DELTA T","  LATITUDE"," LONGITUDE",&
           " DIRECTION","  SPEED","  RMAX","   VMAX","      NAME")')
       DO i=1,ntrack

!        The missing value in the extract_* routines is -9999.0, here it is -999.0.
!        Since I'm outputting positive integers using I3.3, I use ABS() and get 999 for
!        the missing value.  I'll check the array track for -9999 and change them to
!        -999.

          DO j=1,8
	
            IF(track(indx(i),j) == iextmiss) track(indx(i),j)=imiss
	  
          ENDDO


          WRITE(luout,'(i8.8,2x,i4.4,f10.4,2f10.2,6X,4(I3.3,4X),A10)') &
               (track(indx(i),j),j=1,2),dt(indx(i)), &
               ((float(track(indx(i),j))/100.),j=3,4),&
               (ABS((track(indx(i),j))),j=5,8),&
               sname
	     
       END DO

       CLOSE (luout)

     ELSE

       WRITE(*,*) 'INFO: t=0h or t=-12h position missing'
       ierr=101
       CALL EXIT(ierr)
     
     ENDIF

  ELSE

       WRITE(*,*) 'INFO: t=0h or t=-12h position missing'
       ierr=101
       CALL EXIT(ierr)

  END IF

END PROGRAM ShortTermTrack_mirs
