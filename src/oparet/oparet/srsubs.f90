      module srsubs
        use asciidat_routines
        use io
        use sinfo1
        use sinfo3
        implicit none

        contains

        subroutine srread(ludat,jyr,jlday,jtime,swlat,swlon,iyr,imon, &
                          iday,itime,slat00,slon00,slat12,slon12,idir,&
                          ispeed,ivmax,atcfid,sname,npoints,xlat,&
                          xlon,temps,clw,tpw,coord,times,istat,pamsu)

        ! Subroutine to read the file containing storm parameters and AMSU-A
        ! temperature retrieval information. 
        !
        ! Input: 
        !   ludat     - unit number of input file
        !
        ! Output:
        !    
        ! Temperatures are retrieved at 40 levels and are listed from top to
        ! bottom at each scan point.  The levels (mb) of the retrievals are
        ! 0.1, 0.2, 0.5, 1.0, 1.5, 2, 3, 4, 5, 7, 10, 15, 20, 25, 30, 50, 60,
        ! 70, 85, 100, 115, 135, 150, 200, 250, 300, 350, 400, 430, 475, 500,
        ! 570, 620, 670, 700, 780, 850, 920, 950, 1000.
        !
        ! Cloud liquid water and total precipitable water are also calculated by
        ! the retrieval code and are returned in clw and tpw 
        !
        ! Points in data array can be accumulated in multiple calls to 
        ! swread. The starting index for storing data at each call
        ! is ns. 
        !
        !****************************************************************************
        !
        ! INPUT:
        ! ludat  - unit number of the ascii temperature retrieval to be read from
        !
        ! OUTPUT:
        ! jyr                 - Year of AMSU data
        ! jlday               - Julian Day of AMSU data
        ! jtime               - Time at center of AMSU data (hhmmss in UTC)
        ! swlat               - Lat (deg N) of center of AMSU data
        ! swlon               - Lon (deg E) of center of AMSU data
        ! iyr                 - Year of storm
        ! imon                - month of storm
        ! iday                - day of storm
        ! itime               - Time (hr in UTC) of storm
        ! slat00              - Lat of storm center 
        ! slon00              - Lon of storm center
        ! slat12              - Lat of storm center at t-12hr
        ! slon12              - Lon of storm center at t-12 hr
        ! idir                - Storm heading (deg)
        ! ispeed              - Storm translational speed (kt)
        ! ivmax               - Storm max winds (kt)
        ! atcfid              - ATCF ID (a6)
        ! sname               - Storm name (a9)
        ! nsat                - number of the satellite (15=NOAA15, 16=NOAA16, etc)
        ! npoints             - number of points added to the output arrays 
        ! xlat(npoints)       - array of latitudes matching the points in data
        ! xlon(npoints)       - array of longitudes matching the points in data
        ! temps(npoints,mpas) - array of temperature retrieval data at all 40 levels
        ! clw(npoints)        - array of cloud liquid water
        ! tpw(npoints)        - array of total precipitable water
        ! coord               - The "COORDINATES" line as an 80 character variable
        ! times               - The "TIMES" line as an 80 character variable
        ! istat               - status of the read (ok if = 0)
        !                       istat=1 - end of file reached
        !                       istat=2 - error encountered
        !                       istat=3 - mxas too small
        !                       istat=4 - No temperature data found

          ! Note: These parameters must match those in the main oparet program
          integer, parameter :: mxas=30000 
          integer, parameter :: mpas=40
          integer, parameter :: nchan=15

          integer, parameter :: ik6=selected_int_kind(6)

          integer, intent(in) :: ludat
          integer, intent(inout) :: jyr,jlday,jtime,iyr,imon,iday,itime
          real, intent(inout) :: swlat,swlon,slat00,slon00,slat12,slon12
          integer, intent(inout) :: idir,ispeed,ivmax
          character(len=6),intent(inout) :: atcfid
          character(len=10),intent(inout) :: sname
          integer(kind=ik6), intent(inout) :: npoints
          real, intent(inout) :: xlat(mxas),xlon(mxas),clw(mxas),tpw(mxas)
          real, intent(inout) :: temps(mxas,mpas)
          character(len=90),intent(inout) :: coord,times
          integer, intent(inout) :: istat

          ! Added by JFD for using MIRS data
          real, allocatable, dimension(:) :: qcmirs 
          real, allocatable, dimension(:) :: pmirs
          real, allocatable, dimension(:,:) :: tmirs
          real, allocatable, dimension(:,:) :: rvmirs
          real, allocatable, dimension(:,:) :: tvmirs
          real, allocatable, dimension(:) :: fin
          real :: fout
          real :: error
          real, dimension(mpas) :: pamsu
          real, dimension(mxas,mpas) :: rv40

          integer :: ierr,nsat

          real,parameter :: qcFailValue=-999.0
          integer,parameter :: qcFail=2
          integer :: qcFailIdx=0
          integer :: qcFailMax=100
          real :: qcFailRatio=0.20
  
          character(len=120) :: infile
          ! End new variables by JFD
          !logical :: use_tv
          !real scale_rv
          
          ! Internal variable (placeholder for AMSU brightness temperatures)
          real :: btemps(nchan)

          integer :: i,j,k,n
  
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          !scale_rv = 0.001
          !use_tv = .TRUE.
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        
          read(ludat,101,err=920,end=910) coord
          read(ludat,101,err=920,end=910) times
          101 format(a90)
          read(coord,100,err=920,end=910) iyr,imon,iday,itime, &
                                          slat00,slon00,slat12,slon12, &
                                          idir,ispeed,ivmax,atcfid,sname
          100 format(3x,i4,i2,i2,8x,i2,4(f7.1),1x,i3,1x,i3,5x,i3,5x,a6, &
                     2x,a10) 
  
          !01 2012301 150244    29.88   -78.42    29.38   -75.92
          read(times,110,err=920,end=910) jyr,jlday,jtime,swlat,swlon
          110 format(3x,i4,i3,1x,i6,4(1x,f8.2))
          IF(swlon > 180.0) swlon=swlon-360.0
  
          read(ludat,120,err=920,end=910) npoints
          120 format(i6)
  
          if (npoints .gt. mxas) go to 930
          if (npoints .le.    0) go to 940


! Select according to temperature retrieval data. temp_ret.dat for
! statistical technique and COORDINATES for MIRS
          select case(fntinp)
 
          case('temp_ret.dat') 
            do i=1,npoints
              read(ludat,130,err=920,end=910) xlat(i),xlon(i),nsat, &
                                              (btemps(k),k=1,nchan), &
                                              (temps(i,k),k=1,mpas), &
                                              tpw(i),clw(i)
              130 format(15x,f9.3,f9.3,1x,i3,45x,15(f8.2),40(f8.2),2(f8.2))
            end do

            csatid(1:4)='TRET'
            WRITE(csatid(5:6),'(I2.2)') nsat
        
            close(ludat)

          case('COORTIMES') 
            read(ludat,125,err=920,end=910) csatid
            125 format(a6)
            print *, 'csatid ', csatid
            read(ludat,135,err=920,end=910) instr 
            135 format(a5)
            close(ludat)

            ! Read quality control into array qcmirs
            infile='qc.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            allocate(qcmirs(i1num))
            qcFailMax=floor(i1num*qcFailRatio)
            do i=1,i1num
              ! Get QC flag from buffer

              qcmirs(i)=r2_int(i,1)
              if(qcmirs(i) >= qcFail) then
                qcFailIdx=qcFailIdx+1
                if(qcFailIdx >= qcFailMax) then
                  WRITE(*,*) 'WARNING: QC maximum failure threshold ', &
                  qcFailMax,' (',qcFailRatio*100.0,'%) exceeded'

                  ierr=109
                  CALL EXIT(ierr)
                endif
              endif
            enddo
            WRITE(*,*) 'INFO: QC failures ',qcFailIdx

  
            ! Read MIRS data
            ! Read latitudes into array xlat
            infile='latitude.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)

            do i=1,i1num
              if((r1_flt(i) >= inpAtts%latValid) .AND. &
                (qcmirs(i) < qcFail)) then
                xlat(i)=(r1_flt(i)*inpAtts%latScale)+inpAtts%latOffset
              else
                xlat(i)=r1_flt(i)

              endif
              ! write (*,*) xlat(i)
            enddo

            ! Read longitudes into array xlon 
            infile='longitude.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            do i=1,i1num
              if((r1_flt(i) >= inpAtts%lonValid) .AND. & 
                (qcmirs(i) < qcFail)) then
                if(r1_flt(i) > 180.0) then
                  xlon(i)=r1_flt(i)-360.0
                else
                  xlon(i)=r1_flt(i)
                endif
                xlon(i)=(xlon(i)*inpAtts%lonScale)+inpAtts%lonOffset
              else 
                xlon(i)=r1_flt(i)
                WRITE(*,*) 'INFO: QC fail at ',i,' ',xlat(i),' ',xlon(i)
              endif
              ! write (*,*) xlon(i)
            enddo
            ! write(*,*) ' '

            ! Read pressure levels into array pmirs
            infile='pressure_layers.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            allocate(pmirs(i1num))
            do i=1,i1num
              if((r1_flt(i) >= inpAtts%prsValid) .AND. & 
                (qcmirs(i) < qcFail)) then
                pmirs(i)=(r1_flt(i)*inpAtts%prsScale)+inpAtts%prsOffset
              else
                pmirs(i)=qcFailValue
              endif
              ! write (*,*) pmirs(i)
            enddo
            ! write(*,*) ' '

            ! Read total precipitable water into array tpw
            infile='tpw.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            do i=1,i1num
              if((r1_flt(i) >= inpAtts%tpwValid) .AND. & 
                (qcmirs(i) < qcFail)) then
                tpw(i)=r1_flt(i)*inpAtts%tpwScale+inpAtts%tpwOffset
              else
                tpw(i)=qcFailValue
              endif
              ! write (*,*) tpw(i)
            enddo
            ! write(*,*) ' '

            ! Read cloud liquid water into array clw
            infile='liquid_water_path.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            do i=1,i1num
              if((r1_flt(i) >= inpAtts%clwValid) .AND. & 
                (qcmirs(i) < qcFail)) then
                clw(i)=r1_flt(i)*inpAtts%clwScale+inpAtts%clwOffset
              else
                clw(i)=qcFailValue
              endif
              ! write (*,*) clw(i)
            enddo
            ! write(*,*) ' '

            ! Read temperatures into array tmirs
            infile='temperature_profile.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            allocate(tmirs(i1num,i2num))
            ! run qc; set bad values to defult bad value from qc (-999??)
            do i=1,i1num
              do k=1,i2num
                if((r2_flt(i,k) >= inpAtts%tmpValid) .AND. & 
                  (qcmirs(i) < qcFail)) then
                  tmirs(i,k)=(r2_flt(i,k)*inpAtts%tmpScale)+ &
                  inpAtts%tmpOffset
                else
                  tmirs(i,k)=qcFailValue
                endif
                ! write(*,*) tmirs(i,k) 
              enddo
            enddo
            ! write(*,*) ' '
            
            ! Read water vapor mixing ratio into array rvmirs
            infile='water_vapor_profile.txt'
            ! write(*,*) infile
            call rdasciidat(infile,ludat)
            allocate(rvmirs(i1num,i2num))
            do i=1,i1num
              do k=1,i2num
                if((r2_flt(i,k) >= inpAtts%vprValid) .AND. & 
                  (qcmirs(i) < qcFail)) then
                  rvmirs(i,k)=(r2_flt(i,k)*inpAtts%vprScale)+ &
                  inpAtts%vprOffset
                else
                  rvmirs(i,k)=qcFailValue
                endif
                ! write(*,*) rvmirs(i,k) 
              enddo
            enddo
            ! write(*,*) ' '
  

            ! if use virtual temperature: calculate virtual temp and 
            ! replace temp with virtual temperature
            if (use_tv) then
                 allocate(tvmirs(i1num,i2num))
                 call calculate_tv (scale_rv, qcFailValue,  &
                    tmirs, rvmirs, tvmirs)
                 print*,'check max/min vals for Tv'
                 print*, "max(Tmirs), min(Tmirs) " 
                 print*, maxval(tmirs), minval(tmirs)
                 print*, "max(RVmirs), min(RVmirs) " 
                 print*, maxval(rvmirs), minval(rvmirs)
                 print*, "max(TVmirs), min(TVmirs) " 
                 print*, maxval(tvmirs), minval(tvmirs)
                 tmirs  = tvmirs 
            endif

            ! Move temperatures from MIRS 100 levels to the NESDIS 40 levels
            allocate(fin(i2num))
            do n=1,i1num
     	      do k=1,i2num
 	        fin(k)=tmirs(n,k)
 	      enddo
 	      k=1
 	      call xint(pmirs,fin,i2num,1,0,pamsu(k),fout,ierr)
 	      temps(n,k)=fout
 	      if(ierr /= 0) write(*,*) 'Check xint for temp ',ierr,k
 	      do k=2,mpas
 	        call xint(pmirs,fin,i2num,0,0,pamsu(k),fout,ierr)
 	        temps(n,k)=fout
 	        if(ierr /= 0) write(*,*) 'Check xint for temp ',ierr,k
 	      enddo      	 
            enddo

            ! Move water vapor mixing ratio from MIRS 100 levels to the NESDIS 40 levels
            do n=1,i1num
 	      do k=1,i2num
 	        fin(k)=rvmirs(n,k)
 	      enddo
 	      k=1
 	      call xint(pmirs,fin,i2num,1,0,pamsu(k),fout,ierr)
 	      rv40(n,k)=fout
 	      if(ierr /= 0) write(*,*) 'Check xint for rv ',ierr,k
 	      do k=2,mpas
 	        call xint(pmirs,fin,i2num,0,0,pamsu(k),fout,ierr)
 	        rv40(n,k)=fout
 	        if(ierr /= 0) write(*,*) 'Check xint for rv ',ierr,k
 	      enddo      	 
            enddo

            ! Deallocate arrays. These have been allocated
            deallocate(qcmirs)
            deallocate(pmirs)
            deallocate(tmirs)
            deallocate(rvmirs)
            if(allocated(tvmirs)) deallocate(tvmirs)
            deallocate(fin)

            ! These may have been allocated
            if(allocated(r1_int)) deallocate(r1_int)
            if(allocated(r2_int)) deallocate(r2_int)
            if(allocated(r3_int)) deallocate(r3_int)
            if(allocated(r1_flt)) deallocate(r1_flt)
            if(allocated(r2_flt)) deallocate(r2_flt)
            if(allocated(r3_flt)) deallocate(r3_flt)

          end select 

          istat=0
          return
  
          910 continue
          istat=1
          close(ludat)
          return
  
          920  continue
          istat=2
          close(ludat)
          return
   
          930 continue
          istat=3
          close(ludat)
          return

          940  continue
          istat=4
          close(ludat)
          return
      
        end subroutine srread


        subroutine calculate_tv (scale_rv,qcFailValue, &
            tmirs, rvmirs, tvmirs)
          ! calculate virtual temperature from T and mixing ratio
          ! assumes input T in K, and r in kg/kg
          !integer, intent(in) :: i1num,i2num
          real, intent(in) :: scale_rv, qcFailValue
          real,intent(in), dimension(i1num,i2num) :: tmirs
          real,intent(in), dimension(i1num,i2num) :: rvmirs
          real,intent(out),dimension(i1num,i2num) :: tvmirs
          integer :: i,j
          
          !allocate(tmirs(i1num,i2num))
          !allocate(rvmirs(i1num,i2num))
          !allocate(tvmirs(i1num,i2num))

          do i = 1, i1num
            do j = 1, i2num
                if (tmirs(i,j) < qcFailValue+1) then 
                    tvmirs(i,j) = tmirs(i,j)
                elseif (rvmirs(i,j) < qcFailValue+1) then 
                    tvmirs(i,j) = tmirs(i,j)
                else
                    tvmirs(i,j)=    &
                        (1+0.608 * (rvmirs(i,j)* &
                        scale_rv/(1+rvmirs(i,j)*scale_rv)) )*tmirs(i,j)
                endif
            enddo
          enddo

          return

        end subroutine


! The subroutine xint is meant to be a temporary solution only.  Oparet
! is currently hard-wired to use the 40 levels of the Goldberg retrieval
! scheme.  MIRS uses 100 levels.  The first step in the implementation
! of the MIRS version will use the conversion to 40 levels here.  After
! successful running, the code will be updated to use the 100 levels of
! the MIRS.  There is an issue, however.  The pressure levels of the
! MIRS data are at strange values, and in particular, not at any of the
! NCEP values used to write out the information.  That will need to be
! addressed.
      subroutine xint(x,f,n,iflag,lflag,xi,fi,ierr)
!     This routine applies a quadratic interpolation procedure
!     to f(x) between x(1) and x(n). f(x) is assumed to be
!     represented by quadratic polynomials between the points
!     x(i). The polynomials are chosen so that they equal f(i)
!     at the points x(i), the first derviatives on either
!     side of the interior x(i) match at x(i), and the second
!     derivative of the approximated function integrated
!     over the domain is minimized.
!
!     This version is for interpolating longitude
!
!     Input:  x(1),x(2) ... x(n)      The x values (must be sequential)
!             f(1),f(2) ... f(n)      The function values
!             n                       The number of x,f pairs
!             iflag                   Flag for initialization
!                                      =1 for coefficient calculation
!                                      =0 to use previous coefficients
!             lflag                   Flag for linear interpolation
!                                      =0 to perform linear interpolation 
!                                      =1 to perform quadratic interpolation
!             xi                      The x value at which to interpolate f
!
!     Output: fi                      The interpolated function value
!             ierr                    Error flag
!                                      =0  Normal return
!                                      =1  Parameter nmax is too small
!                                      =2  The x values are not sequential
!                                      =3  Coefficient iteration did not
!                                          converge
!                                      =4  Mix-up finding coefficients
!                                      =5  if xi .gt. x(n) or .lt. x(1),
!                                          xi is set to nearest endpoint
!                                          before the interpolation
!
!                                     Note: fi is set to -99.9 if
!                                           ierr=1,2,3 or 4
!
      INTEGER, PARAMETER :: nmax=100
      INTEGER :: n
!
!!      dimension x(n),f(n)
      REAL, DIMENSION(n) :: x,f
!
!     Save variables
!!      dimension ax(nmax),bx(nmax),cx(nmax)
      REAL, DIMENSION(nmax) :: ax,bx,cx
!
!     Temporary local variables
!!      dimension df(nmax),dx(nmax),gm(nmax),ct(nmax)
      REAL, DIMENSION(nmax) :: df,dx,gm,ct

      INTEGER :: idbug,lutest,ierr,iflag,lflag,i,ii,j,k,nit
      REAL :: thresh,fi,d,eps,bb,aa,dev,xi,cf0,cfsave,cft,den,dsdc1
      REAL :: slo,slt,rel
!
!      common /xsave/ ax,bx,cx
!
!     Specify unit number for debug write statements
!     and debug flag
      idbug  = 0
      lutest = 6
!
!     Initialize error flag
      ierr   = 0
!
!     Specify minimum reduction in cost function for convergence
      thresh = 1.0e-10
!
!     Check to make sure nmax is large enough, and n is .gt. 1 
      if (n .gt. nmax .or. n .lt. 2) then
         ierr=1
         fi = -99.9
         return
      endif
!
      if (iflag .eq. 1) then
!        Perform the initialization for later interpolation
!
!        Check to make sure x is sequential
         do 10 i=1,n-1
            if (x(i) .ge. x(i+1)) then
               ierr=2
               fi = -99.9
               return
            endif
   10    continue
!
!        Check for special case where n=2. Only linear interpolation
!        is possible.
         if (n .eq. 2) then
            cx(1) = 0.0
            bx(1) = (f(2)-f(1))/(x(2)-x(1))
            ax(1) = f(1) - bx(1)*x(1)
            go to 1500
         endif
!
!        Calculate x and f differences
         do 15 i=1,n-1
            df(i) = f(i+1)-f(i)
            dx(i) = x(i+1)-x(i)
   15    continue
!
!        Calculate domain size
         d = x(n) - x(1)
!
!        Check for linearity of input points
         eps = 1.0e-10
         bb = (f(2)-f(1))/(x(2)-x(1))
         aa = f(1) - bb*x(1)
         dev = 0.0
         do 12 i=3,n
            dev = dev + abs(aa + bb*x(i) - f(i))
   12    continue
!
         if (dev .lt. eps .or. lflag .eq. 0) then
            do 13 i=1,n-1
               cx(i) = 0.0
   13       continue
            go to 1000
         endif
!
!        Iterate to find the c-coefficients
         cx(1) = 0.0
         nit  = 100
         slt  = 0.01
         cfsave = 1.0e+10
!
         do 20 k=1,nit
!           Calculate c values
            do 25 i=2,n-1
               cx(i) = -cx(i-1)*dx(i-1)/dx(i)-df(i-1)/(dx(i)*dx(i-1))+df(i  )/(dx(i)*dx(i  ))
   25       continue
!
!           Calculate current value of cost function
            cf0 = 0.0
            do 26 i=1,n-1
               cf0 = cf0 + cx(i)*cx(i)*dx(i)
   26       continue
            cf0 = 0.5*cf0/d
!
            if (idbug .ne. 0) then
               write(lutest,101) cf0
  101          format(/,' cf0=',e13.6)
            endif
!
!           Check for convergence
            rel = abs(cf0 - cfsave)/abs(cfsave)
            if (rel .lt. thresh) go to 1000
            cfsave = cf0
!
!           Calculate values of Lagrange multipliers
            gm(n-1) = cx(n-1)*dx(n-1)/d
!
            if (n .gt. 3) then
               do 30 i=n-2,2,-1
                  gm(i) = cx(i)*dx(i)/d - gm(i+1)*dx(i)/dx(i+1)
   30          continue
            endif
!
!           Calculate gradient of cost function with respect to c1
            dsdc1 =  dx(1)*(cx(1)/d - gm(2)/dx(2))
!
!           Adjust cx(1) using trial step
            ct(1) = cx(1) - slt*dsdc1
!
!           Calculate remaining c values at trial step
            do 33 i=2,n-1
               ct(i) = -ct(i-1)*dx(i-1)/dx(i)-df(i-1)/(dx(i)*dx(i-1))+df(i  )/(dx(i)*dx(i  ))
   33       continue
!
!           Calculate cost function at trial step
            cft = 0.0
            do 31 i=1,n-1
               cft = cft + ct(i)*ct(i)*dx(i)
   31       continue
            cft = 0.5*cft/d
!
!            write(6,*) 'dsdc1,cft,cf0',dsdc1,cft,cf0
!           Calculate optimal step length and re-adjust cx(1)
            den = 2.0*((cft-cf0) + slt*dsdc1*dsdc1)
            if (den .ne. 0.0) then
               slo = dsdc1*dsdc1*slt*slt/den
            else
               slo =0.0
            endif
!
!           Adjust slo if desired
            slo = 1.0*slo
!
            cx(1) = cx(1) - slo*dsdc1
!
            if (idbug .ne. 0) then
               write(lutest,100) k,cft,slt,slo
 100          format(' Iteration=',i4,'  cf1=',e11.4,' slt=',e11.4,' slo=',e11.4)
!     
               do 99 j=1,n-1
                  write(lutest,102) j,cx(j)
  102             format('    i=',i2,' c=',f8.4)
   99          continue
            endif
!
!           Calculate trial step for next time step
            slt = 0.5*slo
   20    continue
!
!        Iteration did not converge
         ierr=3
         fi=-99.9
         return
!
!        Iteration converged
 1000    continue
!
         if (idbug .ne. 0) then
            write(lutest,104)
  104       format(/,' Iteration converged')
         endif
!
!        Calculate b and a coefficients
         do 40 i=1,n-1
            bx(i) = df(i)/dx(i) - cx(i)*(x(i+1) + x(i))
            ax(i) = f(i) - bx(i)*x(i) - cx(i)*x(i)*x(i)
   40    continue        
      endif
!
 1500 continue
!     Interpolate the function
!
!     Check for xi out of bounds
      if (xi .lt. x(1)) then
         xi = x(1)
         ierr = 5
      endif
!
      if (xi .gt. x(n)) then
         xi = x(n)
         ierr = 5
      endif
!
!     Find the interval for the interpolation
      ii = 1
      do 50 i=2,n
         if (xi .le. x(i)) then
            ii = i-1
            go to 2000
         endif
   50 continue
!
      fi = -99.9
      ierr=4
      return
!
 2000 continue
      fi = ax(ii) + bx(ii)*xi + cx(ii)*xi*xi
!
      return
      end subroutine xint

      end module srsubs
