      program oparet
      ! This program performs retrievals of temperature, heights,
      ! surface pressure and winds using the AMSU temperatures at
      ! the satellite footprints. 
      !
      ! This is the updated version where all satellite and tropical cyclone
      ! information is in a single file. 
      !
      ! File Updates:
      !  New version created        5/15/2003     M. DeMaria
      !  ATCF fix file option added 9/26/2005     M. DeMaria and J. Knaff
      !  DATELINE ISSUE Resolved    2/20/2006     J. Knaff
      ! 
      ! Input files: 
      !     temp_ret.dat        - File with storm input parameters 
      !                           and AMSU-A radiances and temp/CLW retrievals
      !                           (combination of temp, times and coordinate 
      !                            files, in the old version of oparet)
      !                               
      !     AVN.DAT             - Packed ASCII NCEP analysis file
      !
      !     oparat.cfg          - oparet configuration file
      ! 
      ! Output files:
      !     oparet.log          -      ASCII file with basic information about the 
      !                                retrieval calcuations
      !     Lbnnyy_mmddtt.DATss - loc: ASCII file containing lat/lons of AMSU input data 
      !
      !     Rbnnyy_mmddtt.DATss - rza: ASCII file containing V,T,P,rho as function of 
      !                                (r,z) from AMSU retrieval
      !     Abnnyy_mmddtt.DATss - xya: ASCII file containing U,V,T,Z,Ps,rho,CLW  
      !                                as function of (x,y,P) from AMSU retrieval
      !
      !     Sbnnyy_mmddtt.DATss - sta: ASCII file containing statistical parameters for 
      !                                the TC estimation algorithm
      !     Fbnnyy_mmddtt.DATss - fix: ASCII file containing estimated TC intensity/size 
      !
      !     Gbnnyy_mmddtt.DATss - afx: ATCF formatted fix file
      !
      !     Abnnyy_XmmDD_PACK.DAT - apk: Packed ASCII file containing U,V,T,Z,Ps
      !                                  as function of (x,y,P) from the AMSU retrieval
      !     Nbnnyy_XmmDD_PACK.DAT - npk: Packed ASCII file containing U,V,T,Z,Ps 
      !                                  as function of (x,y,P) from NCEP fields 
      !                                  interpolated to the AMSU analysis grid
      !     All of the output file names can be replaced by generic file
      !     names by setting the parameter igenfn=1. The files will be of the 
      !     for NOAAss.olg, .loc, .rza, etc according the to above list.
      !
      !     Note: b  = basin ID (A=Atlantic, E=East Pacific, W=West Pacific)
      !           nn = ATCF storm number
      !           yy = year
      !           mm = month
      !           dd = day
      !           tt = time in UTC of ATCF record used for storm center
      !           ss = satellite number (15=NOAA15, etc)
      !
      !           X  = X if 00 or 12 UTC analysis
      !              = Y if 06 or 18 UTC analysis
      !           DD = dd    if 00 or 12 UTC analysis
      !              = dd+50 if 06 or 18 UTC analysis
      !
      ! Specify the lon/lat dimensions of the AMSU analysis domain

        ! Source modules
        use dims        ! Dimension Declaration
        use cons        ! Constant Declaration
        use cgrid       ! CGRID common variables
        use ncepfg      ! NCEPG common variables
        use ncepll      ! NCEPLL common variables
        use ncepp       ! NCEPPP common variables
        use sinfo1      ! SINFO1 common variables
        use sinfo2      ! SINFO2 common variables
        use sinfo3      ! SINFO3 common variables
        use io          ! Input/Output/Log variables
        use params      ! Parmeter Declaration

        use utils       ! Utility Subroutines
        use srsubs      ! Storm Retrieval Subroutines 
        use amsusubs    ! AMSU Subroutines
        use opsubs      ! Retrieval Subroutines 
        use besubs      ! Balance Equation Subroutines
        use vradsubs    ! Predictor Subroutines
        use bwcal       ! Balance Winds Subroutines

        implicit none

        ! 
        ! Variable Declaration
        !

        ! Arrays for AMSU variables on analysis grid
        real :: p(np)
        real :: z(nx,ny,np),t(nx,ny,np)
        real :: us(nx,ny),vs(nx,ny),ps(nx,ny),ts(nx,ny),rhos(nx,ny)
        real :: clwxy(nx,ny)

        ! Array for AMSU swath variables
        real :: tas(mxas,mpas),aslat(mxas),aslon(mxas)
        real :: dumasp(mxas,mpas),dumas(mxas)
        integer :: idumas(mxas)
        real :: clwas(mxas),tpwas(mxas)

        ! Total AMSU pressure levels 
        real :: pamsu(mpas) = (/ &
                0.1, 0.2, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 7.0, &
                10., 15., 20., 25., 30., 50., 60., 70., 85.,100., &
                115.,135.,150.,200.,250.,300.,350.,400.,430.,475., &
                500.,570.,620.,670.,700.,780.,850.,920.,950.,1000. &
                /)

        ! Arrays for AMSU output (at NCEP pressure levels)
        real :: ua(nx,ny,npn),va(nx,ny,npn),za(nx,ny,npn),ta(nx,ny,npn)

        ! Arrays for NCEP analysis variables     
        real :: un(nx,ny,npn),vn(nx,ny,npn),zn(nx,ny,npn),tn(nx,ny,npn)
        real :: psn(nx,ny),tsn(nx,ny)

        ! Array for NCEP 1000 mb T for clw temperature adjustment routine
        real :: tn1000(ixmax,iymax)

        ! Arrays for gradient wind calculations
        real :: rr(nr),trp(nr,np),psr(nr),tsr(nr)
        real :: zrp(nr,np),vrp(nr,np),zz(nz)
        real :: trz(nr,nz),prz(nr,nz),rhorz(nr,nz),vrz(nr,nz)
        real :: tarz(nr,nz)

        ! Temporary arrays for radial Barnes analysis
        integer,parameter :: mxb=nx*ny
        real :: temlat(mxb),temlon(mxb),ftem(mxb)

        ! Array for predictors for the stats file
        integer,parameter :: npred=18
        real :: preds(npred)

        ! Arrays for statistical wind radii prediction
        integer :: ir34(4),ir50(4),ir64(4)

        ! Work arrays
        real :: work1(nx,ny),work2(nx,ny),work3(nx,ny)
        real :: work4(nx,ny),work5(nx,ny),work6(nx,ny)
        real :: worky(ny)
        real :: works(mxas)

        ! Arrays for Cartesian grid calculations
        ! The calculation is performed (skipped) if ibwin=1 (0). 
        integer :: ibwin(npn) = (/ &
                                 1, 1, 1, 1, 1, 1, &
                                 1, 1, 1, 1, 1, 1  &
                                /)

        ! Character labels
        character(len=20) :: label
        character(len=23) :: labelr

        ! XXX Work Variables
        real :: adelt,ares,cofr,cofxy,efldr,rinfr,efldxy,rinfxy
        real :: dxk,dyk,rad,radsts,swlat,swlon
        real :: w0,wm

        integer :: idelt,idir,ispeed,inhce,iyr2
        integer :: jlday,jtimeh,jtimem

        integer :: nxas
        integer :: islat,islon

        real :: rlonl,rlatb
        real :: reflat,reflon

        integer :: idayx,iutcx,nyrt,nyr,nmon,nday,ntime
        real :: dayx,utcx
        real :: z1,z2,t1,t2,p1,pstem,tstem

        real :: amkl

        integer :: ispec,ibuf,jbuf,inon,im,jm
        real :: pmaxl,pminxy,rlatpm,rlatpma,rlonpm,rlonpma,wt,wtsum

        real :: pmin,delp,delp3,tmax,ztmax,thlm,vmx0,vmx3
        real :: rmx0,rmx3,dvmax0,dvmax3
        integer :: isz0,isz3

        real :: v15,v25,v35,r035,r025,r015,r335,r325,r315
        real :: rthi,rtho,vbi0,vbi3,vbi5,vbo0,vbo3,vbo5,cbi,cbo
        real :: clwavg,cnt,radmin,radat,cthresh,pcount,pthresh,pcrat

        real :: ppmin,pvmx,rmx,vmaxop
        integer :: ippmin,ipvmax,istnum,irmp,isdist,ias

        integer :: istat,ierr,itest,ios
        integer :: i,j,k,m,n,ii,jj,kk,ikn

        ! Open log file
        open(unit=lulog,file=fnlog,form='formatted', status='unknown')

        !
        ! **** Begin Storm/AMSU Data Input File Read and Related Processing ****
        ! 

        ! Read Configuration File
        call read_config()
        call read_params_config()

        ! Open the storm parameter and temperature retrieval input file
        fntemp=fntinp
        open(unit=lutinp,file=fntinp,form='formatted',status='old')

        ! Read the storm/retrieval file
        call srread(lutinp,jyr,jlday,jtime,swlat,swlon, &
                    iyr,imon,iday,itime,slat00,slon00,slat12,slon12, &
                    idir,ispeed,ivmax,atcfid,sname, &
                    nxas,aslat,aslon,tas,clwas,tpwas, &
                    coord,times,istat,pamsu)
        close(lutinp)
     
        if (istat .ne. 0) call open_error 

        ! J. Knaff -- DATELINE ISSUE resolved 2/20/2006
        ! This fix allows for the use of data across the dateline when storm 
        ! is within 10 degrees of the dateline by using a 0 to 360 longitude
        ! convention in that region.
        if (abs(slon00).gt.170.0 .or. abs(slon12).gt.170.0) then
          if(slon00 .lt. 0.0 ) slon00 = slon00 + 360.0
          if(slon12 .lt. 0.0 ) slon12 = slon12 + 360.0
          if(swlon  .lt. 0.0 ) swlon  = swlon  + 360.0
          do ikn=1,nxas
            if(aslon(ikn) .lt. 0.0) aslon(ikn) = aslon(ikn) + 360.0
          end do
        endif
      
        call ucase(atcfid,6)
        cbasin=atcfid(1:2)
        iyr2 = iyr - 100*(iyr/100)

        call jdayi(jlday,jyr,jmon,jday)

        ! Extract AMSU pressures for calculations and convert to Pa
        do k=1,np
          p(k) = 100.0*pamsu(npst+k-1)
        end do

        ! Estimate storm center at AMSU swath center time
        jtimeh = jtime/10000
        jtimem = (jtime - 10000*jtimeh)/100
        jtime4 = 100*jtimeh + jtimem

        call tdiff(jyr,jmon,jday,jtimeh,iyr,imon,iday,itime,idelt)
        adelt = float(idelt) + float(jtimem)/60.0
        itime4 = 100*itime

        if (abs(slat12) .gt. 0.0 .and.  &
            abs(slon12) .gt. 0.0) then
          w0 = 1.0 + adelt/12
          wm = -adelt/12
        else
          w0 = 1.0
          wm = 0.0
        endif

        sslat = w0*slat00 + wm*slat12
        sslon = w0*slon00 + wm*slon12

        ! Calculate distance from storm to swath center
        call distk(swlon,swlat,sslon,sslat,dxk,dyk,rad)
        radsts=rad

        ! Estimate AMSU resolution
        call amsures(rad,ares)

        ! Adjust e-folding radii based upon horizontal resolution of the data
        if (iefa .eq. 1 .and. ares .gt. 0.0) then
          cofr  = 2.0*sqrt(alog(1./tdxfr))/pi
          cofxy = 2.0*sqrt(alog(1./tdxfxy))/pi
          efldxy = float(nint(cofxy*ares))
          efldr  = float(nint(cofr *ares))

          if (efldxy .lt.  35.0) efldxy = 35.0
          if (efldxy .gt. 150.0) efldxy = 150.0

          if (efldr  .lt.  35.0) efldr  = 35.0
          if (efldr  .gt. 150.0) efldr  = 150.0

          rinfxy = 5.0*efldxy
          rinfr  = 5.0*efldr
          if (rinfxy .lt. 400.0) rinfxy = 400.0
          if (rinfr  .lt. 400.0) rinfr  = 400.0 
        else
          ! Defined in params file
          efldxy=efldxyDefault
          rinfxy=rinfxyDefault
          efldr=efldrDefault
          rinfr=rinfrDefault
        endif

        ! Write storm parameter info to log file
        write(lulog,200) atcfid,sname,iyr,imon,iday,itime, &
                        slat00,slon00,ivmax,slat12,slon12
        200 format(/,' Start program oparet. ', &
                   /,' ATCF INPUT: ',a6,1x,a9,1x,i4.4,1x,i2.2,i2.2,1x, &
                   i2.2,' UTC', &
                   /,' Storm lat/lon (t=  0 hr): ',f5.1,1x,f6.1,1x, &
                   ' Vmax: ',i3, &
                   /,' Storm lat/lon (t=-12 hr): ',f5.1,1x,f6.1)

        write(lulog,202) np,instr,p(1)/100.,p(np)/100.
        202 format(/,i2,a4,' levels from ',f5.0,' to ',f5.0, &
                   ' included')

        write(lulog,204) efldxy,rinfxy
        204 format(/, &
            ' Cartesian Barnes analysis with e-fold, inf. radii: ', &
            f5.0,1x,f5.0)

        if (iaxsym .eq. 1) then
          write(lulog,206) efldr,rinfr,exfac
          206 format( &
              ' Radial    Barnes analysis with e-fold, inf. radii: ', &
              f5.0,1x,f5.0,' exfac=',f4.1)
        endif

        if (itcor .gt. 0) then
          write(lulog,207) itcor,p(ncsm2)/100.,p(ncem2)/100., &
                                 p(ncsm3)/100.,p(ncem3)/100., &
                                 p(ncsm5)/100.,p(ncem5)/100., &
                                 clwth1,clwth2
          207 format(/,' Cold anomalies adjusted using method ',i2, &
                     /,' pm2= ',f5.0,' to ',f5.0, &
                     /,' pm3= ',f5.0,' to ',f5.0, &
                     /,' pm5= ',f5.0,' to ',f5.0, &
                     /,' clwth1=',f5.2,' clwth2=',f5.2) 
        else
          write(lulog,*) ' itcor=0, no correction applied'
        endif

        write(lulog,210) instr,swlat,swlon,jyr,jmon,jday,jtime
        210 format(/,a4,' data swath center: ',f6.2,1x,f7.2,' at ', &
                   i4,1x,i2.2,i2.2,1x,i6)

        write(lulog,212) adelt,sslat,sslon,rad,ares,instr,nxas,csatid
        212 format('Swath data is ',f5.1,' hr from analysis time',/, &
            'Storm center at swath time:',f6.2,1x,f7.2,/, &
            'Distance (km) from storm to swath center: ',f6.0,/, &
            'Approximate data spacing at storm center: ',f6.0,/, &
             a4,' data points read from input,     n=: ',i5,/, &
            'Data from ',a6)

        ! Check to make sure storm is not too close to the swath edge
        inhce = 0
        if (radsts .gt. threshr) then
          write(lulog,905) radsts,threshr
          905 format(/,' Storm too far from swath center, r (km)=',&
                    f6.0, ' max allowable r (km) = ',f5.0)
          inhce = 1

          if (ifixall .eq. 1) go to 5000
          write(*,*) 'INFO: storm too far from swath center'
          ierr=102
          call exit(ierr)
        endif

        ! 
        ! **** End Storm/AMSU Data Input File Read and Related Processing ****
        !
      
        !
        ! **** Begin Calculation of Analysis Grid Parameters ****
        !
        ! Center the domain on the estimated storm position
        ! at the swath time

        ! Calculate lat and lon in deg and radians, and related variables
        islat = nint(sslat)
        islon = nint(sslon)
        rlonl = float(islon) - dlon*float(nx/2)
        rlatb = float(islat) - dlat*float(ny/2)

        ! Initialize NCEPLL module variables
        call init_ncepll(rlonl,rlatb)

        ! Calcuate Cartesian grid variables
        reflat = 0.5*(rlatd(1)+rlatd(ny))
        reflon = 0.5*(rlond(1)+rlond(nx))

        ! Initialize CGRID module variables
        call init_cgrid(reflon,reflat)

        ! Calculate radial and height grids
        do i=1,nr
          rr(i) = dr*float(i-1)
        end do 

        do m=1,nz
          zz(m) = dz*float(m-1)
        end do

        ! Write grid info to the log file
        call gridwrit(rr,zz)

        ! **** End Calculation of Analysis Grid Parameters ****

        !
        ! **** Begin NCEP Input File Read and Related Processing ****
        !

        ! Open NCEP data input file
        fntemp=fnninp
        open(unit=luninp,file=fnninp,form='formatted',status='old')

        ! Get NCEP analysis variables
        call ncepget(pn,un,vn,zn,tn,tn1000,rlond,rlatd,nx,ny,npn, &
                     ixmax,iymax,dlon,dlat,dayx,utcx,luninp,ierr)
        close(luninp)

        if (ierr .ne. 0) then
          write(lulog,910) ierr
          910 format( &
          /,' Halting due to error in routine ncepget, ierr=',i2)
          call open_error 
        endif

        if (ismooth .gt. 0) then
          do n=1,ismooth
            do k=1,npn
              call smooth(un(1,1,k),work1,nx,ny,nx,ny,0)
              call smooth(vn(1,1,k),work1,nx,ny,nx,ny,0)
              call smooth(tn(1,1,k),work1,nx,ny,nx,ny,0)
              call smooth(zn(1,1,k),work1,nx,ny,nx,ny,0)
            enddo
          enddo
        endif
        
        ! Convert NCEP date/time varibles to integers
        idayx = nint(dayx)
        iutcx = nint(utcx)

        nyrt  = idayx/10000
        nmon  = (idayx - 10000*nyrt)/100
        nday  = (idayx - 10000*nyrt - 100*nmon)
        ntime = iutcx/100

        if (nyrt .lt. 50) then
          nyr = nyrt + 2000
        else
          nyr = nyrt + 1900
        endif

        ! Calculate time difference between NCEP analysis to AMSU swath data
        call tdiff(jyr,jmon,jday,jtimeh,nyr,nmon,nday,ntime,idelt)
        adelt = float(idelt) + float(jtimem)/60.0

        write(lulog,216) fnninp
        216 format(/,'NCEP data input file:    ',a25)

        write(lulog,236) nyr,nmon,nday,ntime,adelt,instr
        236 format(/,'NCEP analysis date/time: ',i4,1x,2(i2.2),1x,i2,/,&
            'NCEP analysis is ',f5.1,' hrs from ',a4,' swath')

        if (abs(adelt) .gt. 36.0) then
          write(lulog,*) ' NCEP analysis is too old'
          write(*,*) 'INFO: NCEP analysis is too old'
          ierr=102
          call exit(ierr)
        endif

        ! Scale Pressure levels (put in ncepp module?)
        do k=1,npn
          pn(k) = 100.0*pn(k)
        end do

        ! Use data at lowest two NCEP levels to get surface temperature
        ! and pressure
        do j=1,ny
          do i=1,nx
            z1 = zn(i,j,npn)
            z2 = zn(i,j,npn-1)
            t1 = tn(i,j,npn)
            t2 = tn(i,j,npn-1)
            p1 = pn(npn)
            call pstcal(z1,z2,t1,t2,p1,pstem,tstem)

            psn(i,j) = pstem
            tsn(i,j) = tstem
          end do
        end do

        ! **** End NCEP Input File Read and Related Processing ****

        ! ***** Begin Output File Naming/Opening  ****

        call init_output 

        ! ++ Write NCEP variables at AMSU analysis grid points to packed output file
        if (ipnpk .eq. 1) then
          call wpof(un,vn,tn,zn,psn,iyr2,imon,iday,itime,lunpk)
          close(lunpk)
        endif

        ! ++ Write amsu data locations to a file
        if (iploc .eq. 1) then
          call wloc(sslat,sslon,aslat,aslon,luloc,nxas, &
                    jyr,jmon,jday,jtimeh,jtimem,atcfid,sname)
          close(luloc)
        endif

        ! **** End Output File Naming/Opening  ****

        !
        ! **** Begin Processing of AMSU temperature/CLW data ****
        !

        ! Eliminate data outside analysis domain
        if (idex .eq. 1) then
          call delim(tas,clwas,tpwas,aslat,aslon, &
                     rlatd(1),rlatd(ny),rlatbf, &
                     rlond(1),rlond(nx),rlonbf, &
                     dumas,dumasp,idumas, &
                     mxas,mpas,nxas,np,npst)
        endif

        ! ++ Quality control the AMSU temperatures
        call qualcon(tas,clwas,tpwas,aslat,aslon,pamsu,dumas, &
                     dumasp,idumas,mxas,mpas,nxas,np,npst)

        ! Write AMSU swath info to the log file
        write(lulog,238) instr,nxas,aslat(1),aslon(1), &
                         aslat(nxas),aslon(nxas)
        238 format( &
            /,' Final ',a4,' swath data includes ',i4,' points', &
            /,' First lat/lon: ',f5.1,1x,f6.1,/, &
              ' Last  lat/lon: ',f5.1,1x,f6.1,/)

        ! ++ Correct swath temperatures for cold anomalies if necessary
        if (itcor .eq. 3 .or. itcor .eq. 4 .or. &
            itcor .eq. 6 .or. itcor .eq. 7) then
          write(lulog,*) ' itcor=3 correction applied'
          do k=ncs3,nce3
            call tcorsr(mxas,nxas,tas(1,k),clwas)
          end do
        endif

        if (itcor .eq. 5 .or. itcor .eq. 6 .or. itcor .eq. 7) then
           write(lulog,*) ' itcor=5 correction applied'
           call tcorsv(mxas,nxas,mpas,tas,tn1000,pamsu, &
                       clwas,aslon,aslat,clwth1,clwth2,ncs5,nce5)
        endif

        if (itcor .eq. 2 .or. itcor .eq. 4 .or. itcor .eq. 7) then
          write(lulog,*) ' itcor=2 correction applied'
          do k=ncs2,nce2
            call tcors(mxas,nxas,tas(1,k),clwas,aslon,aslat, &
                       sslon,sslat,works,dt,dtred,tcrmax)
          end do
        end if

        if (itcor .eq. 8 .or. itcor .eq. 10) then 
          write(lulog,*) ' itcor=8 correction applied'
          do k=ncsm8,ncem8
            amkl=((amkx*xlambda)/((pamsu(ncem8)-pamsu(ncsm8))**2))* &
                 (pamsu(k)-pamsu(ncsm8))**2
            write(lulog,460) pamsu(k),amkl
            460 format(' itcor=8 correction for p=',f6.1,1x,' amkl=',e11.4)
            call tcorclw(mxas,nxas,amkl,tas(1,k),clwas,k)
          end do
        endif

        ! ++ Interpolate temperatures to analysis grid
        do k=1,np
          do i=1,nxas
            dumas(i) = tas(i,npst+k-1)
          end do

          call barxy(dumas,aslat,aslon,nxas,efldxy,rinfxy,ispf, &
                     rlatd,rlond,t(1,1,k),nx,ny,ierr)
          if (ierr .ne. 0) then
            write(lulog,940)
            940 format(/,'Error in barxy')
            write(*,*) 'INFO: problem in barxy'
            ierr=103
            call exit(ierr)
          endif
        end do

        ! Interpolate cloud liquid water to analysis grid
        do i=1,nxas
          if (clwas(i) .gt. 0.0) then
            dumas(i) = clwas(i)
          else
            dumas(i) = 0.0
          endif
        enddo

        call barxy(dumas,aslat,aslon,nxas,efldxy,rinfxy,ispf, &
                   rlatd,rlond,clwxy,nx,ny,ierr)
        if (ierr .ne. 0) then
          write(lulog,940)
          write(*,*) 'INFO: problem in barxy'
          ierr=103
          call exit(ierr)
        endif

        ! Correct analysis temperatures for cold anomalies if necessary
        if (itcor .eq. 1) then
          write(lulog,*) ' itcor=1 correction applied'
          do k=ncsm2,ncem2
            call tcor(nx,ny,t(1,1,k),dt,dtred)
          end do
        endif

        ! If ice correction doesn't converge, the corresponding pressure 
        ! level, storm, mon, day, and time will be output to the log file
        if (itcor .eq. 9 .or. itcor .eq. 10) then
          write(lulog,*) ' itcor=9 correction applied'
          do k=ncsm9,ncem9
            write(lulog,470) p(k)/100.0
            470 format(' itcor=9 correction for p=',f6.1)
            call tcorice(k,nx,ny,t(1,1,k),clwxy, &
                         work1,work2,work3, &
                         atcfid,imon,iday,itime)

          end do
          write(lulog,*) 'Finished ice corrections.'
        endif

        itest=0
        if (itest .eq. 1) then
          write (lulog,*) ' itest=1, so program is stopping!'
          write(*,*) 'INFO: itest=1'
          ierr=110
          call exit(ierr)
        endif

        ! **** End Processing of AMSU temperature/CLW data ****

        ! **** Begin hydrostatic integration and wind retrieval on 3-D grid ****

        ! Specify surface temperature and pressure of AMSU analysis 
        ! from NCEP analysis
        ! (All pressures except lateral boundary values will be recomputed)
        do j=1,ny
          do i=1,nx
            ts(i,j) = tsn(i,j)
            ps(i,j) = psn(i,j)
          end do
        end do

        ! Check for special case with top AMSU and NCEP levels of 100 mb.
        ! In this case, the NCEP height field can be used as an upper
        ! boundary condition.
        ispec=0
        if (p(1) .eq. 10000.0 .and. pn(1) .eq. 10000.0) then
          ispec=1
        endif

        if (intop .eq. 1 .and. ispec .ne. 1) then
          write(lulog,920) instr
          920 format(/,a6,' domain top must be at 100 mb for intop=1')
          write(*,*) 'INFO: ',instr,' domain top must be at 100 mb for &
          intop=1'
          ierr=102
          call exit(ierr)
        endif

        ! Use NCEP height field for upper boundary condition
        if (intop .eq. 1) then
          do j=1,ny
            do i=1,nx
              z(i,j,1) = zn(i,j,1)
            end do
          end do

          ! Calculate height field at AMSU levels and surface pressure
          ! for special case
          call zalcals(t,ts,ps,p,z)
        else
          ! Calculate height field at AMSU levels and surface pressure
          call zalcal(t,ts,ps,p,z)
        endif

        ! Calculate surface density
        do j=1,ny
          do i=1,nx
            rhos(i,j) = ps(i,j)/(rd*ts(i,j))
          end do
        end do

        ! Extract AMSU t,z at NCEP levels for output
        call altonl(z,t,p,za,ta,pn,ts,ps,nx,ny,np,npn)

        ! Make first guess for AMSU retrieved winds at NCEP pressure levels.
        ! Use the lowest available NCEP pressure level for the surface winds.
        do k=1,npn
          do j=1,ny
            do i=1,nx
              ua(i,j,k) = un(i,j,k)
              va(i,j,k) = vn(i,j,k)
            end do
          end do
        end do

        do j=1,ny
          do i=1,nx
            us(i,j) = un(i,j,npn)
            vs(i,j) = vn(i,j,npn)
          end do
        end do

        ! Calculate winds from heights using linear balance equation
        if (iwinda .eq. 1 .or. (iwinda .gt. 1 .and. ifgnbe .eq. 1)) then
          do k=1,npn
            do j=1,ny
              do i=1,nx
                work1(i,j) = g*za(i,j,k)
                work2(i,j) =   ua(i,j,k)
                work3(i,j) =   va(i,j,k)
              enddo
            enddo

            if (ibwin(k) .eq. 1) then
              call lbe(work1,work2,work3,nx,ny)
            endif

            if (iwinda .eq. 1) then
              do j=1,ny
                do i=1,nx
                  ua(i,j,k) = work2(i,j)
                  va(i,j,k) = work3(i,j)
                enddo
              enddo
            else
              do j=2,ny-1
                do i=2,nx-1
                  ua(i,j,k) = work2(i,j)
                  va(i,j,k) = work3(i,j)
                enddo
              enddo
            endif

          enddo
        endif

        if (iwinda .gt. 1) then

           do k=1,npn

             if (ibwin(k) .ne. 1) then 
                exit
             endif

               do j=1,ny
                 do i=1,nx
                   work1(i,j) = g*za(i,j,k)
                   work2(i,j) =   ua(i,j,k)
                   work3(i,j) =   va(i,j,k)
                 end do
               end do

            ! Calculate zero curvature first guess
            if (ifgnbe .eq. 2) then
              do j=1,ny
                 worky(j) = 0.0
              enddo

              do j=1,ny
                do i=1,nx
                  work4(i,j) = 0.0
                  work5(i,j) = ua(i,j,k)
                  work6(i,j) = va(i,j,k)
                enddo
              enddo

              call zinter(work5,0.0,nx,ny)
              call zinter(work6,0.0,nx,ny)
              call psonxy(work4,worky,work2,work5,ierr)
              call psonxy(work4,worky,work3,work6,ierr)

            endif

            if (iwinda .eq. 2) then
              call nbei(work1,work2,work3,nx,ny)
            elseif (iwinda .eq. 3) then
              write(6,*) 'call nbev for p=',pn(k)
              inon=1
              ! call nbei(work1,work2,work3,nx,ny)
              call nbev(work1,work2,work3,inon,nx,ny)
            elseif (iwinda .eq. 4) then
              write(6,*) 'call nbevs for p=',pn(k)
              inon=1
              call nbevs(work1,work2,work3,inon,nx,ny)
            endif

            do j=1,ny
              do i=1,nx
                ua(i,j,k) = work2(i,j)
                va(i,j,k) = work3(i,j)
              end do
            end do

          end do

        endif

        itest=0
        if (itest .eq. 1) then
          write(lulog,879) 
          879 format(/,' u,v near domain center')
          do j=23,19,-1
            write(lulog,880) (ua(i,j,8),i=19,23),(va(i,j,8),i=19,23)
            880 format(1x,5(f7.1),2x,5(f7.1))
          enddo
        endif

        ! Calculate spatial averages of ta, tn
        label = instr//' T, NCEP T'
        call spata(ta,tn,pn,nx,ny,npn,ibrem,label)

        ! Calculate spatial averages of za, zn
        label = instr//' Z, NCEP Z'
        call spata(za,zn,pn,nx,ny,npn,ibrem,label)

        ! Calculate spatial averages of ps,psn
        label = instr//' PS, NCEP PS'
        call spata1(ps,psn,nx,ny,ibrem,label)

        ! Calculate spatial averages of tas
        label = instr//' swath T'
        call swata(tas,pamsu,mxas,mpas,nxas,label)

        ! Write AMSU fields to packed file
        if (ipapk .eq. 1) then
          call wpof(ua,va,ta,za,ps,iyr2,imon,iday,itime,luapk)
          close(luapk)
        endif

        if (ipxya .eq. 1) then
          ! Write fields (u,v,t,z,ps,clw) to ASCII file
          ! Main header
          ! write(luxya,176) atcfid,slat00,slon00,iyr,imon,iday, &
          !                  itime,sname, &
          !                  sslat,sslon,jyr,jmon,jday,jtimeh,jtimem
          ! 176 format(' Storm ID: ',a6,f6.2,1x,f7.2, &
          !            ' at ',i4,1x,2(i2.2),1x,i2.2,1x,a10,/, &
          !            ' Swath information:      ',6x,f6.2,1x,f7.2, &
          ! ' at ',i4,1x,2(i2.2),1x,i2.2,i2.2)
          write(luxya,176) coord
          write(luxya,176) times
          176 format(a90)

          ! Write pressure level to file
          do i=1,npn
            write(luxya,78) pn(i),nx,ny
            78 format('Pressure level (Pa)=',f8.1,1x, &
                      'nlat=',i2,1x,'nlon=',i2)

            ! Write header line to file
            write(luxya,79)
            79 format(4x,'Lat',5x,'Lon',11x,'U',11x,'V',11x,'T',11x,'Z')

            do j=1,ny
              do k=1,nx
                write(luxya,178) rlatd(j),rlond(k),ua(k,j,i),va(k,j,i),&
                                 ta(k,j,i),za(k,j,i)
                178 format(f7.2,1x,f7.2,4(1x,f11.2))
              end do
            end do
 
          end do

          ! Write information about surface variables
          write(luxya,80) ny,ny
          80 format('Surface pressure level',1x,'nlat=',i2,1x,&
                    'nlon=',i2)

          ! Write header line for surface pressure level
          write(luxya,81)
          81 format(4x,'Lat',5x,'Lon',11x,'U',11x,'V',11x,'T',11x,'P', &
                   10x,'CLW')

          do j=1,ny
            do k=1,nx
              write(luxya,180) rlatd(j),rlond(k),us(k,j),vs(k,j), &
                               ts(k,j),ps(k,j),clwxy(k,j)
              180 format(f7.2,1x,f7.2,5(1x,f11.2))
            end do
          end do

          close(luxya)

        endif

        ! Find location of minimum surface pressure in x,y analysis
        pminxy = 1.0e+10
        ibuf=10
        jbuf=10
        im = 0
        jm = 0
        do i=ibuf,nx-ibuf
          do j=jbuf,ny-jbuf
            if (ps(i,j) .lt. pminxy) then
              rlatpm = rlatd(j)
              rlonpm = rlond(i)
              im = i
              jm = j
              pminxy = ps(i,j)
            endif
          enddo
        enddo
        pminxy = pminxy/100.0

        ! Refine the min pressure location 
        pmaxl = -1.0e+10
        do i=-1,1
          do j=-1,1
            ii = im + i
            jj = jm + j
            if (ps(ii,jj) .gt. pmaxl) pmaxl = ps(ii,jj)
          enddo
        enddo

        wtsum = 0.0
        rlatpma = 0.0
        rlonpma = 0.0
        do i=-1,1
          do j=-1,1
            ii = im + i 
            jj = jm + j
            wt = (pmaxl-ps(ii,jj))
            wtsum = wtsum + wt
            rlatpma = rlatpma + wt*rlatd(jj)
            rlonpma = rlonpma + wt*rlond(ii)
          enddo
        enddo

        if (wtsum .gt. 0.0) then
          rlatpma = rlatpma/wtsum
          rlonpma = rlonpma/wtsum
        else
          rlatpma = 0.0
          rlonpma = 0.0
        endif
 
        ! **** End hydrostatic integration and wind retrieval on 3-D grid ****

        if (iaxsym .ne. 1) go to 5000

        ! **** Begin axisymmetric retrievals ****

        ! sslat = rlatpma
        ! sslon = rlonpma

        ! Interpolate AMSU swath temperatures to radial grid
        if (iradxy .eq. 1) then
          kk=0
          do j=1,ny
            do i=1,nx
              kk = kk + 1
              temlat(kk) = rlatd(j)
              temlon(kk) = rlond(i)
            end do
          end do

          do k=1,np
            kk=0
            do j=1,ny
              do i=1,nx
                kk = kk + 1
                ftem(kk) = t(i,j,k)
              end do
            end do
            call barr(ftem,temlat,temlon,kk,efldr,exfac,rinfr, &
                      sslat,sslon,trp(1,k),rr,nr,ierr)
          end do

        else
          do k=1,np
            do i=1,nxas
              dumas(i) = tas(i,npst+k-1)
            end do

            call barr(dumas,aslat,aslon,nxas,efldr,exfac,rinfr, &
                      sslat,sslon,trp(1,k),rr,nr,ierr)

            if (ierr .ne. 0) then
              write(lulog,950)
              950 format(/,'Error in barr')
              write(*,*) 'INFO: problem in barr'
              ierr=103
              call exit(ierr)
            endif

          end do

        endif

        ! Interpolate surface temperature and pressure to radial grid 
        ! for first guess
        kk=0
        do j=1,ny
          do i=1,nx
            kk = kk + 1
            ftem(kk)   = tsn(i,j)
            temlat(kk) = rlatd(j)
            temlon(kk) = rlond(i)
          end do
        end do

        call barr(ftem,temlat,temlon,kk,efldr,exfac,rinfr, &
                  sslat,sslon,tsr,rr,nr,ierr)

        if (ierr .ne. 0) then
          write(lulog,950)
          write(*,*) 'INFO: problem in barr'
          ierr=103
          call exit(ierr)
        endif

        kk=0
        do j=1,ny
          do i=1,nx
            kk = kk + 1
            ftem(kk)   = psn(i,j)
            temlat(kk) = rlatd(j)
            temlon(kk) = rlond(i)
          end do
        end do

        call barr(ftem,temlat,temlon,kk,efldr,exfac,rinfr, &
                  sslat,sslon,psr,rr,nr,ierr)

        if (ierr .ne. 0) then
          write(lulog,950)
          write(lulog,950)
          write(*,*) 'INFO: problem in barr'
          ierr=103
        endif

        ! Calculate gradient wind
        call bwgcal(rr,p,trp,psr,tsr,dz,nr,np,nz,sslat, &
                    zrp,vrp,zz,trz,prz,rhorz,vrz)

        ! Print gradient winds to log file
        write(lulog,284)
        284 format(/,'Tangential wind profiles')

        do k=1,nr
          write(lulog,285) rr(k)/1000.,prz(k,1)/100., &
                           zz( 1)/1000.,vrz(k, 1), &
                           zz( 6)/1000.,vrz(k, 6), &
                           zz(11)/1000.,vrz(k,11)  
          285 format(' r=',f5.0,1x,' ps=',f6.1,3('  z=',f5.1,' &
                     v=',f5.1))
        end do

        ! Calculate temperature anomalies
        do k=1,nz
          do i=1,nr
            tarz(i,k) = trz(i,k)-trz(nr,k)
          enddo
        enddo

        ! Write all r,z variables to a file 
        if (iprza .eq. 1 .and. iaxsym .eq. 1) then
          labelr      = '            '
          write(labelr,701) atcfid,imon,iday,itime, &
                            jmon,jday,jtimeh,jtimem
          701 format(a6,1x,3(i2.2),1x,4(i2.2))
          call tcrwrit(prz,rhorz,trz,vrz,nr,nz,sslat,sslon,lurza, &
                       labelr,coord,times,rr(1),rr(nr),zz(1),zz(nz)) 
        endif

        ! Write basic statistics of the analysis to a file
        if (ipsta .eq. 1 .and. iaxsym .eq. 1) then
          write(lusta,700) atcfid,slat00,slon00,iyr,imon,iday, &
                           itime,sname, &
                           sslat,sslon,jyr,jmon,jday,jtimeh,jtimem
          700 format(' Analysis statistics for ',a6,f6.2,1x,f7.2, &
                     ' at ',i4,1x,2(i2.2),1x,i2.2,1x,a10,/, &
                     ' Swath information:      ',6x,f6.2,1x,f7.2, &
                     ' at ',i4,1x,2(i2.2),1x,i2.2,i2.2)

          ! Evaluate parameters from analyses

          ! Find minimum pressure and pressure drop
          pmin =  prz(1,1)/100.0
          delp =  (prz(nr,1)-prz(1,1))/100.0

          ! Find pressure drop at 3 km
          delp3 = (prz(nr,4)-prz(1,4))/100.0

          ! Find max temperature anomalies at 3 km or above
          tmax = -999.0
          ztmax= -99.0
          do k=4,nz
            if (tarz(1,k) .gt. tmax) then
              tmax = tarz(1,k)
              ztmax= zz(k)/1000.0
            endif
          enddo

          ! Find max winds (first local maximum starting at r=0)
          thlm = 5.0
          vmx0 = -99.
          vmx3 = -99.
          rmx0 = -99.
          rmx3 = -99.

          isz0 = 1
          isz3 = 1
          do i=1,nr-1
            dvmax0 = vmx0-vrz(i,1)
            if (isz0 .eq. 1) then
              if (dvmax0 .gt. thlm) isz0=0
            endif

            if (vrz(i,1) .gt. vmx0 .and. isz0 .eq. 1) then
              vmx0 = vrz(i,1)
              rmx0 = rr(i)/1000.0
            endif

            dvmax3 = vmx0-vrz(i,4)
            if (isz3 .eq. 1) then
                if (dvmax3 .gt. thlm) isz3=0
            endif

            if (vrz(i,4) .gt. vmx3 .and. isz3 .eq. 1) then
              vmx3 = vrz(i,4)
              rmx3 = rr(i)/1000.0
            endif

          enddo
          vmx0 = 1.944*vmx0
          vmx3 = 1.944*vmx3

          ! Find wind radii
          v15 = 15.0/1.944
          v25 = 25.0/1.944
          v35 = 35.0/1.944

          r035 = -99.0
          r025 = -99.0
          r015 = -99.0

          r335 = -99.0
          r325 = -99.0
          r315 = -99.0

          if (vmx0 .gt. 35.0) then
            rmx = 1000.0*rmx0
            do i=1,nr
              if (vrz(i,1) .lt. v35 .and. rr(i) .gt. rmx) then
                r035 = rr(i)/1000.0
                exit
              endif
            enddo
          endif

          if (vmx0 .gt. 25.0) then
            rmx = 1000.0*rmx0
            do i=1,nr
              if (vrz(i,1) .lt. v25 .and. rr(i) .gt. rmx) then
                r025 = rr(i)/1000.0
                exit
              endif
            enddo
          endif

          if (vmx0 .gt. 15.0) then
            rmx = 1000.0*rmx0
            do i=1,nr
              if (vrz(i,1) .lt. v15 .and. rr(i) .gt. rmx) then
                r015 = rr(i)/1000.0
                exit
              endif
            enddo
            r015 = rr(nr)/1000.0
          endif

          if (vmx3 .gt. 35.0) then
            rmx = 1000.0*rmx3
            do i=1,nr
              if (vrz(i,4) .lt. v35 .and. rr(i) .gt. rmx) then
                r335 = rr(i)/1000.0
                exit
              endif
            enddo
          endif

          if (vmx3 .gt. 25.0) then
            rmx = 1000.0*rmx0
            do i=1,nr
              if (vrz(i,4) .lt. v25 .and. rr(i) .gt. rmx) then
                r325 = rr(i)/1000.0
                exit
              endif
            enddo
          endif

          if (vmx3 .gt. 15.0) then
            rmx = 1000.0*rmx0
            do i=1,nr
              if (vrz(i,4) .lt. v15 .and. rr(i) .gt. rmx) then
                r315 = rr(i)/1000.0
                exit
              endif
            enddo
            r315 = rr(nr)/1000.0
          endif

          ! Find mean tangential wind at 0,3 and 5 km 
          ! for r=0 to 250 km and r=250 to 500 km
          rthi = 250.0e+3
          rtho = 500.0e+3

          vbi0 = 0.0
          vbi3 = 0.0
          vbi5 = 0.0
          vbo0 = 0.0
          vbo3 = 0.0
          vbo5 = 0.0

          cbi = 0.0
          cbo = 0.0

          do i=1,nr
            if (rr(i) .le. rthi) then
              cbi  = cbi  + 1.0
              vbi0 = vbi0 + vrz(i,1)
              vbi3 = vbi3 + vrz(i,4)
              vbi5 = vbi5 + vrz(i,6)
            endif

            if (rr(i) .gt. rthi .and. rr(i) .le. rtho) then
              cbo  = cbo  + 1.0
              vbo0 = vbo0 + vrz(i,1)
              vbo3 = vbo3 + vrz(i,4)
              vbo5 = vbo5 + vrz(i,6)
            endif
          enddo

          vbi0 = vbi0/cbi
          vbi3 = vbi3/cbi
          vbi5 = vbi5/cbi

          vbo0 = vbo0/cbo
          vbo3 = vbo3/cbo
          vbo5 = vbo5/cbo

          ! Average cloud liquid water near storm center
          ! and find fractional area within radat km covered
          ! by cthresh mm of CLW
          clwavg = 0.0
          cnt    = 0.0
          radmin = 100.0

          radat = 300.0
          cthresh = 0.5
          pcount  = 0.0
          pthresh = 0.0

          do j=1,ny
            do i=1,nx
              call distk(sslon,sslat,rlond(i),rlatd(j),dx,dy,rad)

              if (rad .lt. radmin .and. clwxy(i,j) .ge. 0.0) then
                clwavg = clwavg + clwxy(i,j)
                cnt    = cnt + 1.0
              endif

              if (rad .lt. radat) then
                pcount = pcount + 1.0
                if (clwxy(i,j) .gt. cthresh) then
                  pthresh = pthresh + 1.0
                endif
              endif
            enddo
          enddo

          if (cnt .gt. 0.0) then
            clwavg = clwavg/cnt
          else
            clwavg = 0.0
          endif

          if (pcount .gt. 0.0) then
            pcrat = 100.0*(pthresh/pcount)
          else
            pcrat = 0.0
          endif

          write(lusta,710) pmin,delp,delp3,tmax,ztmax,ares, &
                           vmx0,rmx0,vmx3,rmx3
          710 format(' Min. Pressure=  ',f6.1,/, &
                     ' Sfc. P drop  =  ',f6.1,/, &
                     ' 3 km P drop  =  ',f6.1,/, &
                     ' Max T anomaly=  ',f6.1,/, &
                     ' z(max T)     =  ',f6.1,/, &
                     ' Swath spacing=  ',f6.1,/, &
                     ' Vmx(z=0)     =  ',f6.1,/, &
                     ' r(vmx0)      =  ',f6.1,/, &
                     ' Vmx(z=3)     =  ',f6.1,/, &
                     ' r(vmx3)      =  ',f6.1)

          write(lusta,720) vbi0,vbi3,vbi5,vbo0,vbo3,vbo5,clwavg, &
                           pcrat
          720 format(' vbi0         =  ',f6.1,/, &
                     ' vbi3         =  ',f6.1,/, &
                     ' vbi5         =  ',f6.1,/, &
                     ' vbo0         =  ',f6.1,/, &
                     ' vbo3         =  ',f6.1,/, &
                     ' vbo5         =  ',f6.1,/, &
                     ' Average CLW  =  ',f6.2,/, &
                     ' CLW percent  =  ',f6.2)

          write(lusta,730) pminxy,rlatpm,rlonpm,rlatpma,rlonpma
          730 format(' pminxy       =  ',f6.1,/, &
                     ' lat of pminxy=  ',f6.1,/, &
                     ' lon of pminxy=  ',f6.1,/, &
                     ' adj lat      = ', f7.2,/, &
                     ' adj lon      = ', f7.2)

          ! Estimate max wind and azimuthal mean wind radii 
          ! using statistical relationships
          ! Fill predictor array for new form of vradp routine
          if (ipsta .eq. 1) then
            preds( 1) = pmin
            preds( 2) = delp
            preds( 3) = delp3
            preds( 4) = tmax
            preds( 5) = ztmax
            preds( 6) = ares
            preds( 7) = vmx0
            preds( 8) = rmx0
            preds( 9) = vmx3
            preds(10) = rmx3
            preds(11) = vbi0
            preds(12) = vbi3
            preds(13) = vbi5
            preds(14) = vbo0
            preds(15) = vbo3
            preds(16) = vbo5
            preds(17) = clwavg
            preds(18) = pcrat

            spd  = float(ispeed)
            head = float(idir)
            vmaxop = float(ivmax)
            ias    = 0

            call vradp(preds,sslat,sslon,spd,head,vmaxop,ias, &
                       pvmx,ppmin,rmp,xp,pr34,pr50,pr64)


          ! Apply simple bias correction to account for using AMSU-based
          ! statistical relationships for ATMS data
          ! ** Added by A. Schumacher, 26 June 2014
          ! ** Needs to be replaced with new statistical relationship
          ! ** derived using ATMS data (current Cal/Val project)

          !  pvmx = 13.894*(EXP(0.0234*pvmx))
          !  ppmin = (1.3513*ppmin)-350.93
          !  pr34 = pr34/1.45
          !  pr50 = pr50/1.70
          !  pr64 = pr64/1.98


            write(lusta,735) pvmx,ppmin,instr, &
                             pr34(5),pr50(5),pr64(5), &
                             pr34(6),pr50(6),pr64(6), &
                             vmaxop,spd,head, &
                             rmp,xp,(pr34(kk),kk=1,4), &
                                    (pr50(kk),kk=1,4), &
                                    (pr64(kk),kk=1,4)
            735 format(/,' Statistical Intensity/Radii Estimates', &
            /,1x,f5.0,1x,f5.0,8x,a4,' Max wind (kt), Min P (hPa)  ', &
            /,1x,3(f5.0,1x),     ' Mean 34,50,64 kt wind radius (nm)', &
            /,1x,3(f5.0,1x),     ' Fit  34,50,64 kt wind radius (nm)', &
            /,1x,3(f5.0,1x),     ' ATCF max wind, speed, heading', &
            /,1x,f5.0,1x,f5.2,7x,' rm (nm) and x from vortex fit', &
            /,1x,4(f5.0,1x),' 34 kt radii NE,SE,SW,NW', &
            /,1x,4(f5.0,1x),' 50 kt radii NE,SE,SW,NW', &
            /,1x,4(f5.0,1x),' 64 kt radii NE,SE,SW,NW') 

          endif

        endif

        5000 continue

        ! Write information to NHC fix file
        if (ipfix .eq. 1 .and. iaxsym .eq. 1) then
          ippmin = nint(ppmin)
          ipvmax = nint(pvmx)

          ! No analysis was performed, and fix file name 
          ! not created, so create it now
          if (ifixall .eq. 1 .and. inhce .ne. 0) then
            fnfix        = 'FX0000_000000.DAT'
            fnfix( 2: 2) = atcfid(1:1)
            fnfix( 3: 6) = atcfid(3:6)
            fnfix(18:19) = '99'
            write(fnfix(8:13),226) imon,iday,itime
            226 format(3(i2.2))

            if (igenfn .eq. 1) then
              fnfix='  '
              fnfix(1:6) = csatid
              fnfix(7:10) = '.FIX'
            endif
          endif

          fntemp=fnfix
          open(file=fnfix,unit=lufix,form='formatted',status='unknown',&
               iostat=ios)
          if(ios.ne.0) call open_error

          ! See DATELINE ISSUE. - Knaff 2/20/2006
          if(sslon  .gt. 180.0) sslon  = sslon  - 360.0
          if(slon00 .gt. 180.0) slon00 = slon00 - 360.0
          if(slon12 .gt. 180.0) slon12 = slon12 - 360.0

          call fixprt(lufix,threshr,radsts,inhce,ippmin,ipvmax)

          ! Open and write fix file in ATCF format
          if (inhce .eq. 0) then

            fntemp=fnafx
            open(file=fnafx,unit=luafx,form='formatted',&
                 status='unknown', iostat=ios)
            if(ios.ne.0) call open_error

            read(atcfid(3:4),295) istnum
            295 format(i2)
            irmp = nint(rmp)
            isdist = nint(rad)

            do i=1,4
              ir34(i) = nint(pr34(i))
              ir50(i) = nint(pr50(i))
              ir64(i) = nint(pr64(i))
            enddo

            call AMSUfdeck(cbasin,istnum,csatid, &
                           jyr,jmon,jday,jtimeh,jtimem,sslat,sslon, &
                           ippmin,ipvmax,ir34,ir50,ir64, &
                           irmp,isdist,luafx)

          endif
        endif

        write(lulog,290)
        290 format(/,' Program oparet completed normally.')

      end program oparet

      ! Writes Grid Info
      subroutine gridwrit(rr,zz)
        use params
        use ncepll
        use cgrid
        use io
        use sinfo3

        real,intent(in) :: rr(nr),zz(nz)

        ! Write grid info to the log file
        write(lulog,230) rlond(1),rlond(nx),dlon, &
                         rlatd(1),rlatd(ny),dlat
        230 format(/, &
            ' Longitude domain: ',f6.1,1x,f6.1,'  dlon=',f5.2,/, &
            ' Latitude domain:  ',f6.1,1x,f6.1,'  dlat=',f5.2)

        if (iaxsym .eq. 1) then
          write(lulog,232) nr,dr/1000.0,rr(1)/1000.0,rr(nr)/1000.0
          232 format(i3,' radial grid points, dr=',f5.1,1x, &
              '  rmin=',f5.1,' rmax=',f6.1)
        endif

        write(lulog,234) nz,dz/1000.0,zz(1)/1000.0,zz(nz)/1000.0
        234 format(i3,' z grid points, dz=',f5.1,1x, &
            '  zmin=',f5.1,' zmax=',f6.1)

        write(lulog,239) x(1),x(nx),dx,y(1),y(ny),dy,f0,beta
        239 format( &
            /,' Cartesian grid created for x,y balanced winds: ', &
            /,' x domain: ',e11.4,' to ',e11.4,'  dx=',e11.4, &
            /,' y domain: ',e11.4,' to ',e11.4,'  dy=',e11.4, &
            /,'   f0=',e11.4,' beta=',e11.4)

        if (iwinda .eq. 4) then
          write(lulog,244) instr,ifgnbe
          244 format( &
              /,1x,a4,' x,y winds from nonlinear balance equation', &
              /,' Variational solution (psi form) with ifgnbe= ',i1)
        else if (iwinda .eq. 3) then
          write(lulog,243) instr,ifgnbe
          243 format( &
              /,1x,a4,' x,y winds from nonlinear balance equation', &
              /,' Variational solution (u,v form) with ifgnbe= ',i1)
        else if (iwinda .eq. 2) then
         write(lulog,242) instr,ifgnbe
         242 format( &
             /,1x,a4,' x,y winds from nonlinear balance equation', &
             /,' Iterative solution with ifgnbe= ',i1)
        else if (iwinda .eq. 1) then
          write(lulog,241) instr
          241 format( &
              /,1x,a4,' x,y winds from linear balance equation')
        else
          write(lulog,240) instr
          240 format(/,1x,a4,' x,y winds from NCEP analysis')
        end if

        end subroutine gridwrit

        subroutine fixprt(lufix,thresh,rad,ierr,ippmin,ipvmax)
        ! This routine writes the fix file
          use sinfo1
          use sinfo2
          use sinfo3
          use utils

          integer,intent(in) :: lufix,ippmin,ipvmax
          real, intent(in) :: thresh,rad
          integer,intent(inout) :: ierr

          character(len=3) :: cdow
          integer :: ispd,ihead,irmp,irad,itime2,jtime2
          integer :: ipr34(6),ipr50(6),ipr64(6)
          integer :: k

          ! Convert output variables to integers 
          do k=1,6
            ipr34(k) = nint(pr34(k))
            ipr50(k) = nint(pr50(k))
            ipr64(k) = nint(pr64(k))
          enddo
          ispd = nint(spd)
          ihead= nint(head)
          irmp = nint(rmp)
          irad = nint(rad)

          ! Get current date and time
          call cdt(kyr,kmon,kday,ktime4,cdow)

          ! Calculate time difference between AMSU and ATCF date/times
          itime2 = itime4/100
          jtime2 = jtime4/100
          call tdiff(jyr,jmon,jday,jtime2,iyr,imon,iday,itime2,idtsa)

          if (ierr .eq. 0) then
            write(lufix,200) csatid
            200 format( &
                  ' ***************************************', &
                  '*******************************', &
                  /,' NESDIS Experimental Microwave TC', &
                  ' Intensity/Size Estimation - ',a6,/) 

            write(lufix,210) atcfid(1:4),iyr,sname
            210 format(' Tropical Cyclone ',1x,a4,i4.4,2x,a10)

            write(lufix,220) kyr,kmon,kday,ktime4, &
                             iyr,imon,iday,itime4, &
                             instr,jyr,jmon,jday,jtime4
            220 format(' Current    date/time: ', &
                  i4,1x,2(i2.2),1x,i4.4,' UTC', &
                  /,' ATCF file  date/time: ', &
                  i4,1x,2(i2.2),1x,i4.4,' UTC', &
                  /,/,1x,a4,' swath date/time: ', &
                  i4,1x,2(i2.2),1x,i4.4,' UTC')

            write(lufix,300) ippmin,ipvmax
            300 format( &
                  /,' Minimum Sea-Level Pressure: ',i5,' hPa', &
                  /,' Maximum Surface Winds:      ',i5,' kt',/)

            write(lufix,310) (ipr34(k),k=1,4) 
            310 format(' 34 kt wind radii (NE,SE,SW,NW): ',4(i4),&
                '  nmi')
            write(lufix,312) (ipr50(k),k=1,4) 
            312 format(' 50 kt wind radii (NE,SE,SW,NW): ',4(i4),&
                '  nmi')
            write(lufix,314) (ipr64(k),k=1,4) 
            314 format(' 64 kt wind radii (NE,SE,SW,NW): ',4(i4),&
                '  nmi')

            write(lufix,320) instr,irmp
            320 format(/,1x,a4,'-retrieved max wind radius: ',i4,' nmi')

            write(lufix,330) irad,instr
            330 format( &
            /,' Storm center is ',i4,' km from ',a4,' swath center', &
            /,'                0-300 km is optimal ',  &
            /,'              300-600 km is adequate',  &
            /,'                 >600 km is marginal') 

            write(lufix,340) instr,idtsa
            340 format( &
                  /,1x,a4,' data is ',i4,' hr from time of ATCF input',/)

            write(lufix,201)
            201 format( &
                  ' ***************************************', &
                  '*******************************')

            write(lufix,400) atcfid(1:4),iyr,imon,iday,itime4 
            400 format(' ATCF File Input: ', &
                  /,1x,a4,i4.4,1x,2(i2.2),1x,i4.4,' UTC')

            write(lufix,410) slat00,slon00,slat12,slon12, &
                             idtsa,sslat,sslon,instr
            410 format( &
                  /,' Storm lat,lon (t =   0 hr): ',f6.2,1x,f7.2, &
                  /,' Storm lat,lon (t = -12 hr): ',f6.2,1x,f7.2, &
                  /,' Storm lat,lon (t =',i4,' hr): ',f6.2,1x,f7.2, &
                    ' (',a4,' swath time)')

            write(lufix,420) ivmax,ihead,ispd,instr,instr,instr
            420 format( &
            /,' Storm max winds (ATCF):  ',i3  ,' kt', &
            /,' Storm heading:           ',i3.3,' deg', &
            /,' Storm translation speed: ',i3  ,' kt', &
            //,'Note: ',a4,' wind radii provided for all wind thresholds',&
            /,'       up to the ATCF max winds. Thus, ',a4,' wind radii ',&
            /,'       may be provided for thresholds that exceed the  ',&
            /,'       ',a4,' max wind estimate.         ')

            write(lufix,201)
            return

          elseif (ierr .eq. 1) then

            write(lufix,200) csatid
            write(lufix,210) atcfid(1:4),iyr,sname
            write(lufix,220) kyr,kmon,kday,ktime4, &
                             iyr,imon,iday,itime4, &
                             jyr,jmon,jday,jtime4

            write(lufix,901) instr,rad,thresh
            901 format(/,' INTENSITY/SIZE ESTIMATION FAILED', &
                  /,' Storm too far from center of ',a4,' data swath', &
                  /,' Observed Distance:      ',f5.0,' km', &
                  /,' Max allowable distance: ',f5.0,' km') 

            write(lufix,201)
            write(*,*) 'INFO: intensity/size estimation failed, &
                        storm too far from swath center.'
            ierr=102
            call exit(ierr)

          !JFD 6-19-2013: Not sure this would ever occur.
          elseif (ierr .eq. 2) then 
            write(lufix,200) csatid
            write(lufix,210) atcfid(1:4),iyr,sname
            write(lufix,220) kyr,kmon,kday,ktime4, &
                             iyr,imon,iday,itime4, &
                             jyr,jmon,jday,jtime4

            write(lufix,902) instr
            902 format( &
                /,' INTENSITY/SIZE ESTIMATION FAILED', &
                /,' Error Processing ',a4,' data', &
                /,' (Usually caused by data that is not available yet)')

            write(lufix,201)
            write(*,*) 'INFO: intensity/size estimation failed, &
                        missing data'
            ierr=101
            call exit(ierr)

          endif

          return

        end subroutine fixprt

        subroutine cdt(kyr,kmon,kday,ktime,cdow)
        ! This routine gets the current dat and time (UTC)
        ! from the file tdate.dat that is created by the mdasmu.ksh 
        ! script using the HP-UX command date -u. The output of this
        ! command is of the form
        ! Fri May 17 16:00:42 GMT 2002
          use io

          integer,intent(inout) :: kyr,kmon,kday,ktime
          character(len=3),intent(inout) :: cdow

          character(len=3) :: cmon,cmonl(12)
          integer :: k

          kyr = 0
          kmon= 0
          kday= 0 
          ktime=0
          cdow='XXX'

          cmonl = (/'Jan','Feb','Mar','Apr','May','Jun', &
                    'Jul','Aug','Sep','Oct','Nov','Dec'/)
          open(unit=65,file='tdate.dat',form='formatted',status='old', &
               err=900)

          read(65,100,err=900) cdow,cmon,kday,khr,kmin,kyr
          100 format(a3,1x,a3,1x,i2,1x,i2,1x,i2,8x,i4)
          close(65)

          do k=1,12     
            if (cmon .eq. cmonl(k)) kmon = k
          enddo
          if (kmon .eq. 0) go to 900

          ktime = 100*khr + kmin

          return
  
          900 continue

            kyr = 0
            kmon= 0
            kday= 0 
            ktime=0
            cdow='XXX'

            return

        end subroutine cdt


