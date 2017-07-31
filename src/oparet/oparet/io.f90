      module io 
        implicit none

        ! Variables for input file names
        character(len=25) :: fntinp = 'COORTIMES'
        character(len=25) :: fnninp = 'AVN.DAT'
        character(len=25) :: fncinp = 'oparet.cfg'
        character(len=25) :: fnparam = 'oparet_params.cfg'

        ! Variables for input filenames read from config file
        character(len=25) :: fl_coeff_cvmx
        character(len=25) :: fl_coeff_cpmn
        character(len=25) :: fl_coeff_cr34
        character(len=25) :: fl_coeff_cr50
        character(len=25) :: fl_coeff_cr64
        character(len=25) :: instr
        ! number of coeffs
        integer :: ncoefs  !number of coefficients for
                                           !each var 

        ! Variables for output file names
        character(len=25) :: fnlog = 'oparet.log'
        character(len=25) :: fnloc
        character(len=25) :: fnnpk,fnapk
        character(len=25) :: fnrza,fnxya
        character(len=25) :: fnsta,fnfix,fnafx

        ! Variable for temporary file names
        character(len=25) :: fntemp

        ! Variables for output file headers
        character(len=90) :: coord,times

        ! Input file numbers
        integer,parameter :: lutinp = 21 
        integer,parameter :: luninp = 22 
        integer,parameter :: luainp = 23
        integer,parameter :: luparam = 24
        integer,parameter :: lucoef = 25

        ! Output file numbers
        integer,parameter :: luloc = 31 
        integer,parameter :: lulog = 32
        integer,parameter :: lunpk = 33
        integer,parameter :: luapk = 34
        integer,parameter :: lurza = 35
        integer,parameter :: lusta = 36
        integer,parameter :: lufix = 37
        integer,parameter :: luafx = 38
        integer,parameter :: luxya = 39
 
        ! Input Attributes 
        type inpAtt
          real :: latScale,latOffset,latValid
          real :: lonScale,lonOffset,lonValid
          real :: prsScale,prsOffset,prsValid
          real :: clwScale,clwOffset,clwValid
          real :: tpwScale,tpwOffset,tpwValid
          real :: tmpScale,tmpOffset,tmpValid
          real :: vprScale,vprOffset,vprValid
        end type inpAtt 

        !parameters reaqd from config file
        real    :: scale_rv
        real    :: qcFailValue
        logical :: use_tv = .FALSE. 
        

        type (inpAtt) :: inpAtts 

        save

        contains

        subroutine read_params_config
          integer :: open_status
          integer :: io_status
          integer :: ierr

          integer :: use_tv_int, i, nlines
          character(len=80) :: iline

          ! count number of lines in config file
          open(unit=luparam,file=fnparam,status='OLD',iostat=open_status)
          nlines = 0
          do i = 1, 10000000
            read (luparam,103, end = 900) iline
            nlines = nlines+1
          enddo
  900     rewind(luparam)

          ! read params from config file
          do i = 1, nlines
            read (luparam, 103) iline

              if (iline(1:6) .eq. 'use_tv') then
                read(iline,102) use_tv_int
                if (use_tv_int == 1) then
                    use_tv = .TRUE.
                endif
              endif
              
             if (iline(1:6) .eq. 'ncoefs') then
                read(iline,102) ncoefs
              endif
              
              if (iline(1:8) .eq. 'scale_rv') then
                read(iline,101) scale_rv
              endif
              
              if (iline(1:11) .eq. 'qcFailValue') then
                read(iline,101) qcFailValue
              endif
              
              if (iline(1:8) .eq. 'vmx_coef') then
                fl_coeff_cvmx=adjustl(trim(iline(26:)))
              endif
        
              if (iline(1:8) .eq. 'pmn_coef') then
                fl_coeff_cpmn=adjustl(trim(iline(26:)))
              endif
              
              if (iline(1:8) .eq. 'r34_coef') then
                fl_coeff_cr34=adjustl(trim(iline(26:)))
              endif
              
              if (iline(1:8) .eq. 'r50_coef') then
                fl_coeff_cr50=adjustl(trim(iline(26:)))
              endif
              
              if (iline(1:8) .eq. 'r64_coef') then
                fl_coeff_cr64=adjustl(trim(iline(26:)))
              endif
              
              if (iline(1:5) .eq. 'instr') then
                instr=adjustl(trim(iline(26:)))
              endif


          enddo

  101     format(26x,f18.5)
  102     format(26x,i20)
  103     format(a80)
  104     format(26x,a80)

          close(luparam)
       
          print *, "config params :"
          print *, "nlines in config file ", nlines 
          print *, "use_tv  = " ,       use_tv 
          print *, "scale_rv  = ",      scale_rv
          print *, "qcFailValue  = ",   qcFailValue 
          print *, "ncoefs  = ",   ncoefs
          print *, "fl_coeff_cvmx  = ",   fl_coeff_cvmx
          print *, "fl_coeff_cpmn  = ",   fl_coeff_cpmn
          print *, "fl_coeff_cr34  = ",   fl_coeff_cr34
          print *, "fl_coeff_cr50  = ",   fl_coeff_cr50
          print *, "fl_coeff_cr64  = ",   fl_coeff_cr64
          print *, "instr  = ",   instr

        end subroutine


        subroutine read_config 
          integer :: open_status
          integer :: io_status
          integer :: ierr

          open(unit=luainp,file=fncinp,status='OLD',iostat=open_status)
          if(open_status /= 0) then 
            write(*,*) 'WARNING: could not open input attribute file'
            ierr=66
            call exit(ierr)
          endif 

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%latScale,inpAtts%latOffset,inpAtts%latValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%latScale,inpAtts%latOffset,inpAtts%latValid

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%lonScale,inpAtts%lonOffset,inpAtts%lonValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%lonScale,inpAtts%lonOffset,inpAtts%lonValid

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%prsScale,inpAtts%prsOffset,inpAtts%prsValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%prsScale,inpAtts%prsOffset,inpAtts%prsValid

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%clwScale,inpAtts%clwOffset,inpAtts%clwValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%clwScale,inpAtts%clwOffset,inpAtts%clwValid

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%tpwScale,inpAtts%tpwOffset,inpAtts%tpwValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%tpwScale,inpAtts%tpwOffset,inpAtts%tpwValid

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%tmpScale,inpAtts%tmpOffset,inpAtts%tmpValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%tmpScale,inpAtts%tmpOffset,inpAtts%tmpValid

          read(luainp,'(11X,F15.5,1X,F15.5,1X,F15.5)',iostat=io_status)&
          inpAtts%vprScale,inpAtts%vprOffset,inpAtts%vprValid
          ! write(*,'(3F15.5)') &
          ! inpAtts%vprScale,inpAtts%vprOffset,inpAtts%vprValid

          close(luainp)

        end subroutine read_config 

        subroutine init_output
          use params
          use sinfo1
          use sinfo2

          character(len=3) :: lab3
          integer :: idayr,iprt,ios
          
          ! ++ Name for loc file (AMSU footprint locations)
          fnloc        = 'LX0000_000000.DAT'
          fnloc( 2: 2) = atcfid(1:1)
          fnloc( 3: 6) = atcfid(3:6)
          fnloc(18:19) = '99'
          write(fnloc(8:13),226) imon,iday,itime
          226 format(3(i2.2))

          if (igenfn .eq. 1) then
            fnloc='  '
            fnloc(1:6) = csatid
            fnloc(7:10) = '.LOC'
          endif

          ! ++ Name for rza file (2-D AMSU retrieval output file)
          fnrza        = 'RX0000_000000.DAT'
          fnrza( 2: 2) = atcfid(1:1)
          fnrza( 3: 6) = atcfid(3:6)
          fnrza(18:19) = '99'
          write(fnrza(8:13),226) imon,iday,itime

          if (igenfn .eq. 1) then
            fnrza='  '
            fnrza(1:6) = csatid
            fnrza(7:10) = '.RZA'
          endif

          ! ++ Name for xya file (3-D AMSU retrieval output file)
          fnxya        = 'AX0000_000000.DAT'
          fnxya( 2: 2) = atcfid(1:1)
          fnxya( 3: 6) = atcfid(3:6)
          fnxya(18:19) = '99'
          write(fnxya(8:13),226) imon,iday,itime

          if (igenfn .eq. 1) then
            fnxya='  '
            fnxya(1:6) = csatid
            fnxya(7:10) = '.XYA'
          endif

          ! ++ Name for sta file (TC statistics file)
          fnsta        = 'SX0000_000000.DAT'
          fnsta( 2: 2) = atcfid(1:1)
          fnsta( 3: 6) = atcfid(3:6)
          fnsta(18:19) = '99'
          write(fnsta(8:13),226) imon,iday,itime

          if (igenfn .eq. 1) then
            fnsta='  '
            fnsta(1:6) = csatid
            fnsta(7:10) = '.STA'
          endif

          ! ++ Create file name for fix file
          fnfix        = 'FX0000_000000.DAT'
          fnfix( 2: 2) = atcfid(1:1)
          fnfix( 3: 6) = atcfid(3:6)
          fnfix(18:19) = '99'
          write(fnfix(8:13),226) imon,iday,itime

          if (igenfn .eq. 1) then
            fnfix='  '
            fnfix(1:6) = csatid
            fnfix(7:10) = '.FIX'
          endif

          ! ++ Create file name for afx file
          fnafx        = 'GX0000_000000.DAT'
          fnafx( 2: 2) = atcfid(1:1)
          fnafx( 3: 6) = atcfid(3:6)
          fnafx(18:19) = '99'

          write(fnafx(8:13),226) imon,iday,itime
          if (igenfn .eq. 1) then
            fnafx='  '
            fnafx(1:6) = csatid
            fnafx(7:10) = '.AFX'
          endif

          ! ++ Name for npk file (packed NCEP output file)
          if (itime .eq. 6 .or. itime .eq. 18) then
            rawdid = 'Y'
          else
            rawdid = 'X'
          endif

          if (itime .ge. 12) then
            idayr = iday+50
          else
            idayr = iday
          endif

          fnnpk       = ' '
          fnnpk(1:1)  = 'N'
          fnnpk(2:2)  = atcfid(1:1)
          fnnpk(3:6)  = atcfid(3:6)
          fnnpk(7:21) = '_X0000_PACK.DAT'
          fnnpk(22:23)= '99'
          write(fnnpk(8:12),300) rawdid,imon,idayr
          300 format(a1,i2.2,i2.2)

          if (igenfn .eq. 1) then
            fnnpk='  '
            fnnpk(1:6) = csatid
            fnnpk(7:10) = '.NPK'
          endif

          ! ++ Name for apk file (packed 3-D AMSU retrieval output file)
          ! on lat,lon,P grid
          fnapk=fnnpk
          fnapk(1:1) = 'A'

          if (igenfn .eq. 1) then
            fnapk='  '
            fnapk(1:6) = csatid
            fnapk(7:10) = '.APK'
          endif

          ! ++ Write file names to log file
          lab3   =  'loc'
          fntemp = fnloc
          iprt   = iploc
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'rza'
          fntemp = fnrza
          iprt   = iprza
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'xya'
          fntemp = fnxya
          iprt   = ipxya
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'sta'
          fntemp = fnsta
          iprt   = ipsta
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'fix'
          fntemp = fnfix
          iprt   = ipfix
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'afx'
          fntemp = fnafx
          iprt   = ipfix
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'apk'
          fntemp = fnapk
          iprt   = ipapk
          write(lulog,218) lab3,fntemp,iprt

          lab3   =  'npk'
          fntemp = fnnpk
          iprt   = ipnpk
          write(lulog,218) lab3,fntemp,iprt

          218 format(a3,' data output file: ',a25,' write flag=',i1)

          ! ++ Open the output files, if necessary 
          ! Note: Fix file is opened later if needed, depending
          ! on the outcome of the TC retrieval

          if (iploc .eq. 1) then
            fntemp=fnloc
            open(unit=luloc,file=fnloc,form='formatted',status='unknown',iostat=ios)
            if(ios.ne.0) call open_error
          endif

          if (iprza .eq. 1 .and. iaxsym .eq. 1) then
            fntemp = fnrza
            open(file=fnrza,unit=lurza,form='formatted',status='unknown',iostat=ios)
            if(ios.ne.0) call open_error
          endif

          if (ipxya .eq. 1) then
            fntemp=fnxya
            open(unit=luxya,file=fnxya,form='formatted',status='unknown',iostat=ios)
            if(ios.ne.0) call open_error
          endif

          if (ipsta .eq. 1 .and. iaxsym .eq. 1) then
            fntemp = fnsta
            open(file=fnsta,unit=lusta,form='formatted',status='unknown',iostat=ios)
            if(ios.ne.0) call open_error
          endif

          if (ipapk .eq. 1) then
            fntemp=fnapk
            open(unit=luapk,file=fnapk,form='formatted',status='unknown',iostat=ios)
            if(ios.ne.0) call open_error
          endif

          if (ipnpk .eq. 1) then
            fntemp=fnnpk
            open(unit=lunpk,file=fnnpk,form='formatted',status='unknown',iostat=ios)
            if(ios.ne.0) call open_error
          endif

        end subroutine init_output 

        ! Error processing
        subroutine open_error 
          integer :: ierr

          write(lulog,990) fntemp
          990 format(/,' Error opening file ',a25)
          write(*,995) fntemp
          995 format('WARNING: could not open file ',a25)
          ierr=66
          call exit(ierr)

        end subroutine open_error

      end module io 
