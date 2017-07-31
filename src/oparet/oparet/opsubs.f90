        module opsubs
          use io
          implicit none
  
          contains

          subroutine zalcals(t,ts,ps,p,z)
          ! This routine calculates the height z at all AMSU levels,
          ! and the surface pressure ps at the domain interior points.

          ! This version assumes z at the top level has been specified
          ! and is used as an upper boundary condition
            use dims
            use hstatic
            use io

            real,intent(in) :: t(nx,ny,np),ts(nx,ny),p(np)
            real,intent(inout) :: z(nx,ny,np),ps(nx,ny)

            real :: t1,t2,p1,p2,z1,z2,delz
            integer :: i,j,k

            ! Integrate z down to lowest AMSU level
            do j=1,ny
              do i=1,nx
                do k=2,np
                  t1 = t(i,j,k-1)
                  t2 = t(i,j,k)
                  p1 = p(k-1)
                  p2 = p(k)
                  call tkness(p1,p2,t1,t2,delz)
                  z(i,j,k) = z(i,j,k-1) - delz
                end do
              end do
            end do

            ! Calculate surface pressure by integrating from lowest
            ! AMSU level to the surface
            do j=1,ny
              do i=1,nx
                t1 = t(i,j,np)
                z1 = z(i,j,np)
                p1 = p(np)
                t2 = ts(i,j)
                z2 = 0.0
                call p2cal(z1,z2,t1,t2,p1,p2)
                ps(i,j) = p2
              end do
            end do

            return
 
          end subroutine zalcals

          subroutine zalcal(t,ts,ps,p,z)
          ! This routine calculates the height z at all AMSU levels,
          ! and the surface pressure ps at the domain interior points.
            use dims
            use utils
            use hstatic
            use sphfun
            use io

            real,intent(in) :: t(nx,ny,np),ts(nx,ny),p(np)
            real,intent(inout) :: z(nx,ny,np),ps(nx,ny)
 
            ! Local work arrays
            real :: ta1(nx,ny),ta2(nx,ny)
 
            real :: t1,t2,p1,p2,z1,z2,delz
            integer :: ierr
            integer :: i,j,k
 
            ! Calculate height of lowest AMSU level at boundary points
            do i=1,nx
              ! Bottom edge of domain
              j=1
              t1 = ts(i,j)
              t2 =  t(i,j,np)
              p1 = ps(i,j)
              p2 = p(np)
              call tkness(p1,p2,t1,t2,delz)
              z(i,j,np) = delz
              ! Top edge of domain
              j=ny
              t1 = ts(i,j)
              t2 =  t(i,j,np)
              p1 = ps(i,j)
              p2 = p(np)
              call tkness(p1,p2,t1,t2,delz)
              z(i,j,np) = delz
            end do

            do j=2,ny-1
              ! Left edge of domain
              i=1
              t1 = ts(i,j)
              t2 =  t(i,j,np)
              p1 = ps(i,j)
              p2 = p(np)
              call tkness(p1,p2,t1,t2,delz)
              z(i,j,np) = delz
              ! Right edge of domain
              i=nx
              t1 = ts(i,j)
              t2 =  t(i,j,np)
              p1 = ps(i,j)
              p2 = p(np)
              call tkness(p1,p2,t1,t2,delz)
              z(i,j,np) = delz
            end do

            ! Calculate z at the rest of the AMSU levels at the boundary points
            do k=np-1,1,-1
              do i=1,nx
                ! Bottom edge of domain
                j=1
                t1 = t(i,j,k+1)
                t2 = t(i,j,k  )
                p1 = p(k+1)
                p2 = p(k)
                call tkness(p1,p2,t1,t2,delz)
                z(i,j,k) = z(i,j,k+1) + delz
                ! Top edge of domain
                j=ny
                t1 = t(i,j,k+1)
                t2 = t(i,j,k  )
                p1 = p(k+1)
                p2 = p(k)
                call tkness(p1,p2,t1,t2,delz)
                z(i,j,k) = z(i,j,k+1) + delz
                if (k .eq. 1) then
                ! ???
                endif
              end do

              do j=2,ny-1
                ! Left edge of domain
                i=1
                t1 = t(i,j,k+1)
                t2 = t(i,j,k  )
                p1 = p(k+1)
                p2 = p(k)
                call tkness(p1,p2,t1,t2,delz)
                z(i,j,k) = z(i,j,k+1) + delz
                ! Right edge of domain
                i=nx
                t1 = t(i,j,k+1)
                t2 = t(i,j,k  )
                p1 = p(k+1)
                p2 = p(k)
                call tkness(p1,p2,t1,t2,delz)
                z(i,j,k) = z(i,j,k+1) + delz
                if (k .eq. 1) then
                ! ???
                endif
              end do
            end do

            ! Interpolate z (top pressure level) at the boundary points
            ! to the domain interior using a Laplacian filter
            do j=1,ny
              do i=1,nx
                ta1(i,j) = 0.0
                ta2(i,j) = z(i,j,1)
              end do
            end do
  
            call pson(ta1,ta2,z(1,1,1),ierr)
            if (ierr .ne. 0) then
              write(lulog,960)
              960 format(/, &
                  ' pson routine did not converge in z calculation')
              stop
            endif
  
            ! Integrate z down to lowest AMSU level at interior points
            do j=2,ny-1
              do i=2,nx-1
                do k=2,np
                  t1 = t(i,j,k-1)
                  t2 = t(i,j,k)
                  p1 = p(k-1)
                  p2 = p(k)
                  call tkness(p1,p2,t1,t2,delz)
                  z(i,j,k) = z(i,j,k-1) - delz
                end do
              end do
            end do
  
            ! Calculate surface pressure by integrating from lowest 
            ! AMSU level to the surface
            do j=2,ny-1
              do i=2,nx-1
                t1 = t(i,j,np)
                z1 = z(i,j,np)
                p1 = p(np)
                t2 = ts(i,j)
                z2 = 0.0
                call p2cal(z1,z2,t1,t2,p1,p2)
                ps(i,j) = p2
              end do
            end do

            return

          end subroutine zalcal
          
          subroutine ncepget(pn,un,vn,zn,tn,tn1000,rlond,rlatd,nx,ny,npn, &
                             ixmax,iymax,dlona,dlata,dayx,utcx,lundat,ierr)
          ! This routine gets u,v,z and t from the packed ncep analysis file
            use utils
            use ncepfg
            use io
            implicit none

            integer,intent(in) :: ixmax,iymax
            integer,intent(in) :: nx,ny,npn
            real,intent(in) :: pn(npn)
            real,intent(inout) :: un(nx,ny,npn),vn(nx,ny,npn)
            real,intent(inout) :: zn(nx,ny,npn),tn(nx,ny,npn)
            real,intent(inout) :: tn1000(ixmax,iymax)
            real,intent(inout) :: rlond(nx),rlatd(ny)
            real,intent(in) :: dlona,dlata
            real,intent(inout) :: dayx,utcx
            integer,intent(in) :: lundat
            integer,intent(inout) :: ierr

            ! Local variables

            ! Data for unpacking
            integer,parameter :: imax=65341
            real :: tra(imax)
            character(len=1) :: type
            character(len=2) :: code(imax)

            real :: t2d(ixmax,iymax)

            ! Array for checking that all requested NCEP levels were found
            integer,parameter :: nvar=4
            integer :: ncheck(100,nvar)
            character(len=1) :: ccheck(nvar) = (/'U','V','Z','T'/)

            integer :: lu,linen,lerr
            integer :: interp,ipts,nrow,is,ie,idec,izp
            real :: wx,rlatmn,rlatmx,rlonmn,rlonmx,epsil
            real :: rlatb,rlatt,rlonl,rlonr
            integer :: nlat1,nlon1
            real :: ptmp,bsub,smpy,zstd,tstd,thstd
            real :: alonl,alatb
            real :: pk,pkp,pkm,wtm,wtp
            integer :: i,j,k,ii,kk,ij,m,n

            ! Set interp=1 to allow option of interpolating between 
            ! levels of NCEP data. There must be data above and below 
            ! the levels requiring interpolation. 
            interp=1

            ! Set checking array to zero
            do k=1,npn
              do i=1,nvar
                ncheck(k,i) = 0
              end do
            end do

            lu    = lundat
            linen = 0

            ! Read main header on packed data file
            read(lu,200,err=901,end=901) &
                 wx,dayx,utcx,rlatmn,rlatmx,rlonmn,rlonmx,dlatn,dlonn
            200 format(1x,f3.0,f7.0,f5.0,4f8.3,1x,2f4.2)
            linen = linen + 1

            ! Calculate lat and lon intervals and number of points on NCEP
            ! data file
            epsil = 0.0001
            nlat1 = 1 + nint((rlatmx-rlatmn)/dlatn + epsil)
            nlon1 = 1 + nint((rlonmx-rlonmn)/dlonn + epsil)
            ipts  = nlat1*nlon1
            rlatb = rlatmn
            rlatt = rlatmx
            rlonl = -rlonmx
            rlonr = -rlonmn

            rlonln = rlonl
            rlatbn = rlatb
            nlonn  = nlon1
            nlatn  = nlat1

            write(lulog,300) rlonl,rlonr,dlonn,rlatb,rlatt,dlatn, &
                             nlon1,nlat1,ipts
            300 format(/,' Data read from NCEP file',/, &
                       ' rlonl=',f6.1,' rlonr=',f6.1,'  dlon=',f4.2,/, &
                       ' rlatb=',f6.1,' rlatt=',f6.1,'  dlat=',f4.2,/, &
                       ' nlon1=',i6,' nlat1=',i6,'  ipts=',i6)

            ! Check array size
            if (ipts .gt. imax) then
              ierr = 2
              return
            endif

            if (nlon1 .gt. ixmax .or. nlat1 .gt. iymax) then
              ierr = 2
              return
            endif

            ! Read the rest of the data file
            nrow = 1 + (ipts-1)/36

        500 continue

            read(lu,202,err=901,end=600) type,ptmp,bsub,smpy
            202 format(1x,a1,1x,f6.1,2(1x,g15.9))
            linen = linen + 1

!           write(lulog,320) type,ptmp,linen,bsub,smpy
!           320 format(1x,a1,' p=',f6.1,' read from line ',i5, &
!                      ' of ncep file, b,s: ',2(e10.3))

            ! Read packed data
            do n=1,nrow
              is = 1 + (n-1)*36
              ie = is + 35
              read(lu,204,end=901,err=901) (code(i),i=is,ie)
              204 format(36(a2))
              linen = linen + 1
            end do

            ! Search for required NCEP pressure level and data type
            ii = 0
            do i=1,nvar
              if (type .eq. ccheck(i)) then
                ii = i
              endif
            end do

            kk = 0
            do k=1,npn
              if (ptmp .eq. pn(k)) then
                kk = k 
              endif
            end do

            if (kk .eq. 0 .or. ii .eq. 0) go to 500


            ! This variable is need, so update checking array and unpack it.
            ncheck(kk,ii) = 1

            ! Unpack current variable
            do i=1,ipts
              idec = idecod(code(i))
              tra(i) = float(idec)*smpy - bsub
            end do

            ! Add standard height to height perturbation
            if (type .eq. 'Z') then
              call stndz(ptmp,zstd,tstd,thstd)
              do i=1,ipts
                tra(i) = tra(i) + zstd
              end do
            end if

            ! Put data in 2-D lon/lat array
            do j = 1,nlat1
              do i = 1,nlon1
                ij = i + (j-1)*nlon1
                t2d(i,j) = tra(ij)
              end do
            end do


            alonl = rlond(1)
            if (rlonr .eq. 360.0 .and. alonl .lt. 0.0) then
              alonl = alonl + 360.0
            endif

            alatb = rlatd(1)
            izp   = 0

!           write(6,990) rlonl,rlatb,dlonn,dlatn,ixmax,iymax,nlon1,nlat1 &
!           990 format(/,' rlonl=',f6.1,' rlatb=',f6.1,' dlonn=',f6.1,' dlatn=', &
!                      f6.1,/,' ixmax=',i4,' iymax=',i4,' nlon1=',i4,
!                      ' nlat1=',i4)
!
!           write(6,991) alonl,alatb,dlona,dlata,nx,ny &
!           991 format(' alonl=',f6.1,' alatb=',f6.1,' dlona=',f6.1,' dlata=', &
!                      f6.1,/,' nx=',i4,' ny=',i4)


            ! Interpolate data from NCEP to AMSU grid points
            if (type .eq. 'U') then
              call llintp(t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax, &
                          nlon1,nlat1,un(1,1,kk),alonl,alatb,dlona, &
                          dlata,nx,ny,nx,ny,izp,lerr)
            else if (type .eq. 'V') then
              call llintp(t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax, &
                          nlon1,nlat1,vn(1,1,kk),alonl,alatb,dlona, &
                          dlata,nx,ny,nx,ny,izp,lerr)
            else if (type .eq. 'T') then
              call llintp(t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax, &
                          nlon1,nlat1,tn(1,1,kk),alonl,alatb,dlona, &
                          dlata,nx,ny,nx,ny,izp,lerr)

              ! Save 1000 mb temperature on for tcorsv routine
              if (ptmp .eq. 1000.0) then
                do j=1,nlat1
                  do i=1,nlon1
                    tn1000(i,j) = t2d(i,j)
                  enddo
                enddo
              endif

            else if (type .eq. 'Z') then
              call llintp(t2d,rlonl,rlatb,dlonn,dlatn,ixmax,iymax, &
                          nlon1,nlat1,zn(1,1,kk),alonl,alatb,dlona, &
                          dlata,nx,ny,nx,ny,izp,lerr)
            else
              ierr = 5
              return
            endif

            go to 500

        600 continue

            ! Check to see if data at missing pressure levels can
            ! be filled by interpolation. Top and bottom levels can
            ! not be interpolated. 
            if (interp .eq. 1) then
              do k=2,npn-1
                do m=1,nvar
                  if (ncheck(k,m) .eq. 0 .and. ncheck(k+1,m) .eq. 1 &
                                  .and. ncheck(k-1,m) .eq. 1) then 
                    ncheck(k,m) = 1

                    pk  = pn(k) 
                    pkp = pn(k+1)
                    pkm = pn(k-1)

                    wtm = (pkp-pk)/(pkp-pkm)
                    wtp = (pk-pkm)/(pkp-pkm)

                    if (m .eq. 1) then
                      do j=1,ny
                        do i=1,nx
                          un(i,j,k) = wtm*un(i,j,k-1) + wtp*un(i,j,k+1)
                        end do
                      end do
                    else if (m .eq. 2) then
                      do j=1,ny
                        do i=1,nx
                          vn(i,j,k) = wtm*vn(i,j,k-1) + wtp*vn(i,j,k+1)
                        enddo
                      enddo
                    elseif (m .eq. 3) then
                      do j=1,ny
                        do i=1,nx
                          zn(i,j,k) = wtm*zn(i,j,k-1) + wtp*zn(i,j,k+1)
                        enddo
                      enddo
                    elseif (m .eq. 4) then
                      do j=1,ny
                        do i=1,nx
                          tn(i,j,k) = wtm*tn(i,j,k-1) + wtp*tn(i,j,k+1)
                        enddo
                      enddo
                    endif

                  endif

                enddo
              enddo

            endif    
   
            ! Check to make sure all levels have been found
            do k=1,npn
              do i=1,nvar
                if (ncheck(k,i) .eq. 0) then
                  write(lulog,950) ccheck(i),pn(k)
                  950 format(/, &
                        ' Required NCEP variable not found, type= ' &
                        ,a1,'  P=',f6.1) 
                  ierr = 4
                end if
              end do
            end do

            write(lulog,330) nvar,npn 
            330 format(/,'All ',i1,' NCEP variables found at all ',i2, &
                       ' requested levels')

            ierr = 0
            return
 
        901 continue
            ierr = 1
            return

          end subroutine ncepget

          subroutine aprint(a,nx,ny,plev,iscale,inc,label)
          ! This routine prints the 2-d array a.
          ! If iscale=1, the array is scaled to a number between 0 and 100.
          ! If iscale<0, the array is scaled by abs(iscale)
            use io

            integer,intent(in) :: nx,ny,iscale,inc
            real,intent(in) :: a(nx,ny), plev
            character(len=20),intent(in) :: label
         
            real :: slope,offset,aamax,aamin
            integer :: istart,iend,icut
            integer :: i,j

            slope  = 1.0
            offset = 0.0

            ! Find max and min of a
            aamax = -1.0e+20
            aamin =  1.0e+20
            do j=1,ny
              do i=1,nx
                if (a(i,j) .gt. aamax) aamax=a(i,j)
                if (a(i,j) .lt. aamin) aamin=a(i,j)
              end do
            end do

            if (aamax .ne. aamin .and. iscale .eq. 1) then
              offset = aamin
              slope  = 100.0/(aamax-aamin)
            endif

            if (iscale .lt. 0) then
              offset = 0.0
              slope = 1.0/float(-iscale)
            endif

            write(lulog,200) label,plev/100.0,aamax,aamin,iscale
            200 format(/,1x,a20,' p(mb)=',f6.1,' max,min= ', &
                       e11.4,1x,e11.4,' iscale=',i6)

            istart = 1 
            iend   = nx

            if (nx .gt. 26) then
              icut = 1 + (nx-27)/2 
              istart = istart + icut
              iend   = iend   - icut
            endif

            if (inc .gt. 0) then
              do j=1,ny,inc
                write(lulog,210) j,( (slope*(a(i,j)-offset)),i=istart,iend )
                210 format(1x,i2,1x,26(f6.1),/,4x,26(f6.1))
              end do
            elseif (inc .lt. 0) then
              do j=ny,1,inc
                write(lulog,210) j,( (slope*(a(i,j)-offset)),i=istart,iend)
              end do
            endif

            write(lulog,220) (i,i=istart,iend)
            220 format(6x,26(i2,4x),/,6x,26(i2,4x))

            return

          end subroutine aprint

          subroutine distk(rlon1,rlat1,rlon2,rlat2,dx,dy,rad)
          ! This routine calculates the distance in km (rad) between the
          ! points (rlon1,rlat1) and (rlon2,rlat2) using an approximate
          ! formula. The lon and lat are in deg E and N. The east and
          ! north components of the distance (dx,dy) are also calculated.
            use cons
            
            real,intent(in) :: rlon1,rlat1,rlon2,rlat2
            real,intent(inout) :: dx,dy,rad

            real,parameter :: dtk = 111.1
            real :: cfac

            cfac = cos(0.5*dtr*(rlat1+rlat2))

            dx  = dtk*(rlon2-rlon1)*cfac
            dy  = dtk*(rlat2-rlat1)
            rad = sqrt(dx*dx + dy*dy)

            return

          end subroutine distk

          subroutine wpof(u,v,t,z,ps,iyr,imon,iday,itime,luout)
          ! This routine writes u,v,t,z and ps to a packed output file
            use dims
            use params
            use ncepll
            use ncepp
            use io
            use utils
            use sinfo3

            real,intent(in) :: u(nx,ny,npn),v(nx,ny,npn)
            real,intent(in) :: t(nx,ny,npn),z(nx,ny,npn)
            real,intent(in) :: ps(nx,ny)
            integer,intent(in) :: iyr,imon,iday,itime,luout

            ! Local variables
            integer,parameter :: imax=65341
            real :: tra(imax)
            character(len=1) :: type
            character(len=2) :: code(imax)

            real :: wx,rdate,rtime,ft,epsil,plevx,zstd,tstd,thstd
            real :: tmax,tmin,bsub,smpy
            integer :: ipts,nrow,icount,is,ie
            integer :: i,j,k,n
            
            ! Write header line on the file
            wx = 12.
            rdate = float(10000*iyr + 100*imon + iday)
            rtime = float(itime)
            ft    = 0.0

            write(luout,200) wx,rdate,rtime,rlatd( 1), rlatd(ny), &
                             -rlond(nx),-rlond( 1),dlat,dlon,ft
            200 format(1x,f3.0,f7.0,f5.0,4f8.3,2f4.2,f6.0)

            ! Calculate number of grid points
            ipts = nx*ny
            nrow = 1 + (ipts-1)/36

            if (ipts .gt. imax) then
              write(lulog,900) instr
              900 format(/, &
                  a4,' analysis domain too large, increase imax ', &
                  /,' in routine wpof. ')
              stop
            endif

            ! Fill in code array with 1s before packing
            do i=1,ipts+36
              code(i) = '11'
            end do

            epsil = 0.001
            ! Pack sea-level pressure (in mb)
            type = 'S'

            icount = 0
            do j=1,ny
              do i=1,nx
                icount = icount + 1
                tra(icount) = ps(i,j)/100.0
              end do
            end do

            call maxmin(tra,1,ipts,tmax,tmin)
            call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)

            plevx = 1070.0
            write(luout,210) type,plevx,bsub,smpy
            210 format(1x,a1,1x,f6.1,2(1x,g15.9))

            do n=1,nrow
              is = 1 + (n-1)*36
              ie = is+35
              write(luout,220) (code(i),i=is,ie)
              220 format(36(a2))
            end do

            ! Pack the rest of the variables
            do k=1,npn

              plevx = pn(k)/100.0
              call stndz(plevx,zstd,tstd,thstd)

              type = 'U'
              icount = 0
              do j=1,ny
                do i=1,nx
                  icount = icount + 1
                  tra(icount) = u(i,j,k)
                end do
              end do

              call maxmin(tra,1,ipts,tmax,tmin)
              call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)

              write(luout,210) type,plevx,bsub,smpy

              do n=1,nrow
                is = 1 + (n-1)*36
                ie = is+35
                write(luout,220) (code(i),i=is,ie)
              end do

              type = 'V'
              icount = 0
              do j=1,ny
                do i=1,nx
                  icount = icount + 1
                  tra(icount) = v(i,j,k)
                end do
              end do

              call maxmin(tra,1,ipts,tmax,tmin)
              call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)

              write(luout,210) type,plevx,bsub,smpy

              do n=1,nrow
                is = 1 + (n-1)*36
                ie = is+35
                write(luout,220) (code(i),i=is,ie)
              end do

              type = 'Z'
              icount = 0
              do j=1,ny
                do i=1,nx
                  icount = icount + 1
                  tra(icount) = z(i,j,k) - zstd
                end do
              end do

              call maxmin(tra,1,ipts,tmax,tmin)
              call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)

              write(luout,210) type,plevx,bsub,smpy

              do n=1,nrow
                is = 1 + (n-1)*36
                ie = is+35
                write(luout,220) (code(i),i=is,ie)
              end do

              type = 'T'
              icount = 0
              do j=1,ny
                do i=1,nx
                  icount = icount + 1
                  tra(icount) = t(i,j,k)
                end do
              end do

              call maxmin(tra,1,ipts,tmax,tmin)
              call tstcod(tra,1,ipts,tmax,tmin,bsub,smpy,code)

              write(luout,210) type,plevx,bsub,smpy

              do n=1,nrow
                is = 1 + (n-1)*36
                ie = is+35
                write(luout,220) (code(i),i=is,ie)
              end do

            end do

            return

          end subroutine wpof

          subroutine tcors(mxas,nxas,t,clw,aslon,aslat, &
                           sslon,sslat,works,dt,dtred,tcrmax)
          ! This routine reduces the temperature anomaly below the
          ! threshold dt by a factor dtred. This version uses the
          ! pre-analyzed swath temperature data.

            integer,intent(in) :: mxas,nxas
            real,intent(inout) :: t(mxas),works(mxas)
            real,intent(in) :: clw(mxas)
            real,intent(in) :: aslon(mxas),aslat(mxas)
            real,intent(in) :: sslon,sslat,dt,dtred,tcrmax
  
            real :: tbar,rpts,dtt,dx,dy,rad
            integer :: i
  
            ! Calculate radius of each swath point
            do i=1,nxas
              call distk(sslon,sslat,aslon(i),aslat(i),dx,dy,rad)
              works(i) = rad
            end do 
  
            ! Find mean temperature
            tbar = 0.0
            rpts = 0.0
            do i=1,nxas
              if (works(i) .gt. tcrmax) exit 
              tbar = tbar + t(i)
              rpts = rpts + 1.0
            end do 
  
            tbar = tbar/rpts
  
            do i=1,nxas
              if (works(i) .gt. tcrmax) exit 
              dtt = t(i)-tbar
              if (dtt .lt. dt) then
                t(i) = tbar + dt + (dtt-dt)*dtred
              endif
            end do

            return

          end subroutine tcors
          
          subroutine tcorsv(mxas,nxas,mpas,tas,tn1000,pamsu,clwas, &
                            aslon,aslat,clwth1,clwth2,kp1,kp2)
          ! This routine adjusts the temperature profile t to a constant
          ! lapse rate profile in regions where clw > clwth1. For clw > clwth2,
          ! The original profile is completely replaced by the constant lapse
          ! rate profile between pressure levels kp1 and kp2. The constant
          ! lapse rate is determined from the amsu temperature at pressure
          ! level kp1 and the NCEP analysis temperature at 1000 mb.
            use cons
            use ncepfg
            use utils
            use io

            integer,parameter :: ixmax=361,iymax=181

            integer,intent(in) :: mxas,nxas,mpas,kp1,kp2
            real,intent(inout) :: tas(mxas,mpas)
            real,intent(inout) :: clwas(mxas),aslon(mxas),aslat(mxas)
            real, intent(in) :: tn1000(ixmax,iymax)
            real, intent(in) :: pamsu(mpas)
            real, intent(in) :: clwth1,clwth2
            
            real :: p1,p2,clwt,wtam,wtcl,t1,t2,tcl,gmma,aa
            integer :: icnt,izp,ierr
            integer :: i,k
            ! Assign pressure levels for constant lapse rate atmosphere
            p1 = 1000.0e+2 
            p2 = 100.0*pamsu(kp1)

            ! Search for points that require adjustment
            icnt = 0
            do i=1,nxas
              if (clwas(i) .gt. clwth1) then

                ! Calculate temperature profile weights
                clwt = clwas(i)
                if (clwt .gt. clwth2) then
                  wtam = 0.0
                  wtcl = 1.0
                else
                  wtam = (clwth2-clwt)/(clwth2-clwth1)
                  wtcl = (clwt-clwth1)/(clwth2-clwth1)
                endif
  
                ! Calculate parameters for the constant lapse rate
                ! atmosphere
                t2 = tas(i,kp1)
                izp = 0
                call llintsp(tn1000,rlonln,rlatbn,dlonn,dlatn,ixmax,iymax, &
                             nlonn,nlatn,t1,aslon(i),aslat(i),izp,ierr) 
                gmma = (g/rd)*alog(t2/t1)/alog(p1/p2)
                aa   = -rd*gmma/g
  
                icnt = icnt + 1
                write(lulog,300) icnt,aslat(i),aslon(i),clwt,gmma
                300 format(i4,' lat,lon: ',f6.2,1x,f7.2,' clw: ',f5.2, &
                              ' gmma:',e11.4)
  
                do k=kp1,kp2
                  tcl = t1*(100.0*pamsu(k)/p1)**aa
                  tas(i,k) = wtam*tas(i,k) + wtcl*tcl
                end do

              end if
            end do

            return

          end subroutine tcorsv

          subroutine tcorsr(mxas,nxas,t,clw)
          ! This routine reduces the temperature anomaly by using the
          ! slope relationship between cloud liquid water and temperature
          ! anomalies (base temperature comes from regions with no clw).
          ! This program also adjusts the temperatures over the land areas
          ! where clw is not available. In this case, if negative temperature
          ! anomalies exist over land they are replaced with a temperature .2
          ! degrees colder than the mean + twenty percent of the original
          ! temperature anomaly.
          ! This version uses the pre-analyzed swath temperature data.

            integer,intent(in) :: mxas,nxas
            real,intent(inout) :: t(mxas)
            real,intent(in) :: clw(mxas)

            real :: tbar,rpts,sxx,sxy,slope
            integer :: npts
            integer :: i

            ! Find mean temperature
            tbar = 0.0
            npts=0
            rpts = float(nxas)
            do i=1,nxas
              if (clw(i) .le. 0.001 .and. clw(i) .gt. -1.0) then
                tbar = tbar + t(i)
                npts=npts+1
              endif
            end do

            tbar = tbar/float(npts) ! mean
            sxy=0.0      ! Sum of x*y
            sxx=0.0      ! Sum of x^2

            do i=1,nxas
              if (clw(i) .gt. 0.0) then
                sxx=sxx+clw(i)**2
                sxy=sxy+clw(i)*(t(i)-tbar)
              end if
            end do

            slope=sxy/sxx
!           write(6,*) 'tbar,npts,slope ',tbar,npts,slope

            ! Over land temperature fix
            do i=1,nxas
              if (slope.lt.0 .and. clw(i) .ge. 0.0) then
                t(i) = t(i)-slope*clw(i)
              elseif(clw(i).lt.-50.and.(t(i)-tbar).lt.-0.2)then
                t(i) = tbar+(-0.2+0.2*(t(i)-tbar))
              endif
            end do

            return

          end subroutine tcorsr

          subroutine tcor(nx,ny,t,dt,dtred)
          ! This routine reduces the temperature anomaly below the
          ! threshold dt by a factor dtred. This version uses the analyzed
          ! temperature data.

            integer,intent(in) :: nx,ny
            real,intent(inout) :: t(nx,ny)
            real,intent(in) :: dt,dtred

            real :: tbar,rpts,dtt
            integer :: i,j

            ! Find mean temperature
            tbar = 0.0
            rpts = float(nx*ny)
            do j=1,ny
              do i=1,nx
                tbar = tbar + t(i,j)
              end do
            end do

            tbar = tbar/rpts

            do j=1,ny
              do i=1,nx
                dtt = t(i,j)-tbar
                if (dtt .lt. dt) then
                  t(i,j) = tbar + dt + (dtt-dt)*dtred
                end if
              end do
            end do

            return

          end subroutine tcor

          subroutine tcorclw(mxas,nxas,amkl,tas,clwas,k)
          ! This routine corrects for CLW attenuation using temp and CLW
          ! on the swath.  Regression slopes at each pressure level
          ! were derived by comparing tdev vs CLW for 64 noland storms.
            use io

            integer,intent(in) :: mxas,nxas,k
            real,intent(inout) :: tas(mxas)
            real,intent(in) :: clwas(mxas),amkl
 
            integer, parameter :: clwthres = 0.3
            real :: oldtas(mxas)
            integer :: i

            ! Read in initial temp values
            do i=1,nxas
              oldtas(i) = tas(i)
            end do

            ! Values greater than clwthres are corrected
            do i=1,nxas
              if (clwas(i) .gt. clwthres) then
                tas(i) = oldtas(i) + (amkl*clwas(i))
              endif
            end do

            return

          end subroutine tcorclw

          subroutine tcorice(k,nx,ny,t,clwxy,told,tnew,diff, &
                             atcfid,imon,iday,itime)
          ! This routine reduces the temperature anomaly via Ben Linstid's
          ! algorithm.  It corrects for ice crystal attenuation where
          ! temp < (tmean - thrvar); uses Poisson's (actually Laplace's b/c equ=0)
          ! equation to replace bad temps with ave of nearest 2 to 4 neighbors.  
            use io

            integer,intent(in) :: k,nx,ny,imon,iday,itime
            character(len=*) :: atcfid
            real,intent(inout) :: t(nx,ny),clwxy(nx,ny),told(nx,ny)
            real,intent(inout) :: tnew(nx,ny),diff(nx,ny)

            real :: clwaa,thrvar,tol,smoothmx
            real :: tmean,cnt,thresh,smoothct,diffmax
            integer :: iice,icnt
            integer :: i,j

            ! Specify parameters
            clwaa=0.2 
            thrvar = 0.5
            ! tol = 0.001
            tol = 0.005
            smoothmx = 1000.0

            ! Find mean temperature of points with clwxy < clwaa to establish thresh
            tmean = 0.0
            cnt = 0.0
            do j=1,ny
              do i=1,nx
                told(i,j) = t(i,j)
                if (clwxy(i,j) .lt. clwaa) then
                  tmean = tmean + t(i,j)
                  cnt = cnt + 1.0
                end if
              end do
            end do

            tmean = tmean/cnt
            thresh = tmean - thrvar

            ! Count number of points to be corrected
            iice = 0
            do j=1,ny
              do i=1,nx
                if (t(i,j) .lt. thresh) iice=iice+1
              end do
            end do

            ! Correct where tprime is less than some threshold using
            ! Poisson/Laplace's equation of averages.  Exit subroutine if 
            ! temperature don't converge before mxsmooth number of iterations.
            smoothct = 0.0

         99 continue

            diffmax = 0.0
            smoothct = smoothct + 1.0
            if (smoothct .eq. smoothmx) then
              write(lulog,969) 'NO temp convergence ',k,atcfid,imon,iday,itime
              969 format(a20,i2,1x,a6,1x,3(i2.2))
              goto 101
            endif

            do j=1,ny
              do i=1,nx
                if (t(i,j) .lt. thresh) then
                  if (i.eq.1 .and. j.eq.1) then
                    tnew(i,j) = (told(i,j+1) + told(i+1,j))/2.0
                  else if (i.eq.nx .and. j.eq.1) then
                    tnew(i,j) = (told(i-1,j) + told(i,j+1))/2.0
                  else if (i.eq.1 .and. j.eq.ny) then
                    tnew(i,j) = (told(i,j-1) + told(i+1,j))/2.0
                  else if (i.eq.nx .and. j.eq.ny) then
                    tnew(i,j) = (told(i-1,j) + told(i,j-1))/2.0
		    else if (i.eq.1 .and. j.gt.1 .and. j.lt.ny) then
                    tnew(i,j) = (told(i,j+1) + told(i+1,j) + told(i,j-1))/3.0
                  else if(i.eq.nx .and. j.gt.1 .and. j.lt.ny) then
                    tnew(i,j) = (told(i,j+1) + told(i-1,j) + told(i,j-1))/3.0
                  else if(j.eq.1 .and. i.gt.1 .and. i.lt.nx) then
                    tnew(i,j) = (told(i+1,j) + told(i,j+1) + told(i-1,j))/3.0
                  else if(j.eq.ny .and. i.gt.1 .and. i.lt.nx) then
                    tnew(i,j) = (told(i+1,j) + told(i,j-1) + told(i-1,j))/3.0
                  else
                    tnew(i,j) = ((told(i+1,j) + told(i-1,j) + told(i,j+1) + &
                                told(i,j-1))/4.0)
                  endif
                else
                  tnew(i,j) = told(i,j)
                endif

                diff(i,j) = abs(tnew(i,j) - told(i,j))
                if (diff(i,j) .gt. diffmax) then
                  diffmax = diff(i,j)
                endif

              end do
            end do

            if (diffmax .gt. tol) then
              do j=1,ny
                do i=1,nx
                  told(i,j) = tnew(i,j)
                end do
              end do
              goto 99
            endif

            ! Copy corrected temperature data into old array to return
            do j=1,ny
              do i=1,nx
                t(i,j) = tnew(i,j)
              end do
            end do

        101 continue

            icnt = nint(cnt)
            write(lulog,400) iice,nx*ny
            400 format(/,' Ice correction completed, ',i5,' of ',i5, &
                       ' points adjusted')

            ! Output number of smoothing iterations
            970 format('Ice correction used ',i6.1, &
                       ' smoothing iterations for level ',i2)
            return

          end subroutine tcorice

          subroutine fadj(f,fa)
          ! This routine adjusts the Coriolis parameter f near the
          ! equator to keep it from going to zero
            real,intent(in) :: f
            real,intent(inout) :: fa
 
            real :: fmin,absf,sgnf

            fmin = 1.0e-6
            absf  = abs(f)
            sgnf  = f/absf
  
            if (absf .lt. fmin) then
              fa = fmin*sgnf
            else
              fa = f
            end if

            return

          end subroutine fadj

          subroutine wloc(sslat,sslon,aslat,aslon,luloc,np, &
                          jyr,jmon,jday,jtimeh,jtimem,atcfid,sname)
          ! This routine writes the storm center position (sslat,sslon)
          ! and AMSU data locations to a file for later plotting.
            use sinfo3

            integer,intent(in) :: np,luloc,jyr,jmon,jday,jtimeh,jtimem
            real,intent(in) :: sslat,sslon
            real,intent(in) :: aslat(np),aslon(np)
            character(len=6),intent(in) :: atcfid
            character(len=10),intent(in) ::  sname
     
            integer :: i

            ! Write label
            write(luloc,210) instr,jyr,jmon,jday,jtimeh,jtimem,atcfid,sname
            210 format(a4,' SWATH ',i4.4,1x,i2.2,i2.2,1x,i2.2,i2.2, &
                      ' UTC ',a6,1x,a10)

            write(luloc,200) sslon,sslat
            200 format(f8.2,f7.2)

            do i=1,np
              write(luloc,200) aslon(i),aslat(i)
            end do

            return

          end subroutine wloc

          subroutine barr(fk,flat,flon,nk,efld,exfac,rinf, &
                          rlat,rlon,fr,rr,nr,ierr)
          ! This routine performs a single pass Barnes analysis
          ! of the unevenly values fk to give fr on an evenly
          ! spaced radial grid.
          ! 
          ! Input:
          ! fk   - function values for the analysis
          ! flat - latitudes  (deg N positive) of fk
          ! flon - longitudes (deg E positive) of fk
          ! nk   - number of fk points
          ! efld - e-folding radius (km) for Barnes analysis
          ! exfac- expansion factor to increasing efld as a function of radius
          ! rinf - Influence radius (km) for Barnes analysis
          ! rlat - latitude of center of radial grid (deg N)
          ! rlon - longitude of center of radial grid (deg E)
          ! rr   - radial grid points (m)
          ! nr   - Number of radial grid points
          ! 
          ! Output:
          ! fr  - Analysis of fk on analysis grid
          ! ierr - error flat (0=normal completion)
          !                   (1=error during analysis)

            integer,intent(in) :: nr,nk
            real,intent(in) :: rlat,rlon,efld,exfac,rinf
            real,intent(in) :: fk(nk),flat(nk),flon(nk),rr(nr)
            real,intent(inout) :: fr(nr)
            integer,intent(inout) :: ierr

            integer :: i,j,k,ii
            real :: rmax,wtsum,rkm,efldt,dx,dy,rad,dnorm,wt

            ! Initialize fr to zero
            ierr=0
            do i=1,nr
              fr(i) = 0.0
            end do

            rmax = rr(nr)/1000.0

            ! Perform analysis
            do i=1,nr
              wtsum = 0.0
              rkm   = rr(i)/1000.0
              efldt = efld*(1.0 + exfac*rkm/rmax)
              do k=1,nk
                call distk(rlon,rlat,flon(k),flat(k),dx,dy,rad)
                rad = abs(rad-rkm)
                if (rad .le. rinf) then
                  dnorm = rad/efldt
                  wt = exp(-dnorm*dnorm)
                  wtsum = wtsum + wt
                  fr(i) = fr(i) + wt*fk(k)
                end if
              end do

              if (wtsum .gt. 0.0) then
                fr(i) = fr(i)/wtsum
              else
                do ii=1,nr
                  fr(ii) = 0.0
                end do
                ierr=1
                return
              endif
          
            end do

            return

          end subroutine barr

          subroutine barxy(fk,flat,flon,nk,efld,rinf,ispf, &
                           rlat,rlon,fxy,nx,ny,ierr)
          ! This routine performs a Barnes analysis
          ! of the unevenly values fk to give fxy on an evenly
          ! spaced lat/lon grid. If ispf=1, a second pass is performed.

          ! Input:
          ! fk   - function values for the analysis
          ! flat - latitudes  (deg N positive) of fk
          ! flon - longitudes (deg E positive) of fk
          ! nk   - number of fk points
          ! efld - e-folding radius (km) for Barnes analysis
          ! rinf - Influence radius (km) for Barnes analysis
          ! rlat - 1-D array of latitudes of analysis grid
          ! rlon - 1-D array of longitudes of analysis grid
          ! ispf - flag for second Barnes pass (ispf=1)
          ! nx   - Number of longitudes on analysis grid
          ! ny   - Number of latitudes on analysis grid

          ! Output
          ! fxy  - Analysis of fk on analysis grid
          ! ierr - error flat (0=normal completion)
          !        (1=error during analysis)
            use utils

            integer,intent(in) :: nk,nx,ny,ispf
            real,intent(in) :: efld,rinf
            real,intent(inout) :: fk(nk),flat(nk),flon(nk)
            real,intent(in) :: rlon(nx),rlat(ny)
            real,intent(inout) :: fxy(nx,ny)
            integer,intent(inout) :: ierr

            ! Work arrays
            integer,parameter :: mxas=30000
            real :: fki(mxas)
            real :: iflag(mxas)
 
            real :: wtsum,dx,dy,rad,dnorm,wt
            real :: slat1,slon1,dlat1,dlon1,fsp,fxyt
            integer :: izp,ierrt
            integer :: i,j,k,ii,jj

            ! Initialize fxy to zero
            ierr=0
            do j=1,ny
              do i=1,nx
                fxy(i,j) = 0.0
              end do
            end do

            ! Perform analysis
            do j=1,ny
              do i=1,nx
                wtsum = 0.0
                  do k=1,nk
                  call distk(rlon(i),rlat(j),flon(k),flat(k),dx,dy,rad)
                  if (rad .le. rinf) then
                    dnorm = rad/efld
                    wt = exp(-dnorm*dnorm)
                    wtsum = wtsum + wt
                    fxy(i,j) = fxy(i,j) + wt*fk(k)
                  end if
                end do

                if (wtsum .gt. 0.0) then
                  fxy(i,j) = fxy(i,j)/wtsum
                else
                  do jj=1,ny
                    do ii=1,nx
                      fxy(ii,jj) = 0.0
                    end do
                  end do

                  ierr=1
                  return

                endif

              end do
            end do

            if (ispf .le. 0) return

            ! Perform second pass of Barnes analysis

            ! Interpolate analysis variable back to observation locations
            ! and calcualte deviations from data input values
            slat1 = rlat(1)
            slon1 = rlon(1)
            dlat1 = rlat(2)-rlat(1)
            dlon1 = rlon(2)-rlon(1)
            izp   = 0

            do k=1,nk
              call llintsp(fxy,slon1,slat1,dlon1,dlat1,nx,ny,nx,ny, &
                     fsp,flon(k),flat(k),izp,ierrt)

              if (ierrt .eq. 0) then
                fki(k)   = fk(k)-fsp
                iflag(k) = 0
              else
                fki(k)   = 0.0
                iflag(k) = 1
              endif
            end do

            ! Perform analysis of deviations
            do j=1,ny
              do i=1,nx
                fxyt = 0.0
                wtsum = 0.0
                do k=1,nk
                  call distk(rlon(i),rlat(j),flon(k),flat(k),dx,dy,rad)
                  if (rad .le. rinf .and. iflag(k) .eq. 0) then
                    dnorm = rad/efld
                    wt = exp(-dnorm*dnorm)
                    wtsum = wtsum + wt
                    fxyt = fxyt + wt*fki(k)
                  end if
                end do

                if (wtsum .gt. 0.0) then
                  fxyt = fxyt/wtsum
                else
                  fxyt = 0.0
                endif

                fxy(i,j) = fxy(i,j) + fxyt

              end do
            end do

            return

          end subroutine barxy

          subroutine altonl(z,t,p,za,ta,pn,ts,ps,nx,ny,np,npn)
          ! This routine extracts z,t at NCEP pressure levels 
          ! from za,ta at AMSU pressure levels. Interpolation
          ! is used if necessary.
            use utils
            use hstatic
            use io
            use sinfo3

            integer,intent(in) :: nx,ny,np,npn
            real,intent(in) :: z(nx,ny,np),t(nx,ny,np),p(np)
            real,intent(inout) :: za(nx,ny,npn),ta(nx,ny,npn),pn(npn)
            real,intent(in) :: ts(nx,ny),ps(nx,ny)

            ! Local variable
            integer :: iamsuf(100)

            real :: pup,plo,pt,zt,tt,p1,t1,z1,p2,t2,z2
            integer :: i,j,k,kk,kup

            ! Initialize flag variable to zero
            do k=1,npn
              iamsuf(k) = 0
            end do

            ! Extract AMSU t,z at NCEP levels for output
            do k=1,npn
              do kk=1,np
                if (p(kk) .eq. pn(k)) then
                  iamsuf(k) = 1

                  do j=1,ny
                    do i=1,nx
                      za(i,j,k) = z(i,j,kk)
                      ta(i,j,k) = t(i,j,kk)
                    end do
                  end do

                  exit
                end if

              end do
            end do

            ! Interpolate between amsu levels for t,z at ncep levels, if necessary
            do k=1,npn
              if (iamsuf(k) .eq. 0) then
              ! Find amsu level just above current ncep level
                do kk=1,np
                  if (p(kk) .lt. pn(k)) then
                    kup = kk
                    pup = p(kk)
                  end if
                end do

                if (kup .eq. np) then
                  plo = 1070.0e+2
                else
                  plo = p(kup+1)
                end if

                write(lulog,200) pn(k)/100.0,instr,pup/100.0,plo/100.0
                200 format(/,' Interpolate T,Z at p=',f5.0, &
                           ' from available ',a4,' levels ',f5.0,1x,f5.0)

                do j=1,ny
                  do i=1,nx
                    pt = pn(k)

                    p2 = p(kup)
                    t2 = t(i,j,kup)
                    z2 = z(i,j,kup)

                    if (kup .eq. np) then
                      p1 = ps(i,j)
                      t1 = ts(i,j)
                      z1 = 0.0
                    else
                      p1 = p(kup+1)
                      t1 = t(i,j,kup+1)
                      z1 = z(i,j,kup+1)
                    end if

                    call ztint(p1,p2,z1,z2,t1,t2,pt,zt,tt)
                    ta(i,j,k) = tt
                    za(i,j,k) = zt
                  end do
                end do

              end if

            end do

            return

          end subroutine altonl

          subroutine rzwrit(f,scale,nr,dr,nz,dz,slat,slon,atcfid,sname, &
                            jyr,jmon,jday,jtimeh,jtimem,luout,label)
          ! This routine writes f to a file for later plotting
            use io

            integer,intent(in) :: nr,nz
            real,intent(in) :: f(nr,nz),dr,dz,slat,slon
            real,intent(inout) :: scale
            character(len=6),intent(in) :: atcfid
            character(len=10),intent(in) :: sname
            character(len=20),intent(in) :: label
            integer,intent(in) :: jyr,jmon,jday,jtimeh,jtimem,luout

            ! Local array
            integer,parameter :: mxd=5000
            real :: dum(mxd)

            integer :: npts,i,j,k

            ! Make sure dummy array is large enough
            npts = nr*nz
            if (mxd .lt. npts) then
              write(lulog,900) mxd
              900 format(/,'Dimension mxd too small in routine rzwrit: ',i4)
              stop
            endif

            ! Check scale factor
            if (scale .le. 0.0) scale = 1.0

            ! Write header lines
            write(luout,200) label,jyr,jmon,jday,jtimeh,jtimem,atcfid,sname
            200 format(a20,1x,i4.4,1x,i2.2,i2.2,1x,i2.2,i2.2,1x,a6,1x,a10)
            write(luout,210) slat,slon,dr/1000.,nr,dz/1000.,nz
            210 format(f5.1,1x,f6.1,1x,f6.2,1x,i3,1x,f6.2,1x,i3)
 
            ! Put f in 1-d array and scale for printing
            k = 0
            do j=1,nz
              do i=1,nr
                k = k + 1
                dum(k) = scale*f(i,j)
              end do
            end do

            ! Write dum to file
            write(luout,220) (dum(k),k=1,npts)
            220 format(8(f9.2,1x))

            return

          end subroutine rzwrit

          subroutine xywrit(f,scale,nx,rlonl,rlonr,ny,rlatb,rlatt, &
                            slat,slon,atcfid,sname,jyr,jmon,jday, &
                            jtimeh,jtimem,luout,label)
          ! This routine writes f to a file for later plotting
            use io

            integer,intent(in) :: nx,ny,jyr,jmon,jday,jtimeh,jtimem
            integer,intent(in) :: luout
            real,intent(inout) :: scale
            real,intent(in) :: rlonl,rlonr,rlatb,rlatt,slat,slon
            real,intent(in) :: f(nx,ny)
            character(len=6),intent(in) :: atcfid
            character(len=10),intent(in) :: sname
            character(len=20),intent(in) :: label

            ! Local array
            integer,parameter :: mxd=5000
            real ::dum(mxd)
            integer :: npts,i,j,k

            ! Make sure dummy array is large enough
            npts = nx*ny
            if (mxd .lt. npts) then
              write(lulog,900) mxd
              900 format(/,'Dimension mxd too small in routine xywrit: ',i4)
              stop
            end if

            ! Check scale factor
            if (scale .le. 0.0) scale = 1.0

            ! Write header lines
            write(luout,200) label,jyr,jmon,jday,jtimeh,jtimem,slat,slon, &
                             atcfid,sname
            200 format(a20,1x,i4.4,1x,i2.2,i2.2,1x,i2.2,i2.2,1x,f5.1,1x,f6.1, &
                       1x,a6,1x,a10)
            write(luout,210) rlonl,rlonr,nx,rlatb,rlatt,ny
            210 format(f6.1,1x,f6.1,1x,i3,1x,f6.1,1x,f6.1,1x,i3)

            ! Put f in 1-d array and scale for printing
            k = 0
            do j=1,ny
              do i=1,nx
                k = k + 1
                dum(k) = scale*f(i,j)
              end do
            end do

            ! Write dum to file
            write(luout,220) (dum(k),k=1,npts)
            220 format(8(f9.2,1x))

            return
          end subroutine xywrit

          subroutine tcwrit(p,ps,ts,t,clw,nx,ny,np,lutcp,label, &
                            x1,x2,y1,y2)

          ! This routine writes the surface P,T, the temperature at the AMSU
          ! pressure levels and cloud liquid water to a file

            integer,intent(in) :: nx,ny,np,lutcp
            real,intent(in) :: p(np)
            real,intent(in) :: ps(nx,ny),ts(nx,ny)
            real,intent(in) :: t(nx,ny,np),clw(nx,ny)
            real,intent(in) :: x1,x2,y1,y2
            character(len=20),intent(in) :: label
     
            integer :: i,j,k

            ! Write file header
            write(lutcp,200) label,nx,x1,x2,ny,y1,y2
            200 format(' STORMID ',a13,1x,' x: ',i2,1x,f6.1,1x,f6.1, &
                               ' y: ',i2,1x,f6.1,1x,f6.1)

            ! Write the cloud liquid water
            write(lutcp,201)
            201 format(' CLOUD LIQUID WATER')
            write(lutcp,210) ((clw(i,j),i=1,nx),j=1,ny)
            210 format(10(f6.2,1x))

            ! Write the surface pressure
            write(lutcp,202)
            202 format(' SURFACE PRESSURE (MB)')
            write(lutcp,211) ((ps(i,j)/100.,i=1,nx),j=1,ny)
            211 format(10(f6.1,1x))

            ! Write the surface temperature
            write(lutcp,203)
            203 format(' SURFACE TEMPERATURE (K)')
            write(lutcp,210) ((ts(i,j),i=1,nx),j=1,ny)

            ! Write the temperatures at each pressure level
            do k=1,np
              write(lutcp,204) p(k)/100.
              204 format(' TEMPERATURE (K) AT P= ',f5.0,' MB')
              write(lutcp,210) ((t(i,j,k),i=1,nx),j=1,ny)
            end do

            return

          end subroutine tcwrit

          subroutine tcrwrit(prz,rhorz,trz,vrz,nr,nz,sslat,sslon,lutcrp, &
                             label,coord,times,r1,r2,z1,z2)
          ! This routine writes the p,rho,t and v as a function of r,z
          ! to a file

            integer,intent(in) :: nr,nz,lutcrp
            real,intent(in) :: prz(nr,nz),rhorz(nr,nz),trz(nr,nz),vrz(nr,nz)
            character(len=23),intent(in) :: label
            real,intent(in) :: sslat,sslon,r1,r2,z1,z2

            character(len=90) :: coord,times
            real :: r1k,r2k,z1k,z2k
            integer :: i,j

            ! Write file header
            r1k = r1/1000.
            r2k = r2/1000.
            z1k = z1/1000.
            z2k = z2/1000.

            write(lutcrp,199) coord
            write(lutcrp,199) times
            199 format(a90)

            write(lutcrp,200) label,nr,r1k,r2k,nz,z1k,z2k,sslat,sslon
            200 format(a23,' r(km): ',i2,1x,f6.1,1x,f6.1, &
                           ' z(km): ',i2,1x,f6.1,1x,f6.1, &
                              ' lat=',f6.2,' lon=',f7.2)

            ! Write the pressure
            write(lutcrp,201)
            201 format(' PRESSURE (MB)')
            write(lutcrp,211) ((prz(i,j)/100.,i=1,nr),j=1,nz)
            211 format(10(f6.1,1x))

            ! Write the density
            write(lutcrp,202)
            202 format(' DENSITY (KG/M3)')
            write(lutcrp,212) ((rhorz(i,j),i=1,nr),j=1,nz)
            212 format(10(f6.4,1x))

            ! Write the surface temperature
            write(lutcrp,203)
            203 format(' TEMPERATURE (K)')
            write(lutcrp,213) ((trz(i,j),i=1,nr),j=1,nz)
            213 format(10(f6.2,1x))

            ! Write the gradient wind
            write(lutcrp,204)
            204 format(' GRADIENT WIND (M/S)')
            write(lutcrp,214) ((vrz(i,j),i=1,nr),j=1,nz)
            214 format(10(f6.2,1x))

            return
          end subroutine tcrwrit

          subroutine spata(f1,f2,p,nx,ny,np,ibrem,label)
          ! This routine calculates area averages of f1 and f2.
          ! If ibrem=1, a constant value to the first field (f1)
          ! is added so that the average difference between the fields
          ! is zero, each each pressure level.
            use io

            integer,intent(in) :: nx,ny,np,ibrem
            real,intent(inout) :: f1(nx,ny,np),f2(nx,ny,np),p(np)
            character(len=20),intent(in) :: label

            real :: rpts,f1m,f2m,pmb
            integer :: i,j,k

            write(lulog,200) label
            200 format(/,'Domain average f1,f2: ',a20)

            rpts = float(nx*ny)
            do k=1,np
              f1m = 0.0
              f2m = 0.0
              do j=1,ny
                do i=1,nx
                  f1m = f1m + f1(i,j,k)
                  f2m = f2m + f2(i,j,k)
                end do
              end do
              f1m = f1m/rpts
              f2m = f2m/rpts

              pmb = p(k)/100.0

              write(lulog,210) f1m,f2m,pmb
              210 format(' f1=',f8.1,' f2=',f8.1,'  p=',f5.0)

              if (ibrem .eq. 1) then
                do j=1,ny
                  do i=1,nx
                    f1(i,j,k) = f1(i,j,k) + (f2m-f1m)
                  end do
                end do
              endif

            end do

            if (ibrem .eq. 1) then
              write(lulog,220)
              220 format( &
                ' f1 adjusted to make the mean equal to the f2 mean')
            end if

            return

          end subroutine spata

          subroutine spata1(f1,f2,nx,ny,ibrem,label)
          ! This routine calculates area averages of f1 and f2.
          ! If ibrem=1, a constant value to the first field (f1)
          ! is added so that the average difference between the fields
          ! is zero.

          ! This routine is similar to spata, but for a f1 and f2 defined at
          ! a single pressure level
            use io

            integer,intent(in) :: nx,ny,ibrem
            real,intent(inout) :: f1(nx,ny),f2(nx,ny)
            character(len=20),intent(in) :: label
  
            real :: rpts,f1m,f2m
            integer :: i,j

            write(lulog,200) label
            200 format(/,'Domain average f1,f2: ',a20)

            rpts = float(nx*ny)
            f1m = 0.0
            f2m = 0.0
            do j=1,ny
              do i=1,nx
                f1m = f1m + f1(i,j)
                f2m = f2m + f2(i,j)
              end do
            end do
            f1m = f1m/rpts
            f2m = f2m/rpts
  
            write(lulog,210) f1m,f2m
            210 format(' f1=',f8.1,' f2=',f8.1)
  
            if (ibrem .eq. 1) then
              do j=1,ny
                do i=1,nx
                  f1(i,j) = f1(i,j) + (f2m-f1m)
                end do
              end do
            endif
  
            if (ibrem .eq. 1) then
              write(lulog,220)
              220 format( &
                ' f1 adjusted so to make the mean equal to the f2 mean')
            end if
  
            return

          end subroutine spata1

          subroutine swata(f1,p,mx,np,nx,label)
          ! This routine calculates the average of f1 at the swath points
            use io

            integer,intent(in) :: mx,np,nx
            real,intent(in) :: f1(mx,np),p(np)
            character(len=20),intent(in) :: label

            integer :: i,k
            real :: rpts,f1m,pmb

            write(lulog,200) label
            200 format(/,'Swath average f1: ',a20)

            rpts = float(nx)
            do k=1,np
              f1m = 0.0
              do i=1,nx
                f1m = f1m + f1(i,k)
              end do
              f1m = f1m/rpts

              pmb = p(k)

              write(lulog,210) f1m,pmb
              210 format(' f1=',f6.1,'  p=',f6.1)
            end do

            return

          end subroutine swata
       
          subroutine qualcon(tas,clwas,tpwas,aslat,aslon,pamsu,dumas, &
                             dumasp,idumas,mxas,mpas,nxas,np,npst)
          ! This routine quality controls the AMSU temperatures by comparing
          ! them with standard atmosphere temperatures. If the temperature
          ! difference between the AMSU and standard atmosphere exceeds a
          ! specified amount (devm) at any of the levels to be analyzed, 
          ! the temperatures for the entire column, the clw and tpw are 
          ! eliminated. The varible containing the number of AMSU swath points
          ! (nxas) is reduced to accnt for any deleted data points.
            use utils
            use io
            use sinfo3

            integer,intent(in) :: mxas,mpas,np,npst
            integer,intent(inout) :: nxas
            real,intent(inout) :: tas(mxas,mpas),clwas(mxas),tpwas(mxas)
            real,intent(inout) :: aslat(mxas),aslon(mxas),pamsu(mpas)
            real,intent(inout) :: dumas(mxas),dumasp(mxas,mpas)
            integer,intent(inout) :: idumas(mxas)

            ! Specify maximum allowable temperature deviation (C)
            real,parameter :: devm = 40.0

            ! Local arrays 
            real :: p(200),st(200)
  
            real :: pmb,zstd,tstd,thetas,dev,distth
            integer :: iland,nbad,nxasq
            integer :: i,j,k,ii,kk

            write(lulog,200) instr,devm
            200 format(/, &
                       ' Quality control ',a4,' temperatures with devm= ',&
                       f5.1)

            ! Calculate standard atmosphere temperatures
            ! and extract amsu level pressures
            do k=1,np
              kk = npst+k-1
              pmb = pamsu(kk)
              p(k) = pmb
  
              if (pmb .lt. 10.0) then
                write(lulog,210) pmb
                210 format( &
                    ' Quality control not designed for p<10 mb, p=' &
                    ,f6.1) 
                stop
              end if

              ! Find standard atmosphere temperature
              call stndz(pmb,zstd,tstd,thetas)
              st(k) = tstd

              ! write(lulog,220) pmb,tstd,zstd/1000.0
              ! 220 format(1x,' p (mb)=',f6.1,' tstd (K)=',f6.1,1x, &
              !            ' zstd (km)=',f5.1)
            end do

            ! Start loop over swath points
            do i=1,nxas
              ! Initialize error flag to zero
              idumas(i) = 0

              ! Check all pressure levels at current location
              do k=1,np
                kk = npst+k-1
                dev = abs(tas(i,kk)-st(k))
                if (dev .gt. devm) then
                  idumas(i) = 1
                  write(lulog,230) aslat(i),aslon(i),p(k),tas(i,kk)
                  230 format(' Bad temp at lat/lon: ',f7.2,1x,f7.2, &
                             ' p=',f6.1,1x,' t=',f8.1)
                  exit
                end if
              end do

            end do

            iland=1
            distth=0.0

            ! Eliminate land points
!           if (iland .eq. 0) then
!             do i=1,nxas
!               write(6,*) aslon(i),aslat(i)
!               call aland(aslon(i),aslat(i),dist)
!               write(6,*) dist
!               if (dist .le. distth) idumas(i) = 1
!             end do
!           endif

            ! Count the number of bad data points
            nbad = 0
            do i=1,nxas
              if (idumas(i) .eq. 1) then
                nbad = nbad + 1
              endif
            end do

            if (nbad .gt. 0) then 

              nxasq = nxas - nbad

              ! Eliminate bad data points
              ! ** Temperatures
              ii = 0
              do i=1,nxas
                if (idumas(i) .eq. 0) then
                  ii = ii + 1
                  do k=1,np
                    kk = npst+k-1
                    dumasp(ii,kk) = tas(i,kk)
                  end do
                end if
              end do

              do i=1,nxasq
                do k=1,np
                  kk = npst+k-1
                  tas(i,kk) = dumasp(i,kk)
                end do
              end do

              ! ** clw,tpw,lat,lon
              ii = 0
              do i=1,nxas
                if (idumas(i) .eq. 0) then
                  ii = ii + 1
                  dumasp(ii,1) = clwas(i)
                  dumasp(ii,2) = tpwas(i)
                  dumasp(ii,3) = aslat(i)
                  dumasp(ii,4) = aslon(i)
                end if
              end do

              do i=1,nxasq
                clwas(i) = dumasp(i,1)
                tpwas(i) = dumasp(i,2)
                aslat(i) = dumasp(i,3)
                aslon(i) = dumasp(i,4)
              end do

              nxas = nxasq

            end if

            write(lulog,240) nbad
            240 format(/,i4,' data points eliminated by quality control')

            return

          end subroutine qualcon
    
          subroutine delim(tas,clwas,tpwas,aslat,aslon, &
                           rlatmn,rlatmx,rlatbf,rlonmn,rlonmx,rlonbf, &
                           dumas,dumasp,idumas, &
                           mxas,mpas,nxas,np,npst) 
          ! This routine eliminates data that is outside of the analysis
          ! domain. Data is retained in a buffer region outside the domain
          ! defined by rlonbf and rlatbf.
            use io

            integer,intent(in) :: mxas,mpas,np,npst
            integer,intent(inout) :: nxas
            real,intent(inout) :: tas(mxas,mpas),clwas(mxas),tpwas(mxas)
            real,intent(in) :: rlatmn,rlatmx,rlatbf,rlonmn,rlonmx,rlonbf
            real,intent(inout) :: aslat(mxas),aslon(mxas)
            real,intent(inout) :: dumas(mxas),dumasp(mxas,mpas)
            integer,intent(inout) :: idumas(mxas)

            real :: rltn,rltx,rlnn,rlnx
            integer :: nex, nxasq
            integer :: i,j,k,ii,kk

            ! Initialize elimination flag to zero
            do i=1,nxas
              idumas(i) = 0
            end do

            ! Flag the points outside of the domain
            rltn = rlatmn-rlatbf
            rltx = rlatmx+rlatbf
            rlnn = rlonmn-rlonbf
            rlnx = rlonmx+rlonbf

            do i=1,nxas
              if (aslat(i) .lt. rltn .or. aslat(i) .gt. rltx) idumas(i)=1 
              if (aslon(i) .lt. rlnn .or. aslon(i) .gt. rlnx) idumas(i)=1 
            end do

            ! Count the number of excluded data points
            nex = 0
            do i=1,nxas
              if (idumas(i) .eq. 1) then
                nex = nex + 1
              endif
            end do

            if (nex .gt. 0) then 

              nxasq = nxas - nex
              ! Eliminate data points
              ! ** Temperatures
              ii = 0

              do i=1,nxas
                if (idumas(i) .eq. 0) then
                  ii = ii + 1
                  do k=1,np
                    kk = npst+k-1
                    dumasp(ii,kk) = tas(i,kk)
                  end do
                endif
              end do

              do i=1,nxasq
                do k=1,np
                  kk = npst+k-1
                  tas(i,kk) = dumasp(i,kk)
                end do
              end do

              ! ** clw,tpw,lat,lon
              ii = 0
              do i=1,nxas
                if (idumas(i) .eq. 0) then
                  ii = ii + 1
                  dumasp(ii,1) = clwas(i)
                  dumasp(ii,2) = tpwas(i)
                  dumasp(ii,3) = aslat(i)
                  dumasp(ii,4) = aslon(i)
                endif
              end do

              do i=1,nxasq
                clwas(i) = dumasp(i,1)
                tpwas(i) = dumasp(i,2)
                aslat(i) = dumasp(i,3)
                aslon(i) = dumasp(i,4)
              end do

            end if

            write(lulog,240) nex,nxas
            240 format(/,i5,' of ',i5, &
                       ' data points eliminated by routine delim')
            nxas = nxasq

            return

          end subroutine delim

          subroutine tdevtest(mxas,nxas,tas,clwas,lutdev,atcfid, &
                         imon,iday,itime)
          ! This routine outputs uncorrected temp deviations (from average at
          ! each pressure level) vs CLW, where tdev(i)=tas(i)-tmean
            integer,intent(in) :: mxas,nxas
            real,intent(in) :: tas(mxas)
            real,intent(inout) :: clwas(mxas)
            integer,intent(in) :: lutdev,atcfid,imon,iday,itime

            real :: tdev(mxas)
            real :: tmean,cnt
            integer :: i 

            ! Calculate average of temps at this pressure level
            ! Initialize tdev array in this loop also
            tmean = 0.0
            cnt = 0.0
            do i=1,nxas
              tmean = tmean + tas(i)
              cnt = cnt + 1.0
              tdev(i) = 0.0
            end do
            tmean=tmean/cnt

            do i=1,nxas
              tdev(i) = tas(i)-tmean
              if (clwas(i) .lt. 0) then
                clwas(i) = 0
              endif
              write(lutdev,971)atcfid,imon,iday,itime,tdev(i),clwas(i)
              971 format(a6,3(i2.2),1x,f8.3,1x,f6.2)
            end do

            return
       
          end subroutine tdevtest

          subroutine uvmac(u,v,t,jyr,jlday,jtime,intv)
          ! This routine prints u and v to an ASCII file that can be used
          ! to plot data in McIDAS. Every intv point is printed.
            use dims
            use ncepll
            use ncepp
            use utils

            real,intent(in) :: u(nx,ny,npn),v(nx,ny,npn),t(nx,ny,npn)
            integer,intent(in) :: jyr,jlday,jtime,intv

            integer :: lumac,imac,jdy,jmon,jyr2,jtime4
            real :: pmb,utem,vtem,spd,dir 
            integer :: i,j,k

            lumac = 66

            ! Set imac = 1 for original format for J. Dostalek
            ! or imac = 2 for modified format for J. Evans
            imac = 2

            open(file='uvamsu.dat',unit=lumac,form='formatted', &
                 status='unknown')

            if (jyr .lt. 2000) then
              jyr2 = jyr-1900
            else
              jyr2 = jyr-2000
            end if

            if (imac .eq. 1) then
              do j=1,ny,intv
                do i=1,nx,intv
                  do k=1,npn
                    pmb = pn(k)/100.0
                    utem = u(i,j,k)
                    vtem = v(i,j,k)
                    call ctor(utem,vtem,spd,dir)
                    dir = 270.-dir
                    if (dir .lt. 0.0) dir = dir+360.0
                    write(lumac,200) jyr2,jlday,jtime,rlatd(j), &
                                     -rlond(i),pmb,u(i,j,k),v(i,j,k),&
                                     spd,dir
                    200 format(1x,i2.2,i3,1x,i6,7(1x,f6.1))
                  end do
                end do
              end do
            else
              call jdayi(jlday,jyr,jmon,jdy)
              jtime4 = jtime/100

              do j=1,ny,intv
                do i=1,nx,intv
                  do k=1,npn
                    pmb = pn(k)/100.0

                    write(lumac,210) jyr,jmon,jdy,jtime4,rlatd(j), &
                                     rlond(i),pmb,u(i,j,k),v(i,j,k),&
                                     t(i,j,k)
                    210 format(1x,i4,1x,i2.2,i2.2,1x,i4.4,6(1x,f6.1))
                  end do
                end do
              end do

            end if

            close(lumac)
            return

          end subroutine uvmac

          subroutine AMSUfdeck(cbasin,istnum,cstype,iyr,mm,dd,hh,min, &
                                rlat,rlon,mslp,ivmx,ir34,ir50,ir64,irmw, &
                                isdist,lufix) 
          ! This subroutine creates an ATCF fdeck entry for an AMSU fix.
          !
          ! Written by:  John Knaff,CIRA
          ! July 2005
          !
          ! Last Modified: August 1, 2005
          !
          ! Language: FORTRAN 90
          !
          !***********************************************************************
          !     F-DECK structures 
          !***********************************************************************
          !        atcf fix formats as of 4/02/2002
          !
          !        The first 32 words are common followed by specific structure for
          !        1)Subjective Dvorak (DVTS)
          !        2)Objective Dvorak (DVTO)
          !        3)Microwave (SSMI,ERS2,SEAW,TRMM,AMSU,QSCT,ALTI,ADOS)
          !        4)Radar (RDRT,RDRD)
          !        5)Aircraft (AIRC)
          !        6)Dropsonde (DRPS)
          !        7)Analysis (ANAL)
          !
          !     This subroutine used structures for (3) above.
          !
          !**********************************************************************
            use sinfo3
            ! COMMON FEATURES TO ALL FIXES
            ! All character fix record (first 32 words)
            type cfx_rec
              SEQUENCE
              character     basin*2
              character     cynum*2
              character     dtg*12
              character     fform*3
              character     ftype*4
              character     cip*10
              character     rflag*1
              character     latns*5
              character     lonew*6
              character     hob*5
              character     cconf*1
              character     vmax*3
              character     vconf*1
              character     mslp*4
              character     pconf*1
              character     pder*4
              character     rad*3
              character     wcode*4
              character     rad1*4
              character     rad2*4
              character     rad3*4
              character     rad4*4
              character     radm1*1
              character     radm2*1
              character     radm3*1
              character     radm4*1
              character     rconf*1
              character     rmw*3
              character     eyed*3
              character     sreg*1
              character     fsite*5
              character     finit*3
            end type cfx_rec

            ! Mixed structure... causes a warning message when compiled..
            ! Common part of the fix record (32+2 words )
            type fx_rec
              SEQUENCE
              character     basin*2
              integer       cynum
              integer       dtg
              integer       fform
              character     ftype*4
              character     cip*10
              character     rflag*1
              integer       lat
              character     ns*1
              integer       lon
              character     ew*1
              integer       hob
              integer       cconf
              integer       vmax
              integer       vconf
              integer       mslp
              integer       pconf
              character     pder*4
              integer       rad
              character     wcode*4
              integer       rad1
              integer       rad2
              integer       rad3
              integer       rad4
              character     radm1*1
              character     radm2*1
              character     radm3*1
              character     radm4*1
              integer       rconf
              integer       rmw
              integer       eyed
              character     sreg*1
              character     fsite*5
              character     finit*3
            end type fx_rec

            ! MICROWAVE FIX SPECIFIC FIX STRUCTURE
            ! Character only type
            type cmifix
              sequence
              character    rflag*1
              character    rrate*3
              character    process*6
              character    waveh*2
              character    temp*4
              character    rawslp*4
              character    retslp*4
              character    maxsea*3
              character    stype*6
              character    rad*3
              character    wcode*4
              character    rad1*4
              character    rad2*4
              character    rad3*4
              character    rad4*4
              character    rad5*4
              character    rad6*4
              character    rad7*4
              character    rad8*4
              character    radm1*1
              character    radm2*1
              character    radm3*1
              character    radm4*1
              character    radm5*1
              character    radm6*1
              character    radm7*1
              character    radm8*1
              character    rconf*1
              character    comments*52
            end type cmifix

            ! Mixed structure type ... causes warnings when compiled
            type mifix
              sequence
              character    rflag*1
              integer      rrate
              character    process*6
              integer      waveh
              integer      temp
              integer      rawslp
              integer      retslp
              integer      maxsea
              character    stype*6
              integer      rad
              character    wcode*4
              integer      rad1
              integer      rad2
              integer      rad3
              integer      rad4
              integer      rad5
              integer      rad6
              integer      rad7
              integer      rad8
              character    radm1*1
              character    radm2*1
              character    radm3*1
              character    radm4*1
              character    radm5*1
              character    radm6*1
              character    radm7*1
              character    radm8*1
              integer      rconf
              character    comments*52   
            end type mifix

            ! End F90 Structures for the Fixes

            !************************************************************************
            !     Program defined variables
            !************************************************************************

            integer :: iyr,mm,dd,hh,min,sdist,istnum,mslp,ivmx
            integer :: ir34(4),ir50(4),ir64(4)
            integer :: irmw,isdist,lufix
            real :: rlat,rlon
            character*4 :: clat
            character*5 :: clon
            character*31 :: fixname
            character*2 :: cbasin
            character*6 cstype
            logical :: lr34,lr50,lr64
            type (cfx_rec) :: fix         ! all characters
            type (fx_rec) :: cfx         ! mix of types
            type (mifix) ::  mfx          ! mix of types
            type (cmifix) :: mfix        ! all characters
           
            ! Local Variables
            integer :: i 
            !
            ! Mark...here's your data statement...preceeding the executables
            !
            !**********************************************************************
            ! Data Statements
            !**********************************************************************
            ! Note: The data statements below were converted to assignment 
            ! statements
            !
            !     data lr34,lr50,lr64/.false.,.false.,.false./
            !
            !**********************************************************************
            ! End Variables
            !**********************************************************************

            lr34=.false.
            lr50=.false.
            lr64=.false.

            ! Set some constants for AMSU retrievals
            fix%fform=' 30'
            fix%ftype=instr
            fix%cip='        IP'      ! HARDWIRED Intensity and Pressure estimate
            fix%rflag=' '
            fix%hob='    '
            fix%pder='MEAS'
            fix%rad='   '
            fix%wcode='    '
            fix%rad1='    '
            fix%rad2='    '
            fix%rad3='    '
            fix%rad4='    '
            fix%radm1=' '
            fix%radm2=' '
            fix%radm3=' '
            fix%radm4=' '
            fix%eyed='   '
            fix%fsite=' NSOF'
            fix%finit='OPS'
            fix%cconf='1'

            mfix%rflag=' '
            mfix%rrate='   '
            mfix%process='      '
            mfix%waveh='  '
            mfix%temp='    '
            mfix%rawslp='    '
            mfix%maxsea='   '
            mfix%stype=cstype
            mfix%rad='   '
            mfix%wcode='    '
            mfix%rad1='    '
            mfix%rad2='    '
            mfix%rad3='    '
            mfix%rad4='    '
            mfix%rad5='    '
            mfix%rad6='    '
            mfix%rad7='    '
            mfix%rad8='    '
            mfix%radm1=' '
            mfix%radm2=' '
            mfix%radm3=' '
            mfix%radm4=' '
            mfix%radm5=' '
            mfix%radm6=' '
            mfix%radm7=' '
            mfix%radm8=' '
            mfix%comments= &
             'storm center extrapolated from t=-12 and t=0 adeck'

            fix%basin=cbasin

            write(fix%cynum,15)istnum
            15 format(i2.2)
            ! read(*,15,end=999)fix%basin,fix%cynum
            ! 15 format(/,19x,a2,a2,/,/,/)
            ! read(*,30,end=999) iyr,mm,dd,hh,min
            ! 30   format(23x,i4,1x,i2,i2,1x,i2,i2,/)
            write(fix%dtg,30)iyr,mm,dd,hh,min
            30 format(i4.4,i2.2,i2.2,i2.2,i2.2)
            write(fix%mslp,45)mslp
            45 format(i4)
            mfix%retslp=fix%mslp
            write(fix%vmax,60)ivmx
            60 format(i3)

            do i=1,4
              if(ir34(i).gt.0)lr34=.true.
              if(ir50(i).gt.0)lr50=.true.
              if(ir64(i).gt.0)lr64=.true.
            enddo

            write(fix%rmw,75)irmw
            75 format(i3)

            ! Determine confidence given the distance from the satellite swath center
            ! (i.e., isdist) passed to the subroutine.
            !
            cfx%cconf=2
            if(isdist.le.300)then
              fix%vconf='1'
              fix%pconf='1'
              fix%rconf='1'
              mfix%rconf='1'
            else
              fix%vconf='2'
              fix%pconf='2'
              fix%rconf='2'
              mfix%rconf='2'
            end if

            ! Latitude and Longitude
            cfx%lat=nint(rlat*100)
            cfx%lon=nint(rlon*100)
            if(cfx%lat.ge.0)cfx%ns='N'
            if(cfx%lat.lt.0)cfx%ns='S'
            if(cfx%lon.ge.0)cfx%ew='E'
            if(cfx%lon.lt.0)cfx%ew='W'
            if(cfx%lon.gt.18000) then
              cfx%lon=cfx%lon-36000.
              cfx%ew='W'
            end if
            write(fix%latns,151)abs(cfx%lat),cfx%ns
            151 format(i4,a1)
            write(fix%lonew,152)abs(cfx%lon),cfx%ew
            152 format(i5,a1)

            ! Determine sub-basin
            if (fix%basin.eq.'WP')then
              fix%sreg=fix%basin(1:1)
            else if (fix%basin.eq.'AL')then
              fix%sreg='L'
            else if (fix%basin.eq.'CP')then
              fix%sreg=fix%basin(1:1)
            else if (fix%basin.eq.'EP')then
              fix%sreg=fix%basin(1:1)
            else if (fix%basin.eq.'IO')then
              if (rlon .lt. 80.)fix%sreg='A'
              if (rlon .ge. 80.)fix%sreg='B'
            else if (fix%basin.eq.'SH')then
              if (rlon .lt. 135.0 .and. rlon .ge. 0.)fix%sreg='S'
              if (rlon .ge. 135.0 .or. rlon .lt. 0.)fix%sreg='P'
            end if

            !write(fixname,200)fix%dtg,fix%cynum,fix%sreg
            !200  format('CIRA_',a12,'_',a2,a1,'_FIX')
            !open(unit=lufix,file=fixname,status='replace')
            !
            !write the center/intensity/pressure fix

            if(.not.lr34)then
              write(lufix,165)fix,mfix
            end if

            ! if 34 knot winds exist print the intensity/pressure/Radii fix
            ! for 34 knot winds
            if(lr34)then
              fix%cip='       IPR'
              fix%rad=' 34'
              fix%wcode='NEQ'
              mfix%rad=' 34'
              mfix%wcode='NEQ'
              write(fix%rad1,175)ir34(1)
              write(fix%rad2,175)ir34(2)
              write(fix%rad3,175)ir34(3)
              write(fix%rad4,175)ir34(4)
              175 format(i4)
              write(mfix%rad1,175)ir34(1)
              write(mfix%rad2,175)ir34(2)
              write(mfix%rad3,175)ir34(3)
              write(mfix%rad4,175)ir34(4)
              write(lufix,165)fix,mfix
            end if

            ! if 50 knot winds exist print the Radii fix
            ! for 50 knot winds
            if(lr50)then
              fix%cip='         R'
              fix%rad=' 50'
              fix%wcode='NEQ'
              mfix%rad=' 50'
              mfix%wcode='NEQ'
              write(fix%rad1,175)ir50(1)
              write(fix%rad2,175)ir50(2)
              write(fix%rad3,175)ir50(3)
              write(fix%rad4,175)ir50(4)
              write(mfix%rad1,175)ir50(1)
              write(mfix%rad2,175)ir50(2)
              write(mfix%rad3,175)ir50(3)
              write(mfix%rad4,175)ir50(4)
              write(lufix,165)fix,mfix
            end if

            ! if 64 knot winds exist print the Radii fix
            ! for 64 knot winds
            if(lr64)then
              fix%cip='         R'
              fix%rad=' 64'
              fix%wcode='NEQ'
              mfix%rad=' 64'
              mfix%wcode='NEQ'
              write(fix%rad1,175)ir64(1)
              write(fix%rad2,175)ir64(2)
              write(fix%rad3,175)ir64(3)
              write(fix%rad4,175)ir64(4)
              write(mfix%rad1,175)ir64(1)
              write(mfix%rad2,175)ir64(2)
              write(mfix%rad3,175)ir64(3)
              write(mfix%rad4,175)ir64(4)
              write(lufix,165)fix,mfix
            end if
 
            165 format(32(a,', '),28(a,', '),a)
 
            close(lufix)
            stop

          end subroutine AMSUfdeck

        end module opsubs
