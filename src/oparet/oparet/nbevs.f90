        subroutine nbevs(phi,u,v,inon,nxt,nyt)
        ! This routine solves the balance equation for u and v
        ! given phi and boundary values for u and v. A variational method
        ! is used for solving the be. If inon=1, then the nonlinear version
        ! is used, otherwise, the linear version is used.

        ! This routine solves the balance equation in streamfunction form
        ! and then calculates u,v from psi
          use dims
          use cgrid
          use io

          ! Passed arrays
          integer,intent(in) :: nxt,nyt,inon
          real,intent(in) :: phi(nxt,nyt),
          real,intent(inout) :: u(nxt,nyt),v(nxt,nyt)

          ! Local arrays
          real :: d2phi(nx,ny)
          real :: psi(nx,ny),zeta(nx,ny),dvdx(nx,ny),dudy(nx,ny)
          real :: psifg(nx,ny),gy(ny)
          real :: d2pdxx(nx,ny),d2pdyy(nx,ny),d2pdxy(nx,ny)
          real :: d2psi(nx,ny),dpdy(nx,ny)
          real :: res(nx,ny),dedpsi(nx,ny)
          real :: cpta(nx,ny)

          ! Specify maximum number of iterations
          integer :: nit=1500

          ! Specify maximum number of corrections before updating gradient
          integer :: ncg=20

          ! Specify coefficient of the smoothness penalty term
          real :: cpt = 1.0e-10

          ! Specify reduction factor for adjusting alpha before
          ! new gradient calculation
          real :: redfac = 0.1

          ! Specify first guess for step length alpha
          real :: alpha = 1.0e+25

          ! Initialize penalty term coefficient array
          call zinter(cpta,cpt,nx,ny)
          call zbound(cpta,0.0,nx,ny)

          ! Set gy=0 for poisson solver
          do j=1,ny
            gy(j) = 0.0
          end do

          ! Find Laplacian of phi
          call del2xy(phi,nx,ny,dx,dy,d2phi)

          ! Calculate vorticity from u,v
          call dxcal(v,nx,ny,dx,dy,dvdx)
          call dycal(u,nx,ny,dx,dy,dudy)

          do j=1,ny
            do i=1,nx
              zeta(i,j) = dvdx(i,j)-dudy(i,j)
              psifg(i,j) = 0.0
            end do
          end do

          ! Calcuate boundary values of psi from u,v
          call psibd(psifg,nx,ny,dx,dy,u,v)

          ! Calculate first guess for psi 
          call psonxy(zeta,gy,psi,psifg,ierr)

          ! Calculate x,y derviatives of psi
          call d2xxcal(psi,nx,ny,dx,dy,d2pdxx)
          call d2yycal(psi,nx,ny,dx,dy,d2pdyy)
          call d2xycal(psi,nx,ny,dx,dy,d2pdxy)
          call dycal(psi,nx,ny,dx,dy,dpdy)

          ! Calculate Laplacian of psi
          call del2xy(psi,nx,ny,dx,dy,d2psi)

          ! Calculate initial value of residual
          call rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy, &
                       d2pdxy,dpdy,inon,nx,ny,res)

          ! Calculate initial value of cost function
          call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)

          write(lulog,600) cfunr,cfunp,cfunt
          600 format(//,' Initial Cost function (r,p,t): ',3(e11.4,1x),/)

          if (nit .le. 0) then
            call uvcal(psi,nx,ny,dx,dy,u,v)
            return
          end if

          ngdate = 1

 1000 continue

          !  Calculate x,y derviatives of psi
          call d2xxcal(psi,nx,ny,dx,dy,d2pdxx)
          call d2yycal(psi,nx,ny,dx,dy,d2pdyy)
          call d2xycal(psi,nx,ny,dx,dy,d2pdxy)
          call dycal(psi,nx,ny,dx,dy,dpdy)

          ! Calculate Laplacian of psi
          call del2xy(psi,nx,ny,dx,dy,d2psi)

          ! Calculate residual
          call rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy, &
                      inon,nx,ny,res)

          ! Calculate cost function gradient
          call cfgcals(res,d2pdxx,d2pdyy,d2pdxy,d2psi,cpta, &
                      inon,nx,ny,dedpsi)

!         write(lulog,776) ngdate
!         776 format(/,' ngdate=',i4,' res')
!         do j=17,15,-1
!           write(lulog,777) (res(i,j),i=15,17)
!           777 format(1x,3(e11.4,1x))
!         end do

!         write(lulog,*) ' dedpsi'
!         do 77 j=17,15,-1
!           write(lulog,777) (dedpsi(i,j),i=15,17)
!   77    continue
!
!         write(lulog,*) ' psi'
!         do 78 j=17,15,-1
!           write(lulog,777) (psi(i,j),i=15,17)
!   78    continue
!
!        Calculate cost function
!        call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)
!
          nudate = 0
 1100     continue
          cfunpr = cfunt

          ! Update psi
          do  j=2,ny-1
            do i=2,nx-1
              psi(i,j) = psi(i,j) - alpha*dedpsi(i,j)
            end do
          end do

          nudate = nudate + 1

          ! Calculate x,y derviatives of psi
          call d2xxcal(psi,nx,ny,dx,dy,d2pdxx)
          call d2yycal(psi,nx,ny,dx,dy,d2pdyy)
          call d2xycal(psi,nx,ny,dx,dy,d2pdxy)
          call dycal(psi,nx,ny,dx,dy,dpdy)

          ! Calculate Laplacian of psi
          call del2xy(psi,nx,ny,dx,dy,d2psi)

          ! Calculate residual
          call rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy, &
                       inon,nx,ny,res)

          ! Calculate cost function
          call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)

!          write(6,701) cfunr,cfunp,cfunt
!          701 format(1x,'cfunr=',e11.4,1x,'cfunp=',e11.4,' cfunt=',e11.4)

          if (cfunt .lt. cfunpr .and. nudate .lt. ncg) then
            ! Cost function is decreasing, so continue
            ! down the gradient
            nupdate = nupdate + 1
            go to 1100
          else
            ! Cost function is not decreasing, or maximum gradient
            ! steps has been exceeded
            if (cfunt .ge. cfunpr) then
              ! Undo the last step down the gradient
              do j=2,ny-1
                do i=2,nx-1
                  psi(i,j) = psi(i,j) + alpha*dedpsi(i,j)
                end do
              end do
            end if

            if (mod(ngdate,10) .eq. 0) then
              write(lulog,402) cfunr,cfunp,cfunt, &
                               ngdate,nudate,alpha
              402 format(' CF:',e9.3,e9.3,e11.4, &
                         ' ng=',i5,' nu=',i5,' al=',e10.3)
            end if

            ! Calculate optimal step and adjust it for next
            ! gradient calculation
            rnu = float(nudate)
            if (rnu .lt. 1.0) rnu=1.0
            alpha = rnu*alpha*redfac

            if (ngdate .ge. nit) go to 1200
            if (alpha .le. 0.0) go to 1200
            ngdate = ngdate + 1
            go to 1000
          endif

 1200     continue

          call uvcal(psi,nx,ny,dx,dy,u,v)

          return

        end subroutine nbevs
