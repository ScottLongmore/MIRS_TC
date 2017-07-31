        module besubs
          implicit none

          contains

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
            real,intent(in) :: phi(nxt,nyt)
            real,intent(inout) :: u(nxt,nyt),v(nxt,nyt)
  
            ! Local arrays
            real :: d2phi(nx,ny)
            real :: psi(nx,ny),zeta(nx,ny),dvdx(nx,ny),dudy(nx,ny)
            real :: psifg(nx,ny),gy(ny)
            real :: d2pdxx(nx,ny),d2pdyy(nx,ny),d2pdxy(nx,ny)
            real :: d2psi(nx,ny),dpdy(nx,ny)
            real :: res(nx,ny),dedpsi(nx,ny)
            real :: cpta(nx,ny)
  
            integer :: nit,ncg,ngdate,nudate,nupdate,rnu,ierr
            real :: cpt,redfac,alpha
            real :: cfunr,cfunp,cfunt,cfunpr
            integer :: i,j

            ! Specify maximum number of iterations
            nit=1500
  
            ! Specify maximum number of corrections before updating gradient
            ncg=20
  
            ! Specify coefficient of the smoothness penalty term
            cpt = 1.0e-10
  
            ! Specify reduction factor for adjusting alpha before
            ! new gradient calculation
            redfac = 0.1
  
            ! Specify first guess for step length alpha
            alpha = 1.0e+25
  
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
  
 1000       continue

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
  
!           write(lulog,776) ngdate
!           776 format(/,' ngdate=',i4,' res')
!           do j=17,15,-1
!             write(lulog,777) (res(i,j),i=15,17)
!             777 format(1x,3(e11.4,1x))
!           end do

!           write(lulog,*) ' dedpsi'
!           do j=17,15,-1
!             write(lulog,777) (dedpsi(i,j),i=15,17)
!           end do
!
!           write(lulog,*) ' psi'
!           do j=17,15,-1
!             write(lulog,777) (psi(i,j),i=15,17)
!           end do

!           Calculate cost function
!           call cfcals(res,d2psi,cpta,nx,nx,cfunr,cfunp,cfunt)
!
            nudate = 0

 1100       continue
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

!           write(6,701) cfunr,cfunp,cfunt
!           701 format(1x,'cfunr=',e11.4,1x,'cfunp=',e11.4,' cfunt=',e11.4)

            ! Cost function is decreasing, so continue
            ! down the gradient
            if (cfunt .lt. cfunpr .and. nudate .lt. ncg) then
              nupdate = nupdate + 1
              go to 1100

            ! Cost function is not decreasing, or maximum gradient
            ! steps has been exceeded
            else
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

 1200       continue

            call uvcal(psi,nx,ny,dx,dy,u,v)

            return

          end subroutine nbevs

          subroutine nbev(phi,u,v,inon,nxt,nyt)
          ! This routine solves the balance equation for u and v
          ! given phi and boundary values for u and v. A variational method
          ! is used for solving the be. If inon=1, then the nonlinear version
          ! is used, if inon=0, the linear version is used. If inon=-1, only the
          ! cost function penalty term is included in the minimization procedure

          ! This routine solves the balance equation in u,v form
            use dims
            use cgrid
            use io

            ! Passed arrays
            integer,intent(in) :: inon,nxt,nyt
            real,intent(in) :: phi(nxt,nyt)
            real,intent(inout) :: u(nxt,nyt),v(nxt,nyt)

            ! Local arrays
            real :: d2phi(nx,ny),d2u(nx,ny),d2v(nx,ny)
            real :: dudx(nx,ny),dvdx(nx,ny),dudy(nx,ny),dvdy(nx,ny)
            real :: res(nx,ny),dedu(nx,ny),dedv(nx,ny)
            real :: cpta(nx,ny)

            integer :: nit,ncg,ngdate,nudate,nupdate,rnu
            real :: cpt,redfac,alpha
            real :: cfunr,cfunp,cfunt,cfunpr
            integer :: i,j

            ! Specify maximum number of iterations
            nit=3000

            ! Specify maximum number of corrections before updating gradient
            ncg=20

            ! Specify coefficient of the smoothness penalty term
            cpt =20.0

            ! Specify reduction factor for adjusting alpha before
            ! new gradient calculation
            redfac = 0.1

            ! Specify first guess for step length alpha
            alpha = 1.0e+17

            ! Initialize penalty term coefficient array
            call zinter(cpta,cpt,nx,ny)
            call zbound(cpta,0.0,nx,ny)

            ! Find Laplacian of phi
            call del2xy(phi,nx,ny,dx,dy,d2phi)

            ! Calculate x,y derviatives of u,v
            call dxcal(u,nx,ny,dx,dy,dudx)
            call dxcal(v,nx,ny,dx,dy,dvdx)
            call dycal(u,nx,ny,dx,dy,dudy)
            call dycal(v,nx,ny,dx,dy,dvdy)

            ! Calculate Laplacian of u,v
            call del2xy(u,nx,ny,dx,dy,d2u)
            call del2xy(v,nx,ny,dx,dy,d2v)

            ! Calculate initial value of residual
            call rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nx,ny,res)

            ! Calculate initial value of cost function
            call cfcal(res,d2u,d2v,cpta,nx,nx,cfunr,cfunp,cfunt)

            write(lulog,600) cfunr,cfunp,cfunt
            600 format(//,' Initial Cost function (r,p,t): ',3(e11.4,1x),/)

            ngdate = 1

 1000       continue

            ! Calculate x,y derivaties of u,v
            call dxcal(u,nx,ny,dx,dy,dudx)
            call dxcal(v,nx,ny,dx,dy,dvdx)
            call dycal(u,nx,ny,dx,dy,dudy)
            call dycal(v,nx,ny,dx,dy,dvdy)

            ! Calculate Laplacian of u,v
            call del2xy(u,nx,ny,dx,dy,d2u)
            call del2xy(v,nx,ny,dx,dy,d2v)

            ! Calcualte residual
            call rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nx,ny,res)

            ! Calculate cost function gradient
            call cfgcal(res,u,v,dudx,dvdx,dudy,dvdy,d2u,d2v,cpta, &
                        inon,nx,ny,dedu,dedv)

            ! Calculate cost function
            call cfcal(res,d2u,d2v,cpta,nx,nx,cfunr,cfunp,cfunt)

            nudate = 0

 1100       continue

            cfunpr = cfunt

            ! Update u,v
            do j=2,ny-1
              do i=2,nx-1
                u(i,j) = u(i,j) - alpha*dedu(i,j)
                v(i,j) = v(i,j) - alpha*dedv(i,j)
              end do
            end do

            nudate = nudate + 1

            ! Calculate x,y derivaties of u,v
            call dxcal(u,nx,ny,dx,dy,dudx)
            call dxcal(v,nx,ny,dx,dy,dvdx)
            call dycal(u,nx,ny,dx,dy,dudy)
            call dycal(v,nx,ny,dx,dy,dvdy)

            ! Calculate Laplacian of u,v
            call del2xy(u,nx,ny,dx,dy,d2u)
            call del2xy(v,nx,ny,dx,dy,d2v)

            ! Calcualte residual
            call rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nx,ny,res)

            ! Calculate cost function
            call cfcal(res,d2u,d2v,cpta,nx,nx,cfunr,cfunp,cfunt)

!           write(6,701) cfunpr,cfunt
!           701 format(1x,'cfunpr=',e11.4,1x,'cfunt=',e11.4)

            ! Cost function is decreasing, so continue down the gradient
            if (cfunt .lt. cfunpr .and. nudate .lt. ncg) then
               nupdate = nupdate + 1
               go to 1100

            ! Cost function is not decreasing, or maximum gradient
            ! steps has been exceeded
            else

              ! Undo the last step down the gradient
              if (cfunt .ge. cfunpr) then
                do j=2,ny-1
                  do i=2,nx-1
                    u(i,j) = u(i,j) + alpha*dedu(i,j)
                    v(i,j) = v(i,j) + alpha*dedv(i,j)
                  end do 
                end do
              endif

              if (mod(ngdate,50) .eq. 0) then
                write(lulog,402) cfunr,cfunp,cfunt, &
                                 ngdate,nudate,alpha
                402 format(' CF:',e9.3,e9.3,e11.4, &
                           ' ng=',i5,' nu=',i5,' al=',e10.3)
              endif

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

 1200       continue

            return

          end subroutine nbev

          subroutine cfgcals(res,d2pdxx,d2pdyy,d2pdxy,d2psi,cpta, &
                             inon,nxt,nyt,dedpsi)
          ! This routine calculates the gradient of the cost function
          ! with respect to u and v (dedu and dedv) required for the variational
          ! solution to the balance equation.
            use dims
            use cgrid

            ! Passed arrays
            integer,intent(in) :: inon,nxt,nyt 
            real,intent(in) :: d2pdxx(nxt,nyt),d2pdyy(nxt,nyt)
            real,intent(in) :: d2psi(nxt,nyt),cpta(nxt,nyt)
            real,intent(inout) :: dedpsi(nxt,nyt), d2pdxy(nxt,nyt)
            real,intent(inout) :: res(nxt,nyt)

            real :: dxi,dyi,dxi2,dyi2,dxyi
            integer :: i,j

            ! Set cost function gradient to zero at the domain boundaries
            call zbound(dedpsi,0.0,nxt,nyt)

            dxi = 1.0/dx
            dyi = 1.0/dy
            dxi2  = dxi*dxi
            dyi2  = dyi*dyi
            dxyi  = dxi*dyi

            ! Interior points

            ! Linear terms:
            do j=2,ny-1
              do  i=2,nx-1
                dedpsi(i,j) =  &
                dxi2*(res(i-1,j)*fy(j  )+res(i+1,j)*fy(j  ) -2.*res(i,j)*fy(j)) &
                +dyi2*(res(i,j-1)*fy(j-1)+res(i,j+1)*fy(j+1)-2.*res(i,j)*fy(j)) &
                +beta*0.5*dyi*(res(i,j-1)-res(i,j+1))
              end do
            end do

            ! Penalty terms:
            do j=2,ny-1
              do i=2,nx-1
                dedpsi(i,j) = dedpsi(i,j) + dxi2*cpta(i+1,j)*d2psi(i+1,j) &
                                + dxi2*cpta(i-1,j)*d2psi(i-1,j) &
                                -2.0*dxi2*cpta(i  ,j)*d2psi(i  ,j) &
                                 +   dyi2*cpta(i,j+1)*d2psi(i,j+1) &
                                 +   dyi2*cpta(i,j-1)*d2psi(i,j-1) &
                                -2.0*dyi2*cpta(i,j  )*d2psi(i,j  )
              end do
            end do

            ! Nonlinear terms
            if (inon .ne. 1) return

            do j=2,ny-1
              do i=2,nx-1
                dedpsi(i,j) = dedpsi(i,j) + &
                  dxyi*( d2pdxy(i-1,j-1)*res(i-1,j-1) + &
                  d2pdxy(i+1,j+1)*res(i+1,j+1) - &
                  d2pdxy(i-1,j+1)*res(i-1,j+1) - &
                  d2pdxy(i+1,j-1)*res(i+1,j-1) ) &
                  -2.0*dyi2*( d2pdxx(i,j-1)*res(i,j-1) + &
                  d2pdxx(i,j+1)*res(i,j+1) - &
                  2.0*d2pdxx(i,j  )*res(i,j)) &
                  -2.0*dxi2*( d2pdyy(i-1,j)*res(i-1,j) + &
                  d2pdyy(i+1,j)*res(i+1,j) - &
                  2.0*d2pdyy(i,j)*res(i,j))
              end do
            end do

            return

          end subroutine cfgcals
 
          subroutine cfgcal(res,u,v,dudx,dvdx,dudy,dvdy,d2u,d2v,cpta, &
                            inon,nxt,nyt,dedu,dedv)
          ! This routine calculates the gradient of the cost function
          ! with respect to u and v (dedu and dedv) required for the variational
          ! solution to the balance equation.
            use dims
            use cgrid

            ! Passed arrays
            
            integer,intent(in) :: inon,nxt,nyt
            real,intent(in) :: res(nxt,nyt)
            real,intent(in) :: u(nxt,nyt),v(nxt,nyt)
            real,intent(in) :: dudx(nxt,nyt),dvdx(nxt,nyt)
            real,intent(in) :: dudy(nxt,nyt),dvdy(nxt,nyt)
            real,intent(in) :: d2u(nxt,nyt),d2v(nxt,nyt),cpta(nxt,nyt)
            real,intent(inout) :: dedu(nxt,nyt),dedv(nxt,nyt)

            real :: dxi,dyi,tdxi,tdyi,dxi2,dyi2

            integer :: i,j

            ! Set cost function gradient to zero at the domain boundaries
            call zbound(dedu,0.0,nxt,nyt)
            call zbound(dedv,0.0,nxt,nyt)

            dxi = 1.0/dx
            dyi = 1.0/dy
            tdxi = 0.5*dxi
            tdyi = 0.5*dyi
            dxi2 = dxi*dxi
            dyi2 = dyi*dyi

            ! Interior points

            ! Penalty terms:
            do j=2,ny-1
              do i=2,nx-1
                dedu(i,j) = dxi2*cpta(i+1,j)*d2u(i+1,j) &
                            + dxi2*cpta(i-1,j)*d2u(i-1,j) &
                            -2.0*dxi2*cpta(i  ,j)*d2u(i  ,j) &
                            + dyi2*cpta(i,j+1)*d2u(i,j+1) &
                            + dyi2*cpta(i,j-1)*d2u(i,j-1) &
                            -2.0*dyi2*cpta(i,j  )*d2u(i,j  )

                dedv(i,j) = dxi2*cpta(i+1,j)*d2v(i+1,j) &
                            + dxi2*cpta(i-1,j)*d2v(i-1,j) &
                            -2.0*dxi2*cpta(i  ,j)*d2v(i  ,j) &
                            + dyi2*cpta(i,j+1)*d2v(i,j+1) &
                            + dyi2*cpta(i,j-1)*d2v(i,j-1) &
                            -2.0*dyi2*cpta(i,j  )*d2v(i,j  )
              end do
            end do

            ! Linear terms:
            if (inon .eq. -1) return

            do j=2,ny-1
              do i=2,nx-1
                dedu(i,j) = dedu(i,j) + tdyi*(res(i,j-1)*fy(j-1)- &
                            res(i,j+1)*fy(j+1)) &
                            + beta*res(i,j)

                dedv(i,j) = dedv(i,j) - tdxi*(res(i-1,j)*fy(j)- &
                            res(i+1,j)*fy(j))
              end do
            end do 

            ! Nonlinear terms
            if (inon .ne. 1) return

            do j=2,ny-1
              do i=2,nx-1
                dedu(i,j) = dedu(i,j) + dxi*(res(i-1,j)*dudx(i-1,j)- &
                            res(i+1,j)*dudx(i+1,j)) &
                            + dyi*(res(i,j-1)*dvdx(i,j-1)- &
                            res(i,j+1)*dvdx(i,j+1))

                dedv(i,j) = dedv(i,j) + dxi*(res(i-1,j)*dudy(i-1,j)- &
                            res(i+1,j)*dudy(i+1,j)) &
                            + dyi*(res(i,j-1)*dvdy(i,j-1)- &
                            res(i,j+1)*dvdy(i,j+1))
              end do
            end do

            return

          end subroutine cfgcal

          subroutine cfcals(res,d2psi,cpta,nx,ny,cfunr,cfunp,cfunt)
          ! This routine calculates the cost function. The contribution
          ! from the residual of the nonlinear balance equation and from
          ! the smoothness penalty term are also calculated.

          ! This version is for the balance equation in streamfunction form

            integer,intent(in) :: nx,ny
            real,intent(in) :: d2psi(nx,ny),cpta(nx,ny)
            real,intent(inout) :: cfunr,cfunp,cfunt 
            real,intent(inout) :: res(nx,ny)

            integer :: i,j

            ! Calculate the contribution from the residual
            cfunr = 0.0
            do j=2,ny-1
              do i=2,nx-1
                cfunr = cfunr + res(i,j)*res(i,j)
              end do
            end do
            cfunr = 0.5*cfunr

            ! Calculate the contribution from the penalty term
            cfunp = 0.0
            do j=2,ny-1
              do i=2,nx-1
                cfunp = cfunp + cpta(i,j)*(d2psi(i,j)**2)
              end do
            end do
            cfunp = 0.5*cfunp

            cfunt = cfunr+cfunp

            return

          end subroutine cfcals

          subroutine cfcal(res,d2u,d2v,cpta,nx,ny,cfunr,cfunp,cfunt)
          ! This routine calculates the cost function. The contribution
          ! from the residual of the nonlinear balance equation and from
          ! the smoothness penalty term are also calculated.
          !
          ! This version is for the balance equation in u,v form

            integer,intent(in) :: nx,ny
            real,intent(inout) :: cfunr,cfunp,cfunt
            real,intent(in) :: d2u(nx,ny),d2v(nx,ny),cpta(nx,ny)
            real,intent(inout) :: res(nx,ny)

            integer :: i,j

            ! Calculate the contribution from the residual
            cfunr = 0.0
            do j=2,ny-1
              do i=2,nx-1
                cfunr = cfunr + res(i,j)*res(i,j)
              end do
            end do

            cfunr = 0.5*cfunr

            ! Calculate the contribution from the penalty term
            cfunp = 0.0
            do j=2,ny-1
              do i=2,nx-1
                cfunp = cfunp + cpta(i,j)*(d2u(i,j)**2 + d2v(i,j)**2)
              end do
            end do

            cfunp = 0.5*cfunp
            cfunt = cfunr+cfunp

            return

          end subroutine cfcal

          subroutine rescals(psi,d2phi,d2psi,d2pdxx,d2pdyy,d2pdxy,dpdy, &
                             inon,nxt,nyt,res)
          ! The routine calculates the residual of the balance equation
          ! in Cartesian coordinates on a beta plane. This version
          ! is for the balance equation in terms of streamfunction.
            use dims
            use cgrid

            integer,intent(in) :: inon,nxt,nyt
            real,intent(in) :: psi(nxt,nyt)
            real,intent(in) :: d2phi(nxt,nyt),d2psi(nxt,nyt)
            real,intent(in) :: d2pdxx(nxt,nyt),d2pdyy(nxt,nyt)
            real,intent(in) :: dpdy(nxt,nyt),d2pdxy(nxt,nyt)
            real,intent(inout) :: res(nxt,nyt)

            integer :: i,j

            ! Set residual to zero at boundary points because they
            ! are not included in the cost function
            call zbound(res,0.0,nxt,nyt)

            ! Calculate residual at interior points
            do j=2,ny-1
              do i=2,nx-1
                res(i,j) =  fy(j)*d2psi(i,j) + &
                            beta*dpdy(i,j) - d2phi(i,j)
              end do
            end do

            ! Add nonlinear terms
            if (inon .ne. 1) return

            do j=2,ny-1
              do i=2,nx-1
                res(i,j) = res(i,j) + 2.0*d2pdxy(i,j)*d2pdxy(i,j) &
                           - 2.0*d2pdxx(i,j)*d2pdyy(i,j)
              end do 
            end do

            return

          end subroutine rescals

          subroutine rescal(u,v,d2phi,dudx,dvdx,dudy,dvdy,inon,nxt,nyt, &
                            res)
          ! The routine calculates the residual of the balance equation
          ! in Cartesian coordinates on a beta plane. This version is
          ! for the balance equation in terms of u,v.
            use dims
            use cgrid

            integer,intent(in) :: inon,nxt,nyt
            real,intent(in) :: u(nxt,nyt),v(nxt,nyt)
            real,intent(in) :: d2phi(nxt,nyt)
            real,intent(in) :: dudx(nxt,nyt),dvdx(nxt,nyt)
            real,intent(in) :: dudy(nxt,nyt),dvdy(nxt,nyt)
            real,intent(inout) :: res(nxt,nyt)

            integer :: i,j

            ! Set residual to zero at boundary points because they
            ! are not included in the cost function
            call zbound(res,0.0,nxt,nyt)

            ! Minimization only includes penalty term, so set res=0
            if (inon .eq. -1) then
              call zinter(res,0.0,nxt,nyt)
              return
            endif

            ! Calculate residual at interior points
            do j=2,ny-1
              do i=2,nx-1
                res(i,j) = -fy(j)*(dvdx(i,j)-dudy(i,j)) + &
                           beta*u(i,j) + d2phi(i,j)
              end do
            end do

            ! Add nonlinear terms
            if (inon .ne. 1) return

            do  j=2,ny-1
              do  i=2,nx-1
                res(i,j) = res(i,j) + dudx(i,j)*dudx(i,j) + &
                           dvdy(i,j)*dvdy(i,j) + &
                           2.0*dvdx(i,j)*dudy(i,j)
              end do
            end do 

            return

          end subroutine rescal

          subroutine nbei(phi,u,v,nxt,nyt)
          ! This routine solves the nonlinear balance equation for u and v
          ! given phi and boundary values for u and v. An iterative
          ! procedure is used to solve the balance equation.
            use dims
            use cgrid
            use io

            ! Passed arrays
            integer,intent(in) :: nxt,nyt
            real,intent(inout) :: phi(nxt,nyt)
            real,intent(inout) ::u(nxt,nyt),v(nxt,nyt)
    
            ! Local arrays
            real :: psi(nx,ny),psifg(nx,ny),d2phi(nx,ny)
            real :: zeta(nx,ny),zetat(nx,ny),phib(nx,ny)
            real :: dudx(nx,ny),dvdx(nx,ny),dudy(nx,ny),dvdy(nx,ny)
            real ::cterm(nx,ny),disc(nx,ny),div(nx,ny),gy(ny)
    
            character(len=30) :: label
    
            integer :: nit,itest
            real :: urc,urcc,dphi,dphimax,dphiavg
            integer :: ierr
            integer :: i,k,j
    
            ! Specify maximum number of iterations
            nit=10
    
            ! Specify under-relaxation coefficient
            ! (  zeta(new) = (1-urc)*zeta(update) + urc*zeta(old)  )
            urc  = 0.2
            urcc = 1.0-urc
    
            ! Find Laplacian of phi
            call del2xy(phi,nx,ny,dx,dy,d2phi)

            ! Set gy=0 for poisson routine
            do j=1,ny
              gy(j) = 0.0
            end do

            ! Make first guess for psi
            do j=1,ny
              do i=1,nx
                psifg(i,j) = 0.0
              end do
            end do

            ! Calculate psi boundary conditions
            call psibd(psifg,nx,ny,dx,dy,u,v)

            ! Calculate vorticity
            call dxcal(v,nx,ny,dx,dy,dvdx)
            call dycal(u,nx,ny,dx,dy,dudy)
            do j=2,ny-1
              do i=2,nx-1
                zeta(i,j) = dvdx(i,j)-dudy(i,j)
              end do
            end do

            ! Calculate stream function
            call psonxy(zeta,gy,psi,psifg,ierr)

            ! Calculate u,v from psi
            call uvcal(psi,nx,ny,dx,dy,u,v)

            ! Calculate x,y derivatives of u,v
            call dxcal(u,nx,ny,dx,dy,dudx)
            call dxcal(v,nx,ny,dx,dy,dvdx)
            call dycal(u,nx,ny,dx,dy,dudy)
            call dycal(v,nx,ny,dx,dy,dvdy)

            ! Start iteration
            do k=1,nit

              ! Calculate c term of quadratic form of vorticity equation
              do j=2,ny-1
                do i=2,nx-1
                  cterm(i,j) = ( dvdx(i,j)+dudy(i,j))**2 + &
                               (-dudx(i,j)+dvdy(i,j))**2 + &
                               2.0*(d2phi(i,j) + beta*u(i,j))
                end do
              end do

              ! Calculate the discriminant of the quadratic (in zeta) form of the BE
              do j=2,ny-1
                do i=2,nx-1
                  disc(i,j) = fy(j)*fy(j) + cterm(i,j)
                end do
              end do

              ! Calculate updated vorticity
              do j=2,ny-1
                do i=2,nx-1
                  if (disc(i,j) .gt. 0.0) then
                    div(i,j) = 0.0
                    zetat(i,j) = -fy(j) + sqrt(disc(i,j))
                  else
                    div(i,j) = abs(disc(i,j))
                    zetat(i,j) = -fy(j)
                  end if
                end do
              end do

              ! Apply under-relaxation
              do j=2,ny-1
                do i=2,nx-1
                  zeta(i,j) = urcc*zetat(i,j) + urc*zeta(i,j)
                end do
              end do

              ! Calculate stream function
              call psonxy(zeta,gy,psi,psifg,ierr)

              ! Calculate u,v from psi
              call uvcal(psi,nx,ny,dx,dy,u,v)

              ! Calculate x,y derivatives of u,v
              call dxcal(u,nx,ny,dx,dy,dudx)
              call dxcal(v,nx,ny,dx,dy,dvdx)
              call dycal(u,nx,ny,dx,dy,dudy)
              call dycal(v,nx,ny,dx,dy,dvdy)

              ! Calculate the forcing in the "reverse" balance equation
              ! to test for convergence
              do j=2,ny-1
                do i=2,nx-1
                  cterm(i,j) = fy(j)*(dvdx(i,j)-dudy(i,j)) - beta*u(i,j) &
                              -dudx(i,j)**2 - dvdy(i,j)**2 &
                              -2.0*dvdx(i,j)*dudy(i,j)
                end do
              end do

              ! Solve reverse balance equation for phi
              call psonxy(cterm,gy,phib,phi,ierr)

              ! Calculate average and max difference 
              ! between phi and phib (scaled by g)
              dphimax = 0.0
              dphiavg = 0.0
              do j=2,ny-1
                do i=2,nx-1
                  dphi = abs(phib(i,j)-phi(i,j))
                  dphiavg = dphiavg + dphi
                  if (dphi .gt. dphimax) dphimax = dphi
                end do
              end do

              dphiavg = dphiavg/(9.81*float(nx-2)*float(ny-2))
              dphimax = dphimax/9.81

              itest=1
              if (itest .eq. 1) then
                write(lulog,200) k,dphiavg,dphimax
                200 format( &
                    ' Iteration ',i4,' dphiavg=',e11.4,'  dphimax=',e11.4)
              end if

            end do

            itest=0
            if (itest .eq. 1) then
              do j=1,ny
                do i=1,nx
                  phib(i,j) = (phib(i,j)-phi(i,j))/9.81
                  ! phib(i,j) = (phib(i,j))/9.81 - 2000.0
                end do 
              end do

              label=' z error from nbei'
              call fprint(phib,nx,ny,label)

            end if

            return

          end subroutine nbei

          subroutine uvcal(psi,nx,ny,dx,dy,u,v)
          ! This routine calculates u,v from the streamfunction psi

            integer,intent(in) :: nx,ny
            real,intent(in) :: psi(nx,ny),dx,dy
            real,intent(inout) ::u(nx,ny),v(nx,ny)

            integer :: i,j

            call dxcal(psi,nx,ny,dx,dy,v)
            call dycal(psi,nx,ny,dx,dy,u)

            do j=1,ny
              do i=1,nx
                u(i,j) = -1.0*u(i,j)
              end do
            end do

            return

          end subroutine uvcal

          subroutine lbe(phi,u,v,nxt,nyt)
          ! This routine solves the linear balance equation for u and v
          ! given phi and boundary values for u and v.
            use dims
            use cgrid
            use io

            ! Passed arrays
            integer,intent(in) :: nxt,nyt
            real,intent(in) :: phi(nxt,nyt) 
            real,intent(inout) :: u(nxt,nyt),v(nxt,nyt)

            ! Local arrays
            real :: psi(nx,ny),psifg(nx,ny),d2phi(nx,ny),gy(ny)

            integer :: ierr
            integer :: i,j

            ! Find Laplacian of phi
            call del2xy(phi,nx,ny,dx,dy,d2phi)

            ! Divide d2phi by Coriolis parameter
            do j=1,ny
              do i=1,nx
                d2phi(i,j) = d2phi(i,j)/fy(j)
              end do
            end do

            ! Calculate coefficient for d/dx term of lbe
            do j=1,ny
              gy(j) = beta/fy(j)
            end do

            ! Make first guess for psi
            do j=1,ny
              do i=1,nx
                psifg(i,j) = 0.0
              end do
            end do

            ! Calculate psi boundary conditions
            call psibd(psifg,nx,ny,dx,dy,u,v)

            ! Calculate stream function
            call psonxy(d2phi,gy,psi,psifg,ierr)

            call dxcal(psi,nx,ny,dx,dy,v)
            call dycal(psi,nx,ny,dx,dy,u)

            do j=1,ny
              do i=1,nx
                u(i,j) = -1.0*u(i,j)
              end do
            end do

            return

          end subroutine lbe

          subroutine psibd(psi,nx,ny,dx,dy,u,v)
          ! This routine calculates psi on the domain boundaries
          ! by integrating u,v

            integer,intent(in) :: nx,ny
            real,intent(in) :: dx,dy,u(nx,ny),v(nx,ny)
            real,intent(inout) :: psi(nx,ny)

            real :: rptt,diff,rpt
            integer :: i,j

            ! Start in the lower left corner
            psi(1,1) = 0.0

            ! Integrate along left boundary
            i = 1
            do j=2,ny
              psi(i,j) = psi(i,j-1) - dy*0.5*(u(i,j)+u(i,j-1))
            end do

            ! Integrate along top boundary
            j=ny
            do i=2,nx
              psi(i,j) = psi(i-1,j) + dx*0.5*(v(i,j)+v(i-1,j))
            end do

            ! Integrate along right boundary
            i=nx
            do j=ny-1,1,-1
              psi(i,j) = psi(i,j+1) + dy*0.5*(u(i,j)+u(i,j+1))
            end do

            ! Integrate along bottom boundary
            j=1
            do i=nx-1,2,-1
              psi(i,j) = psi(i+1,j) - dx*0.5*(v(i,j)+v(i+1,j))
            end do

            ! Spread the difference between psi(1,1) and psi(2,1) 
            ! over the entire boundary

            rptt = 2.0*float(nx+ny)
            diff = (psi(1,1)-psi(2,1))

            rpt = 0.0
            i = 1
            do j=2,ny
              rpt = rpt+1.0
              psi(i,j) = psi(i,j) + diff*rpt/rptt
            end do

            j=ny
            do i=2,nx
              rpt = rpt+1.0
              psi(i,j) = psi(i,j) + diff*rpt/rptt
            end do

            i = nx
            do j=ny-1,1,-1
              rpt = rpt+1.0
              psi(i,j) = psi(i,j) + diff*rpt/rptt
            end do

            j=1
            do i=nx-1,2,-1
              rpt = rpt+1.0
              psi(i,j) = psi(i,j) + diff*rpt/rptt
            end do

            return

          end subroutine psibd

          subroutine dxcal(f,nx,ny,dx,dy,dfdx)
          ! This routine calculates the x-derivative of f

            integer,intent(in) :: nx,ny
            real,intent(in) :: f(nx,ny),dx,dy
            real,intent(inout) ::dfdx(nx,ny)
          
            real :: dxi,tdxi
            integer :: i,j

            dxi  = 1.0/(    dx)
            tdxi = 1.0/(2.0*dx)

            ! Points not affected by boundaries
            do j=1,ny
              do i=2,nx-1
                dfdx(i,j) = tdxi*(f(i+1,j) - f(i-1,j))
              end do
            end do

            ! Side boundaries
            do j=1,ny
              ! Left boundary
              i=1
              dfdx(i,j) = dxi*(f(i+1,j) - f(i,j))

              ! Right boundary
              i=nx
              dfdx(i,j) = dxi*(f(i,j) - f(i-1,j))
            end do

            return

          end subroutine dxcal

          subroutine dycal(f,nx,ny,dx,dy,dfdy)
          ! This routine calculates the y-derivative of f

            integer,intent(in) :: nx,ny
            real,intent(in) :: f(nx,ny),dx,dy
            real,intent(inout) :: dfdy(nx,ny)
 
            real :: dyi,tdyi
            integer :: i,j

            dyi  = 1.0/(    dy)
            tdyi = 1.0/(2.0*dy)

            ! Points not affected by boundaries
            do j=2,ny-1
              do i=1,nx
                dfdy(i,j) = tdyi*(f(i,j+1) - f(i,j-1))
              end do
            end do

            ! Top/Bottom boundaries
            do i=1,nx
              ! bottom boundary
              j=1
              dfdy(i,j) = dyi*(f(i,j+1) - f(i,j))

              ! top boundary
              j=ny
              dfdy(i,j) = dyi*(f(i,j) - f(i,j-1))
            end do

            return

          end subroutine dycal

          subroutine d2xxcal(f,nx,ny,dx,dy,d2fdx2)
          ! This routine calculates d2f/dx2 in Cartesian coordinates

            integer :: nx,ny
            real,intent(in) :: f(nx,ny),dx,dy
            real,intent(inout) :: d2fdx2(nx,ny)

            real :: dx2i
            integer :: i,j

            dx2i = 1.0/(dx*dx)

            ! Interior points
            do j=1,ny
              do i=2,nx-1
                d2fdx2(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  ))
              end do
            end do

            do j=1,ny
              ! Left edge
              i = 1
              d2fdx2(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  ))

              ! Right edge
              i = nx
              d2fdx2(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  ))
            end do

            return
         
          end subroutine d2xxcal

          subroutine d2yycal(f,nx,ny,dx,dy,d2fdy2)
          ! This routine calculates d2f/dy2 in Cartesian coordinates

            integer,intent(in) :: nx,ny
            real,intent(in) :: f(nx,ny),dx,dy
            real,intent(inout) :: d2fdy2(nx,ny)

            real :: dy2i
            integer :: i,j

            dy2i = 1.0/(dy*dy)

            ! Interior points
            do j=2,ny-1
              do i=1,nx
                d2fdy2(i,j) = dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
              end do
            end do

            do i=1,nx
              ! Bottom edge
              j = 1
              d2fdy2(i,j) = dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))

              ! Top edge
              j = ny
              d2fdy2(i,j) = dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))
            end do

            return

          end subroutine d2yycal

          subroutine d2xycal(f,nx,ny,dx,dy,d2fdxy)
          ! This routine calculates d2f/dxdy in Cartesian coordinates,
          ! using centered differences. The mixed derivative is only
          ! calculated at the domain interior points, and is set to zero
          ! on the domain boundaries.

            integer,intent(in) :: nx,ny
            real,intent(in) :: f(nx,ny),dx,dy
            real,intent(inout) :: d2fdxy(nx,ny)
 
            real :: dxdyi
            integer :: i,j

            dxdyi = 0.25/(dx*dy)

            ! Interior points
            do j=1,ny
              do i=2,nx-1
                d2fdxy(i,j) = dxdyi*( f(i+1,j+1)+f(i-1,j-1) &
                             -f(i+1,j-1)-f(i-1,j+1) )
              end do
            end do

            ! Set boundary values to zero
            call zbound(d2fdxy,0.0,nx,ny)

            return

          end subroutine d2xycal

          subroutine del2xy(f,nx,ny,dx,dy,d2f)
          ! This routine calculates the Laplacian of f in Cartesian coordinates

            integer,intent(in) :: nx,ny
            real,intent(in) :: dx,dy,f(nx,ny)
            real,intent(inout) :: d2f(nx,ny)

            real :: dx2i,dy2i
            integer :: i,j

            dx2i = 1.0/(dx*dx)
            dy2i = 1.0/(dy*dy)

            ! Interior points
            do j=2,ny-1
              do i=2,nx-1
                d2f(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) + &
                dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
              end do
            end do

            do j=2,ny-1
            ! Left edge
              i = 1
              d2f(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) + &
                         dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
            ! Right edge
              i = nx
              d2f(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) + &
                         dy2i*(f(i  ,j+1)+f(i  ,j-1)-2.0*f(i  ,j  ))
            end do

            do i=2,nx-1
            ! Bottom edge
              j = 1
              d2f(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) + &
                         dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))

            ! Top edge
              j = ny
              d2f(i,j) = dx2i*(f(i+1,j  )+f(i-1,j  )-2.0*f(i  ,j  )) + &
                         dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))
            end do

            ! Lower left corner
            i=1
            j=1
            d2f(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) + &
                       dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))

            ! Upper left corner
            i=1
            j=ny
            d2f(i,j) = dx2i*(f(i+2,j  )+f(i  ,j  )-2.0*f(i+1,j  )) + &
                       dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))

            ! Upper right corner
            i=nx
            j=ny
            d2f(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) + &
                       dy2i*(f(i  ,j  )+f(i  ,j-2)-2.0*f(i  ,j-1))

            ! Lower right corner
            i=nx
            j=1
            d2f(i,j) = dx2i*(f(i  ,j  )+f(i-2,j  )-2.0*f(i-1,j  )) + &
                       dy2i*(f(i  ,j+2)+f(i  ,j  )-2.0*f(i  ,j+1))

            return

          end subroutine del2xy 

          subroutine psonxy(f,gy,h,hfg,ierr)
          ! This routine solves (del**2 + g(y)d/dy)h = f in Cartesian
          ! geometry using over-relaxation. The boundary values for h
          ! and the first guess for h at the interior points
          ! are assumed to be in the array hfg.

          ! If the relaxation converges, ierr=0, otherwise, ierr=1.
            use dims
            use cons
            use cgrid 

            real,intent(in) :: f(nx,ny),gy(ny)
            real,intent(inout) :: h(nx,ny),hfg(nx,ny)
            integer,intent(inout) :: ierr

            integer :: nit,iecal
            real :: emax,def,omega,oomega,enorm
            real :: bsum,cnt,c1,c2,c3,c4,errt,errtp
            integer :: i,j,k

            ! Specify max number of iterations and error check increment
            nit = 200
            iecal = 10
            emax = 1.0e-5

            ! Calculate constants for over-relaxation
            def    = 1.0 - 2.0*((sin(pi/(2.0*float(ny-1))))**2)
            omega  = 2.0/(1.0 + sqrt(1.0-def*def))
            oomega = 1.0-omega

!           write(6,410) omega,oomega
!           410 format(' omega, oomega: ',e11.4,1x,e11.4)

            ! Move first guess h to h array
            do j=1,ny
              do i=1,nx
                h(i,j) = hfg(i,j)
              end do
            end do

            ! Find the maximum magnitude of f for normalizing the error
            enorm = 0.0
            do j=2,ny-1
              do i=2,nx-1
                if (abs(f(i,j)) .gt. enorm) enorm = abs(f(i,j))
              end do
            end do

            ! The forcing term is zero. Scale error from boundary values.
            if (enorm .le. 0.0) then
              bsum = 0.0
              cnt = 0.0
              do j=1,ny
                do i=1,nx
                  if (j .ne. 1 .and. j .ne. ny .and. &
                      i .ne. 1 .and. i .ne. nx     ) exit 
                  cnt = cnt + 1.0
                  bsum = bsum + abs(h(i,j))
                end do
              end do

              bsum = bsum/cnt
              enorm = bsum/(dx*dy)

            endif

!           write(6,400) enorm
!           400 format(' enorm=',e11.4)

!           Calculate common factors for iteration
            c4 = 1.0/( 2./(dx*dx) + 2./(dy*dy) )
            c3 = c4/(2.0*dy)
            c2 = c4/(dy*dy)
            c1 = c4/(dx*dx)

            ! Perform iteration
            errtp = 1.0e+10

            do k=1,nit
              do j=2,ny-1
                do i=2,nx-1
                  h(i,j) = oomega*h(i,j) + omega*( & 
                           c1*(h(i+1,j)+h(i-1,j)) + &
                           c2*(h(i,j+1)+h(i,j-1)) + &
                           c3*(h(i,j+1)-h(i,j-1))*gy(j) - &
                           c4*(f(i,j)))
                end do
              end do

              ! Check for convergence
              if (mod(k,iecal) .eq. 0) then
                call rchkxy(f,gy,h,dx,dy,enorm,errt)

!               write(6,888) k,errt,emax,enorm,h(11,11)
!               888 format(1x,'k=',i3,' errt,emax,enorm,h= ',4(e11.4))

                ! Converged, return
                if (errt .le. emax .or. errt .gt. errtp) then 
                  ierr = 0
                  return
                end if

                errtp = errt

              endif
 
            end do

            ! Didn't converge
            ierr = 1
            return

          end subroutine psonxy

          subroutine rchkxy(f,g,h,dx,dy,enorm,errt)
          ! This routine calculates the relative error between
          ! (del**2 + g(y)d/dx)h and f for determining if the
          ! relaxation in routine psonxy has converged.
            use dims
 
            real,intent(in) :: f(nx,ny),h(nx,ny),g(ny),dx,dy,enorm
            real,intent(inout) :: errt
 
            ! Local arrays
            real :: d2h(nx,ny),dhdy(nx,ny)
            
            real :: rpts,scale
            integer :: i,j
 
            rpts = float( (nx-1)*(ny-1) )
            errt = 0.0
            scale = 1.0
 
            call del2xy(h,nx,ny,dx,dy,d2h)
            call dycal(h,nx,ny,dx,dy,dhdy)
 
            do j=2,ny-1
              do i=2,nx-1
                d2h(i,j) = d2h(i,j) + g(j)*dhdy(i,j)
              end do
            end do
  
            do j=2,ny-1
              do i=2,nx-1
                errt = errt + (f(i,j)-d2h(i,j))**2
              end do
            end do
 
            errt = ( sqrt(errt/rpts) )/enorm
  
            return
  
          end subroutine rchkxy

          subroutine zinter(f,c,nx,ny)
          ! This routine sets the interior points in the array f to a constant

            real,intent(inout) :: f(nx,ny)
            real,intent(in) :: c 
            integer,intent(in) :: nx,ny
 
            integer :: i,j

            do j=2,ny-1
              do i=2,nx-1
                f(i,j) = c
              end do 
            end do

            return

         end subroutine zinter
 
         subroutine zbound(f,c,nx,ny)
         ! This routine sets the boundary points in the array f to a constant

            real,intent(inout) :: f(nx,ny)
            real,intent(in) :: c 
            integer,intent(in) :: nx,ny
 
            integer :: i,j

            do j=1,ny
              f( 1,j) = c
              f(nx,j) = c
            end do

            do i=2,nx-1
              f(i, 1) = c
              f(i,ny) = c
            end do

            return

          end subroutine zbound

          subroutine fprint(f,nx,ny,label)
          ! This routine prints f(x,y)
            use io

            integer,intent(in) :: nx,ny
            real,intent(in) :: f(nx,ny)
            character(len=*) :: label

            integer :: i,j

            write(lulog,200) label
            200 format(/,1x,a30)

            do j=ny,1,-1
              write(lulog,220) j,( nint(f(i,j)+0.5*sign(1.0,f(i,j)) ),i=1,nx)
              220 format(1x,i2,1x,31(i4))
            end do
            write(lulog,230) (i,i=1,nx)
            230 format(3x,31(i4))

            return

          end subroutine fprint

        end module besubs
