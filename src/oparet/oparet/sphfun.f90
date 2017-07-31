      module sphfun
      ! This code contains several subroutines for
      ! manipulating arrays in spherical coordinates. Most of the
      ! routines require the variables in common blocks /cons/
      ! and /ncepll/.
      !
      ! Included routines:
      !   ** ddx
      !   ** ddy
      !   ** del2
      !   ** pson
        use io
        implicit none

        contains

        subroutine ddx(h,scale,dhdx)
        ! This routine calculates the x-derivative of h, multiplied by
        ! scale to give dhdx, where x = a*cos(lat)*lon
          use ncepll 

          real, intent(in) :: h(nx,ny) 
          real, intent(in) :: scale
          real, intent(inout) :: dhdx(nx,ny)
  
          real :: cf
          integer :: i,j
  
          ! Interior points
          cf = scale/(erad*2.0*dlonr)
  
          do j=1,ny
            do i=2,nx-1
              dhdx(i,j) = cf*(h(i+1,j)-h(i-1,j))/coslat(j)
            end do
          end do

          ! x-boundary points
          cf = 2.0*cf
          do j=1,ny
            dhdx( 1,j) = cf*(h( 2,j) - h(   1,j))/coslat(j)
            dhdx(nx,j) = cf*(h(nx,j) - h(nx-1,j))/coslat(j)
          end do

          return

        end subroutine ddx

        subroutine ddy(h,scale,dhdy)
        ! This routine calculates the y-derivative of h, multiplied by
        ! scale to give dhdy, where y = a*lat
          use ncepll 

          real, intent(in) :: h(nx,ny) 
          real, intent(in) :: scale
          real, intent(inout) :: dhdy(nx,ny)

          real :: cf
          integer :: i,j

          ! Interior points
          cf = scale/(erad*2.0*dlatr)

          do i=1,nx
            do j=2,ny-1
              dhdy(i,j) = cf*(h(i,j+1)-h(i,j-1))
            end do
          end do

          ! y-boundary points
          cf = 2.0*cf
          do  i=1,nx
            dhdy(i, 1) = cf*(h(i, 2) - h(i,   1))
            dhdy(i,ny) = cf*(h(i,ny) - h(i,ny-1))
          end do

         return

        end subroutine ddy

        subroutine del2(h,scale,del2h)
        ! This routine calculates the Laplacian of h, multiplied by
        ! scale, to give del2h
          use ncepll 

          real,intent(in) :: h(nx,ny)
          real,intent(in) :: scale
          real,intent(inout) ::del2h(nx,ny)

          real :: cfx2,cfy2,cfy1,cli2
          integer :: i,j

          cfx2 =  scale/(erad*erad*dlonr*dlonr)
          cfy2 =  scale/(erad*erad*dlatr*dlatr)
          cfy1 = -scale/(erad*erad*2.0*dlatr)

          ! Interior points
          do j=2,ny-1
            cli2 = 1.0/(coslat(j)**2)
            do i=2,nx-1
              del2h(i,j) = cfx2*(h(i+1,j)+h(i-1,j) - 2.0*h(i,j))/cli2 + &
                           cfy2*(h(i,j+1)+h(i,j-1) - 2.0*h(i,j)) + &
                           cfy1*(h(i,j+1)-h(i,j-1))*tanlat(j)
            end do
          end do

          ! Bottom and top boundary points
          do i=2,nx-1
            j = 1
            cli2 = 1.0/(coslat(j)**2)
            del2h(i,j) = cfx2*(h(i+1,j)+h(i-1,j) - 2.0*h(i,j  ))/cli2 + &
                         cfy2*(h(i,j+2)+h(i  ,j) - 2.0*h(i,j+1)) + &
                         cfy1*(h(i,j+1)-h(i,j))*2.0*tanlat(j)

            j = ny
            cli2 = 1.0/(coslat(j)**2)
            del2h(i,j) = cfx2*(h(i+1,j)+h(i-1,j  ) - 2.0*h(i,j  ))/cli2 + &
                         cfy2*(h(i  ,j)+h(i  ,j-2) - 2.0*h(i,j-1)) + &
                         cfy1*(h(i,j)-h(i,j-1))*2.0*tanlat(j)
          end do

          ! left and right boundary points
          do j=2,ny-1
            i = 1
            cli2 = 1.0/(coslat(j)**2)
            del2h(i,j) = cfx2*(h(i+2,j  )+h(i,j  ) - 2.0*h(i+1,j))/cli2 + &
                         cfy2*(h(i  ,j+1)+h(i,j-1) - 2.0*h(i,j)) + &
                         cfy1*(h(i  ,j+1)-h(i,j-1))*tanlat(j)
            i = nx
            cli2 = 1.0/(coslat(j)**2)
            del2h(i,j) = cfx2*(h(i,j  )+h(i-2,j  ) - 2.0*h(i-1,j))/cli2 + &
                         cfy2*(h(i,j+1)+h(i  ,j-1) - 2.0*h(i  ,j)) + &
                         cfy1*(h(i,j+1)-h(i  ,j-1))*tanlat(j)
          end do 

          ! Upper-left corner
          i = 1
          j = ny
          cli2 = 1.0/(coslat(j)**2)
          del2h(i,j) = cfx2*(h(i+2,j)+h(i,j  ) - 2.0*h(i+1,j  ))/cli2 + &
                      cfy2*(h(i  ,j)+h(i,j-2) - 2.0*h(i  ,j-1)) + &
                      cfy1*(h(i  ,j)-h(i,j-1))*tanlat(j)

          ! Lower-left corner
          i = 1
          j = 1
          cli2 = 1.0/(coslat(j)**2)
          del2h(i,j) = cfx2*(h(i+2,j  )+h(i,j) - 2.0*h(i+1,j  ))/cli2 + &
                       cfy2*(h(i  ,j+2)+h(i,j) - 2.0*h(i  ,j+1)) + &
                       cfy1*(h(i  ,j+1)-h(i,j))*tanlat(j)

          ! Upper-right corner
          i = nx
          j = ny
          cli2 = 1.0/(coslat(j)**2)
          del2h(i,j) = cfx2*(h(i-2,j  )+h(i,j) - 2.0*h(i-1,j  ))/cli2 + &
                       cfy2*(h(i  ,j)+h(i,j-2) - 2.0*h(i  ,j-1)) + &
                       cfy1*(h(i  ,j)-h(i,j-1))*tanlat(j)

          ! Lower-right corner
          i = nx
          j = 1
          cli2 = 1.0/(coslat(j)**2)
          del2h(i,j) = cfx2*(h(i-2,j  )+h(i,j) - 2.0*h(i-1,j  ))/cli2 + &
                       cfy2*(h(i  ,j+2)+h(i,j) - 2.0*h(i  ,j+1)) + &
                       cfy1*(h(i  ,j+1)-h(i,j))*tanlat(j)
 
          return

        end subroutine del2

        subroutine pson(f,hfg,h,ierr)
        ! This routine solves (del**2)h = f in spherical geometry using
        ! over-relaxation. The boundary values for h and the first guess
        ! for h at the interior points are assumed to be in the array hfg.
        ! If the relaxation converges, ierr=0, otherwise, ierr=1.
          use ncepll 
          use io

          real,intent(inout) :: f(nx,ny),hfg(nx,ny),h(nx,ny)
          integer,intent(inout) :: ierr

          ! Local variables for storing coefficients
          real ::  c1(ny),c2(ny),c3(ny),c4(ny)

          integer :: nit,iecal
          real :: emax
          real :: def,omega,oomega
          real :: enorm
          real :: sdlonr,t1
          real :: bsum, cnt
          real :: errtp,errt
          integer :: i,j,k

          ! Specify max number of iterations and error check increment
          nit = 200
          iecal = 10
          emax = 2.0e-5

          ! Calculate constants for over-relaxation
          def    = 1.0 - 2.0*((sin(pi/(2.0*float(ny-1))))**2)
          omega  = 2.0/(1.0 + sqrt(1.0-def*def))
          oomega = 1.0-omega

          ! Move first guess h to h array
          do j=1,ny
            do  i=1,nx
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

          if (enorm .le. 0.0) then
            ! The forcing term is zero. Scale error from boundary values.
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

            enorm = bsum/(dlatr*dlatr*erad*erad)
          end if

          ! Calculate common factors for iteration
          do j=1,ny
            sdlonr = coslat(j)*dlonr
            t1 = ( (dlatr*sdlonr)**2 )/ &
                 ( dlatr**2 + sdlonr**2 )

            c1(j) = 0.5*t1/(sdlonr*sdlonr)
            c2(j) = 0.5*t1/(dlatr*dlatr)
            c3(j) = -0.25*t1/(dlatr)
            c4(j) = -0.5*t1*erad*erad
          end do

          ! Perform iteration
          errtp = 1.0e+10

          do k=1,nit

            do j=2,ny-1
              do i=2,nx-1
                h(i,j) = oomega*h(i,j) + omega*(     &
                         c1(j)*(h(i+1,j)+h(i-1,j)) + &
                         c2(j)*(h(i,j+1)+h(i,j-1)) + &
                         c3(j)*(h(i,j+1)-h(i,j-1)) + &
                         c4(j)*(f(i,j)) )
              end do
            end do

            ! Check for convergence
            if (mod(k,iecal) .eq. 0) then
              call rchk(f,h,enorm,errt)

              write(lulog,888) k,errt,emax,enorm
              888 format(1x,'k=',i3,' errt,emax,enorm= ',3(e11.4))

              if (errt .le. emax .or. errt .gt. errtp) then 
                ierr = 0
                return
              end if

              errtp = errt

            end if

          end do

          ! Never converged
          ierr = 1
          return

        end subroutine pson

        subroutine rchk(f,h,enorm,errt)
        ! This routine calculates the relative error between (del**2)h and f for
        ! determining if the relaxation in routine pson has converged.
          use ncepll 

          real,intent(in) :: f(nx,ny),h(nx,ny),enorm
          real,intent(inout) :: errt

          real :: d2h(nx,ny)
          real :: rpts,scale
          integer :: i,j
          
          rpts = float( (nx-1)*(ny-1) )
          errt = 0.0
          scale = 1.0

          call del2(h,scale,d2h)
          do j=2,ny
            do i=2,nx
              errt = errt + (f(i,j)-d2h(i,j))**2
            end do
          end do

          errt = ( sqrt(errt/rpts) )/enorm

          return

        end subroutine rchk

      end module sphfun
