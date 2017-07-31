      module cgrid 
        use dims
        use cons
        use ncepll
        implicit none

        ! Arrays for Cartesian grid calculations 
        real :: x(nx),y(ny),fy(ny),dx,dy,f0,beta

        save

        contains

        subroutine init_cgrid(reflon,reflat)
          real,intent(in) :: reflon,reflat

          real :: crl
          integer :: i,j

          do j=1,ny
            y(j) = erad*dtr*(rlatd(j)-reflat)
          enddo
  
          crl = cos(dtr*reflat)
          do i=1,nx
            x(i) = erad*dtr*(rlond(i)-reflon)*crl
          enddo
  
          dx = x(2)-x(1)
          dy = y(2)-y(1)
          f0   = 2.0*erot*sin(dtr*reflat)
          beta = 2.0*erot*cos(dtr*reflat)/erad

          do j=1,ny
            fy(j) = f0 + beta*y(j)
          enddo
        
        end subroutine init_cgrid

      end module cgrid 
