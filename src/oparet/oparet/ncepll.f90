      module ncepll 
        use dims
        use cons
        implicit none

        ! Specify lat/lon spacing of AMSU analysis grid
        real :: dlonr, dlatr 
        real :: rlond(nx),rlatd(ny)
        real :: rlonr(nx),rlatr(ny)
        real :: sinlat(ny),coslat(ny),tanlat(ny)

        save

        contains

        subroutine init_ncepll(rlonl,rlatb)
        ! Calculate lat and lon in deg and radians, and related variables
          use params
          real,intent(in) :: rlonl,rlatb

          integer :: i,j

          dlonr = dlon*dtr
          dlatr = dlat*dtr

          do i=1,nx
            rlond(i) = rlonl + dlon*float(i-1)
            rlonr(i) = dtr*rlond(i)
          end do
  
          do j=1,ny
            rlatd(j) = rlatb + dlat*float(j-1)
            rlatr(j) = dtr*rlatd(j)
  
            sinlat(j) = sin(rlatr(j))
            coslat(j) = cos(rlatr(j))
            tanlat(j) = tan(rlatr(j))
          end do

        end subroutine init_ncepll

      end module ncepll 
