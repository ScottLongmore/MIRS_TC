      module amsusubs
        implicit none

        real,parameter :: a = 5.47e-4
        real,parameter :: b = 8.57e-1
        real,parameter :: c = 48.0

        contains

        subroutine amsures(dist,res)
        ! This routine estimates the resolution of the AMSU data 
        ! as a function of the distance from the satellite sub point

        ! Input:  dist = distance in km from satellite sub-point
        ! Output: res  = Approximate distance (km) between scan points
          real,intent(in) :: dist
          real,intent(inout) :: res

          real :: x

        ! Check for invalid range of dist
          if (dist .lt. 0.0 .or. dist .gt. 1500.0) then
	    res = -99.9
	    return
          end if

          x = dist/100.0
          res = a*(x**4) + b*(x**2) + c

          return

        end subroutine amsures

      end module amsusubs 
