      module cons
        implicit none

        ! Specify physical constants (mks units)
        real, parameter :: pi   = 3.14159
        real, parameter :: dtr  = 0.0174533 
        real, parameter :: rtd  = 180/pi 
        real, parameter :: erad = 6371.0e+3
        real, parameter :: erot = 7.292e-5
        real, parameter :: adtr = erad*dtr

        real, parameter :: rd = 287.0
        real, parameter :: g  = 9.81

      end module cons

