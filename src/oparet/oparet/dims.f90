      module dims 
        implicit none

        ! Specify the lon/lat dimensions of the AMSU analysis domain
        integer, parameter :: nx=61,ny=61

        ! Specify number of AMSU pressure levels for calculations
        ! and first pressure (out of 40, see array pamsu) to use
        integer, parameter :: np=23,npst=16

        ! Specify number of NCEP pressure levels
        integer, parameter :: npn=12

        ! Specify maximum dimensions of NCEP grid arrays
        integer, parameter :: ixmax=361,iymax=181

        ! Specify max number of AMSU swath points and pressure levels
        integer, parameter :: mxas=30000,mpas=40

        ! Specify number of radial and height points for gradient wind
        ! calculations
        integer, parameter :: nr=31,nz=21

      end module dims 
