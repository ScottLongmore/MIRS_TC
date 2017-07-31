module test1
        !use io
        implicit none

        contains

        !subroutine readvarcoeffs(lucoef,flcoef,varcoeff)
        subroutine readvarcoeffs(flcoef,varcoeff, ncoefs)

        ! Subroutine to read coefficients from file
         ! for single variable
        ! Input:
        !   lucoef   - unit number of input file
        ! Output: 
        ! one array of coefficients

        !integer, parameter :: ncoefs = 25  !number of coefficients for
                                           !each var
        character(len=25), intent(in) :: flcoef
        !integer, intent(in) :: lucoef
        integer :: lucoef
        integer, intent(in) :: ncoefs
        !integer,parameter :: ncoefs=25
        integer :: istat, i, open_status, io_status
        real, dimension(ncoefs) :: varcoeff
        real :: coef
        open_status = 0
        lucoef = 35
        open(unit=lucoef,file=flcoef,status='OLD',iostat=open_status)

        do i = 1,ncoefs
            !read(lucoef,101,err=920,end = 910,iostat=io_status) & 
            print*, "************************** ", varcoeff(i),lucoef
            read(lucoef,101,iostat=io_status), coef
            print*,'coef', coef,open_status
            varcoeff(i) = coef
            print*, "****************************** ", flcoef
            print*, "****************************** ", varcoeff(i)
        enddo

  101   format(f18.7)

        close(lucoef)
        stop

        istat=0
        return

        910 continue
        istat=1
        close(lucoef)
        return

        920  continue
        istat=2
        close(lucoef)
        return

        end subroutine readvarcoeffs
end module test1

