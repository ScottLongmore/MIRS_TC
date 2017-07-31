
!subroutine readvarcoeffs(lucoef,varcoeff)
    
 
    ! Subroutine to read coefficients from file
    ! for single variable
    ! Input:
    !   lucoef   - unit number of input file
    ! Output: 
    ! one array of coefficients
    
    implicit none

    integer, parameter :: ncoefs = 25  !number of coefficients for
                                       !each var
    !character(len=120), intent(in) :: flcoef
    character(len=120) :: flcoef
    !integer, intent(in) :: lucoef, io_status,open_status
    integer :: lucoef, io_status,open_status, istat,i
    real, dimension(ncoefs) :: varcoeff
    
    lucoef = 25
    flcoef = "test_coef_dat.coef"    

    !open(unit=lucoef,file=flcoef,status='OLD',iostat=open_status)
    open(unit=lucoef,file=flcoef,status='OLD')
    !read(lucoef, 101, err=920, end = 910,iostat=io_status) varcoeff
    do i = 1,ncoefs
        read(lucoef, 101,err=920, end = 910,iostat=io_status) varcoeff(i)
        !read(lucoef, 101,end = 910,iostat=io_status) varcoeff(i)
        !read(lucoef, end = 910,iostat=io_status) varcoeff(i)
        !read(lucoef, end = 910) varcoeff(i)
        print*,  varcoeff(i)
    enddo

    101 format(f18.7)

    close(unit=lucoef)


    istat=0
    !return

    910 continue
    istat=1
    print*, 'error: istat 1'
    close(lucoef)
    !return

    920  continue
    istat=2
    print*, 'error: istat 2'
    close(lucoef)
    !return
    end
!end subroutine readvarcoeffs

