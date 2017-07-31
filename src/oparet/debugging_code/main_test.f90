program  readcoeffs

        !use io
        use test1
        implicit none
        ! Subroutine to read coefficients from file
        ! Input:
        !   lucoef   - unit number of input file
        ! Output: 
        ! 5 arrays of coefficients:cvmx, cpmn, cr34,cr50,cr64

        !integer, parameter :: ncoefs = 25  !number of coefficients for
        !                                   !each var
        integer  :: io_status,open_status, istat
        integer,parameter  :: ncoefs=25
        !real, dimension(ncoefs) :: cvmx,cpmn,cr34,cr50,cr64
        real, allocatable :: cvmx(:),cpmn(:),cr34(:),cr50(:),cr64(:)
        character(25) :: fl_coeff_cvmx
        fl_coeff_cvmx = "vmx_atms0.inp.coef"

        allocate(cvmx(ncoefs))

        call readvarcoeffs(fl_coeff_cvmx,cvmx,ncoefs)
        !call readvarcoeffs(lucoef,fl_coeff_cpmn,cpmn)
        !call readvarcoeffs(lucoef,fl_coeff_cr34,cr34)
        !call readvarcoeffs(lucoef,fl_coeff_cr50,cr50)
        !call readvarcoeffs(lucoef,fl_coeff_cr64,cr64)

        print*, 'debug debug debug debug debug ',   &
            cvmx,cpmn,cr34,cr50,cr64
end
