PROGRAM mreg
!==============================================================================
! mreg.f90 Jack Dostalek
!
! This program provides a (hopefully) flexible way to perform multiple
! linear regression. It was initially written for the PSDI and Cal/Val
! projects but will be written in a manner general enough for use
! outside those two projects.
!
! History:
! 22 Apr 2015   Programming begun
! 23 Apr 2015   Interface to rstep complete. Results consistent with output
!               from heritage program mreg_oparet.f90
! 23 Apr 2015   Interface to rlse complete.
! 27 Apr 2015   Call to rstep using non-standardized values complete
!               Creation of verification module mreg_verify.f90 complete.
!
!==============================================================================
USE mreg_read
USE mreg_regress
USE mreg_verify

IMPLICIT NONE

!==============================================================================
! Read configuration file
CALL read_config

! Read in data
CALL read_data

! Regression options
CALL stepwise

! Check regression results
CALL verify_wintercept
CALL verify_atcf




!==============================================================================
END PROGRAM mreg
 
