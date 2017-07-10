REAL FUNCTION timediff (cdtg1,cdtg2)
  !
  ! Given two character date time groups YYYYMMDDHHMM return the real 
  ! number of hours hours=(cdtg2 - cdtg1)
  
  IMPLICIT NONE

  CHARACTER (LEN=12) :: cdtg1,cdtg2

  INTEGER :: min1
  INTEGER :: min2,ihours,istat
  CHARACTER (LEN=10) cdtga,cdtgb
  
  cdtga=cdtg1(1:10)
  cdtgb=cdtg2(1:10)

  CALL dtgdif2(cdtga,cdtgb,ihours,istat)

  READ(cdtg1,('(10x,i2)')) min1
  READ(cdtg2,('(10x,i2)')) min2

  timediff=float(ihours) + float((min2-min1))/60.0

END FUNCTION timediff
