        module params
          ! **** Begin Parameter Specification ****
          use dims
          implicit none

          ! ++ 3-D analysis grid parameters

          ! Specify lat/lon spacing of AMSU analysis grid
          real, parameter :: dlat = 0.20
          real, parameter :: dlon = 0.20

          ! Set idex=1 to exclude data outside of analysis domain. Also
          ! set lat/lon buffers for exclusion
          integer, parameter :: idex = 1
          real, parameter :: rlonbf =3.0
          real, parameter :: rlatbf =3.0

          ! Specify maximum distance (km) the storm can be from the swath center
          ! to perform an analysis
          real, parameter :: threshr =700.0

          ! ++ Gradient wind parameters

          ! Set iaxsym =1 for axisymmetric analysis
          integer, parameter :: iaxsym=1

          ! Specify the radial and height spacing (m) for gradient wind calculations
          real, parameter :: dr = 20.0e+3
          real, parameter :: dz = 1.0e+3

          ! ++ 3-D wind parameters
          ! Set iwinda = 0 to use NCEP x,y winds  
          !            = 1 to use linear balance equation for x,y winds
          !            = 2 to use nonlinear balance equation for x,y winds
          !                       (iterative solution)
          !            = 3 to use nonlinear balance equation for x,y winds
          !                       (variational solution, u,v form)
          !            = 4 to use nonlinear balance equation for x,y winds
          !                       (variational solution, psi form)
          integer, parameter :: iwinda=3

          ! If iwinda = 2, 3 or 4 , select method for first guess u,v:
          !    Set ifgnbe = 0 to use NCEP winds as first guess 
          !    Set ifgnbe = 1 to use linear balance winds at first guess
          !    Set ifgnbe = 2 to use zero curvature winds as first guess
          integer, parameter :: ifgnbe=0

          ! ++ Set flags for output files (0=don't write file, 1=write file)
          ! 
          ! Set igenfn=1 to use generic output file names, otherwise igenfn=0
          integer, parameter :: igenfn=1

          ! fix file (requires iaxsym=1, ifixall=1 writes fix file even if TC
          !                              analysis is not performed)
          integer, parameter :: ipfix = 1
          integer, parameter :: ifixall = 0

          ! loc file
          integer, parameter :: iploc = 1 

          ! rza file (requires iaxsym=1)
          integer, parameter :: iprza=1

          ! xya file 
          integer, parameter :: ipxya = 1 

          ! sta file (requires iaxsym=1 and ivrapd=1)
          integer, parameter :: ipsta = 1  

          ! apk file 
          integer, parameter :: ipapk = 0

          ! npk file
          integer, parameter :: ipnpk = 0

          ! ++ Barnes analysis parameters

          ! Specify e-folding and influence radii (km) for 2-D Barnes analysis
          ! of AMSU temperatures.
          ! Rule of thumb: rinfxy = efldxy*5, but rinfxy must be at least 400
          !  efldxy=40.0
          !  rinfxy=200.0
          ! efldxy=100.0
          real,parameter :: efldxyDefault=100.0
          real,parameter :: rinfxyDefault=6.0*efldxyDefault

          ! Set ispf=1 for a second pass of the x,y Barnes analyses or
          !     ispf=0 for single pass
          integer, parameter :: ispf=1

          ! Specify e-folding and influence radii (km) for 1-D Barnes analysis
          ! of AMSU temperatures. Also specify expansion factor (exfac) for
          ! increasing e-folding radius as a function of radius. 
          real,parameter :: efldrDefault=20.0
          real,parameter :: rinfrDefault=100.0
          ! efldr=120.0
          ! rinfr=480.0
          real, parameter :: exfac=0.0

          ! Set iradxy=1 to perform radial Barnes temperature analysis using
          ! gridded temperature from x,y analysis instead of swath data
          integer, parameter :: iradxy=1 

          ! Set iefa = 1 to adjust e-folding radii based upon horizontal 
          ! resolution, else set iefa=0. Also, specify reduction
          ! factor for 2dx wave, to choose e-folding radius.
          integer, parameter :: iefa = 0
          ! tdxfxy = 0.05
          ! tdxfr  = 0.20 
          real, parameter :: tdxfxy = 0.04
          real, parameter :: tdxfr  = 0.16

          ! ++ CLW attenuation and ice scattering corretion parameters

          ! Set itcor   = 0-10 to correct cold anomalies for water 
          !               vapor attenuation. 
          !               If itcor=0 no correction is applied 
          !               If itcor=1  temp analysis is corrected, using a
          !                           cold anomaly threshold
          !               If itcor=2  original swath data is corrected using a
          !                           cold anomaly threshold
          !               If itcor=3  original swath data is corrected using a
          !                           linear regression technique
          !               If itcor=4  original swath data is corrected using the
          !                           linear regression method (3), followed
          !                           by the cold anomaly threshold method (2). 
          !               If itcor=5  orginal swath data is adjusted in regions
          !                           with high liquid water content to a 
          !                           constant lapse rate sounding
          !               If itcor=6  method 3 is applied, followed by method 5
          !               If itcor=7  method 3, 5 and 2 are applied
          !               If itcor=8  temp analysis corrected; quadratically 
          !                           smoothed m(k) from Tdev vs CLW regression
          !                           slopes; m(k)=ap^2+bp+c where p=pressure in Pa
          !                           and b=c=0 according to initial conditions.
          !               If itcor=9  temp analysis corrected, for ice crystals
          !                           via B. Linstid's algorithm
          !               If itcor=10 temp analysis correction via method (8) and (9)

          !     ncemx,ncsmx = first,last AMSU level (out of analysis levels) 
          !                   to apply correction (x=2 applies to method 1 or 2,
          !                                        x=3 applies to method 3,
          !                                        x=5 applies to method 5,
          !                                        x=8 applies to method 8 or 10,
          !                                        x=9 applied to method 9 or 10)
          !     dt          = anomaly threshold for applying correction
          !                   (for itcor=1 or 2)
          !     dtred       = Factor to reduce fraction of cold anomaly below dt
          !                   (for itcor=1 or 2)
          !     tcrmax      = Max radius (km) from storm for applying method 3

          integer, parameter :: itcor=10

          ! Specify variables for method 1 and 2
          integer, parameter :: ncsm2 = np-5
          integer, parameter :: ncem2 = np
          real, parameter :: dt    = -0.5
          real, parameter :: dtred = 0.10
          real, parameter :: tcrmax = 900.0

          ! Specify variables for method 3
          integer, parameter :: ncsm3 = 11
          integer, parameter :: ncem3 = np

          integer, parameter :: ncs2 = ncsm2 + npst - 1
          integer, parameter :: nce2 = ncem2 + npst - 1
          integer, parameter :: ncs3 = ncsm3 + npst - 1
          integer, parameter :: nce3 = ncem3 + npst - 1

          ! Specify variables for method 5
          integer, parameter :: ncsm5 = 13
          integer, parameter :: ncem5 = np
          real, parameter :: clwth1 = 1.00
          real, parameter :: clwth2 = 2.00

          integer, parameter :: ncs5 = ncsm5 + npst - 1
          integer, parameter :: nce5 = ncem5 + npst - 1

          ! Specify variables for method 8
          ! Variables for Ben Linstid's original correction
          !  ncsm8 = 10
          !  ncem8 = np
          !  amkx = 5.78
          !  xlambda = 1.55

          ! Variables for Tdev vs CLW regression correction on grid
          ! The first amkx value is with no core removed from the grid data,
          ! and the second is with a 2 x 2 degree core removed (comment one out).
          !  ncsm8 = 12
          !  ncem8 = np
          !  amkx = 4.27435
          !  amkx = 4.04594
          !  xlambda = 1.0

          ! Variables for Tdev vs CLW regression correction on swath
          integer, parameter :: ncsm8 = 12 + npst - 1
          integer, parameter :: ncem8 = np + npst - 1
          real, parameter :: amkx = 4.57166
          real, parameter :: xlambda = 1.0

          ! Specify variables for method 9
          integer, parameter :: ncsm9 = 12 
          integer, parameter :: ncem9 = np 

          ! ++ NCEP analysis parameters

          ! Set ismooth to the number of times to smooth the NCEP 
          ! fields after interpolating them to the AMSU analysis grid
          integer, parameter :: ismooth=3

          ! Set intop=1 to use NCEP height field at top of AMSU domain
          ! as boundary condition. This is only possible if AMSU domain
          ! top is at 100 mb. 
          integer, parameter :: intop=0

          ! Set ibrem=1 to correct amsu T,Z and surface P to make the mean
          ! equal to that in the NCEP analysis
          integer, parameter :: ibrem = 0

          ! **** End Parameter Specification ****
   
          save
  
        end module params
