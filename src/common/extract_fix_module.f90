MODULE extract_fix_module
  !
  ! This module contains code to extract the information from the ATCF
  ! fdecks
  ! The primary routine extract_fix(fn,FTYPE) fills global arrays with the
  ! information from the fdeck for use in programs using this module
  !
  ! USES: datio.f dtgutls.f upcase.f dataformats.inc dataioparms.inc
  !
  ! WRITTEN BY:  John Knaff, NOAA/NESDIS/StAR/RAMMB
  !
  ! LAST MODIFIED:  November 19, 2008
  !****************************************************************************
  
  IMPLICIT NONE

  ! GLOBAL VARIABLES

  INTEGER:: ft_num ! number of the type of fix in the fdeck
  

  ! GLOBAL VARIABLES:    FIX RECORD VARIABLES

  INTEGER, ALLOCATABLE ::fr_time(:)  ! time hhmm
  INTEGER, ALLOCATABLE ::fr_date(:)  ! date YYYYmmdd
  INTEGER, ALLOCATABLE :: fr_height(:) ! height of the fix
  INTEGER, ALLOCATABLE :: fr_lcon(:) ! center fix confidence
  INTEGER, ALLOCATABLE :: fr_pcon(:) ! pressure fix confidence
  INTEGER, ALLOCATABLE :: fr_vcon(:) ! intensity fix confidence
  INTEGER, ALLOCATABLE :: fr_rcon(:) ! wind radii confidence
  REAL,ALLOCATABLE     :: fr_lat(:)  !latitude -90 to 90
  REAL,ALLOCATABLE     :: fr_lon(:)  !longitude 0 to 360
  REAL,ALLOCATABLE     :: fr_vmax(:) !maximum winds
  REAL,ALLOCATABLE     :: fr_mslp(:) !minimum sea level pressure
  REAL,ALLOCATABLE     :: fr_rmw(:)  !radius of maximum winds
  REAL,ALLOCATABLE     :: fr_eye(:)  !eye diameter
  REAL,ALLOCATABLE     :: fr_r34(:,:)! 34-kt wind radii
  REAL,ALLOCATABLE     :: fr_r50(:,:)! 50-kt wind radii
  REAL,ALLOCATABLE     :: fr_r64(:,:)! 64-kt wind radii 
  INTEGER              :: fr_cynum ! storm number asigned only once
  CHARACTER (LEN=2)    :: fr_basin ! basin asigned only once
  CHARACTER (LEN=12), ALLOCATABLE :: fr_cdtg(:) ! Character date/time group
  CHARACTER (LEN=5), ALLOCATABLE  :: fr_site(:) ! Character date/time group
  CHARACTER (LEN=1), ALLOCATABLE  :: fr_Flag(:) ! FLAGGED
  LOGICAL, ALLOCATABLE :: fr_lcent(:)! center fix? 
  LOGICAL, ALLOCATABLE :: fr_lpres(:)! pressure fix? 
  LOGICAL, ALLOCATABLE :: fr_lvmax(:)! intensity fix?   
  LOGICAL, ALLOCATABLE :: fr_lradi(:)! radii fix?   

  ! GLOBAL VARIABLES SUBJECTIVE DVORAK (DVTS) (10)

  CHARACTER (LEN=4), ALLOCATABLE :: dvts_sensor(:)    ! sensor
  INTEGER, ALLOCATABLE           :: dvts_pcn(:)       ! depreciation
  INTEGER, ALLOCATABLE           :: dvts_24hf(:)      ! 24-h forecast ci
  CHARACTER (LEN=6),ALLOCATABLE  :: dvts_sat(:)       ! satellite
  CHARACTER (LEN=4),ALLOCATABLE  :: dvts_centype(:)   ! center type
  CHARACTER (LEN=1),ALLOCATABLE  :: dvts_tropical(:)  ! type of cyclone T,S,E
  CHARACTER (LEN=52),ALLOCATABLE :: dvts_comments(:)  ! Comments

  INTEGER, ALLOCATABLE           :: dvts_lttnum(:)    ! long-term t number     
  INTEGER, ALLOCATABLE           :: dvts_ltcinum(:)   ! long-term ci number    
  CHARACTER (LEN=1), ALLOCATABLE :: dvts_ltintchg(:)  ! long-term intensity chg
  CHARACTER (LEN=1), ALLOCATABLE :: dvts_ltpastchg(:) ! long-term past chg     
  INTEGER, ALLOCATABLE           :: dvts_lttnumchg(:) ! long-term t-num chg    
  INTEGER, ALLOCATABLE           :: dvts_ltlasteval(:)! long-term last eval(hr)

  INTEGER, ALLOCATABLE           :: dvts_sttnum(:)    ! short-term t number    
  INTEGER, ALLOCATABLE           :: dvts_stcinum(:)   ! short-term ci number   
  CHARACTER (LEN=1), ALLOCATABLE :: dvts_stintchg(:)  ! short-term intensit chg
  CHARACTER (LEN=1), ALLOCATABLE :: dvts_stpastchg(:) ! short-term past chg   
  INTEGER, ALLOCATABLE           :: dvts_sttnumchg(:) ! short-term t-num chg  
  INTEGER, ALLOCATABLE           :: dvts_stlasteval(:)! long-term last eval(hr)
  
  ! GLOBAL VARIABLES OBJECTIVE DVORAK (DVTO) (20)

  CHARACTER (LEN=4), ALLOCATABLE  :: dvto_sensor(:)     ! sensor
  INTEGER, ALLOCATABLE            :: dvto_cinum(:)      ! CI number
  INTEGER, ALLOCATABLE            :: dvto_ciconf(:)     ! CI confidence
  INTEGER, ALLOCATABLE            :: dvto_tnumavg(:)    ! Avg tnum
  INTEGER, ALLOCATABLE            :: dvto_tnumavgtime(:)! averging time
  CHARACTER (LEN=1), ALLOCATABLE  :: dvto_tnumavgderv(:)! averaging durration
  INTEGER, ALLOCATABLE            :: dvto_tnumraw(:)    ! raw t number
  INTEGER, ALLOCATABLE            :: dvto_eyetemp(:)    ! eye temperature (c)
  INTEGER, ALLOCATABLE            :: dvto_cloudtemp(:)  ! cloud temp (c)
  CHARACTER (LEN=4), ALLOCATABLE  :: dvto_scenetype(:)  ! scene type
  CHARACTER (LEN=2), ALLOCATABLE  :: dvto_algorithm(:)  ! algorithm
  CHARACTER (LEN=6), ALLOCATABLE  :: dvto_sattype(:)    ! satellite type
  CHARACTER (LEN=1), ALLOCATABLE  :: dvto_tropical(:)   ! type of cyclone T,S,E
  CHARACTER (LEN=52), ALLOCATABLE :: dvto_comments(:)   ! Comments

  ! GLOBAL VARIABLES MICROWAVE (MICR)(30)

  LOGICAL, ALLOCATABLE            :: micr_rain(:)     ! rain flag
  INTEGER, ALLOCATABLE            :: micr_rainrate(:) ! rain rate (mm)
  CHARACTER (LEN=6), ALLOCATABLE  :: micr_algorithm(:)! algorithm
  INTEGER, ALLOCATABLE            :: micr_wave(:)     ! wave height (ft)
  INTEGER, ALLOCATABLE            :: micr_temp(:)     ! tempurature (passive)
  INTEGER, ALLOCATABLE            :: micr_slpraw(:)   ! raw mslp
  INTEGER, ALLOCATABLE            :: micr_slpretr(:)  ! retrieved mslp
  INTEGER, ALLOCATABLE            :: micr_seas(:)     ! max seas (ft)
  CHARACTER (LEN=6), ALLOCATABLE  :: micr_sattype(:)  ! satellite type
!  INTEGER, ALLOCATABLE            :: micr_rad(:)      ! radii (34,50,64)
!  CHARACTER (LEN=4), ALLOCATABLE  :: micr_windcode(:) ! radii wind code
  CHARACTER (LEN=4), ALLOCATABLE  :: micr_wc34(:)
  CHARACTER (LEN=4), ALLOCATABLE  :: micr_wc50(:)
  CHARACTER (LEN=4), ALLOCATABLE  :: micr_wc64(:)
  REAL, ALLOCATABLE               :: micr_r34(:,:)    ! r34 (octants)
  REAL, ALLOCATABLE               :: micr_r50(:,:)    ! r50 (octants)
  REAL, ALLOCATABLE               :: micr_r64(:,:)    ! r64 (octants)
  LOGICAL, ALLOCATABLE            :: micr_edge(:,:)     ! edges (octants)
  LOGICAL, ALLOCATABLE            :: micr_cut(:,:)      ! cut (edges)
  INTEGER, ALLOCATABLE            :: micr_radconf(:)  ! radii confidence
  CHARACTER (LEN=52), ALLOCATABLE :: micr_comments(:) ! Comments
  
  ! GLOBAL VARIABLES SCATTEROMETER (SCAT) (31)

  ! GLOBAL VARIABLES RADAR (RDR) (40)

  CHARACTER (LEN=1), ALLOCATABLE :: rdr_rdrtype(:)
  LOGICAL, ALLOCATABLE :: rdr_radobcode(:)
  LOGICAL, ALLOCATABLE :: rdr_plainlanguage(:)
  LOGICAL, ALLOCATABLE :: rdr_doppler(:)
  CHARACTER (LEN=10), ALLOCATABLE :: rdr_radob(:)
  CHARACTER (LEN=2), ALLOCATABLE :: rdr_eyeshape(:)
  INTEGER, ALLOCATABLE :: rdr_eyewallob(:)
  INTEGER, ALLOCATABLE :: rdr_spiralov(:)
  REAL, ALLOCATABLE :: rdr_lat(:)
  CHARACTER (LEN=1), ALLOCATABLE :: rdr_ns(:)
  REAL, ALLOCATABLE :: rdr_lon(:)
  CHARACTER (LEN=1), ALLOCATABLE :: rdr_ew(:)
  INTEGER, ALLOCATABLE :: rdr_vmaxin(:)
  INTEGER, ALLOCATABLE :: rdr_azimuthin(:)
  INTEGER, ALLOCATABLE :: rdr_rangein(:)
  INTEGER, ALLOCATABLE :: rdr_elevin(:)
  INTEGER, ALLOCATABLE :: rdr_vmaxout(:)
  INTEGER, ALLOCATABLE :: rdr_azimuthout(:)
  INTEGER, ALLOCATABLE :: rdr_rangeout(:)
  INTEGER, ALLOCATABLE :: rdr_elevout(:)
  INTEGER, ALLOCATABLE :: rdr_cloudheight(:)
  REAL, ALLOCATABLE :: rdr_rainaccum(:)
  INTEGER, ALLOCATABLE :: rdr_rainactimeint(:)
  REAL, ALLOCATABLE :: rdr_rainlat(:)
  CHARACTER (LEN=1), ALLOCATABLE :: rdr_rainns(:)
  REAL, ALLOCATABLE :: rdr_rainlon(:)
  CHARACTER (LEN=1), ALLOCATABLE :: rdr_rainew(:)
  CHARACTER (LEN=52), ALLOCATABLE :: rdr_comments(:)

  ! GLOBAL VARIABLES AIRCRAFT (AIRC) (50)
  
  INTEGER, ALLOCATABLE :: airc_flightlevelft(:)
  INTEGER, ALLOCATABLE :: airc_flightlevelmb(:)
  INTEGER, ALLOCATABLE :: airc_minheight(:)
  INTEGER, ALLOCATABLE :: airc_maxsurfacewind(:)
  INTEGER, ALLOCATABLE :: airc_maxsurfacewindbearing(:)
  INTEGER, ALLOCATABLE :: airc_maxsurfacewindrange(:)
  INTEGER, ALLOCATABLE :: airc_maxflightlevelwinddir(:)
  INTEGER, ALLOCATABLE :: airc_maxflightlevelwindint(:)
  INTEGER, ALLOCATABLE :: airc_maxflightlevelwindbearing(:)
  INTEGER, ALLOCATABLE :: airc_maxflightlevelwindrange(:)
  INTEGER, ALLOCATABLE :: airc_minsealevelpressure(:)
  INTEGER, ALLOCATABLE :: airc_eyetempoutside(:)
  INTEGER, ALLOCATABLE :: airc_eyetempinside(:)
  INTEGER, ALLOCATABLE :: airc_dewpoint(:)
  INTEGER, ALLOCATABLE :: airc_seasurface(:)
  INTEGER, ALLOCATABLE :: airc_eyewallthickness(:)
  INTEGER, ALLOCATABLE :: airc_eyeshape(:)
  INTEGER, ALLOCATABLE :: airc_eyeorientation(:)
  INTEGER, ALLOCATABLE :: airc_eyediameterlongaxis(:)
  INTEGER, ALLOCATABLE :: airc_eyediametershortaxis(:)
  REAL, ALLOCATABLE :: airc_navacc(:)
  REAL, ALLOCATABLE :: airc_navmet(:)
  INTEGER, ALLOCATABLE :: airc_missionnumber(:)
  CHARACTER (LEN=52), ALLOCATABLE :: airc_comments(:)

  ! GLOBAL VARIABLES DROPSONDE (DROP) (60)

  CHARACTER (LEN=10), ALLOCATABLE :: drop_sondeenv(:)
  INTEGER, ALLOCATABLE :: drop_height150(:)
  INTEGER, ALLOCATABLE :: drop_vspd150(:)
  INTEGER, ALLOCATABLE :: drop_vspd500(:)
  CHARACTER (LEN=52), ALLOCATABLE :: drop_comments(:)

  ! GLOBAL VARIABLES ANALYSIS (ANAL) (70)  

  CHARACTER (LEN=3), ALLOCATABLE :: anal_ititials(:)
  CHARACTER (LEN=12), ALLOCATABLE :: anal_startdtg(:)
  CHARACTER (LEN=12), ALLOCATABLE :: anal_enddtg(:)
  INTEGER, ALLOCATABLE :: anal_dist2data(:)
  INTEGER, ALLOCATABLE :: anal_sst(:)
  CHARACTER (LEN=24), ALLOCATABLE :: anal_obsources(:)
  CHARACTER (LEN=52), ALLOCATABLE :: anal_comments(:)
  
  

  CONTAINS
    
    LOGICAL FUNCTION extract_fix(ffn,iftype,fid,fsite)

      IMPLICIT NONE
      
      INCLUDE 'dataformats.inc'
      INCLUDE 'dataioparms.inc'

      ! Calling arguments
      CHARACTER (LEN=*), INTENT(IN) :: ffn ! fdeck file name
      INTEGER, INTENT(IN)::iftype ! fix type 10,20,30,31,40,50,60,70
      CHARACTER (LEN=4), INTENT(IN), OPTIONAL :: fid ! fix identifyer
      CHARACTER (LEN=5), INTENT(IN), OPTIONAL :: fsite ! fix site 

      ! Internal variables
      TYPE(FIX_DATA):: fixdata
      INTEGER:: result
      INTEGER:: imiss_small=-9
      INTEGER:: imiss=-99
      INTEGER:: luff=60
      INTEGER:: ios_open=0
      INTEGER:: frec_ok
      INTEGER:: ii,i,j
      REAL :: rmiss=-99.9
      CHARACTER*2:: bmiss='XX'
      LOGICAL :: check_fid, check_fsite,first

      ! Executable statements

      extract_fix = .false.
      first=.true.
      ft_num = 0
      check_fid = PRESENT(fid)
      check_fsite = PRESENT(fsite)

      ! Make sure globals are deallocated

      ! GLOBAL VARIABLES:    FIX RECORD VARIABLES

      IF (ALLOCATED  (fr_time)) DEALLOCATE(fr_time)  ! time hhmm
      IF (ALLOCATED  (fr_date)) DEALLOCATE(fr_date)  ! date YYYYmmdd
      IF (ALLOCATED(fr_height)) DEALLOCATE(fr_height)! height of the fix
      IF (ALLOCATED  (fr_lcon)) DEALLOCATE(fr_lcon)  ! center fix confidence
      IF (ALLOCATED  (fr_pcon)) DEALLOCATE(fr_pcon)  ! pressure fix confidence
      IF (ALLOCATED  (fr_vcon)) DEALLOCATE(fr_vcon)  ! intensity fix confidence
      IF (ALLOCATED  (fr_rcon)) DEALLOCATE(fr_rcon)  ! wind radii confidence
      IF (ALLOCATED  (fr_Flag)) DEALLOCATE(fr_flag)  ! FLAGGED
      IF (ALLOCATED   (fr_lat)) DEALLOCATE(fr_lat)   !latitude -90 to 90
      IF (ALLOCATED   (fr_lon)) DEALLOCATE(fr_lon)   !longitude 0 to 360
      IF (ALLOCATED  (fr_vmax)) DEALLOCATE(fr_vmax)  !maximum winds
      IF (ALLOCATED  (fr_mslp)) DEALLOCATE(fr_mslp)  !minimum sea level pressure
      IF (ALLOCATED   (fr_rmw)) DEALLOCATE(fr_rmw)   !radius of maximum winds
      IF (ALLOCATED   (fr_eye)) DEALLOCATE(fr_eye)   !eye diameter
      IF (ALLOCATED   (fr_r34)) DEALLOCATE(fr_r34)   ! 34-kt wind radii
      IF (ALLOCATED   (fr_r50)) DEALLOCATE(fr_r50)   ! 50-kt wind radii
      IF (ALLOCATED   (fr_r64)) DEALLOCATE(fr_r64)   ! 64-kt wind radii 
      IF (ALLOCATED  (fr_cdtg)) DEALLOCATE(fr_cdtg)  ! Character date/time group
      IF (ALLOCATED  (fr_site)) DEALLOCATE(fr_site)  ! Character date/time group
      IF (ALLOCATED (fr_lcent)) DEALLOCATE(fr_lcent) ! Center fix?
      IF (ALLOCATED (fr_lpres)) DEALLOCATE(fr_lpres) ! Pressure fix?
      IF (ALLOCATED (fr_lvmax)) DEALLOCATE(fr_lvmax) ! Intensity fix? 
      IF (ALLOCATED (fr_lradi)) DEALLOCATE(fr_lradi)  ! Radii fix?

      fr_cynum = imiss_small ! storm number asigned only once
      fr_basin =       bmiss ! basin asigned only once

      ! GLOBAL VARIABLES SUBJECTIVE DVORAK (DVTS) (10)

      IF (ALLOCATED(dvts_sensor    ))DEALLOCATE(dvts_sensor  )  
      IF (ALLOCATED(dvts_pcn       ))DEALLOCATE(dvts_pcn     )  
      IF (ALLOCATED(dvts_24hf      ))DEALLOCATE(dvts_24hf    )  
      IF (ALLOCATED(dvts_sat       ))DEALLOCATE(dvts_sat     )  
      IF (ALLOCATED(dvts_centype   ))DEALLOCATE(dvts_centype )  
      IF (ALLOCATED(dvts_tropical  ))DEALLOCATE(dvts_tropical)  
      IF (ALLOCATED(dvts_comments  ))DEALLOCATE(dvts_comments)  
                                                                                    
      IF (ALLOCATED(dvts_lttnum    ))DEALLOCATE(dvts_lttnum    )
      IF (ALLOCATED(dvts_ltcinum   ))DEALLOCATE(dvts_ltcinum   )
      IF (ALLOCATED(dvts_ltintchg  ))DEALLOCATE(dvts_ltintchg  )
      IF (ALLOCATED(dvts_ltpastchg ))DEALLOCATE(dvts_ltpastchg )
      IF (ALLOCATED(dvts_lttnumchg ))DEALLOCATE(dvts_lttnumchg )
      IF (ALLOCATED(dvts_ltlasteval))DEALLOCATE(dvts_ltlasteval)
                                                                                    
      IF (ALLOCATED(dvts_sttnum    ))DEALLOCATE(dvts_sttnum    )
      IF (ALLOCATED(dvts_stcinum   ))DEALLOCATE(dvts_stcinum   )
      IF (ALLOCATED(dvts_stintchg  ))DEALLOCATE(dvts_stintchg  )
      IF (ALLOCATED(dvts_stpastchg ))DEALLOCATE(dvts_stpastchg )
      IF (ALLOCATED(dvts_sttnumchg ))DEALLOCATE(dvts_sttnumchg )
      IF (ALLOCATED(dvts_stlasteval))DEALLOCATE(dvts_stlasteval)
                                                              
      ! GLOBAL VARIABLES OBJECTIVE DVORAK (DVTO) (20)

      IF (ALLOCATED(dvto_sensor     ))DEALLOCATE(dvto_sensor     )
      IF (ALLOCATED(dvto_cinum      ))DEALLOCATE(dvto_cinum      )
      IF (ALLOCATED(dvto_ciconf     ))DEALLOCATE(dvto_ciconf     )
      IF (ALLOCATED(dvto_tnumavg    ))DEALLOCATE(dvto_tnumavg    )
      IF (ALLOCATED(dvto_tnumavgtime))DEALLOCATE(dvto_tnumavgtime)
      IF (ALLOCATED(dvto_tnumavgderv))DEALLOCATE(dvto_tnumavgderv)
      IF (ALLOCATED(dvto_tnumraw    ))DEALLOCATE(dvto_tnumraw    )
      IF (ALLOCATED(dvto_eyetemp    ))DEALLOCATE(dvto_eyetemp    )
      IF (ALLOCATED(dvto_cloudtemp  ))DEALLOCATE(dvto_cloudtemp  )
      IF (ALLOCATED(dvto_scenetype  ))DEALLOCATE(dvto_scenetype  )
      IF (ALLOCATED(dvto_algorithm  ))DEALLOCATE(dvto_algorithm  )
      IF (ALLOCATED(dvto_sattype    ))DEALLOCATE(dvto_sattype    )
      IF (ALLOCATED(dvto_tropical   ))DEALLOCATE(dvto_tropical   )
      IF (ALLOCATED(dvto_comments   ))DEALLOCATE(dvto_comments   )
                                                                
      ! GLOBAL VARIABLES MICROWAVE (MICR)(30)

      IF (ALLOCATED(micr_rain      ))DEALLOCATE(micr_rain      )
      IF (ALLOCATED(micr_rainrate  ))DEALLOCATE(micr_rainrate  )
      IF (ALLOCATED(micr_algorithm ))DEALLOCATE(micr_algorithm )
      IF (ALLOCATED(micr_wave      ))DEALLOCATE(micr_wave      )
      IF (ALLOCATED(micr_temp      ))DEALLOCATE(micr_temp      )
      IF (ALLOCATED(micr_slpraw    ))DEALLOCATE(micr_slpraw    )
      IF (ALLOCATED(micr_slpretr   ))DEALLOCATE(micr_slpretr   )
      IF (ALLOCATED(micr_seas      ))DEALLOCATE(micr_seas      )
      IF (ALLOCATED(micr_sattype   ))DEALLOCATE(micr_sattype   )
!      IF (ALLOCATED(micr_rad       ))DEALLOCATE(micr_rad       )
!      IF (ALLOCATED(micr_windcode  ))DEALLOCATE(micr_windcode  )
      IF (ALLOCATED(micr_wc34       ))DEALLOCATE(micr_wc34       )
      IF (ALLOCATED(micr_wc50       ))DEALLOCATE(micr_wc50       )
      IF (ALLOCATED(micr_wc64       ))DEALLOCATE(micr_wc64       )
      IF (ALLOCATED(micr_r34       ))DEALLOCATE(micr_r34       )
      IF (ALLOCATED(micr_r50       ))DEALLOCATE(micr_r50       )
      IF (ALLOCATED(micr_r64       ))DEALLOCATE(micr_r64       )
      IF (ALLOCATED(micr_edge      ))DEALLOCATE(micr_edge      )
      IF (ALLOCATED(micr_cut       ))DEALLOCATE(micr_cut       )
      IF (ALLOCATED(micr_radconf   ))DEALLOCATE(micr_radconf   )
      IF (ALLOCATED(micr_comments  ))DEALLOCATE(micr_comments  ) 
                                                              
      ! GLOBAL VARIABLES SCATTEROMETER (SCAT) (31)            
                                                              
      ! GLOBAL VARIABLES RADAR (RDR) (40)

      IF (ALLOCATED(rdr_rdrtype       ))DEALLOCATE(rdr_rdrtype       )
      IF (ALLOCATED(rdr_radobcode     ))DEALLOCATE(rdr_radobcode     )
      IF (ALLOCATED(rdr_plainlanguage ))DEALLOCATE(rdr_plainlanguage )
      IF (ALLOCATED(rdr_doppler       ))DEALLOCATE(rdr_doppler       )
      IF (ALLOCATED(rdr_radob         ))DEALLOCATE(rdr_radob         )   
      IF (ALLOCATED(rdr_eyeshape      ))DEALLOCATE(rdr_eyeshape      )   
      IF (ALLOCATED(rdr_eyewallob     ))DEALLOCATE(rdr_eyewallob     )   
      IF (ALLOCATED(rdr_spiralov      ))DEALLOCATE(rdr_spiralov      )   
      IF (ALLOCATED(rdr_lat           ))DEALLOCATE(rdr_lat           )
      IF (ALLOCATED(rdr_ns            ))DEALLOCATE(rdr_ns            )
      IF (ALLOCATED(rdr_lon           ))DEALLOCATE(rdr_lon           )
      IF (ALLOCATED(rdr_ew            ))DEALLOCATE(rdr_ew            )
      IF (ALLOCATED(rdr_vmaxin        ))DEALLOCATE(rdr_vmaxin        )
      IF (ALLOCATED(rdr_azimuthin     ))DEALLOCATE(rdr_azimuthin     )
      IF (ALLOCATED(rdr_rangein       ))DEALLOCATE(rdr_rangein       )
      IF (ALLOCATED(rdr_elevin        ))DEALLOCATE(rdr_elevin        )
      IF (ALLOCATED(rdr_vmaxout       ))DEALLOCATE(rdr_vmaxout       )
      IF (ALLOCATED(rdr_azimuthout    ))DEALLOCATE(rdr_azimuthout    )
      IF (ALLOCATED(rdr_rangeout      ))DEALLOCATE(rdr_rangeout      )
      IF (ALLOCATED(rdr_elevout       ))DEALLOCATE(rdr_elevout       )
      IF (ALLOCATED(rdr_cloudheight   ))DEALLOCATE(rdr_cloudheight   )
      IF (ALLOCATED(rdr_rainaccum     ))DEALLOCATE(rdr_rainaccum     )
      IF (ALLOCATED(rdr_rainactimeint ))DEALLOCATE(rdr_rainactimeint )
      IF (ALLOCATED(rdr_rainlat       ))DEALLOCATE(rdr_rainlat       )
      IF (ALLOCATED(rdr_rainns        ))DEALLOCATE(rdr_rainns        )
      IF (ALLOCATED(rdr_rainlon       ))DEALLOCATE(rdr_rainlon       )
      IF (ALLOCATED(rdr_rainew        ))DEALLOCATE(rdr_rainew        )
      IF (ALLOCATED(rdr_comments      ))DEALLOCATE(rdr_comments      )

      ! GLOBAL VARIABLES AIRCRAFT (AIRC) (50)
  
      IF (ALLOCATED(airc_flightlevelft            )) & 
           DEALLOCATE(airc_flightlevelft              )   
      IF (ALLOCATED(airc_flightlevelmb            )) &   
           DEALLOCATE(airc_flightlevelmb              )   
      IF (ALLOCATED(airc_minheight                )) &   
           DEALLOCATE(airc_minheight                  )   
      IF (ALLOCATED(airc_maxsurfacewind           )) &   
           DEALLOCATE(airc_maxsurfacewind             )   
      IF (ALLOCATED(airc_maxsurfacewindbearing    )) &   
           DEALLOCATE(airc_maxsurfacewindbearing      )   
      IF (ALLOCATED(airc_maxsurfacewindrange      )) &   
           DEALLOCATE(airc_maxsurfacewindrange        )   
      IF (ALLOCATED(airc_maxflightlevelwinddir    )) &   
           DEALLOCATE(airc_maxflightlevelwinddir      )   
      IF (ALLOCATED(airc_maxflightlevelwindint    )) &   
           DEALLOCATE(airc_maxflightlevelwindint      )   
      IF (ALLOCATED(airc_maxflightlevelwindbearing)) &   
           DEALLOCATE(airc_maxflightlevelwindbearing  )   
      IF (ALLOCATED(airc_maxflightlevelwindrange  )) & 
           DEALLOCATE(airc_maxflightlevelwindrange    )   
      IF (ALLOCATED(airc_minsealevelpressure      )) &   
           DEALLOCATE(airc_minsealevelpressure        )   
      IF (ALLOCATED(airc_eyetempoutside           )) &   
           DEALLOCATE(airc_eyetempoutside             )   
      IF (ALLOCATED(airc_eyetempinside            )) &   
           DEALLOCATE(airc_eyetempinside              )   
      IF (ALLOCATED(airc_dewpoint                 )) &   
           DEALLOCATE(airc_dewpoint                   )   
      IF (ALLOCATED(airc_seasurface               )) &   
           DEALLOCATE(airc_seasurface                 )   
      IF (ALLOCATED(airc_eyewallthickness         )) &   
           DEALLOCATE(airc_eyewallthickness           )   
      IF (ALLOCATED(airc_eyeshape                 )) &   
           DEALLOCATE(airc_eyeshape                   )   
      IF (ALLOCATED(airc_eyeorientation           )) &   
           DEALLOCATE(airc_eyeorientation             )   
      IF (ALLOCATED(airc_eyediameterlongaxis      )) &   
           DEALLOCATE(airc_eyediameterlongaxis        )   
      IF (ALLOCATED(airc_eyediametershortaxis     )) &   
           DEALLOCATE(airc_eyediametershortaxis       )   
      IF (ALLOCATED(airc_navacc                   )) &   
           DEALLOCATE(airc_navacc                     )   
      IF (ALLOCATED(airc_navmet                   )) &   
           DEALLOCATE(airc_navmet                     )   
      IF (ALLOCATED(airc_missionnumber            )) &   
           DEALLOCATE(airc_missionnumber              )   
      IF (ALLOCATED(airc_comments                 )) &   
           DEALLOCATE(airc_comments                   )   
      
      ! GLOBAL VARIABLES DROPSONDE (DROP) (60)
      
      IF (ALLOCATED(drop_sondeenv ))DEALLOCATE(drop_sondeenv )
      IF (ALLOCATED(drop_height150))DEALLOCATE(drop_height150)
      IF (ALLOCATED(drop_vspd150  ))DEALLOCATE(drop_vspd150  )
      IF (ALLOCATED(drop_vspd500  ))DEALLOCATE(drop_vspd500  )
      IF (ALLOCATED(drop_comments ))DEALLOCATE(drop_comments )
                                                            
      ! GLOBAL VARIABLES ANALYSIS (ANAL) (70)               

      IF (ALLOCATED(anal_ititials ))DEALLOCATE(anal_ititials )
      IF (ALLOCATED(anal_startdtg ))DEALLOCATE(anal_startdtg )
      IF (ALLOCATED(anal_enddtg   ))DEALLOCATE(anal_enddtg   )
      IF (ALLOCATED(anal_dist2data))DEALLOCATE(anal_dist2data)
      IF (ALLOCATED(anal_sst      ))DEALLOCATE(anal_sst      )
      IF (ALLOCATED(anal_obsources))DEALLOCATE(anal_obsources)
      IF (ALLOCATED(anal_comments ))DEALLOCATE(anal_comments )
  

      ! Open fdeck
      OPEN(UNIT=luff,FILE=TRIM(ffn),STATUS='OLD',IOSTAT=ios_open)

      ! Check opening file

      IF (ios_open /= 0 ) THEN
         PRINT*,'extract_fix:  IOSTATUS = ',ios_open,' filename= ',TRIM(ffn)
         CLOSE(luff)
         print*,'ios_open = ', ios_open,' .....RETURNING'
         RETURN
      ENDIF

      BACKSPACE(luff)
      
      frec_ok = 1

      DO WHILE (frec_ok == 1)

         CALL readfrecord(luff,fixdata,frec_ok)
!         print*,fixdata%frecord(1)%fixformat, iftype 
!         print*,check_fid, check_fsite
         
         IF (fixdata%frecord(1)%fixformat == iftype ) THEN
           
            IF (check_fid .AND. check_fsite) THEN

               !check fid and fsite when counting

               IF (fixdata%frecord(1)%fixtype == fid .AND. &
                    fixdata%frecord(1)%fixsite == fsite ) THEN

                  ft_num = ft_num + 1

               END IF

            ELSE IF (check_fid .AND. .NOT. check_fsite) THEN
            
               !check fid only when counting

               IF (fixdata%frecord(1)%fixtype == fid ) THEN
               
                  ft_num = ft_num + 1

               END IF

            ELSE IF (.NOT. check_fid .AND. check_fsite ) THEN
            
               !check fsite only when counting

               IF (fixdata%frecord(1)%fixsite == fsite ) THEN
               
                  ft_num = ft_num + 1

               END IF

            ELSE 

            ! count all 
               ft_num = ft_num +1

            END IF

         END IF

      END DO
      
      REWIND (luff)

      BACKSPACE(luff)
      
      ALLOCATE  (fr_time(ft_num))  ! time hhmm
      ALLOCATE  (fr_date(ft_num))  ! date YYYYmmdd
      ALLOCATE(fr_height(ft_num))  ! height of the fix
      ALLOCATE  (fr_lcon(ft_num))  ! center fix confidence
      ALLOCATE  (fr_pcon(ft_num))  ! pressure fix confidence
      ALLOCATE  (fr_vcon(ft_num))  ! intensity fix confidence
      ALLOCATE  (fr_rcon(ft_num))  ! wind radii confidence
      ALLOCATE   (fr_lat(ft_num))  ! latitude -90 to 90
      ALLOCATE   (fr_lon(ft_num))  ! longitude 0 to 360
      ALLOCATE  (fr_Flag(ft_num))  ! FLAGGED
      ALLOCATE  (fr_vmax(ft_num))  ! maximum winds
      ALLOCATE  (fr_mslp(ft_num))  ! minimum sea level pressure
      ALLOCATE   (fr_rmw(ft_num))  ! radius of maximum winds
      ALLOCATE   (fr_eye(ft_num))  ! eye diameter
      ALLOCATE   (fr_r34(ft_num,4))! 34-kt wind radii
      ALLOCATE   (fr_r50(ft_num,4))! 50-kt wind radii
      ALLOCATE   (fr_r64(ft_num,4))! 64-kt wind radii 
      ALLOCATE  (fr_cdtg(ft_num))  ! Character date/time group
      ALLOCATE  (fr_site(ft_num))  ! Character date/time group
      ALLOCATE (fr_lcent(ft_num))  ! Center fix?
      ALLOCATE (fr_lpres(ft_num))  ! Pressure fix?
      ALLOCATE (fr_lvmax(ft_num))  ! Intensity fix? 
      ALLOCATE (fr_lradi(ft_num))  ! Radii fix?

      ! GLOBAL VARIABLES SUBJECTIVE DVORAK (DVTS) (10)
      IF (iftype == 10 ) THEN

         ALLOCATE(dvts_sensor    (ft_num))
         ALLOCATE(dvts_pcn       (ft_num))
         ALLOCATE(dvts_24hf      (ft_num))
         ALLOCATE(dvts_sat       (ft_num))
         ALLOCATE(dvts_centype   (ft_num))
         ALLOCATE(dvts_tropical  (ft_num))
         ALLOCATE(dvts_comments  (ft_num))
         
         ALLOCATE(dvts_lttnum    (ft_num))
         ALLOCATE(dvts_ltcinum   (ft_num))
         ALLOCATE(dvts_ltintchg  (ft_num))
         ALLOCATE(dvts_ltpastchg (ft_num))
         ALLOCATE(dvts_lttnumchg (ft_num))
         ALLOCATE(dvts_ltlasteval(ft_num))
         
         ALLOCATE(dvts_sttnum    (ft_num))
         ALLOCATE(dvts_stcinum   (ft_num))
         ALLOCATE(dvts_stintchg  (ft_num))
         ALLOCATE(dvts_stpastchg (ft_num))
         ALLOCATE(dvts_sttnumchg (ft_num))
         ALLOCATE(dvts_stlasteval(ft_num))

!         dvts_sensor     = 'XXXX'
!         dvts_pcn        = imiss
!         dvts_ci24hr     = imiss
!         dvts_sattype    = 'XXXXXX'
!         dvts_centertype ='XXXX'
!         dvts_tropical   ='X'
         
!         dvts_lttnum     = imiss
!         dvts_ltcinum    = imiss
!         dvts_ltintchg   = 'X'
!         dvts_ltpastchg  = 'X'
!         dvts_lttnumchg  = imiss
!         dvts_ltlasteval = imiss

!         dvts_sttnum     = imiss
!         dvts_stcinum    = imiss
!         dvts_stintchg   = 'X'
!         dvts_stpastchg  = 'X'
!         dvts_sttnumchg  = imiss
!         dvts_stlasteval = imiss

      END IF
                                                              
      ! GLOBAL VARIABLES OBJECTIVE DVORAK (DVTO) (20)

      IF (iftype == 20 ) THEN

         ALLOCATE(dvto_sensor     (ft_num))
         ALLOCATE(dvto_cinum      (ft_num))
         ALLOCATE(dvto_ciconf     (ft_num))
         ALLOCATE(dvto_tnumavg    (ft_num))
         ALLOCATE(dvto_tnumavgtime(ft_num))
         ALLOCATE(dvto_tnumavgderv(ft_num))
         ALLOCATE(dvto_tnumraw    (ft_num))
         ALLOCATE(dvto_eyetemp    (ft_num))
         ALLOCATE(dvto_cloudtemp  (ft_num))
         ALLOCATE(dvto_scenetype  (ft_num))
         ALLOCATE(dvto_algorithm  (ft_num))
         ALLOCATE(dvto_sattype    (ft_num))
         ALLOCATE(dvto_tropical   (ft_num))
         ALLOCATE(dvto_comments   (ft_num))

 !        dvto_sensor = 'XXXX'
 !        dvto_cinum  = imiss
 !        dvto_ciconf = imiss
 !        dvto_tnumavg= imiss
 !        dvto_tnumavgtime =imiss
 !        dvto_tnumavgderv = 'X'
 !        dvto_tnumraw = imiss
 !        dvto_eyetemp =imiss
 !        dvto_cloudtemp = imiss
 !        dvto_scenetype = 'XXXX'
 !        dvto_algorithm = 'XX'
 !        dvto_sattype = 'XXXXXX'
 !        dvto_tropical = 'X'
         
      END IF

      ! GLOBAL VARIABLES MICROWAVE (MICR)(30)

      IF (iftype == 30 ) THEN
      
         ALLOCATE(micr_rain      (ft_num))
         ALLOCATE(micr_rainrate  (ft_num))
         ALLOCATE(micr_algorithm (ft_num))
         ALLOCATE(micr_wave      (ft_num))
         ALLOCATE(micr_temp      (ft_num))
         ALLOCATE(micr_slpraw    (ft_num))
         ALLOCATE(micr_slpretr   (ft_num))
         ALLOCATE(micr_seas      (ft_num))
         ALLOCATE(micr_sattype   (ft_num))
!         ALLOCATE(micr_rad       (ft_num))
!         ALLOCATE(micr_windcode  (ft_num))
         ALLOCATE(micr_wc34      (ft_num))
         ALLOCATE(micr_wc50      (ft_num))
         ALLOCATE(micr_wc64      (ft_num))
         ALLOCATE(micr_r34       (ft_num,8))
         ALLOCATE(micr_r50       (ft_num,8))
         ALLOCATE(micr_r64       (ft_num,8))
         ALLOCATE(micr_edge      (ft_num,8))
         ALLOCATE(micr_cut       (ft_num,8))
         ALLOCATE(micr_radconf   (ft_num))
         ALLOCATE(micr_comments  (ft_num)) 

      END IF
                                                              
      ! GLOBAL VARIABLES SCATTEROMETER (SCAT) (31)            
                                                              
      ! GLOBAL VARIABLES RADAR (RDR) (40)

      IF (iftype == 40 ) THEN

         ALLOCATE(rdr_rdrtype       (ft_num))
         ALLOCATE(rdr_radobcode     (ft_num))
         ALLOCATE(rdr_plainlanguage (ft_num))
         ALLOCATE(rdr_doppler       (ft_num))
         ALLOCATE(rdr_radob         (ft_num))   
         ALLOCATE(rdr_eyeshape      (ft_num))   
         ALLOCATE(rdr_eyewallob     (ft_num))   
         ALLOCATE(rdr_spiralov      (ft_num))   
         ALLOCATE(rdr_lat           (ft_num))
         ALLOCATE(rdr_ns            (ft_num))
         ALLOCATE(rdr_lon           (ft_num))
         ALLOCATE(rdr_ew            (ft_num))
         ALLOCATE(rdr_vmaxin        (ft_num))
         ALLOCATE(rdr_azimuthin     (ft_num))
         ALLOCATE(rdr_rangein       (ft_num))
         ALLOCATE(rdr_elevin        (ft_num))
         ALLOCATE(rdr_vmaxout       (ft_num))
         ALLOCATE(rdr_azimuthout    (ft_num))
         ALLOCATE(rdr_rangeout      (ft_num))
         ALLOCATE(rdr_elevout       (ft_num))
         ALLOCATE(rdr_cloudheight   (ft_num))
         ALLOCATE(rdr_rainaccum     (ft_num))
         ALLOCATE(rdr_rainactimeint (ft_num))
         ALLOCATE(rdr_rainlat       (ft_num))
         ALLOCATE(rdr_rainns        (ft_num))
         ALLOCATE(rdr_rainlon       (ft_num))
         ALLOCATE(rdr_rainew        (ft_num))
         ALLOCATE(rdr_comments      (ft_num))

      END IF

      ! GLOBAL VARIABLES AIRCRAFT (AIRC) (50)
  
      IF (iftype == 50 ) THEN

         ALLOCATE(airc_flightlevelft            (ft_num))
         ALLOCATE(airc_flightlevelmb            (ft_num))
         ALLOCATE(airc_minheight                (ft_num))
         ALLOCATE(airc_maxsurfacewind           (ft_num))
         ALLOCATE(airc_maxsurfacewindbearing    (ft_num))
         ALLOCATE(airc_maxsurfacewindrange      (ft_num))
         ALLOCATE(airc_maxflightlevelwinddir    (ft_num))
         ALLOCATE(airc_maxflightlevelwindint    (ft_num))
         ALLOCATE(airc_maxflightlevelwindbearing(ft_num))
         ALLOCATE(airc_maxflightlevelwindrange  (ft_num))
         ALLOCATE(airc_minsealevelpressure      (ft_num))
         ALLOCATE(airc_eyetempoutside           (ft_num))
         ALLOCATE(airc_eyetempinside            (ft_num))
         ALLOCATE(airc_dewpoint                 (ft_num))
         ALLOCATE(airc_seasurface               (ft_num))
         ALLOCATE(airc_eyewallthickness         (ft_num))
         ALLOCATE(airc_eyeshape                 (ft_num))
         ALLOCATE(airc_eyeorientation           (ft_num))
         ALLOCATE(airc_eyediameterlongaxis      (ft_num))
         ALLOCATE(airc_eyediametershortaxis     (ft_num))
         ALLOCATE(airc_navacc                   (ft_num))
         ALLOCATE(airc_navmet                   (ft_num))
         ALLOCATE(airc_missionnumber            (ft_num))
         ALLOCATE(airc_comments                 (ft_num))

      END IF
      
      ! GLOBAL VARIABLES DROPSONDE (DROP) (60)
      
      IF (iftype == 60 ) THEN

         ALLOCATE(drop_sondeenv (ft_num))
         ALLOCATE(drop_height150(ft_num))
         ALLOCATE(drop_vspd150  (ft_num))
         ALLOCATE(drop_vspd500  (ft_num))
         ALLOCATE(drop_comments (ft_num))
                                    
      END IF

      ! GLOBAL VARIABLES ANALYSIS (ANAL) (70)               

      IF (iftype == 70 ) THEN

         ALLOCATE(anal_ititials (ft_num))
         ALLOCATE(anal_startdtg (ft_num))
         ALLOCATE(anal_enddtg   (ft_num))
         ALLOCATE(anal_dist2data(ft_num))
         ALLOCATE(anal_sst      (ft_num))
         ALLOCATE(anal_obsources(ft_num))
         ALLOCATE(anal_comments (ft_num))

      END IF



      fr_cynum = imiss_small ! storm number asigned only once
      fr_basin =       bmiss ! basin asigned only once
      frec_ok = 1
      
      ii=0
      DO WHILE (frec_ok == 1)

         CALL readfrecord(luff,fixdata,frec_ok)
         
         IF (fixdata%frecord(1)%fixformat == iftype ) THEN

            IF (check_fid .AND. check_fsite) THEN

               !check fid and fsite when counting

               IF (fixdata%frecord(1)%fixtype == fid .AND. &
                    fixdata%frecord(1)%fixsite == fsite ) THEN
                  
                  ii=ii+1

                  IF (first) THEN
                     fr_cynum = fixdata%frecord(1)%cynum
                     fr_basin = fixdata%frecord(1)%basin
                     first=.false.
                  END IF
               
                  fr_height(ii) = fixdata%frecord(1)%height
                  fr_lcon(ii) =  fixdata%frecord(1)%positconf 
                  fr_pcon(ii) = fixdata%frecord(1)%presConf
                  fr_vcon(ii) = fixdata%frecord(1)%vconf
                  fr_rcon(ii) = fixdata%frecord(1)%radconf
                  fr_flag(ii) = fixdata%frecord(1)%flagged
                  IF (fixdata%frecord(1)%centerFix) THEN
                     
                     fr_lat(ii) =  fixdata%frecord(1)%lat
                     IF(fixdata%frecord(1)%NS .EQ.'S') THEN
                        fr_lat(ii)=fr_lat(ii)*(-1.0)
                     END IF

                     fr_lon(ii) =  fixdata%frecord(1)%lon
                     IF(fixdata%frecord(1)%EW .EQ.'W') THEN
                        fr_lon(ii) = 360.0 - fr_lon(ii)
                     END IF

                     ELSE

                        fr_lat(ii)=rmiss
                        fr_lon(ii)=rmiss
                     
                  END IF

                  IF (fixdata%frecord(1)%intensityFix) THEN

                     fr_vmax(ii) = float(fixdata%frecord(1)%v)

                  ELSE
                     
                     fr_vmax(ii) = rmiss
   
                  END IF

                  
                  IF (fixdata%frecord(1)%pressureFix) THEN

                     fr_mslp(ii) = float(fixdata%frecord(1)%pressure)

                  ELSE IF (.NOT. fixdata%frecord(1)%pressureFix .AND. &
                     (iftype == 50 .or. iftype == 60)) THEN

                     IF (fixdata%frecord(1)%pressure >= 800 .AND. &
                          fixdata%frecord(1)%pressure <= 1100 ) THEN

                        fr_mslp(ii) = float(fixdata%frecord(1)%pressure)
                        
                     ELSE
                        
                        fr_mslp(ii)=rmiss
                        
                     END IF

                  ELSE

                     fr_mslp(ii) = rmiss

                  END IF
                  
                  fr_rmw(ii) = float(fixdata%frecord(1)%mrd)
                  IF (fr_rmw(ii) <= 0.0) fr_rmw(ii)=rmiss
                  fr_eye(ii) = float(fixdata%frecord(1)%eye)
                  IF (fr_eye(ii) <= 0.0) fr_rmw(ii)=rmiss

                  fr_cdtg(ii) = fixdata%frecord(1)%DTG 
                  READ(fr_cdtg(ii),'(i8,i4)')fr_date(ii),fr_time(ii)

                  fr_site(ii) = fixdata%frecord(1)%fixsite
                  fr_lcent(ii) = fixdata%frecord(1)%centerfix
                  fr_lpres(ii) = fixdata%frecord(1)%pressurefix
                  fr_lvmax(ii) = fixdata%frecord(1)%intensityfix
                  fr_lradi(ii) = fixdata%frecord(1)%radiifix

                  IF (iftype == 10 ) THEN

                     dvts_sensor    (ii) = fixdata%frecord(1)%dvts%sensor
                     dvts_pcn       (ii) = fixdata%frecord(1)%dvts%pcn
                     dvts_24hf      (ii) = fixdata%frecord(1)%dvts%ci24hr
                     dvts_sat       (ii) = fixdata%frecord(1)%dvts%sattype
                     dvts_centype   (ii) = fixdata%frecord(1)%dvts%centertype
                     dvts_tropical  (ii) = fixdata%frecord(1)%dvts%tropical
                     dvts_comments  (ii) = fixdata%frecord(1)%dvts%comments
                     
                     dvts_lttnum    (ii) = & 
                          fixdata%frecord(1)%dvts%longterm%tnum          
                     dvts_ltcinum   (ii) = &
                          fixdata%frecord(1)%dvts%longterm%cinum         
                     dvts_ltintchg  (ii) = &
                          fixdata%frecord(1)%dvts%longterm%intchg        
                     dvts_ltpastchg (ii) = &
                          fixdata%frecord(1)%dvts%longterm%pastchg       
                     dvts_lttnumchg (ii) = &
                          fixdata%frecord(1)%dvts%longterm%tnumchg       
                     dvts_ltlasteval(ii) = &
                          fixdata%frecord(1)%dvts%longterm%lastevalhrsago
                     
                     dvts_sttnum    (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%tnum          
                     dvts_stcinum   (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%cinum         
                     dvts_stintchg  (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%intchg        
                     dvts_stpastchg (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%pastchg       
                     dvts_sttnumchg (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%tnumchg       
                     dvts_stlasteval(ii) = &
                          fixdata%frecord(1)%dvts%shortterm%lastevalhrsago

                  ELSE IF (iftype == 20) THEN

                     dvto_sensor     (ii) = fixdata%frecord(1)%dvto%sensor
                     dvto_cinum      (ii) = fixdata%frecord(1)%dvto%cinum
                     dvto_ciconf     (ii) = fixdata%frecord(1)%dvto%ciconf
                     dvto_tnumavg    (ii) = fixdata%frecord(1)%dvto%tnumavg
                     dvto_tnumavgtime(ii) = fixdata%frecord(1)%dvto%tnumavgtime
                     dvto_tnumavgderv(ii) = &
                          fixdata%frecord(1)%dvto%tnumavgderiv
                     dvto_tnumraw    (ii) = fixdata%frecord(1)%dvto%tnumraw
                     dvto_eyetemp    (ii) = fixdata%frecord(1)%dvto%eyetemp
                     dvto_cloudtemp  (ii) = fixdata%frecord(1)%dvto%cloudtemp
                     dvto_scenetype  (ii) = fixdata%frecord(1)%dvto%scenetype
                     dvto_algorithm  (ii) = fixdata%frecord(1)%dvto%algorithm
                     dvto_sattype    (ii) = fixdata%frecord(1)%dvto%sattype
                     dvto_tropical   (ii) = fixdata%frecord(1)%dvto%tropical
                     dvto_comments   (ii) = fixdata%frecord(1)%dvto%comments

                  ELSE IF (iftype == 30) THEN

                     micr_rain      (ii) = fixdata%frecord(1)%micro%rain
                     micr_rainrate  (ii) = fixdata%frecord(1)%micro%rainrate
                     micr_algorithm (ii) = fixdata%frecord(1)%micro%algorithm
                     micr_wave      (ii) = fixdata%frecord(1)%micro%wave
                     micr_temp      (ii) = fixdata%frecord(1)%micro%temp
                     micr_slpraw    (ii) = fixdata%frecord(1)%micro%slpraw
                     micr_slpretr   (ii) = fixdata%frecord(1)%micro%slpretr
                     micr_seas      (ii) = fixdata%frecord(1)%micro%seas
                     micr_sattype   (ii) = fixdata%frecord(1)%micro%sattype
!                     micr_rad       (ii) = fixdata%frecord(1)%micro%rad
!                     micr_windcode  (ii) = fixdata%frecord(1)%micro%windcode
                     micr_radconf   (ii) = fixdata%frecord(1)%micro%radconf
                     micr_comments  (ii) = fixdata%frecord(1)%micro%comments

                     DO i = 1,8

                        micr_edge(ii,i) = fixdata%frecord(1)%micro%edge(i)
                        micr_cut (ii,i) = fixdata%frecord(1)%micro%cut(i)

                        micr_r34 (ii,i) = rmiss
                        micr_r50 (ii,i) = rmiss
                        micr_r64 (ii,i) = rmiss

                     END DO

                     DO j=1,fixdata%numrcrds
                        
                        IF (fixdata%frecord(j)%radiiFix) THEN

                           IF (fixdata%frecord(j)%micro%rad == 34) THEN

                              micr_wc34=fixdata%frecord(j)%micro%windcode

                              DO i=1,4
                                 
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'NEQ ') THEN
                                 
                                    micr_r34(ii,i)= &
                                    float(fixdata%frecord(j)%micro%radii(i))
                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR. &
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'AAA ') THEN
                                    
                                    micr_r34(ii,i)= &
                                    float(fixdata%frecord(j)%micro%radii(i))

                                 ELSE
                                 
                                    PRINT*,&
                                    'extract_fix_937: unexpected windcode = ' &
                                    ,fixdata%frecord(j)%micro%windcode, 'R34'

                                 END IF

                              END DO
                           
                              IF (fixdata%frecord(j)%micro%rad == 50) THEN

                                 micr_wc50=fixdata%frecord(j)%micro%windcode
                                 
                                 DO i=1,4
                              
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'NEQ ') THEN
                                 
                                    micr_r50(ii,i)=&
                                    float(fixdata%frecord(j)%micro%radii(i))
                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR. &
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'AAA ') THEN
                                       
                                    micr_r50(ii,i)=&
                                    float(fixdata%frecord(j)%micro%radii(i))
                                       
                                 ELSE
                                 
                                    PRINT*,&
                                    'extract_fix_965: unexpected windcode = ' &
                                    ,fixdata%frecord(j)%micro%windcode,'R50'
                                       
                                 END IF

                              END DO
                           
                              IF (fixdata%frecord(j)%micro%rad == 64) THEN

                                 micr_wc64=fixdata%frecord(j)%micro%windcode
                                    
                                 DO i=1,4
                              
                                    IF (fixdata%frecord(j)%micro%windcode &
                                         .EQ. ' NEQ' .OR. &
                                       fixdata%frecord(j)%micro%windcode .EQ. &
                                       'NEQ ') THEN

                                 
                                       micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(i))
                                    ELSE IF(fixdata%frecord(j)%micro%windcode &
                                         .EQ. ' AAA' .OR. &
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'AAA ') THEN
                                          
                                       micr_r64(ii,i)= &
                                            float(fixdata%frecord(j)%micro%radii(1))
                                          
                                    ELSE
                                       
                                       PRINT*,&
                                      'extract_fix_994: unexpected windcode = '&
                                       ,fixdata%frecord(j)%micro%windcode, &
                                         'R64'

                                    END IF

                                    END DO

                                 END IF

                              END IF

                           END IF

                        END IF

                     END DO
                  
                  ELSE IF (iftype == 40) THEN

                     rdr_rdrtype       (ii) = fixdata%frecord(1)%radar%rdrtype
                     rdr_radobcode     (ii) = & 
                          fixdata%frecord(1)%radar%radobcode
                     rdr_plainlanguage (ii) = &
                          fixdata%frecord(1)%radar%plainlanguage
                     rdr_doppler       (ii) = fixdata%frecord(1)%radar%doppler 
                     rdr_radob         (ii) = fixdata%frecord(1)%radar%radob
                     rdr_eyeshape      (ii) = & 
                          fixdata%frecord(1)%radar%eyeshape 
                     rdr_eyewallob     (ii) = & 
                          fixdata%frecord(1)%radar%eyewallob
                     rdr_spiralov      (ii) = fixdata%frecord(1)%radar%spiralov
                     rdr_lat           (ii) = fixdata%frecord(1)%radar%lat
                     rdr_ns            (ii) = fixdata%frecord(1)%radar%ns
                     rdr_lon           (ii) = fixdata%frecord(1)%radar%lon
                     rdr_ew            (ii) = fixdata%frecord(1)%radar%ew
                     rdr_vmaxin        (ii) = fixdata%frecord(1)%radar%vmaxin
                     rdr_azimuthin     (ii) = & 
                          fixdata%frecord(1)%radar%azimuthin
                     rdr_rangein       (ii) = fixdata%frecord(1)%radar%rangein
                     rdr_elevin        (ii) = fixdata%frecord(1)%radar%elevin
                     rdr_vmaxout       (ii) = fixdata%frecord(1)%radar%vmaxout
                     rdr_azimuthout    (ii) = &
                          fixdata%frecord(1)%radar%azimuthout
                     rdr_rangeout      (ii) = fixdata%frecord(1)%radar%rangeout
                     rdr_elevout       (ii) = fixdata%frecord(1)%radar%elevout
                     rdr_cloudheight   (ii) = & 
                          fixdata%frecord(1)%radar%cloudheight
                     rdr_rainaccum     (ii) = &
                          fixdata%frecord(1)%radar%rainaccum 
                     rdr_rainactimeint (ii) = & 
                          fixdata%frecord(1)%radar%rainactimeint
                     rdr_rainlat       (ii) = fixdata%frecord(1)%radar%rainlat
                     rdr_rainns        (ii) = fixdata%frecord(1)%radar%rainns
                     rdr_rainlon       (ii) = fixdata%frecord(1)%radar%rainlon
                     rdr_rainew        (ii) = fixdata%frecord(1)%radar%rainew
                     rdr_comments      (ii) = fixdata%frecord(1)%radar%comments

                  ELSE IF (iftype == 50) THEN

                     airc_flightlevelft            (ii) = &
                          fixdata%frecord(1)%air%flightlevelft
                     airc_flightlevelmb            (ii) = &
                          fixdata%frecord(1)%air%flightlevelmb
                     airc_minheight                (ii) = & 
                          fixdata%frecord(1)%air%minheight
                     airc_maxsurfacewind           (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewind 
                     airc_maxsurfacewindbearing    (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewindbearing 
                     airc_maxsurfacewindrange      (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewindrange
                     airc_maxflightlevelwinddir    (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwinddir 
                     airc_maxflightlevelwindint    (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindint 
                     airc_maxflightlevelwindbearing(ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindbearing
                     airc_maxflightlevelwindrange  (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindrange 
                     airc_minsealevelpressure      (ii) = & 
                          fixdata%frecord(1)%air%minsealevelpressure
                     airc_eyetempoutside           (ii) = & 
                          fixdata%frecord(1)%air%eyetempoutside 
                     airc_eyetempinside            (ii) = & 
                          fixdata%frecord(1)%air%eyetempinside 
                     airc_dewpoint                 (ii) = & 
                          fixdata%frecord(1)%air%dewpoint 
                     airc_seasurface               (ii) = & 
                          fixdata%frecord(1)%air%seasurface 
                     airc_eyewallthickness         (ii) = & 
                          fixdata%frecord(1)%air%wallcloudthickness
                     airc_eyeshape                 (ii) = & 
                          fixdata%frecord(1)%air%eyeshape 
                     airc_eyeorientation           (ii) = &
                          fixdata%frecord(1)%air%eyeorientation
                     airc_eyediameterlongaxis      (ii) = &
                          fixdata%frecord(1)%air%diameterlongaxis
                     airc_eyediametershortaxis     (ii) = & 
                          fixdata%frecord(1)%air%diametershortaxis
                     airc_navacc                   (ii) = &
                          fixdata%frecord(1)%air%navigationalaccuracy
                     airc_navmet                   (ii) = & 
                          fixdata%frecord(1)%air%navigationalmeteorological
                     airc_missionnumber            (ii) = & 
                          fixdata%frecord(1)%air%missionnumber 
                     airc_comments                 (ii) = &
                          fixdata%frecord(1)%air%comments  

                  ELSE IF (iftype == 60) THEN

                     drop_sondeenv (ii) = fixdata%frecord(1)%drop%sondeenv
                     drop_height150(ii) = fixdata%frecord(1)%drop%height150
                     drop_vspd150  (ii) = fixdata%frecord(1)%drop%vspd150
                     drop_vspd500  (ii) = fixdata%frecord(1)%drop%vspd500
                     drop_comments (ii) = fixdata%frecord(1)%drop%comments

                  ELSE IF (iftype == 70) THEN

                     anal_ititials (ii) = fixdata%frecord(1)%anal%initials
                     anal_startdtg (ii) = fixdata%frecord(1)%anal%startdtg
                     anal_enddtg   (ii) = fixdata%frecord(1)%anal%enddtg
                     anal_dist2data(ii) = & 
                          fixdata%frecord(1)%anal%distancetonearestdata
                     anal_sst      (ii) = fixdata%frecord(1)%anal%sst 
                     anal_obsources(ii) = fixdata%frecord(1)%anal%obsources 
                     anal_comments (ii) = fixdata%frecord(1)%anal%comments

                  END IF


                  DO i = 1,4
                     fr_r34(ii,i)=rmiss
                     fr_r50(ii,i)=rmiss
                     fr_r64(ii,i)=rmiss
                  END DO
                  
                  DO j=1, fixdata%numrcrds
                        
                     IF (fixdata%frecord(j)%radiiFix) THEN
                     
                        IF (fixdata%frecord(j)%rad == 34) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode &
                                   .EQ. 'NEQ ') THEN
                                 
                                 fr_r34(ii,i)=float(fixdata%frecord(j)%radii(i))
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA'&
                                   .OR. fixdata%frecord(j)%windcode &
                                   .EQ. 'AAA ') THEN


                                 fr_r34(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_1149: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R34'

                              END IF

                           END DO

                        ELSE IF (fixdata%frecord(j)%rad == 50) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode &
                                   .EQ. 'NEQ ') THEN
                                 
                                 fr_r50(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA'&
                                   .OR. fixdata%frecord(j)%windcode &
                                   .EQ. 'AAA ') THEN

                                 fr_r50(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_1171: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R50'

                              END IF

                           END DO
         
                        ELSE IF (fixdata%frecord(j)%rad == 64) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode &
                                   .EQ. 'NEQ ') THEN
                                 
                                 fr_r64(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA'&
                                   .OR. fixdata%frecord(j)%windcode &
                                   .EQ. 'AAA ') THEN

                                 fr_r64(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_1193: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R64'

                              END IF

                           END DO

                        END IF

                     END IF
                     
                  END DO
   
               END IF
               

            ELSE IF (check_fid .AND. .NOT. check_fsite) THEN
            
               !check fid only when counting

               IF (fixdata%frecord(1)%fixtype == fid ) THEN

                  ii=ii+1

                  IF (first) THEN
                     fr_cynum = fixdata%frecord(1)%cynum
                     fr_basin = fixdata%frecord(1)%basin
                     first=.false.
                  END IF
               
                  fr_height(ii) = fixdata%frecord(1)%height
                  fr_lcon(ii) =  fixdata%frecord(1)%positconf 
                  fr_pcon(ii) = fixdata%frecord(1)%presConf
                  fr_vcon(ii) = fixdata%frecord(1)%vconf
                  fr_rcon(ii) = fixdata%frecord(1)%radconf
                  fr_flag(ii) = fixdata%frecord(1)%flagged
                  IF (fixdata%frecord(1)%centerFix) THEN
                     
                     fr_lat(ii) =  fixdata%frecord(1)%lat
                     IF(fixdata%frecord(1)%NS .EQ.'S') THEN
                        fr_lat(ii)=fr_lat(ii)*(-1.0)
                     END IF

                     fr_lon(ii) =  fixdata%frecord(1)%lon
                     IF(fixdata%frecord(1)%EW .EQ.'W') THEN
                        fr_lon(ii) = 360.0 - fr_lon(ii)
                     END IF

                     ELSE

                        fr_lat(ii)=rmiss
                        fr_lon(ii)=rmiss
                     
                  END IF

                  IF (fixdata%frecord(1)%intensityFix) THEN

                     fr_vmax(ii) = float(fixdata%frecord(1)%v)

                  ELSE
                     
                     fr_vmax(ii) = rmiss
   
                  END IF

                  IF (fixdata%frecord(1)%pressureFix) THEN

                     fr_mslp(ii) = float(fixdata%frecord(1)%pressure)

                  ELSE IF (.NOT. fixdata%frecord(1)%pressureFix .AND. &
                     (iftype == 50 .or. iftype == 60)) THEN

                     IF (fixdata%frecord(1)%pressure >= 800 .AND. &
                          fixdata%frecord(1)%pressure <= 1100 ) THEN

                        fr_mslp(ii) = float(fixdata%frecord(1)%pressure)
                        
                     ELSE
                        
                        fr_mslp(ii)=rmiss
                        
                     END IF

                  ELSE

                     fr_mslp(ii) = rmiss

                  END IF
                  
                  fr_rmw(ii) = float(fixdata%frecord(1)%mrd)
                  IF (fr_rmw(ii) <= 0.0) fr_rmw(ii)=rmiss
                  fr_eye(ii) = float(fixdata%frecord(1)%eye)
                  IF (fr_eye(ii) <= 0.0) fr_rmw(ii)=rmiss

                  fr_cdtg(ii) = fixdata%frecord(1)%DTG 
                  READ(fr_cdtg(ii),'(i8,i4)')fr_date(ii),fr_time(ii)

                  fr_site(ii) = fixdata%frecord(1)%fixsite
                  fr_lcent(ii) = fixdata%frecord(1)%centerfix
                  fr_lpres(ii) = fixdata%frecord(1)%pressurefix
                  fr_lvmax(ii) = fixdata%frecord(1)%intensityfix
                  fr_lradi(ii) = fixdata%frecord(1)%radiifix
                  IF (iftype == 10 ) THEN

                     dvts_sensor    (ii) = fixdata%frecord(1)%dvts%sensor
                     dvts_pcn       (ii) = fixdata%frecord(1)%dvts%pcn
                     dvts_24hf      (ii) = fixdata%frecord(1)%dvts%ci24hr
                     dvts_sat       (ii) = fixdata%frecord(1)%dvts%sattype
                     dvts_centype   (ii) = fixdata%frecord(1)%dvts%centertype
                     dvts_tropical  (ii) = fixdata%frecord(1)%dvts%tropical
                     dvts_comments  (ii) = fixdata%frecord(1)%dvts%comments
                     
                     dvts_lttnum    (ii) = & 
                          fixdata%frecord(1)%dvts%longterm%tnum          
                     dvts_ltcinum   (ii) = &
                          fixdata%frecord(1)%dvts%longterm%cinum         
                     dvts_ltintchg  (ii) = &
                          fixdata%frecord(1)%dvts%longterm%intchg        
                     dvts_ltpastchg (ii) = &
                          fixdata%frecord(1)%dvts%longterm%pastchg       
                     dvts_lttnumchg (ii) = &
                          fixdata%frecord(1)%dvts%longterm%tnumchg       
                     dvts_ltlasteval(ii) = &
                          fixdata%frecord(1)%dvts%longterm%lastevalhrsago
                     
                     dvts_sttnum    (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%tnum          
                     dvts_stcinum   (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%cinum         
                     dvts_stintchg  (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%intchg        
                     dvts_stpastchg (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%pastchg       
                     dvts_sttnumchg (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%tnumchg       
                     dvts_stlasteval(ii) = &
                          fixdata%frecord(1)%dvts%shortterm%lastevalhrsago

                  ELSE IF (iftype == 20) THEN

                     dvto_sensor     (ii) = fixdata%frecord(1)%dvto%sensor
                     dvto_cinum      (ii) = fixdata%frecord(1)%dvto%cinum
                     dvto_ciconf     (ii) = fixdata%frecord(1)%dvto%ciconf
                     dvto_tnumavg    (ii) = fixdata%frecord(1)%dvto%tnumavg
                     dvto_tnumavgtime(ii) = fixdata%frecord(1)%dvto%tnumavgtime
                     dvto_tnumavgderv(ii) = &
                          fixdata%frecord(1)%dvto%tnumavgderiv
                     dvto_tnumraw    (ii) = fixdata%frecord(1)%dvto%tnumraw
                     dvto_eyetemp    (ii) = fixdata%frecord(1)%dvto%eyetemp
                     dvto_cloudtemp  (ii) = fixdata%frecord(1)%dvto%cloudtemp
                     dvto_scenetype  (ii) = fixdata%frecord(1)%dvto%scenetype
                     dvto_algorithm  (ii) = fixdata%frecord(1)%dvto%algorithm
                     dvto_sattype    (ii) = fixdata%frecord(1)%dvto%sattype
                     dvto_tropical   (ii) = fixdata%frecord(1)%dvto%tropical
                     dvto_comments   (ii) = fixdata%frecord(1)%dvto%comments

                  ELSE IF (iftype == 30) THEN

                     micr_rain      (ii) = fixdata%frecord(1)%micro%rain
                     micr_rainrate  (ii) = fixdata%frecord(1)%micro%rainrate
                     micr_algorithm (ii) = fixdata%frecord(1)%micro%algorithm
                     micr_wave      (ii) = fixdata%frecord(1)%micro%wave
                     micr_temp      (ii) = fixdata%frecord(1)%micro%temp
                     micr_slpraw    (ii) = fixdata%frecord(1)%micro%slpraw
                     micr_slpretr   (ii) = fixdata%frecord(1)%micro%slpretr
                     micr_seas      (ii) = fixdata%frecord(1)%micro%seas
                     micr_sattype   (ii) = fixdata%frecord(1)%micro%sattype
!                     micr_rad       (ii) = fixdata%frecord(1)%micro%rad
!                     micr_windcode  (ii) = fixdata%frecord(1)%micro%windcode
                     micr_radconf   (ii) = fixdata%frecord(1)%micro%radconf
                     micr_comments  (ii) = fixdata%frecord(1)%micro%comments

                     DO i = 1,8

                        micr_edge(ii,i) = fixdata%frecord(1)%micro%edge(i)
                        micr_cut (ii,i) = fixdata%frecord(1)%micro%cut(i)

                        micr_r34 (ii,i) = rmiss
                        micr_r50 (ii,i) = rmiss
                        micr_r64 (ii,i) = rmiss

                     END DO

                     DO j=1,fixdata%numrcrds
                        
                        IF (fixdata%frecord(j)%radiiFix) THEN

                           IF (fixdata%frecord(j)%micro%rad == 34) THEN

                              micr_wc34=fixdata%frecord(j)%micro%windcode

                              DO i=1,4
                                 
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'NEQ ') THEN
                                 
                                    micr_r34(ii,i)= &
                                    float(fixdata%frecord(j)%micro%radii(i))
                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR.&
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'AAA ') THEN
                                    
                                    micr_r34(ii,i)= &
                                    float(fixdata%frecord(j)%micro%radii(i))

                                 ELSE
                                 
                                    PRINT*,&
                                    'extract_fix_1401: unexpected windcode = ' &
                                    ,fixdata%frecord(j)%windcode, 'R34'

                                 END IF

                              END DO
                           
                              IF (fixdata%frecord(j)%micro%rad == 50) THEN

                                 micr_wc50=fixdata%frecord(j)%micro%windcode
                                 
                                 DO i=1,4
                              
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'NEQ ') THEN
                                 
                                       micr_r50(ii,i)=&
                                       float(fixdata%frecord(j)%micro%radii(i))
                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR.&
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'AAA ') THEN
                                       
                                       micr_r50(ii,i)=&
                                       float(fixdata%frecord(j)%micro%radii(i))
                                       
                                    ELSE
                                 
                                       PRINT*,&
                                       'extract_fix_1429: unexpected windcode = ' &
                                       ,fixdata%frecord(j)%micro%windcode,'R50'
                                       
                                    END IF

                                 END DO
                           
                                 IF (fixdata%frecord(j)%micro%rad == 64) THEN

                                    micr_wc64=fixdata%frecord(j)%micro%windcode
                                    
                                    DO i=1,4
                              
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'NEQ ') THEN
                                 
                                          micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(i))
                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR.&
                                      fixdata%frecord(j)%micro%windcode .EQ. &
                                      'AAA ') THEN
                                          
                                          micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(1))
                                          
                                       ELSE
                                 
                                          PRINT*,&
                                         'extract_fix_1458: unexpected windcode = '&
                                         ,fixdata%frecord(j)%micro%windcode, &
                                         'R64'

                                       END IF

                                    END DO

                                 END IF

                              END IF

                           END IF

                        END IF

                     END DO
                  
                  ELSE IF (iftype == 40) THEN

                     rdr_rdrtype       (ii) = fixdata%frecord(1)%radar%rdrtype
                     rdr_radobcode     (ii) = & 
                          fixdata%frecord(1)%radar%radobcode
                     rdr_plainlanguage (ii) = &
                          fixdata%frecord(1)%radar%plainlanguage
                     rdr_doppler       (ii) = fixdata%frecord(1)%radar%doppler 
                     rdr_radob         (ii) = fixdata%frecord(1)%radar%radob
                     rdr_eyeshape      (ii) = & 
                          fixdata%frecord(1)%radar%eyeshape 
                     rdr_eyewallob     (ii) = & 
                          fixdata%frecord(1)%radar%eyewallob
                     rdr_spiralov      (ii) = fixdata%frecord(1)%radar%spiralov
                     rdr_lat           (ii) = fixdata%frecord(1)%radar%lat
                     rdr_ns            (ii) = fixdata%frecord(1)%radar%ns
                     rdr_lon           (ii) = fixdata%frecord(1)%radar%lon
                     rdr_ew            (ii) = fixdata%frecord(1)%radar%ew
                     rdr_vmaxin        (ii) = fixdata%frecord(1)%radar%vmaxin
                     rdr_azimuthin     (ii) = & 
                          fixdata%frecord(1)%radar%azimuthin
                     rdr_rangein       (ii) = fixdata%frecord(1)%radar%rangein
                     rdr_elevin        (ii) = fixdata%frecord(1)%radar%elevin
                     rdr_vmaxout       (ii) = fixdata%frecord(1)%radar%vmaxout
                     rdr_azimuthout    (ii) = &
                          fixdata%frecord(1)%radar%azimuthout
                     rdr_rangeout      (ii) = fixdata%frecord(1)%radar%rangeout
                     rdr_elevout       (ii) = fixdata%frecord(1)%radar%elevout
                     rdr_cloudheight   (ii) = & 
                          fixdata%frecord(1)%radar%cloudheight
                     rdr_rainaccum     (ii) = &
                          fixdata%frecord(1)%radar%rainaccum 
                     rdr_rainactimeint (ii) = & 
                          fixdata%frecord(1)%radar%rainactimeint
                     rdr_rainlat       (ii) = fixdata%frecord(1)%radar%rainlat
                     rdr_rainns        (ii) = fixdata%frecord(1)%radar%rainns
                     rdr_rainlon       (ii) = fixdata%frecord(1)%radar%rainlon
                     rdr_rainew        (ii) = fixdata%frecord(1)%radar%rainew
                     rdr_comments      (ii) = fixdata%frecord(1)%radar%comments

                  ELSE IF (iftype == 50) THEN

                     airc_flightlevelft            (ii) = &
                          fixdata%frecord(1)%air%flightlevelft
                     airc_flightlevelmb            (ii) = &
                          fixdata%frecord(1)%air%flightlevelmb
                     airc_minheight                (ii) = & 
                          fixdata%frecord(1)%air%minheight
                     airc_maxsurfacewind           (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewind 
                     airc_maxsurfacewindbearing    (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewindbearing 
                     airc_maxsurfacewindrange      (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewindrange
                     airc_maxflightlevelwinddir    (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwinddir 
                     airc_maxflightlevelwindint    (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindint 
                     airc_maxflightlevelwindbearing(ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindbearing
                     airc_maxflightlevelwindrange  (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindrange 
                     airc_minsealevelpressure      (ii) = & 
                          fixdata%frecord(1)%air%minsealevelpressure
                     airc_eyetempoutside           (ii) = & 
                          fixdata%frecord(1)%air%eyetempoutside 
                     airc_eyetempinside            (ii) = & 
                          fixdata%frecord(1)%air%eyetempinside 
                     airc_dewpoint                 (ii) = & 
                          fixdata%frecord(1)%air%dewpoint 
                     airc_seasurface               (ii) = & 
                          fixdata%frecord(1)%air%seasurface 
                     airc_eyewallthickness         (ii) = & 
                          fixdata%frecord(1)%air%wallcloudthickness
                     airc_eyeshape                 (ii) = & 
                          fixdata%frecord(1)%air%eyeshape 
                     airc_eyeorientation           (ii) = &
                          fixdata%frecord(1)%air%eyeorientation
                     airc_eyediameterlongaxis      (ii) = &
                          fixdata%frecord(1)%air%diameterlongaxis
                     airc_eyediametershortaxis     (ii) = & 
                          fixdata%frecord(1)%air%diametershortaxis
                     airc_navacc                   (ii) = &
                          fixdata%frecord(1)%air%navigationalaccuracy
                     airc_navmet                   (ii) = & 
                          fixdata%frecord(1)%air%navigationalmeteorological
                     airc_missionnumber            (ii) = & 
                          fixdata%frecord(1)%air%missionnumber 
                     airc_comments                 (ii) = &
                          fixdata%frecord(1)%air%comments  

                  ELSE IF (iftype == 60) THEN

                     drop_sondeenv (ii) = fixdata%frecord(1)%drop%sondeenv
                     drop_height150(ii) = fixdata%frecord(1)%drop%height150
                     drop_vspd150  (ii) = fixdata%frecord(1)%drop%vspd150
                     drop_vspd500  (ii) = fixdata%frecord(1)%drop%vspd500
                     drop_comments (ii) = fixdata%frecord(1)%drop%comments

                  ELSE IF (iftype == 70) THEN

                     anal_ititials (ii) = fixdata%frecord(1)%anal%initials
                     anal_startdtg (ii) = fixdata%frecord(1)%anal%startdtg
                     anal_enddtg   (ii) = fixdata%frecord(1)%anal%enddtg
                     anal_dist2data(ii) = & 
                          fixdata%frecord(1)%anal%distancetonearestdata
                     anal_sst      (ii) = fixdata%frecord(1)%anal%sst 
                     anal_obsources(ii) = fixdata%frecord(1)%anal%obsources 
                     anal_comments (ii) = fixdata%frecord(1)%anal%comments

                  END IF

                  DO i = 1,4
                     fr_r34(ii,i)=rmiss
                     fr_r50(ii,i)=rmiss
                     fr_r64(ii,i)=rmiss
                  END DO
                  
                  DO j=1, fixdata%numrcrds
                        
                     IF (fixdata%frecord(j)%radiiFix) THEN
                     
                        IF (fixdata%frecord(j)%rad == 34) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'NEQ ') THEN
                                 
                                 fr_r34(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'AAA ')THEN

                                 fr_r34(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_1613: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R34'

                              END IF

                           END DO

                        ELSE IF (fixdata%frecord(j)%rad == 50) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'NEQ ') THEN
                                 
                                 fr_r50(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'AAA ')THEN

                                 fr_r50(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_1635: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R50'

                              END IF

                           END DO
                        ELSE IF (fixdata%frecord(j)%rad == 64) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'NEQ ') THEN
                                 
                                 fr_r64(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'AAA ')THEN

                                 fr_r64(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_1656: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R64'

                              END IF

                           END DO

                        END IF

                     END IF
   
                  END DO

               END IF

            ELSE IF (.NOT. check_fid .AND. check_fsite ) THEN
            
               IF (fixdata%frecord(1)%fixsite == fsite ) THEN

                  ii=ii+1
               
                  IF (first) THEN
                     fr_cynum = fixdata%frecord(1)%cynum
                     fr_basin = fixdata%frecord(1)%basin
                     first=.false.
                  END IF

                  fr_height(ii) = fixdata%frecord(1)%height
                  fr_lcon(ii) =  fixdata%frecord(1)%positconf 
                  fr_pcon(ii) = fixdata%frecord(1)%presConf
                  fr_vcon(ii) = fixdata%frecord(1)%vconf
                  fr_rcon(ii) = fixdata%frecord(1)%radconf
                  fr_flag(ii) = fixdata%frecord(1)%flagged
                  IF (fixdata%frecord(1)%centerFix) THEN
                     
                     fr_lat(ii) =  fixdata%frecord(1)%lat
                     IF(fixdata%frecord(1)%NS .EQ.'S') THEN
                        fr_lat(ii)=fr_lat(ii)*(-1.0)
                     END IF

                     fr_lon(ii) =  fixdata%frecord(1)%lon
                     IF(fixdata%frecord(1)%EW .EQ.'W') THEN
                        fr_lon(ii) = 360.0 - fr_lon(ii) 
                     END IF

                     ELSE

                        fr_lat(ii)=rmiss
                        fr_lon(ii)=rmiss
                     
                  END IF

                  IF (fixdata%frecord(1)%intensityFix) THEN

                     fr_vmax(ii) = float(fixdata%frecord(1)%v)

                  ELSE
                     
                     fr_vmax(ii) = rmiss
   
                  END IF

                  IF (fixdata%frecord(1)%pressureFix) THEN

                     fr_mslp(ii) = float(fixdata%frecord(1)%pressure)

                  ELSE IF (.NOT. fixdata%frecord(1)%pressureFix .AND. &
                     (iftype == 50 .or. iftype == 60)) THEN

                     IF (fixdata%frecord(1)%pressure >= 800 .AND. &
                          fixdata%frecord(1)%pressure <= 1100 ) THEN

                        fr_mslp(ii) = float(fixdata%frecord(1)%pressure)
                        
                     ELSE
                        
                        fr_mslp(ii)=rmiss
                        
                     END IF

                  ELSE

                     fr_mslp(ii) = rmiss

                  END IF
                  
                  fr_rmw(ii) = float(fixdata%frecord(1)%mrd)
                  IF (fr_rmw(ii) <= 0.0) fr_rmw(ii)=rmiss
                  fr_eye(ii) = float(fixdata%frecord(1)%eye)
                  IF (fr_eye(ii) <= 0.0) fr_rmw(ii)=rmiss

                  fr_cdtg(ii) = fixdata%frecord(1)%DTG 
                  READ(fr_cdtg(ii),'(i8,i4)')fr_date(ii),fr_time(ii)

                  fr_site(ii) = fixdata%frecord(1)%fixsite
                  fr_lcent(ii) = fixdata%frecord(1)%centerfix
                  fr_lpres(ii) = fixdata%frecord(1)%pressurefix
                  fr_lvmax(ii) = fixdata%frecord(1)%intensityfix
                  fr_lradi(ii) = fixdata%frecord(1)%radiifix

                  IF (iftype == 10 ) THEN

                     dvts_sensor    (ii) = fixdata%frecord(1)%dvts%sensor
                     dvts_pcn       (ii) = fixdata%frecord(1)%dvts%pcn
                     dvts_24hf      (ii) = fixdata%frecord(1)%dvts%ci24hr
                     dvts_sat       (ii) = fixdata%frecord(1)%dvts%sattype
                     dvts_centype   (ii) = fixdata%frecord(1)%dvts%centertype
                     dvts_tropical  (ii) = fixdata%frecord(1)%dvts%tropical
                     dvts_comments  (ii) = fixdata%frecord(1)%dvts%comments
                     
                     dvts_lttnum    (ii) = & 
                          fixdata%frecord(1)%dvts%longterm%tnum          
                     dvts_ltcinum   (ii) = &
                          fixdata%frecord(1)%dvts%longterm%cinum         
                     dvts_ltintchg  (ii) = &
                          fixdata%frecord(1)%dvts%longterm%intchg        
                     dvts_ltpastchg (ii) = &
                          fixdata%frecord(1)%dvts%longterm%pastchg       
                     dvts_lttnumchg (ii) = &
                          fixdata%frecord(1)%dvts%longterm%tnumchg       
                     dvts_ltlasteval(ii) = &
                          fixdata%frecord(1)%dvts%longterm%lastevalhrsago
                     
                     dvts_sttnum    (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%tnum          
                     dvts_stcinum   (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%cinum         
                     dvts_stintchg  (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%intchg        
                     dvts_stpastchg (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%pastchg       
                     dvts_sttnumchg (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%tnumchg       
                     dvts_stlasteval(ii) = &
                          fixdata%frecord(1)%dvts%shortterm%lastevalhrsago

                  ELSE IF (iftype == 20) THEN

                     dvto_sensor     (ii) = fixdata%frecord(1)%dvto%sensor
                     dvto_cinum      (ii) = fixdata%frecord(1)%dvto%cinum
                     dvto_ciconf     (ii) = fixdata%frecord(1)%dvto%ciconf
                     dvto_tnumavg    (ii) = fixdata%frecord(1)%dvto%tnumavg
                     dvto_tnumavgtime(ii) = fixdata%frecord(1)%dvto%tnumavgtime
                     dvto_tnumavgderv(ii) = &
                          fixdata%frecord(1)%dvto%tnumavgderiv
                     dvto_tnumraw    (ii) = fixdata%frecord(1)%dvto%tnumraw
                     dvto_eyetemp    (ii) = fixdata%frecord(1)%dvto%eyetemp
                     dvto_cloudtemp  (ii) = fixdata%frecord(1)%dvto%cloudtemp
                     dvto_scenetype  (ii) = fixdata%frecord(1)%dvto%scenetype
                     dvto_algorithm  (ii) = fixdata%frecord(1)%dvto%algorithm
                     dvto_sattype    (ii) = fixdata%frecord(1)%dvto%sattype
                     dvto_tropical   (ii) = fixdata%frecord(1)%dvto%tropical
                     dvto_comments   (ii) = fixdata%frecord(1)%dvto%comments

                  ELSE IF (iftype == 30) THEN

                     micr_rain      (ii) = fixdata%frecord(1)%micro%rain
                     micr_rainrate  (ii) = fixdata%frecord(1)%micro%rainrate
                     micr_algorithm (ii) = fixdata%frecord(1)%micro%algorithm
                     micr_wave      (ii) = fixdata%frecord(1)%micro%wave
                     micr_temp      (ii) = fixdata%frecord(1)%micro%temp
                     micr_slpraw    (ii) = fixdata%frecord(1)%micro%slpraw
                     micr_slpretr   (ii) = fixdata%frecord(1)%micro%slpretr
                     micr_seas      (ii) = fixdata%frecord(1)%micro%seas
                     micr_sattype   (ii) = fixdata%frecord(1)%micro%sattype
!                     micr_rad       (ii) = fixdata%frecord(1)%micro%rad
!                     micr_windcode  (ii) = fixdata%frecord(1)%micro%windcode
                     micr_radconf   (ii) = fixdata%frecord(1)%micro%radconf
                     micr_comments  (ii) = fixdata%frecord(1)%micro%comments

                     DO i = 1,8

                        micr_edge(ii,i) = fixdata%frecord(1)%micro%edge(i)
                        micr_cut (ii,i) = fixdata%frecord(1)%micro%cut(i)

                        micr_r34 (ii,i) = rmiss
                        micr_r50 (ii,i) = rmiss
                        micr_r64 (ii,i) = rmiss

                     END DO

                     DO j=1,fixdata%numrcrds
                        
                        IF (fixdata%frecord(j)%radiiFix) THEN

                           IF (fixdata%frecord(j)%micro%rad == 34) THEN

                              micr_wc34=fixdata%frecord(j)%micro%windcode

                              DO i=1,4
                                 
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'NEQ ') THEN
                                 
                                    micr_r34(ii,i)= &
                                    float(fixdata%frecord(j)%micro%radii(i))

                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'AAA ')THEN
                                    
                                    micr_r34(ii,i)= &
                                    float(fixdata%frecord(j)%micro%radii(i))

                                 ELSE
                                 
                                    PRINT*,&
                                    'extract_fix_1862: unexpected windcode = ' &
                                    ,fixdata%frecord(j)%micro%windcode, 'R34'

                                 END IF

                              END DO
                           
                              IF (fixdata%frecord(j)%micro%rad == 50) THEN

                                 micr_wc50=fixdata%frecord(j)%micro%windcode
                                 
                                 DO i=1,4
                              
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'NEQ ') THEN
                                 
                                       micr_r50(ii,i)=&
                                       float(fixdata%frecord(j)%micro%radii(i))

                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'AAA ')THEN
                                       
                                       micr_r50(ii,i)=&
                                       float(fixdata%frecord(j)%micro%radii(i))
                                       
                                    ELSE
                                 
                                       PRINT*,&
                                       'extract_fix_1890: unexpected windcode = ' &
                                       ,fixdata%frecord(j)%micro%windcode,'R50'
                                       
                                    END IF

                                 END DO
                           
                                 IF (fixdata%frecord(j)%micro%rad == 64) THEN

                                    micr_wc64=fixdata%frecord(j)%micro%windcode
                                    
                                    DO i=1,4
                              
                                 IF (fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' NEQ' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'NEQ ') THEN
                                 
                                          micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(i))

                                 ELSE IF(fixdata%frecord(j)%micro%windcode &
                                      .EQ. ' AAA' .OR. &
                                      fixdata%frecord(j)%micro%windcode &
                                      .EQ. 'AAA ')THEN
                                          
                                          micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(1))
                                          
                                       ELSE
                                 
                                          PRINT*,&
                                         'extract_fix_1919: unexpected windcode = '&
                                         ,fixdata%frecord(j)%micro%windcode, &
                                         'R64'

                                       END IF

                                    END DO

                                 END IF

                              END IF

                           END IF

                        END IF

                     END DO
                  
                  ELSE IF (iftype == 40) THEN

                     rdr_rdrtype       (ii) = fixdata%frecord(1)%radar%rdrtype
                     rdr_radobcode     (ii) = & 
                          fixdata%frecord(1)%radar%radobcode
                     rdr_plainlanguage (ii) = &
                          fixdata%frecord(1)%radar%plainlanguage
                     rdr_doppler       (ii) = fixdata%frecord(1)%radar%doppler 
                     rdr_radob         (ii) = fixdata%frecord(1)%radar%radob
                     rdr_eyeshape      (ii) = & 
                          fixdata%frecord(1)%radar%eyeshape 
                     rdr_eyewallob     (ii) = & 
                          fixdata%frecord(1)%radar%eyewallob
                     rdr_spiralov      (ii) = fixdata%frecord(1)%radar%spiralov
                     rdr_lat           (ii) = fixdata%frecord(1)%radar%lat
                     rdr_ns            (ii) = fixdata%frecord(1)%radar%ns
                     rdr_lon           (ii) = fixdata%frecord(1)%radar%lon
                     rdr_ew            (ii) = fixdata%frecord(1)%radar%ew
                     rdr_vmaxin        (ii) = fixdata%frecord(1)%radar%vmaxin
                     rdr_azimuthin     (ii) = & 
                          fixdata%frecord(1)%radar%azimuthin
                     rdr_rangein       (ii) = fixdata%frecord(1)%radar%rangein
                     rdr_elevin        (ii) = fixdata%frecord(1)%radar%elevin
                     rdr_vmaxout       (ii) = fixdata%frecord(1)%radar%vmaxout
                     rdr_azimuthout    (ii) = &
                          fixdata%frecord(1)%radar%azimuthout
                     rdr_rangeout      (ii) = fixdata%frecord(1)%radar%rangeout
                     rdr_elevout       (ii) = fixdata%frecord(1)%radar%elevout
                     rdr_cloudheight   (ii) = & 
                          fixdata%frecord(1)%radar%cloudheight
                     rdr_rainaccum     (ii) = &
                          fixdata%frecord(1)%radar%rainaccum 
                     rdr_rainactimeint (ii) = & 
                          fixdata%frecord(1)%radar%rainactimeint
                     rdr_rainlat       (ii) = fixdata%frecord(1)%radar%rainlat
                     rdr_rainns        (ii) = fixdata%frecord(1)%radar%rainns
                     rdr_rainlon       (ii) = fixdata%frecord(1)%radar%rainlon
                     rdr_rainew        (ii) = fixdata%frecord(1)%radar%rainew
                     rdr_comments      (ii) = fixdata%frecord(1)%radar%comments

                  ELSE IF (iftype == 50) THEN

                     airc_flightlevelft            (ii) = &
                          fixdata%frecord(1)%air%flightlevelft
                     airc_flightlevelmb            (ii) = &
                          fixdata%frecord(1)%air%flightlevelmb
                     airc_minheight                (ii) = & 
                          fixdata%frecord(1)%air%minheight
                     airc_maxsurfacewind           (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewind 
                     airc_maxsurfacewindbearing    (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewindbearing 
                     airc_maxsurfacewindrange      (ii) = & 
                          fixdata%frecord(1)%air%maxsurfacewindrange
                     airc_maxflightlevelwinddir    (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwinddir 
                     airc_maxflightlevelwindint    (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindint 
                     airc_maxflightlevelwindbearing(ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindbearing
                     airc_maxflightlevelwindrange  (ii) = & 
                          fixdata%frecord(1)%air%maxflightlevelwindrange 
                     airc_minsealevelpressure      (ii) = & 
                          fixdata%frecord(1)%air%minsealevelpressure
                     airc_eyetempoutside           (ii) = & 
                          fixdata%frecord(1)%air%eyetempoutside 
                     airc_eyetempinside            (ii) = & 
                          fixdata%frecord(1)%air%eyetempinside 
                     airc_dewpoint                 (ii) = & 
                          fixdata%frecord(1)%air%dewpoint 
                     airc_seasurface               (ii) = & 
                          fixdata%frecord(1)%air%seasurface 
                     airc_eyewallthickness         (ii) = & 
                          fixdata%frecord(1)%air%wallcloudthickness
                     airc_eyeshape                 (ii) = & 
                          fixdata%frecord(1)%air%eyeshape 
                     airc_eyeorientation           (ii) = &
                          fixdata%frecord(1)%air%eyeorientation
                     airc_eyediameterlongaxis      (ii) = &
                          fixdata%frecord(1)%air%diameterlongaxis
                     airc_eyediametershortaxis     (ii) = & 
                          fixdata%frecord(1)%air%diametershortaxis
                     airc_navacc                   (ii) = &
                          fixdata%frecord(1)%air%navigationalaccuracy
                     airc_navmet                   (ii) = & 
                          fixdata%frecord(1)%air%navigationalmeteorological
                     airc_missionnumber            (ii) = & 
                          fixdata%frecord(1)%air%missionnumber 
                     airc_comments                 (ii) = &
                          fixdata%frecord(1)%air%comments  

                  ELSE IF (iftype == 60) THEN

                     drop_sondeenv (ii) = fixdata%frecord(1)%drop%sondeenv
                     drop_height150(ii) = fixdata%frecord(1)%drop%height150
                     drop_vspd150  (ii) = fixdata%frecord(1)%drop%vspd150
                     drop_vspd500  (ii) = fixdata%frecord(1)%drop%vspd500
                     drop_comments (ii) = fixdata%frecord(1)%drop%comments

                  ELSE IF (iftype == 70) THEN

                     anal_ititials (ii) = fixdata%frecord(1)%anal%initials
                     anal_startdtg (ii) = fixdata%frecord(1)%anal%startdtg
                     anal_enddtg   (ii) = fixdata%frecord(1)%anal%enddtg
                     anal_dist2data(ii) = & 
                          fixdata%frecord(1)%anal%distancetonearestdata
                     anal_sst      (ii) = fixdata%frecord(1)%anal%sst 
                     anal_obsources(ii) = fixdata%frecord(1)%anal%obsources 
                     anal_comments (ii) = fixdata%frecord(1)%anal%comments

                  END IF

                  DO i = 1,4
                     fr_r34(ii,i)=rmiss
                     fr_r50(ii,i)=rmiss
                     fr_r64(ii,i)=rmiss
                  END DO
                  
                  DO j=1, fixdata%numrcrds
                        
                     IF (fixdata%frecord(j)%radiiFix) THEN
                     
                        IF (fixdata%frecord(j)%rad == 34) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'NEQ ') THEN
                                 
                                 fr_r34(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA'&
                                   .OR. fixdata%frecord(j)%windcode .EQ.&
                                   'AAA ') THEN

                                 fr_r34(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_2074: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R34'

                              END IF

                           END DO

                        ELSE IF (fixdata%frecord(j)%rad == 50) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'NEQ ') THEN
                                 
                                 fr_r50(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA'&
                                   .OR. fixdata%frecord(j)%windcode .EQ.&
                                   'AAA ') THEN

                                 fr_r50(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_2096: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R50'

                              END IF

                           END DO
                        ELSE IF (fixdata%frecord(j)%rad == 64) THEN

                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                   .OR. fixdata%frecord(j)%windcode .EQ. &
                                   'NEQ ') THEN
                                 
                                 fr_r64(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                              ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA'&
                                   .OR. fixdata%frecord(j)%windcode .EQ.&
                                   'AAA ') THEN

                                 fr_r64(ii,i)=float(fixdata%frecord(j)%radii(1))

                              ELSE
                                 
                                 PRINT*,'extract_fix_2117: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%windcode, 'R64'

                              END IF

                           END DO

                        END IF

                     END IF
   
                  END DO

               END IF

            ELSE

               ii=ii+1
               
               IF (first) THEN
                  fr_cynum = fixdata%frecord(1)%cynum
                  fr_basin = fixdata%frecord(1)%basin
                  first=.false.
               END IF

               fr_height(ii) = fixdata%frecord(1)%height
               fr_lcon(ii) =  fixdata%frecord(1)%positconf 
               fr_pcon(ii) = fixdata%frecord(1)%presConf
               fr_vcon(ii) = fixdata%frecord(1)%vconf
               fr_rcon(ii) = fixdata%frecord(1)%radconf
               fr_flag(ii) = fixdata%frecord(1)%flagged
               IF (fixdata%frecord(1)%centerFix) THEN
                  
                  fr_lat(ii) =  fixdata%frecord(1)%lat
                  IF(fixdata%frecord(1)%NS .EQ.'S') THEN
                     fr_lat(ii)=fr_lat(ii)*(-1.0)
                  END IF

                  fr_lon(ii) =  fixdata%frecord(1)%lon
                  IF(fixdata%frecord(1)%EW .EQ.'W') THEN
                     fr_lon(ii) = 360.0 - fr_lon(ii)
                  END IF

               ELSE

                  fr_lat(ii)=rmiss
                  fr_lon(ii)=rmiss
                     
               END IF

               IF (fixdata%frecord(1)%intensityFix) THEN
                  
                  fr_vmax(ii) = float(fixdata%frecord(1)%v)

               ELSE
                     
                  fr_vmax(ii) = rmiss
   
               END IF

               IF (fixdata%frecord(1)%pressureFix) THEN

                  fr_mslp(ii) = float(fixdata%frecord(1)%pressure)

               ELSE IF (.NOT. fixdata%frecord(1)%pressureFix .AND. &
                    (iftype == 50 .or. iftype == 60)) THEN

                  IF (fixdata%frecord(1)%pressure >= 800 .AND. &
                          fixdata%frecord(1)%pressure <= 1100 ) THEN
                     
                     fr_mslp(ii) = float(fixdata%frecord(1)%pressure)
                        
                  ELSE
                        
                     fr_mslp(ii)=rmiss
                        
                  END IF

                  
               ELSE

                  fr_mslp(ii) = rmiss
                  
               END IF
                  
               fr_rmw(ii) = float(fixdata%frecord(1)%mrd)
               IF (fr_rmw(ii) <= 0.0) fr_rmw(ii)=rmiss
               fr_eye(ii) = float(fixdata%frecord(1)%eye)
               IF (fr_eye(ii) <= 0.0) fr_rmw(ii)=rmiss

               fr_cdtg(ii) = fixdata%frecord(1)%DTG 
               READ(fr_cdtg(ii),'(i8,i4)')fr_date(ii),fr_time(ii)
               
               fr_site(ii) = fixdata%frecord(1)%fixsite
               fr_lcent(ii) = fixdata%frecord(1)%centerfix
               fr_lpres(ii) = fixdata%frecord(1)%pressurefix
               fr_lvmax(ii) = fixdata%frecord(1)%intensityfix
               fr_lradi(ii) = fixdata%frecord(1)%radiifix

               IF (iftype == 10 ) THEN

                  dvts_sensor    (ii) = fixdata%frecord(1)%dvts%sensor
                  dvts_pcn       (ii) = fixdata%frecord(1)%dvts%pcn
                  dvts_24hf      (ii) = fixdata%frecord(1)%dvts%ci24hr
                  dvts_sat       (ii) = fixdata%frecord(1)%dvts%sattype
                  dvts_centype   (ii) = fixdata%frecord(1)%dvts%centertype
                  dvts_tropical  (ii) = fixdata%frecord(1)%dvts%tropical
                  dvts_comments  (ii) = fixdata%frecord(1)%dvts%comments
                  
                  dvts_lttnum    (ii) = & 
                       fixdata%frecord(1)%dvts%longterm%tnum          
                  dvts_ltcinum   (ii) = &
                       fixdata%frecord(1)%dvts%longterm%cinum         
                  dvts_ltintchg  (ii) = &
                       fixdata%frecord(1)%dvts%longterm%intchg        
                  dvts_ltpastchg (ii) = &
                       fixdata%frecord(1)%dvts%longterm%pastchg       
                  dvts_lttnumchg (ii) = &
                       fixdata%frecord(1)%dvts%longterm%tnumchg       
                  dvts_ltlasteval(ii) = &
                       fixdata%frecord(1)%dvts%longterm%lastevalhrsago
                  
                  dvts_sttnum    (ii) = &
                       fixdata%frecord(1)%dvts%shortterm%tnum          
                  dvts_stcinum   (ii) = &
                          fixdata%frecord(1)%dvts%shortterm%cinum         
                  dvts_stintchg  (ii) = &
                       fixdata%frecord(1)%dvts%shortterm%intchg        
                  dvts_stpastchg (ii) = &
                       fixdata%frecord(1)%dvts%shortterm%pastchg       
                  dvts_sttnumchg (ii) = &
                       fixdata%frecord(1)%dvts%shortterm%tnumchg       
                  dvts_stlasteval(ii) = &
                       fixdata%frecord(1)%dvts%shortterm%lastevalhrsago
                  
               ELSE IF (iftype == 20) THEN
                     
                  dvto_sensor     (ii) = fixdata%frecord(1)%dvto%sensor
                  dvto_cinum      (ii) = fixdata%frecord(1)%dvto%cinum
                  dvto_ciconf     (ii) = fixdata%frecord(1)%dvto%ciconf
                  dvto_tnumavg    (ii) = fixdata%frecord(1)%dvto%tnumavg
                  dvto_tnumavgtime(ii) = fixdata%frecord(1)%dvto%tnumavgtime
                  dvto_tnumavgderv(ii) = &
                       fixdata%frecord(1)%dvto%tnumavgderiv
                  dvto_tnumraw    (ii) = fixdata%frecord(1)%dvto%tnumraw
                  dvto_eyetemp    (ii) = fixdata%frecord(1)%dvto%eyetemp
                  dvto_cloudtemp  (ii) = fixdata%frecord(1)%dvto%cloudtemp
                  dvto_scenetype  (ii) = fixdata%frecord(1)%dvto%scenetype
                  dvto_algorithm  (ii) = fixdata%frecord(1)%dvto%algorithm
                  dvto_sattype    (ii) = fixdata%frecord(1)%dvto%sattype
                  dvto_tropical   (ii) = fixdata%frecord(1)%dvto%tropical
                  dvto_comments   (ii) = fixdata%frecord(1)%dvto%comments

               ELSE IF (iftype == 30) THEN

                  micr_rain      (ii) = fixdata%frecord(1)%micro%rain
                  micr_rainrate  (ii) = fixdata%frecord(1)%micro%rainrate
                  micr_algorithm (ii) = fixdata%frecord(1)%micro%algorithm
                  micr_wave      (ii) = fixdata%frecord(1)%micro%wave
                  micr_temp      (ii) = fixdata%frecord(1)%micro%temp
                  micr_slpraw    (ii) = fixdata%frecord(1)%micro%slpraw
                  micr_slpretr   (ii) = fixdata%frecord(1)%micro%slpretr
                  micr_seas      (ii) = fixdata%frecord(1)%micro%seas
                  micr_sattype   (ii) = fixdata%frecord(1)%micro%sattype
                  micr_radconf   (ii) = fixdata%frecord(1)%micro%radconf
                  micr_comments  (ii) = fixdata%frecord(1)%micro%comments
                  
                  DO i = 1,8
                     
                     micr_edge(ii,i) = fixdata%frecord(1)%micro%edge(i)
                     micr_cut (ii,i) = fixdata%frecord(1)%micro%cut(i)
                     
                     micr_r34 (ii,i) = rmiss
                     micr_r50 (ii,i) = rmiss
                     micr_r64 (ii,i) = rmiss
                     
                  END DO

                  DO j=1,fixdata%numrcrds
                     
                     IF (fixdata%frecord(j)%radiiFix) THEN
                        
                        IF (fixdata%frecord(j)%micro%rad == 34) THEN
                           
                           micr_wc34=fixdata%frecord(j)%micro%windcode
                           
                           DO i=1,4
                              
                              IF (fixdata%frecord(j)%micro%windcode &
                                   .EQ. ' NEQ' .OR. &
                                   fixdata%frecord(j)%micro%windcode .EQ.&
                                   'NEQ ') THEN
                                 
                                 micr_r34(ii,i)= &
                                      float(fixdata%frecord(j)%micro%radii(i))

                              ELSE IF(fixdata%frecord(j)%micro%windcode &
                                   .EQ. ' AAA' .OR. &
                                   fixdata%frecord(j)%micro%windcode .EQ. &
                                   'AAA ')THEN
                                 
                                 micr_r34(ii,i)= &
                                      float(fixdata%frecord(j)%micro%radii(i))
                                 
                              ELSE
                                 
                                 PRINT*,&
                                      'extract_fix_2320: unexpected windcode = ' &
                                      ,fixdata%frecord(j)%micro%windcode, 'R34'
                                 
                              END IF
                              
                           END DO
                           
                           IF (fixdata%frecord(j)%micro%rad == 50) THEN
                              
                              micr_wc50=fixdata%frecord(j)%micro%windcode
                                 
                              DO i=1,4
                                 
                              IF (fixdata%frecord(j)%micro%windcode &
                                   .EQ. ' NEQ' .OR. &
                                   fixdata%frecord(j)%micro%windcode .EQ.&
                                   'NEQ ') THEN
                                    
                                    micr_r50(ii,i)=&
                                         float(fixdata%frecord(j)%micro%radii(i))
                             ELSE IF(fixdata%frecord(j)%micro%windcode &
                                   .EQ. ' AAA' .OR. &
                                   fixdata%frecord(j)%micro%windcode .EQ. &
                                   'AAA ')THEN
                                    
                                    micr_r50(ii,i)=&
                                         float(fixdata%frecord(j)%micro%radii(i))
                                    
                                 ELSE
                                    
                                    PRINT*,&
                                        'extract_fix_2348: unexpected windcode = ' &
                                       ,fixdata%frecord(j)%micro%windcode,'R50'
                                    
                                 END IF

                              END DO
                           
                              IF (fixdata%frecord(j)%micro%rad == 64) THEN
                                 
                                 micr_wc64=fixdata%frecord(j)%micro%windcode
                                    
                                 DO i=1,4
                              
                              IF (fixdata%frecord(j)%micro%windcode &
                                   .EQ. ' NEQ' .OR. &
                                   fixdata%frecord(j)%micro%windcode .EQ.&
                                   'NEQ ') THEN
                                 
                                       micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(i))

                                    ELSE IF(fixdata%frecord(j)%micro%windcode &
                                         .EQ. ' AAA' .OR. &
                                         fixdata%frecord(j)%micro%windcode .EQ. &
                                         'AAA ')THEN

                                          
                                       micr_r64(ii,i)= &
                                       float(fixdata%frecord(j)%micro%radii(1))
                                          
                                    ELSE
                                 
                                       PRINT*,&
                                       'extract_fix_2377: unexpected windcode = '&
                                       ,fixdata%frecord(j)%micro%windcode, &
                                         'R64'

                                    END IF

                                 END DO

                              END IF

                           END IF

                        END IF

                     END IF

                  END DO
                  
               ELSE IF (iftype == 40) THEN

                  rdr_rdrtype       (ii) = fixdata%frecord(1)%radar%rdrtype
                  rdr_radobcode     (ii) = & 
                       fixdata%frecord(1)%radar%radobcode
                  rdr_plainlanguage (ii) = &
                       fixdata%frecord(1)%radar%plainlanguage
                  rdr_doppler       (ii) = fixdata%frecord(1)%radar%doppler 
                  rdr_radob         (ii) = fixdata%frecord(1)%radar%radob
                  rdr_eyeshape      (ii) = & 
                       fixdata%frecord(1)%radar%eyeshape 
                  rdr_eyewallob     (ii) = & 
                       fixdata%frecord(1)%radar%eyewallob
                  rdr_spiralov      (ii) = fixdata%frecord(1)%radar%spiralov
                  rdr_lat           (ii) = fixdata%frecord(1)%radar%lat
                  rdr_ns            (ii) = fixdata%frecord(1)%radar%ns
                  rdr_lon           (ii) = fixdata%frecord(1)%radar%lon
                  rdr_ew            (ii) = fixdata%frecord(1)%radar%ew
                  rdr_vmaxin        (ii) = fixdata%frecord(1)%radar%vmaxin
                  rdr_azimuthin     (ii) = & 
                       fixdata%frecord(1)%radar%azimuthin
                  rdr_rangein       (ii) = fixdata%frecord(1)%radar%rangein
                  rdr_elevin        (ii) = fixdata%frecord(1)%radar%elevin
                  rdr_vmaxout       (ii) = fixdata%frecord(1)%radar%vmaxout
                  rdr_azimuthout    (ii) = &
                       fixdata%frecord(1)%radar%azimuthout
                  rdr_rangeout      (ii) = fixdata%frecord(1)%radar%rangeout
                  rdr_elevout       (ii) = fixdata%frecord(1)%radar%elevout
                  rdr_cloudheight   (ii) = & 
                       fixdata%frecord(1)%radar%cloudheight
                  rdr_rainaccum     (ii) = &
                       fixdata%frecord(1)%radar%rainaccum 
                  rdr_rainactimeint (ii) = & 
                       fixdata%frecord(1)%radar%rainactimeint
                  rdr_rainlat       (ii) = fixdata%frecord(1)%radar%rainlat
                  rdr_rainns        (ii) = fixdata%frecord(1)%radar%rainns
                  rdr_rainlon       (ii) = fixdata%frecord(1)%radar%rainlon
                  rdr_rainew        (ii) = fixdata%frecord(1)%radar%rainew
                  rdr_comments      (ii) = fixdata%frecord(1)%radar%comments

               ELSE IF (iftype == 50) THEN
                  
                  airc_flightlevelft            (ii) = &
                       fixdata%frecord(1)%air%flightlevelft
                  airc_flightlevelmb            (ii) = &
                       fixdata%frecord(1)%air%flightlevelmb
                  airc_minheight                (ii) = & 
                       fixdata%frecord(1)%air%minheight
                  airc_maxsurfacewind           (ii) = & 
                       fixdata%frecord(1)%air%maxsurfacewind 
                  airc_maxsurfacewindbearing    (ii) = & 
                       fixdata%frecord(1)%air%maxsurfacewindbearing 
                  airc_maxsurfacewindrange      (ii) = & 
                       fixdata%frecord(1)%air%maxsurfacewindrange
                  airc_maxflightlevelwinddir    (ii) = & 
                       fixdata%frecord(1)%air%maxflightlevelwinddir 
                  airc_maxflightlevelwindint    (ii) = & 
                       fixdata%frecord(1)%air%maxflightlevelwindint 
                  airc_maxflightlevelwindbearing(ii) = & 
                       fixdata%frecord(1)%air%maxflightlevelwindbearing
                  airc_maxflightlevelwindrange  (ii) = & 
                       fixdata%frecord(1)%air%maxflightlevelwindrange 
                  airc_minsealevelpressure      (ii) = & 
                       fixdata%frecord(1)%air%minsealevelpressure
                  airc_eyetempoutside           (ii) = & 
                       fixdata%frecord(1)%air%eyetempoutside 
                  airc_eyetempinside            (ii) = & 
                       fixdata%frecord(1)%air%eyetempinside 
                  airc_dewpoint                 (ii) = & 
                       fixdata%frecord(1)%air%dewpoint 
                  airc_seasurface               (ii) = & 
                       fixdata%frecord(1)%air%seasurface 
                  airc_eyewallthickness         (ii) = & 
                       fixdata%frecord(1)%air%wallcloudthickness
                  airc_eyeshape                 (ii) = & 
                       fixdata%frecord(1)%air%eyeshape 
                  airc_eyeorientation           (ii) = &
                       fixdata%frecord(1)%air%eyeorientation
                  airc_eyediameterlongaxis      (ii) = &
                       fixdata%frecord(1)%air%diameterlongaxis
                  airc_eyediametershortaxis     (ii) = & 
                       fixdata%frecord(1)%air%diametershortaxis
                  airc_navacc                   (ii) = &
                       fixdata%frecord(1)%air%navigationalaccuracy
                  airc_navmet                   (ii) = & 
                       fixdata%frecord(1)%air%navigationalmeteorological
                  airc_missionnumber            (ii) = & 
                       fixdata%frecord(1)%air%missionnumber 
                  airc_comments                 (ii) = &
                       fixdata%frecord(1)%air%comments  
                  
               ELSE IF (iftype == 60) THEN

                  drop_sondeenv (ii) = fixdata%frecord(1)%drop%sondeenv
                  drop_height150(ii) = fixdata%frecord(1)%drop%height150
                  drop_vspd150  (ii) = fixdata%frecord(1)%drop%vspd150
                  drop_vspd500  (ii) = fixdata%frecord(1)%drop%vspd500
                  drop_comments (ii) = fixdata%frecord(1)%drop%comments

               ELSE IF (iftype == 70) THEN
                  
                  anal_ititials (ii) = fixdata%frecord(1)%anal%initials
                  anal_startdtg (ii) = fixdata%frecord(1)%anal%startdtg
                  anal_enddtg   (ii) = fixdata%frecord(1)%anal%enddtg
                  anal_dist2data(ii) = & 
                       fixdata%frecord(1)%anal%distancetonearestdata
                  anal_sst      (ii) = fixdata%frecord(1)%anal%sst 
                  anal_obsources(ii) = fixdata%frecord(1)%anal%obsources 
                  anal_comments (ii) = fixdata%frecord(1)%anal%comments
                  
               END IF
               
               DO i = 1,4
                  fr_r34(ii,i)=rmiss
                  fr_r50(ii,i)=rmiss
                  fr_r64(ii,i)=rmiss
               END DO
                  
               DO j=1, fixdata%numrcrds
                  
                  IF (fixdata%frecord(j)%radiiFix) THEN
                     
                     IF (fixdata%frecord(j)%rad == 34) THEN
                        
                        DO i=1,4
                              
                           IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                .OR. fixdata%frecord(j)%windcode &
                                .EQ. 'NEQ ') THEN
                              
                              fr_r34(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                           ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA' &
                                .OR. fixdata%frecord(j)%windcode .EQ.&
                                'AAA ') THEN

                              fr_r34(ii,i)=float(fixdata%frecord(j)%radii(1))

                           ELSE
                                 
                              PRINT*,'extract_fix_2532: unexpected windcode = ' &
                                   ,fixdata%frecord(j)%windcode, 'R34'

                           END IF

                        END DO

                     ELSE IF (fixdata%frecord(j)%rad == 50) THEN

                        DO i=1,4
                              
                          IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                .OR. fixdata%frecord(j)%windcode &
                                .EQ. 'NEQ ') THEN
                                 
                              fr_r50(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                           ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA' &
                                .OR. fixdata%frecord(j)%windcode .EQ.&
                                'AAA ') THEN

                              fr_r50(ii,i)=float(fixdata%frecord(j)%radii(1))
                              
                           ELSE
                                 
                              PRINT*,'extract_fix_2554: unexpected windcode = ' &
                                   ,fixdata%frecord(j)%windcode, 'R50'
                              
                           END IF

                        END DO
                     ELSE IF (fixdata%frecord(j)%rad == 64) THEN

                        DO i=1,4
                              
                          IF (fixdata%frecord(j)%windcode .EQ. ' NEQ' &
                                .OR. fixdata%frecord(j)%windcode &
                                .EQ. 'NEQ ') THEN
                                 
                              fr_r64(ii,i)=float(fixdata%frecord(j)%radii(i))
                              
                           ELSE IF(fixdata%frecord(j)%windcode .EQ. ' AAA' &
                                .OR. fixdata%frecord(j)%windcode .EQ.&
                                'AAA ') THEN

                              fr_r64(ii,i)=float(fixdata%frecord(j)%radii(1))

                           ELSE
                                 
                              PRINT*,'extract_fix_2575: unexpected windcode = ' &
                                   ,fixdata%frecord(j)%windcode, 'R64'

                           END IF

                        END DO

                     END IF

                  END IF
   
               END DO

            END IF

         END IF

      END DO
      extract_fix=.true.

      CLOSE (luff)

    END FUNCTION extract_fix

  END MODULE extract_fix_module
