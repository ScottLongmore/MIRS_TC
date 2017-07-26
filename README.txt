README.TXT -  NPOESS Preparatory Project (NPP) Tropical Cyclone (TC) Science Algorithm (SA) README.TXT file
             
              Author: Scott Longmore, Cooperative Institute for Research in the Atmosphere (CIRA) 
              Contact: Scott.Longmore@colostate.edu
              Last Edit: 2017-07-26


Version Number: v2.0.0

Release Date: alpha 

Configuration Target:

  - Source Directory Structure

    src/
       makefile - Master makefile
       gfspack/ - gfspack code
       ShortTermTrack/ - ShortTermTrack code
       afdeck/ - afdeck code
       satcenter/ - satcenter code
       oparet/ - oparet TC wind estimation code
       common/ - common source code 

    scripts/
       bash/
           NPP_TC.sh - Master control script
           NPP_TC_LIB.sh - Library functions
           NPP_TC_GRIB2PACK.sh - Converts GFS grib2 files to binary, then pack utilizing bin2pack.x
           NPP_TC_MKCONFIG.sh - Create query configuration file for MIRS query data script
                             files to create the NPP_TC.PCF
           NPP_TC_CRON.sh - crontab script that sets time, rundir and executes NPP_TC.sh
    
       python/ - python scripts
          mirs_tc/ - MIRS scheduling software
             mirs_tc.py - scheduling script for MIRS, links latest ATCF, GFS, and 
                          MIRS/NPP data to current run directory, runs NPP_TC.sh, 
                          eventually will replace NPP_TC.sh
          pool/ - MIRS data subset extraction code
             data_pool.py - Extracts a subset of MIRS data from netCDF files to the query subdirectory
          convert/ - converts XYA/RZA files to netCDF
            convert2netCDF.py - netCDF converter
          plot/ - TC wind estimation plotting software
            main_read_plot_xya.py - Plots Wind Field Estimation File
          common/ - common python source code

    bin/ - compiled binaries
       bin2pack.x - converts GFS GRIB to pack format
       ShortTermTrack_mirs.x - find track from current or previous synoptic time
       afdeck.x - determines coordinates from track file
       satcenter.x -  determine satelite scanline time closest to storm
       oparet.x - wind intensity estimation algorithm 
   
    etc/ - configuration and supplemenatry data files
          MIRS_TC.json - master MIRS-TC configuration file 
          MIRS_TC_POOL.ini - configuration file for data pool
          oparet.cfg - scale, offset, and valid values for oparet input data
          plots_config.txt - ploting configuration file
          exitCodes.txt - general and script/executable specific error codes
          MIRS_TC_RT.json - configuration file for task scheduler
          MIRS_TC.crontab - scheduling script crontab
          MIR_TC_CRON.sh - scheduling crontab script
    
    doc/ - documentation files

    test/ - component test directory

  - Working Directory Structure

    <WORKDIR>/ - Working Directory

       MIRS_TC.PCF - Process Control File, including all path, directory, external program, variables, settings, data filename, etc
       MIRS_TC.PSF - Process Status File
       MIRS_TC.LOG - Process Log File

       data/ - Input data files: storm track (deck text), GFS (grib), and MIRS ATMS (netCDF) 

          <aBaSnYYYY.dat> - Input adeck file 
          <gfs analysis file> - GFS analysis file (grib or pack)
          <mirs data files> - MIRS data files (netCDF)

       model/ - Conversion of GFS grib to pack files
          *.bin - variable bin files
          AVN.DAT - GFS pack file
          
       database/ - MIRS subset data

       <BaSnYYYY>/ - Storm processing sub-directories
                     Ba - Basins: [al|ep|wp|cp|io|sh] 
                     Sn - Storm Number
                     YYYYY - Year

           ShortTermTrack_mirs.x - Find track from current or previous synoptic time
              <aBaSnYYYY.dat> - Input adeck file 
              <BaSnYYYY.inp>  - Output storm track file
             
           afdeck.x - Determines coordinates from track file
              <BaSnYYYY.inp> - Input storm track file
              COORDINATES - Output coordinates file

           satcenter.x - Determine satelite scanline time closest to storm
              <BaSnYYYY_YYYYMMDD>.<variable> - Input variable files from pool query (renamed with prefix)
              TIMES - Output time file

           oparet.x - Determines wind fields from MIRS satelite data and GFS model boundary conditions
              AVN.DAT - Input GFS model data file
              <variable.txt> - Input variable files linked to <BaSnYYYY_YYYYMMDD>.<variable> 
              COORTIMES - Input COORDINATE/TIMES concatenated file
              <BaSnYYYY_YYYYMMDD>.LOC - Output Location file 
              <BaSnYYYY_YYYYMMDD>.XYA - Output Wind Field Estimation file 
              <BaSnYYYY_YYYYMMDD>.STA - Output Statistics file 
              <BaSnYYYY_YYYYMMDD>.RZA - Output Variable file 
              <BaSnYYYY_YYYYMMDD>.FIX - Output Intensity/Size Estimation
              <BaSnYYYY_YYYYMMDD>.AFX - Output Aid File  
              <BaSnYYYY_YYYYMMDD>.log - Output Log file 

            main_read_plot_xya.py - Plots Wind Field Estimation File
              <BaSnYYYY_YYYYMMDD>.XYA - Input Wind Field Estimation file 
              <BaSnYYYY_YYYYMMDD>_T_ATMS_250mb.png -    Output 250mb temperature image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_200mb.png - Output 200mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_250mb.png - Output 250mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_300mb.png - Output 300mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_400mb.png - Output 400mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_500mb.png - Output 500mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_600mb.png - Output 600mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_700mb.png - Output 700mb wind field image file
              <BaSnYYYY_YYYYMMDD>_UV+Z_ATMS_800mb.png - Output 800mb wind field image file

       output/
         TC-<BaSnYYYY>_<ver>_<sat>_sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS.AFX - AMSU TC AFX fix file
         TC-<BaSnYYYY>_<ver>_<sat>_sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS_<SnB>_FIX - AMSU TC (JTWC) fix file
         TC-<BaSnYYYY>-XYA_<ver>_<sat>_sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS.nc - XYA netCDF files 
         TC-<BaSnYYYY>-RZA_<ver>_<sat>_sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS.nc - RZA netCDF files 
         TC-<BaSnYYYY>-<field>-<level>_<ver>_<sat>_sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS.png - Storm image files 

       log/
         NPP_TC.LOG - Process Log File 
         <subprocess>.log - Sub-process log files

Reference Documents:
  


Compilation Instructions:

  The master makefile is located in the src/ directory. To compile simply run

     make

  To install executables in the bin/ directory

    make install

  - Compiler Warnings

    No compiler warning with gcc/gfortran 4.4.7-1

Product Control File Description:

    The following data files are needed for NPP TC wind estimation algorithm:

       -  The latest adeck storm track text files avaiable from NHC/JTWC 
       -  The latest GFS model analysis file in grib format 
       -  The latest set of MIRS netCDF files within the last 16hr

Product Status File Description:
      
    The following products will be generated and staged in the working
    directory output/ subdirectory

       TC-<BaSnYYYY>_sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS.<EXT> - Product files for each active storm defined in PSF
       TC-<BaSnYYYY>-<field>-<level>__sYYYYMMDDHHMMSSS_eYYYYMMDDHHMMSSS_cYYYYMMDDHHMMSSS.png - Product image for each active storm files defined in PSF

Production Rule Definitions:

    To be added
