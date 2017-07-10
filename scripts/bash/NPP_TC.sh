#!/bin/bash
#
# NPP_TC.sh - Processing script to generate aircraft-based tropical cyclone 
#             surface wind analysis utilizing best tropical cyclone track,
#             GFS analysis and MIRS ATMS satellite data
#
# Script designed to 
# 1) determine which active storms are under aircraft reconnaissance,
# 2) compile the dataset necessary for the creation of an aircraft-based 
#    tropical cyclone surface wind analysis,
# 3) call routines to create the analysis
#
# Written by J. Knaff (NOAA/NESDIS)
#
# Last Modified:   Jan.  27, 2014  v1.3  S. Longmore (CIRA)
# Last Modified:   Oct.  31, 2013  v1.2  S. Longmore (CIRA)
# Last Modified:   June  26, 2013  v1.1  S. Longmore (CIRA)
# Last Modified:   May   21, 2013  v1.0  S. Longmore (CIRA)
# Last Modified:   April 22, 2013  BETA  J. Dostalek (CIRA)
#
# History: 
#	This version is an adaptation of John's code for use with the
#       ATMS-MIRS data, the processing of which will be run at NESDIS.
#
#************************************************************************

#   Retrieve Working and Source Directories
    if [ -e $1 ]; then
       WORKDIR=$1
    else
       WORKDIR=$PWD
    fi
    SRCDIR=$( dirname $0 )


#   Source Process Control File (PCF) 
    NPP_TC_PCF="$WORKDIR/NPP_TC.PCF"
    if [ -e $NPP_TC_PCF ]; then
      . $NPP_TC_PCF
    else
      echo "PCF FILE: $NPP_TC_PCF not found, exiting"
      exit 66
    fi

#************************************************************************

#   NPP_TC VARIABLES
    MAIN=NPP_TC
    VER=v1r5
    SAT=SNPP01
    SATS=("SNPP01")
    OUTSATS=("npp")
#    SATS=("SNPP01" "JPSS01" "JPSS02")
#    OUTSATS=("npp" "J01" "J02")
    INSTR=ATMS
    OUTSTRING=TC

    STATUS=0

#   NPP_TC WORKING DIRECTORIES ($working_directory SET IN PCF)
    WORKDIR=$working_directory
    LOGDIR=$WORKDIR/log/
    DATADIR=$WORKDIR/data/
    MODELDIR=$WORKDIR/model/
    QUERYDIR=$WORKDIR/query/
    OUTDIR=$WORKDIR/output/
    WORKSUBDIRS=($LOGDIR $DATADIR $MODELDIR $QUERYDIR $OUTDIR)

#   NPP_TC PROCESS FILES
    PCFFILE=$WORKDIR/NPP_TC.PCF
    PSFFILE=$WORKDIR/NPP_TC.PSF
    LOGFILE=$WORKDIR/NPP_TC.LOG

#   NPP_TC SOURCE SUB-DIRECTORIES (ROOTDIR SET IN PCF)
    BINDIR=$ROOTDIR/bin/
    ETCDIR=$ROOTDIR/etc/
    SCPDIR=$ROOTDIR/scripts/
    PYDIR=$SCPDIR/python/
    PCF_VALID_SRC=$PYDIR/validator/
    DATETIME_SRC=$PYDIR/datetimes/
    MIRS_DATAPOOL_SRC=$PYDIR/data_pool/
    PLOT_SRC=$PYDIR/plots/
    CONVERT_SRC=$PYDIR/convert/
    PYCOMDIR=$SCPDIR/python/common/

#   PYTHON LIBRARY PATH 
    PYTHONPATH=$PYTHONPATH:$PYCOMDIR
    export PYTHONPATH

#   EXECUTABLE FILES
    BIN2PACK=$BINDIR/bin2pack.x
    SHORTTERMTRACK=$BINDIR/ShortTermTrack_mirs.x
    AFDECK=$BINDIR/afdeck.x
    SATCENTER=$BINDIR/satcenter.x
    OPARET=$BINDIR/oparet.x

#   CONFIGURATION FILES
    OPARET_INPATTS=$ETCDIR/oparet.cfg

#   SCRIPT FILES
    NPP_TC_LIB=$SCPDIR/NPP_TC_LIB.sh
    NPP_TC_GETTIMES=$SCPDIR/NPP_TC_GETTIME.sh
    NPP_TC_GRIB2PACK=$SCPDIR/NPP_TC_GRIB2PACK.sh
    NPP_TC_MKCONFIG=$SCPDIR/NPP_TC_MKCONFIG.sh

    # PCF validation
    PCF_VALID_FLAG='ON'
    PCF_VALID=$PCF_VALID_SRC/validatePCF.py
    PCF_VALID_CONFIG=$PCF_VALID_SRC/NPP_TC_VALID.json

    # DateTimes script
    DATETIME_SCRIPT=$DATETIME_SRC/NPP_TC_DateTimes.py
    DATETIME_CONFIG=$DATETIME_SRC/NPP_TC_DataHours.csv
    DATETIMES=NPP_TC_DATETIMES.sh

    # MIRS data and query python scripts 
    MIRS_DATA_POOL=$MIRS_DATAPOOL_SRC/data_pool.py
    MIRS_POOL_QUERY=$MIRS_DATAPOOL_SRC/pool_query.py

    # MIRS data and query configuration files 
    MIRS_QUERY_CONFIG=$MIRS_DATAPOOL_SRC/query_config.txt
    MIRS_QUERY_VAR_LIST=$MIRS_DATAPOOL_SRC/query_variable_list.txt
    MIRS_VAR_INFO=$MIRS_DATAPOOL_SRC/variable_info.txt
    MIRS_SATID_LOOKUP=$MIRS_DATAPOOL_SRC/sat_id_lookup_config.txt
    MIRS_SATID_REGEXP=$MIRS_DATAPOOL_SRC/sat_id_regex_config.txt
    MIRS_CONFIG_FILES=($MIRS_QUERY_CONFIG $MIRS_QUERY_VAR_LIST $MIRS_VAR_INFO $MIRS_SATID_LOOKUP $MIRS_SATID_REGEXP)

    # Plot Script 
    PLOT_SCRIPT=$PLOT_SRC/main_read_plot_xya.py

    # Convert Script
    CONVERT_SCRIPT=$CONVERT_SRC/convert2netCDF.py
    CONVERT_XYA_CONFIG=$CONVERT_SRC/xya2netCDF.json
    CONVERT_RZA_CONFIG=$CONVERT_SRC/rza2netCDF.json

#   PCF data file key designators
    ADECKFILEKEY='adeck_input'
    GFSFILEKEY='gfs_input'
    MIRSFILEKEY='mirs_'

#   Regular Expressions
    ADECKRE="^(a([a-zA-Z]{2})([0-9]{2})([0-9]{4}))\.dat$"
    ADECKRE_NDE="^(nhc|jtwc)_(a([a-zA-Z]{2})([0-9]{2})([0-9]{4}))\.dat\.([0-9]{12})$"
    # G00012_X0818_PACK.DAT
    GFSPACKGLOB="G?????_?????_PACK.DAT"
    GFSPACKRE="^G([0-9]{5})_([XY])([0-9]{4})_PACK\.DAT$"
    # gfs.t18z.pgrb2f00.1p0deg.20121027
    # GFSGRIBRE="^gfs\.t([0-9]{2})z\.pgrb2f([0-9]{2})\.1p0deg\.([0-9]{8})$"

    # gfs.t12z.pgrb2.1p00.f006.20141217 (Changed Jan 14 2015)
    GFSGRIBRE="^gfs\.t([0-9]{2})z\.pgrb2\.1p00\.f([0-9]{3})\.([0-9]{2})([0-9]{6})$"

#   EXTENSIONS
    PYEXT="*.py"
    CFEXT="*.txt"
    PNGEXT="*.png"

#   WORKING INPUT/OUTPUT FILES 

    # AFDECK output files 
    COORDINATES=COORDINATES

    # MIRS data query output files 
    MIRS_SATCENTER_FILES=("years.txt" "months.txt" "days.txt" "hours.txt" "minutes.txt" "seconds.txt" "latitude.txt" "longitude.txt"\
                          "scanline_index.txt" "scanline_center_lat.txt" "scanline_center_lon.txt" "element_index.txt")
    MIRS_OPARET_FILES=("pressure_layers.txt" "surface_pressure.txt" "temperature_profile.txt" "water_vapor_profile.txt"\
                       "skin_temperature.txt" "surface_type.txt" "tpw.txt" "clw.txt" "qc.txt" "lwp.txt" "rwp.txt")
    MIRS_DATA_FILES=(${MIRS_SATCENTER_FILES[@]} ${MIRS_OPARET_FILES[@]})

    # SATCENTER output files 
    TIMES=TIMES
    COORTIMES=COORTIMES

    # OPARET input files
    AVNFILE=AVN.DAT

#************************************************************************

#   Source script library functions
    . $NPP_TC_LIB 

#   Initalize output files
    touch $LOGFILE
    touch $PSFFILE
#
#   Starting MAIN Program
#
    logEntry $LOGFILE $MAIN "Started"

#   Test/Make directories
    for workSubDir in ${WORKSUBDIRS[@]}
        do
        logEntry $LOGFILE $MAIN "Creating working subdirectory: $workSubDir"
        mkDir $workSubDir
    done

#   Validate PCF
    if [ $PCF_VALID_FLAG == "ON" ]; then
       NPP_TC_PCF_ORG=$NPP_TC_PCF".org"
       mv $NPP_TC_PCF $NPP_TC_PCF_ORG
       logEntry $LOGFILE $MAIN "Validating PCF inputs: $PCF_VALID -c $PCF_VALID_CONFIG -i $NPP_TC_PCF_ORG -o $NPP_TC_PCF"
       subLogFile=$LOGDIR/$(basename "${PCF_VALID%%.*}")".log"
       touch $subLogFile
       $PYTHON $PCF_VALID -c $PCF_VALID_CONFIG -i $NPP_TC_PCF_ORG -o $NPP_TC_PCF  >> $subLogFile 2>&1
       returnCode="$?"
       formatLog $subLogFile $MAIN $PCF_VALID":" 
       cat $subLogFile >> $LOGFILE 
   
       if [ $returnCode -ne 0 ];  then
           errorLogEntry $logFile $program "$PCF_VALID: returned with exit code: $returnCode, exiting"
           STATUS=$returnCode
           exit $STATUS 
       fi
       logEntry $logFile $program "$PCF_VALID: exited normally"
    fi

#
#   Set Analysis/Forecast Time Variables
#
    INIT_DATETIME=$job_coverage_end
    logEntry $LOGFILE $MAIN "Setting Date/Time Variables: $DATETIME_SCRIPT -d $INIT_DATETIME -m $DATETIME_CONFIG -l $DATETIMES"
    subLogFile=$LOGDIR/$(basename "${DATETIME_SCRIPT%%.*}")".log"
    touch $subLogFile
    $PYTHON $DATETIME_SCRIPT -d $INIT_DATETIME -m $DATETIME_CONFIG -l $DATETIMES >> $subLogFile 2>&1
    returnCode="$?"
    formatLog $subLogFile $MAIN $DATETIME_SCRIPT":" 
    cat $subLogFile >> $LOGFILE 

    if [ $returnCode -ne 0 ];  then
       errorLogEntry $logFile $program "$DATETIME_SCRIPT: returned with exit code: $returnCode, exiting"
       STATUS=$returnCode
       exit $STATUS 
    fi

    . $DATETIMES



#   Test data files (deck,gfs,mirs) existance and move to data directory, Get Basin
# 
#   Test for adeck files e.g. aal192012.dat

    adeckfiles=()
    idx=0
    while read envLine; do 
       if [[ $envLine =~ $ADECKFILEKEY ]] 
         then
         adeckfile=${envLine##*=}
        
         if [ -e $adeckfile ] 
            then

            # aal182012.dat
            if [[ $adeckfile =~ $ADECKRE ]]
               then
               adeckfiles[$idx]=${BASH_REMATCH[1]}".dat"
               inputfiles[$idx]=${BASH_REMATCH[1]:1}".inp"
               tcids[$idx]=${BASH_REMATCH[1]:1}
               basins[$idx]=${BASH_REMATCH[2]}
               tcids2[$idx]=${BASH_REMATCH[3]}$( echo "${basins[$idx]:0:1}" | tr -s  '[:lower:]'  '[:upper:]' )
               
            # nhc_aal182012.dat.201311250313
            elif [[ $adeckfile =~ $ADECKRE_NDE ]]
               then
               adeckfiles[$idx]=${BASH_REMATCH[2]}".dat"
               inputfiles[$idx]=${BASH_REMATCH[2]:1}".inp"
               tcids[$idx]=${BASH_REMATCH[2]:1}
               basins[$idx]=${BASH_REMATCH[3]}
               tcids2[$idx]=${BASH_REMATCH[4]}$( echo "${basins[$idx]:0:1}" | tr -s  '[:lower:]'  '[:upper:]' )
            else
               errorLogEntry $LOGFILE $MAIN "$adeckfile unknown name format, exiting"
               STATUS=66
               exit $STATUS 
            fi

            checkBasin ${basins[$idx]} ${adeckfiles[$idx]} $LOGFILE $MAIN
   
            logEntry $LOGFILE $MAIN "Moving $adeckfile to $DATADIR"
            mv $adeckfile $DATADIR
            logEntry $LOGFILE $MAIN "Linking $DATADIR/$adeckfile to $DATADIR/${adeckfiles[$idx]}"
            if [ ! -e $DATADIR/${adeckfiles[$idx]} ]; then
                ln -s $DATADIR/$adeckfile $DATADIR/${adeckfiles[$idx]}
            fi
   
            logEntry $LOGFILE $MAIN "Adeck File: ${adeckfiles[$idx]} Storm ID: ${tcids[$idx]} Input file: ${inputfiles[$idx]}"
   
            idx=$idx+1
          else
            errorLogEntry $LOGFILE $MAIN "$adeckfile file not found, exiting"
            STATUS=66
            exit $STATUS 
          fi
             
       fi
    done < $NPP_TC_PCF


    if test ${#adeckfiles[@]} -eq 0 
        then
        errorLogEntry $LOGFILE $MAIN "No adeck files found, exiting"
        # Exiting with cannot open input
        STATUS=66
        exit $STATUS 
    fi 

#   Test for GFS model grib file and convert to pack file  
    idx=0
    while read envLine; do 
      if [[ $envLine =~ $GFSFILEKEY ]] 
        then
        GFS_FILE=${envLine##*=}
        if [ -e $GFS_FILE ];
          then
          logEntry $LOGFILE $MAIN "GFS file: $GFS_FILE , found:"  
          logEntry $LOGFILE $MAIN "Moving $GFS_FILE to $DATADIR"  
          mv $GFS_FILE $DATADIR

          # Convert GRIB to PACK files
          if [[ $GFS_FILE =~ $GFSGRIBRE ]]
            then
            # GFSGRIBRE="^gfs\.t([0-9]{2})z\.pgrb2\.1p00\.f([0-9]{3})\.([0-9]{2})([0-9]{6})$"
            gribFile=$GFS_FILE
            gribTime=${BASH_REMATCH[1]}
            gribFTime=${BASH_REMATCH[2]}
            gribDate=${BASH_REMATCH[4]}

            logEntry $LOGFILE $MAIN "Linking $DATADIR/$GFS_FILE to $MODELDIR"
            ln -s $DATADIR/$GFS_FILE $MODELDIR
            logEntry $LOGFILE $MAIN "Moving to $MODELDIR directory"
            cd $MODELDIR
            logEntry $LOGFILE $MAIN "Converting $GFS_FILE file to $AVNFILE"
            . $NPP_TC_GRIB2PACK
            logEntry $LOGFILE $MAIN "Returning to $WORKDIR directory"
            GFS_PACK_FILES=($GFSPACKGLOB)
            if [[ ${GFS_PACK_FILES[0]} =~ $GFSPACKRE ]];
              then
              ln -s $MODELDIR/${GFS_PACK_FILES[0]} $DATADIR/$AVNFILE 
            else
              errorLogEntry $LOGFILE $MAIN "Converted GFS pack file NOT found, exiting"
              # Exiting with cannot open input
              STATUS=66
              exit $STATUS 
            fi
            cd $WORKDIR

          # GFSPACK file
          elif [[ $GFS_FILE =~ $GFSPACKRE ]]
            then
            ln -s $DATADIR/$GFS_FILE $DATADIR/$AVNFILE 

          # Unrecognized GFS file
          else
             errorLogEntry $LOGFILE $MAIN "$GFS_FILE unrecognized naming convention, exiting"
             # Exiting with cannot open input
             STATUS=66
             exit $STATUS 
          fi
           
        else
          errorLogEntry $LOGFILE $MAIN "$GFS_FILE file NOT found, exiting"
          # Exiting with cannot open input
          STATUS=66
          exit $STATUS 
        fi
      fi
      idx=$idx+1
    done < $NPP_TC_PCF

#   Test for MIRS satellite netCDF file
    mirsfiles=()
    idx=0
    while read envLine; do 
       if [[ $envLine =~ $MIRSFILEKEY ]] 
         then
         mirsfiles[$idx]=${envLine##*=}
         if [ -e ${mirsfiles[$idx]} ];
           then
           logEntry $LOGFILE $MAIN "Moving ${mirsfiles[$idx]} to $DATADIR"  
           mv ${mirsfiles[$idx]} $DATADIR
         else
           errorLogEntry $LOGFILE $MAIN "${mirsfiles[$idx]} file NOT found, exiting"
           # Exiting with cannot open input
           STATUS=66
           exit $STATUS 
         fi
         idx=$idx+1
       fi
    done < $NPP_TC_PCF

    if test ${#mirsfiles[@]} -gt 0 
        then
            logEntry $LOGFILE $MAIN "MIRS files ("${#mirsfiles[@]}"), found"  
            mirsfile=${mirsfiles[0]##*/}
        else
            errorLogEntry $LOGFILE $MAIN "No mirs files found, exiting"
            # Exiting with cannot open input
            STATUS=66
            exit $STATUS 
    fi


#   Extract MIRS subset data
    logEntry $LOGFILE $MAIN "Copying MIRS configuration files to" $WORKDIR
    for mirfile in ${MIRS_CONFIG_FILES[@]}
        do
        logEntry $LOGFILE $MAIN "Copying $mirfile to $WORKDIR"
        cp $mirfile $WORKDIR
    done

#    Legacy Pool Code (no longer needed) 4/10/2017

#    logEntry $LOGFILE $MAIN "Extracting MIRS netCDF data: $PYTHON $MIRS_DATA_POOL $DATADIR $QUERYDIR -r $MIRS_SATID_REGEXP -m -1"
#    subLogFile=$LOGDIR/$(basename "${MIRS_DATA_POOL%%.*}")".log"
#    touch $subLogFile
#    $PYTHON $MIRS_DATA_POOL $DATADIR $QUERYDIR "-r" $MIRS_SATID_REGEXP "-m -1" >> $subLogFile 2>&1
#    returnCode="$?"
#    formatLog $subLogFile $MAIN $MIRS_DATA_POOL":" 
#    cat $subLogFile >> $LOGFILE 
#
#    if [ $returnCode -ne 0 ];  then
#        errorLogEntry $logFile $program "$MIRS_DATA_POOL: returned with exit code: $returnCode, exiting"
#        STATUS=$returnCode
#        exit $STATUS 
#    fi
#    logEntry $logFile $program "$MIRS_DATA_POOL: exited normally"

 
#
#   Master Storm loop - Run Oparet for each storm, creating 
#     input file (*.inp) from ShortTermTrack,
#     COORDINATES file from afdeck, 
#     TIMES from satcenter.
# 
    logEntry $LOGFILE $MAIN "Starting storm processing loop"

    idx=-1
    for adeckfile in ${adeckfiles[@]} 
	do

        idx=$idx+1

        logEntry $LOGFILE $MAIN "Processing Storm ID: ${tcids[$idx]}" 

        productPrefix=${OUTSTRING}"-"${tcids[$idx]}

#
#   Create storm sub-directory, copy scripts, programs and files
#
        STORMDIR=$WORKDIR/${tcids[$idx]}
        logEntry $LOGFILE $MAIN "Creating storm sub-directory: $STORMDIR"
        mkdir $STORMDIR

#       Link data files to storm directory
	logEntry $LOGFILE $MAIN "Linking $DATADIR/$adeckfile to $STORMDIR/$adeckfile"
        ln -s "$DATADIR/$adeckfile" "$STORMDIR/$adeckfile"


        logEntry $LOGFILE $MAIN "Linking $DATADIR/$AVNFILE to $STORMDIR/$AVNFILE"
        ln -s "$DATADIR/$AVNFILE" "$STORMDIR/$AVNFILE"

#       Move to storm directory
        logEntry $LOGFILE $MAIN "Moving to storm directory: $STORMDIR" 
        cd $STORMDIR

#       Copy config files into storm directory
        for mirfile in ${MIRS_CONFIG_FILES[@]}
            do
            logEntry $LOGFILE $MAIN "Copying $mirfile to $STORMDIR"
            cp "$mirfile" $STORMDIR
        done
        cp $OPARET_INPATTS $STORMDIR




#
#   StormTermTrack - generates input (.inp) file
#
#       Run ShortTermTrack for current synoptic time
        logEntry $LOGFILE $MAIN "$SHORTTERMTRACK: Running $SHORTTERMTRACK $adeckfile $SYNP_DATETIME CURR" 
        subLogFile=$LOGDIR/$(basename "${SHORTTERMTRACK%%.*}")_${tcids[$idx]}".log"
        touch $subLogFile
        $SHORTTERMTRACK $adeckfile $SYNP_DATETIME "CURR" >> $subLogFile 2>&1 
        returnCode="$?"
        formatLog $subLogFile $MAIN $SHORTTERMTRACK":" 
        cat $subLogFile >> $LOGFILE 

#       Run ShortTermTrack for 12hr previous of synoptic time, if 
        if  [ $returnCode -ne 0 ] || [ ! -f "${inputfiles[$idx]}" ];
            then

            logEntry $LOGFILE $MAIN "$SHORTTERMTRACK: returned with exit code $returnCode, or input file ${inputfiles[$idx]} not found for current synoptic time"
            logEntry $LOGFILE $MAIN "$SHORTTERMTRACK: Running 12hrs previous: $SHORTTERMTRACK $adeckfile $SYMP_DATETIME M12H" 
            $SHORTTERMTRACK $adeckfile $SYNP_DATETIME "M12H" >> $subLogFile 2>&1
            returnCode="$?"
            formatLog $subLogFile $MAIN $SHORTTERMTRACK":" 
            cat $subLogFile >> $LOGFILE 

            if [ $returnCode -ne 0 ];  then
                warningLogEntry $LOGFILE $MAIN "$SHORTTERMTRACK: returned with exit code: $returnCode, continuing to next storm"
                continue 
            fi

            if [ ! -f "${inputfiles[$idx]}" ];
                then
                warningLogEntry $LOGFILE $MAIN "$SHORTTERMTRACK: Input file ${inputfiles[$idx]} not found for 12 hours previous of synoptic time, continuing to next storm"
                continue
            fi

        fi
        logEntry $LOGFILE $MAIN "$SHORTTERMTRACK: exited normally"


#
#   AFDECK - generates COORDINATES file
#
        logEntry $LOGFILE $MAIN "$AFDECK: Running $AFDECK ${inputfiles[$idx]}" 
        subLogFile=$LOGDIR/$(basename "${AFDECK%%.*}")_${tcids[$idx]}".log"
        touch $subLogFile
        $AFDECK ${inputfiles[$idx]} >> $subLogFile 2>&1 
        returnCode="$?"
        formatLog $subLogFile $MAIN $AFDECK":"
        cat $subLogFile >> $LOGFILE 

        if [ $returnCode -ne 0 ];  then
            warningLogEntry $LOGFILE $MAIN "$AFDECK: returned with exit code: $returnCode, continuing to next storm"
            continue 
        fi
        logEntry $LOGFILE $MAIN "$AFDECK: exited normally"

        if [ ! -f $COORDINATES ];
            then
            warningLogEntry $LOGFILE $MAIN "$AFDECK: $COORDINATES file not found, continuing to next storm"
            continue
        fi

#   Update - comment out...nowcombined with MIRS_TC_lib.py poolConfig
#
#   MIRS DATA - Generate MIRS satellite data files 
#               (.years, .months, .days, .hours, .seconds, # .scanline_center_lat, .scanline_center_lon)  
#
#        # Create tilalo_hold info file for mkconfig.sh
#        nlines=`wc -l ${inputfiles[$idx]} | cut -f 1 -d ' '`
#       n=4
#       while [[ $n -le $nlines ]]
#           do
#           deltat=`head -$n ${inputfiles[$idx]} | tail -1 | cut -c 17-19`
#           if [[ $deltat -eq 0 ]]
#               then
#               yyyy=`head -$n ${inputfiles[$idx]} | tail -1 | cut -c 1-4`
#               echo $yyyy > tilalo_hold
#               mm=`head -$n ${inputfiles[$idx]} | tail -1 | cut -c 5-6`
#               echo $mm >> tilalo_hold
#               dd=`head -$n ${inputfiles[$idx]} | tail -1 | cut -c 7-8`
#               echo $dd >> tilalo_hold
#               hh=`head -$n ${inputfiles[$idx]} | tail -1 | cut -c 11-12`
#               echo $hh >> tilalo_hold
#               head -$n ${inputfiles[$idx]} | tail -1 | cut -c 29-34 >> tilalo_hold
#               head -$n ${inputfiles[$idx]} | tail -1 | cut -c 38-44 >> tilalo_hold
#          fi
#          let n=n+1
#       done
#
# END UPDATE
#

#
#   Satellite Processing Loop - Generate wind fields for each satellite pass
#
        logEntry $LOGFILE $MAIN "Starting satellite processing loop for ${tcids[$idx]}"

        for satIdx in ${!SATS[@]}
            do
            SAT=${SATS[$satIdx]}
            OUTSAT=${OUTSATS[$satIdx]} 

            logEntry $LOGFILE $MAIN "Processing Satellite: $SAT"

            rm -f $STORMDIR/"qc.txt"
            rm -f $STORMDIR/"lat.txt"
            rm -f $STORMDIR/"lon.txt"
            rm -f $STORMDIR/"pressure.txt"
            rm -f $STORMDIR/"clw.txt"
            rm -f $STORMDIR/"rwp.txt"
            rm -f $STORMDIR/"lwp.txt"
            rm -f $STORMDIR/"ptemp.txt"
            rm -f $STORMDIR/"pvapor.txt"
            rm -f $STORMDIR/"tpw.txt"

            #
            # UPDATE with MIRS_TC_lib.py poolConfig <args> 
            #
            #
            # Run NPP_TC_MKCONFIG.sh - creates query_config.$SAT file to query MIRS subset data
            # contains QUERYDIR and STORMDIR path variables
            logEntry $LOGFILE $MAIN "Creating query_config.$SAT file"
	    . $NPP_TC_MKCONFIG

            QUERY_CONFIG_SAT="query_config."$SAT
            if [ ! -f $QUERY_CONFIG_SAT ]
                then
                warningLogEntry "$QUERY_CONFIG_SAT file not found, continuing to next storm"
                continue
            fi
        
            # Retrieve MIRS satellite info and data files form subset data  
            logEntry $LOGFILE $MAIN "$MIRS_POOL_QUERY: Running $PYTHON $MIRS_POOL_QUERY $QUERY_CONFIG_SAT"
            subLogFile=$LOGDIR/$(basename "${MIRS_POOL_QUERY%%.*}")_${tcids[$idx]}".log"
            touch $subLogFile
            $PYTHON $MIRS_POOL_QUERY $QUERY_CONFIG_SAT >> $subLogFile 2>&1
            returnCode="$?"
            formatLog $subLogFile $MAIN $MIRS_POOL_QUERY":" 
            cat $subLogFile >> $LOGFILE 

            if [ $returnCode -ne 0 ];  then
                warningLogEntry $LOGFILE $MAIN "$MIRS_POOL_QUERY: returned with exit code: $returnCode, continuing to next storm"
                continue 
            fi
            logEntry $LOGFILE $MAIN "$MIRS_POOL_QUERY: exited normally"

            # Test MIRS output files existance and rename
            outprefix=${tcids[$idx]}'_'$yyyy$mm$dd$hh'_'$SAT
            mirsFileFlag=0
            for mirsFile in ${MIRS_DATA_FILES[@]}
                do
                if [ ! -f $mirsFile ]
                    then
                    warningLogEntry $LOGFILE $MAIN "$MIRS_POOL_QUERY: $mirsFile file not found, continuing to next storm" 
                    mirsFileFlag=1
                    continue
                fi
               
                ext=`echo $mirsFile | cut -f 1 -d '.' | cut -f 2 -d '/'`
                logEntry $LOGFILE $MAIN "$MIRS_POOL_QUERY: Renaming $mirsFile to $outprefix.$ext"
                mv $mirsFile "$outprefix.$ext"
            done 
         
            if [ $mirsFileFlag -eq 1 ]
                then
                continue 
            fi

            #
            # END UPDATE
            #

#
#   SATCENTER - generates TIMES file
#
            # Run SATCENTER, create TIMES file
            logEntry $LOGFILE $MAIN "$SATCENTER: Running $SATCENTER ${tcids[$idx]} $SYNP_DATETIME $SAT" 
            subLogFile=$LOGDIR/$(basename "${SATCENTER%%.*}")_${tcids[$idx]}".log"
            touch $subLogFile
            $SATCENTER ${tcids[$idx]} $SYNP_DATETIME $SAT >> $subLogFile 2>&1 
            returnCode="$?"
            formatLog $subLogFile $MAIN $SATCENTER":" 
            cat $subLogFile >> $LOGFILE 

            if [ $returnCode -ne 0 ];  then
                warningLogEntry $LOGFILE $MAIN "$SATCENTER: returned with exit code: $returnCode, continuing to next storm"
                continue 
            fi
            logEntry $LOGFILE $MAIN "$SATCENTER: exited normally"
    
            if [ ! -f $TIMES ];
                then
                warningLogEntry $LOGFILE $MAIN "$SATCENTER: $TIMES file not found, continuing to next storm"
                continue
            fi
       
            # Combine COORDINATES,TIMES, and SAT files to COORTIMES for OPARET
            logEntry $LOGFILE $MAIN "Creating "$COORTIMES" file" 
            cat $COORDINATES $TIMES > $COORTIMES
            echo $SAT >> $COORTIMES
            echo $INSTR >> $COORTIMES

            cp "$STORMDIR/$COORTIMES" "$STORMDIR/$COORTIMES.$SAT"
            cp "$STORMDIR/$TIMES" "$STORMDIR/$TIMES.$SAT"

#
#   OPARET - generates final output files (.AFX,.FIX,.LOC,.RZA,.STA,.XYA and oparet.log) 
#
            # Link MIRS data files to OPARET data files
            logEntry $LOGFILE $MAIN "Linking MIRS output data files to OPARET input data files"
            ln -s $outprefix".qc" "qc.txt"
            ln -s $outprefix".latitude" "lat.txt"
            ln -s $outprefix".longitude" "lon.txt"
            ln -s $outprefix".pressure_layers" "pressure.txt"
            ln -s $outprefix".clw" "clw.txt"
            ln -s $outprefix".rwp" "rwp.txt"
            ln -s $outprefix".lwp" "lwp.txt"
            ln -s $outprefix".temperature_profile" "ptemp.txt"
            ln -s $outprefix".water_vapor_profile" "pvapor.txt"
            ln -s $outprefix".tpw" "tpw.txt"

            # Run Oparet
            logEntry $LOGFILE $MAIN "$OPARET: Running $OPARET"
            subLogFile=$LOGDIR/$(basename "${OPARET%%.*}")_${tcids[$idx]}".log"
            touch $subLogFile
            $OPARET >> $subLogFile 2>&1
            returnCode="$?"
            formatLog $subLogFile $MAIN $OPARET":"
            cat $subLogFile >> $LOGFILE 

            if [ $returnCode -ne 0 ];  then
                warningLogEntry $LOGFILE $MAIN "$OPARET: returned with exit code: $returnCode, continuing to next storm"
                continue 
            fi
            logEntry $LOGFILE $MAIN "$OPARET: exited normally"

            # Check/Rename output files
            # OPARET output files 
            OPARET_FILES=($SAT".AFX" $SAT".FIX" $SAT".LOC" $SAT".RZA" $SAT".STA" $SAT".XYA" "oparet.log") 
            for oparetFile in ${OPARET_FILES[@]}
                do
                if [ ! -f $oparetFile ];
                    then
                        warningLogEntry $LOGFILE $MAIN "$OPARET: $oparetFile file not found"
                    else
                        ext=${oparetFile#*.}
                        outFile=$outprefix.$ext
                        logEntry $LOGFILE $MAIN "$OPARET: renaming $oparetFile to $outFile"
                        cp $oparetFile $outFile
                fi
            done

            # PRODUCT files
            printf -v productPrefix "%s-%s" "${OUTSTRING}" "${tcids[$idx]}"
            printf -v productSuffix "%s_%s_s%s_e%s_c%s0" "$VER" "$OUTSAT" "$job_coverage_start" "$job_coverage_end" "$(runTime)"
            PRODUCT_FILES=($SAT".AFX")
            for productFile in ${PRODUCT_FILES[@]}
                do
                ext=${productFile#*.}
                productOutFile=${productPrefix}"_"${productSuffix}"."$ext
                cp $productFile $OUTDIR/$productOutFile
                echo "$OUTDIR/$productOutFile" >> $PSFFILE
            done

            # JTWC convention for AFX/FIX file
            afxOutFile="$SAT.AFX"
            productOutFile=${productPrefix}"_"${productSuffix}"_"${tcids2[$idx]}"_FIX"
            cp $afxOutFile $OUTDIR/$productOutFile
            echo "$OUTDIR/$productOutFile" >> $PSFFILE

            # Convert XYA/RZA files to netCDF
            PARAM_CONFIG=$SAT"_param.json"
	    echo '{' >> $PARAM_CONFIG 
            echo '  "Params":{' >> $PARAM_CONFIG
            echo '    "date_created":"'$(runTime)'0",' >> $PARAM_CONFIG
            echo '    "time_coverage_start":"'$job_coverage_start'",' >> $PARAM_CONFIG
            echo '    "time_coverage_end":"'$job_coverage_end'",' >> $PARAM_CONFIG
            echo '    "satellite_name":"'$OUTSAT'",' >> $PARAM_CONFIG
            echo '    "instrument_name":"'$INSTR'",' >> $PARAM_CONFIG
            echo '    "Metadata_Link":"'$convertOutFile'"' >> $PARAM_CONFIG
            echo '  }' >> $PARAM_CONFIG
            echo '}' >> $PARAM_CONFIG 
    
            convertOutFile=${productPrefix}"-XYA_"${productSuffix}".nc"
            subLogFile=$LOGDIR/$(basename "${CONVERT_SCRIPT%%.*}")"-XYA_"${tcids[$idx]}".log"
            touch $subLogFile
            logEntry $LOGFILE $MAIN "Running $CONVERT_SCRIPT -c $CONVERT_XYA_CONFIG -p $PARAM_CONFIG -i $outprefix'.XYA' -o $convertOutFile"
            $PYTHON $CONVERT_SCRIPT -c $CONVERT_XYA_CONFIG -p $PARAM_CONFIG -i $outprefix'.XYA' -o $convertOutFile >> $subLogFile 2>&1
            returnCode="$?"
            cat $subLogFile >> $LOGFILE 
            if [ $returnCode -ne 0 ];  then
                warningLogEntry $LOGFILE $MAIN "$CONVERT_SCRIPT: returned with exit code: $returnCode"
            fi
            cp $convertOutFile $OUTDIR/$convertOutFile
            echo "$OUTDIR/$convertOutFile" >> $PSFFILE
    
            convertOutFile=${productPrefix}"-RZA_"${productSuffix}".nc"
            subLogFile=$LOGDIR/$(basename "${CONVERT_SCRIPT%%.*}")"-RZA_"${tcids[$idx]}".log"
            touch $subLogFile
            logEntry $LOGFILE $MAIN "Running $CONVERT_SCRIPT -c $CONVERT_RZA_CONFIG -p $PARAM_CONFIG -i $outprefix'.RZA' -o $convertOutFile"
            $PYTHON $CONVERT_SCRIPT -c $CONVERT_RZA_CONFIG -p $PARAM_CONFIG -i $outprefix'.RZA' -o $convertOutFile >> $subLogFile 2>&1
            returnCode="$?"
            cat $subLogFile >> $LOGFILE 
            if [ $returnCode -ne 0 ];  then
                warningLogEntry $LOGFILE $MAIN "$CONVERT_SCRIPT: failed with exit code: $returnCode"
            fi
            cp $convertOutFile $OUTDIR/$convertOutFile
            echo "$OUTDIR/$convertOutFile" >> $PSFFILE

#
#   Visualization 
#
           # Copy visualization scripts to storm directory
           logEntry $LOGFILE $MAIN "Copying visualization configuration files to storm directory"
           cp $PLOT_SRC/$CFEXT $STORMDIR 

           # Plot XYA file
           logEntry $LOGFILE $MAIN "$PLOT_SCRIPT: Ploting OPARET XYA output file"
           subLogFile=$LOGDIR/$(basename "${PLOT_SCRIPT%%.*}")"_"${tcids[$idx]}".log"
           touch $subLogFile
           $PYTHON $PLOT_SCRIPT $outprefix'.XYA' >> $subLogFile 2>&1
           returnCode="$?"
           cat $subLogFile >> $LOGFILE 

           if [ $returnCode -ne 0 ];  then
               warningLogEntry $LOGFILE $MAIN "$PLOT_SCRIPT: failed with exit code: $returnCode, continuing to next storm"
               continue 
           fi
           logEntry $LOGFILE $MAIN "$PLOT_SCRIPT: exited normally"


           pngfiles=($STORMDIR/$PNGEXT)
           for pngfile in ${pngfiles[@]}
              do
              file=${pngfile##*/}
    
              OIFS="$IFS"
              IFS='_'
              read -a fields <<< "${file%%.*}"
              IFS="$OIFS"

              productFile=${productPrefix}"-"${fields[3]}"-"${fields[5]}"_"${productSuffix}".png" 
              cp $pngfile $OUTDIR/$productFile
              echo "$OUTDIR/$productFile" >> $PSFFILE
           done

            logEntry $LOGFILE $MAIN "Completed Satellite: $SAT"
#
#       End Satelite loop 
#
        done


        logEntry $LOGFILE $MAIN "Completed Storm ID: ${tcids[$idx]}" 
#
#   Update storm file index, next storm
#
        cd $WORKDIR

    done
#
#   Completed MAIN Program
#
    logEntry $LOGFILE $MAIN "Completed (exit status $STATUS)"


exit $STATUS

: <<'END'
END
