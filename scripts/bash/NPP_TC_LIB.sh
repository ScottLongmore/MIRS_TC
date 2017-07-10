#!/bin/bash
# Library of script functions

function runTime() {
    echo `date -u +%Y%m%d%H%M%S`
}

function logEntry() {

    logFile=$1
    program=$2
    message="$3"

    echo "$(runTime) | $program | $message" >> $logFile
}

function infoLogEntry() {

    logFile=$1
    program=$2
    message=$3

    logEntry $logFile $program "INFO: $message" 
}

function errorLogEntry() {

    logFile=$1
    program=$2
    message=$3

    logEntry $logFile $program "$algorithmLogMessageError: $message" 
}

function warningLogEntry() {

    logFile=$1
    program=$2
    message=$3

    logEntry $logFile $program "$algorithmLogMessageWarn: $message" 
}


function mkDir() {
    
    dir=$1

    if [ ! -d $dir ]; then
        mkdir $dir
    fi
}

function formatLog() {

    logFile=$1
    program=$2
    prefixString=$3

    dateRE="^[[:space:]]*[0-9]{14}"
    origLogFile=$logFile".orig"

    if [[ -s $logFile ]] ; then

        mv $logFile $origLogFile

        while read line; do
    
          if [[ $line =~ $dateRE ]] ; then
            echo $line >> $logFile
          else
            logEntry $logFile $program "$prefixString $line"
          fi 

        done < $origLogFile 
    fi

}

function checkBasin() {

    basin=$1
    file=$2
    logFile=$3
    program=$4

    case $basin in
 
        al)
            logEntry $logFile $program "Basin is Atlantic ("$basin") for "$file
            ;;
        ep)
            logEntry $logFile $program "Basin is Eastern Pacific ("$basin") for "$file
            ;;
        cp)
            logEntry $logFile $program "Basin is Central Pacific ("$basin") for "$file
            ;;
        wp)
            logEntry $logFile $program "Basin is Western Pacific ("$basin") for "$file
            ;;
        sh)
            logEntry $logFile $program "Basin is Southern Hemisphere ("$basin") for "$file
            ;;
        io)
            logEntry $logFile $program "Basin is Indian ("$basin") for "$file
            ;;
        *)
            errorLogEntry $logFile $program "Unknown basin" $basin ", exiting"
            # Exiting with configuration error
            STATUS=78
            exit $STATUS 
            ;;
     esac

}

