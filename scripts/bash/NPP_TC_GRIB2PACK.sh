#!/bin/sh
#
# Script GRIB2PACK - Dumps specified GRIB fields to bin files, 
#                    that are then converted to pack format
#

inputOptions="-s -i -no_header"
outputOptions="-order raw -bin"

patterns=(':UGRD:70 mb:' ":UGRD:100 mb:" ":UGRD:150 mb:" ":UGRD:200 mb:" ":UGRD:250 mb:" ":UGRD:300 mb:" ":UGRD:400 mb:" ":UGRD:500 mb:"\
          ":UGRD:600 mb:" ":UGRD:700 mb:" ":UGRD:850 mb:" ":UGRD:925 mb:" ":UGRD:1000 mb:" ":UGRD:0.995 sigma level:"\
          ":VGRD:70 mb:" ":VGRD:100 mb:" ":VGRD:150 mb:" ":VGRD:200 mb:" ":VGRD:250 mb:" ":VGRD:300 mb:" ":VGRD:400 mb:" ":VGRD:500 mb:"\
          ":VGRD:600 mb:" ":VGRD:700 mb:" ":VGRD:850 mb:" ":VGRD:925 mb:" ":VGRD:1000 mb:" ":VGRD:0.995 sigma level:"\
          ":HGT:70 mb:" ":HGT:100 mb:" ":HGT:150 mb:" ":HGT:200 mb:" ":HGT:250 mb:" ":HGT:300 mb:" ":HGT:400 mb:" ":HGT:500 mb:"\
          ":HGT:600 mb:" ":HGT:700 mb:" ":HGT:850 mb:" ":HGT:925 mb:" ":HGT:1000 mb:" ":HGT:surface:"\
          ":TMP:70 mb:" ":TMP:100 mb:" ":TMP:150 mb:" ":TMP:200 mb:" ":TMP:250 mb:" ":TMP:300 mb:" ":TMP:400 mb:" ":TMP:500 mb:"\
          ":TMP:600 mb:" ":TMP:700 mb:" ":TMP:850 mb:" ":TMP:925 mb:" ":TMP:1000 mb:" ":TMP:0.995 sigma level:"\
          ":RH:100 mb:" ":RH:70 mb:" ":RH:150 mb:" ":RH:200 mb:" ":RH:250 mb:" ":RH:300 mb:" ":RH:400 mb:" ":RH:500 mb:"\
          ":RH:600 mb:" ":RH:700 mb:" ":RH:850 mb:" ":RH:925 mb:" ":RH:1000 mb:" ":RH:0.995 sigma level:" ":PRMSL:mean sea level:")

fieldIds=("u70" "u100" "u150" "u200" "u250" "u300" "u400" "u500" "u600" "u700" "u850" "u925" "u1000" "u1070"\
          "v70" "v100" "v150" "v200" "v250" "v300" "v400" "v500" "v600" "v700" "v850" "v925" "v1000" "v1070"\
          "height70" "height100" "height150" "height200" "height250" "height300" "height400" "height500"\
          "height600" "height700" "height850" "height925" "height1000" "height1070"\
          "temp70" "temp100" "temp150" "temp200" "temp250" "temp300" "temp400" "temp500"\
          "temp600" "temp700" "temp850" "temp925" "temp1000" "temp1070"\
          "relh100" "relh70" "relh150" "relh200" "relh250" "relh300" "relh400" "relh500"\
          "relh600" "relh700" "relh850" "relh925" "relh1000" "relh1070" "p1070")


# Extract fields from grib to binary file

subLogFile=$LOGDIR/$(basename "$WGRIB")".log"
for ((i = 0; i < ${#patterns[@]}; i++))
  do
  
  binFile=$gribDate$gribTime"_F"$gribFTime"_"${fieldIds[$i]}".bin"
  $WGRIB -s $gribFile | egrep "${patterns[$i]}" | $WGRIB $inputOptions $gribFile $outputOptions $binFile >> $subLogFile 2>&1

done
formatLog $subLogFile $MAIN $(basename "$WGRIB")":"
cat $subLogFile >> $LOGFILE

# Combine binary files to pack file
subLogFile=$LOGDIR/$(basename "${BIN2PACK%.*}")".log"
logEntry $LOGFILE $MAIN "Running $BIN2PACK $gribDate $gribTime $gribFTime"
$BIN2PACK $gribDate $gribTime $gribFTime >> $subLogFile 2>&1
formatLog $subLogFile $MAIN $BIN2PACK":"
cat $subLogFile >> $LOGFILE

