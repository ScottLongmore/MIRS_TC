#!/usr/bin/python
"""
MIRS_TC - library of MIRS_TC routines 
"""

# Stock modules
import sys
import os
import re
import logging
import traceback
import datetime
import collections
import operator
import json
import jsonschema
import numpy
import pandas as pd


# Local modules
import error_codes
import utils

# Logging
LOG = logging.getLogger(__name__)

DTFormat="%Y%m%d%H%M%S%f"
synHourDeltas={
   0:{"forward":0,"backward":6},
   1:{"forward":1,"backward":6},
   2:{"forward":2,"backward":6},
   3:{"forward":3,"backward":6},
   4:{"forward":4,"backward":6},
   5:{"forward":4,"backward":6}
}

basins={
   "al":"Atlantic",
   "ep":"Eastern Pacific",
   "cp":"Central Pacific",
   "wp":"Western Pacific",
   "io":"Indian Ocean",
   "sh":"Southern Hemisphere"
}

gfsGribFields={
    "u70":":UGRD:70 mb:", "u100":":UGRD:100 mb:", "u150":":UGRD:150 mb:", "u200":":UGRD:200 mb:", "u250":":UGRD:250 mb:", 
    "u300":":UGRD:300 mb:", "u400":":UGRD:400 mb:", "u500":":UGRD:500 mb:", "u600":":UGRD:600 mb:", "u700":":UGRD:700 mb:", 
    "u850":":UGRD:850 mb:", "u925":":UGRD:925 mb:", "u1000":":UGRD:1000 mb:", "u1070":":UGRD:0.995 sigma level:",
    "v70":":VGRD:70 mb:", "v100":":VGRD:100 mb:", "v150":":VGRD:150 mb:", "v200":":VGRD:200 mb:", "v250":":VGRD:250 mb:",
    "v300":":VGRD:300 mb:", "v400":":VGRD:400 mb:", "v500":":VGRD:500 mb:", "v600":":VGRD:600 mb:", "v700":":VGRD:700 mb:",
    "v850":":VGRD:850 mb:", "v925":":VGRD:925 mb:", "v1000":":VGRD:1000 mb:", "v1070":":VGRD:0.995 sigma level:",
    "height70":":HGT:70 mb:", "height100":":HGT:100 mb:", "height150":":HGT:150 mb:", "height200":":HGT:200 mb:", "height250":":HGT:250 mb:",
    "height300":":HGT:300 mb:", "height400":":HGT:400 mb:", "height500":":HGT:500 mb:", "height600":":HGT:600 mb:", "height700":":HGT:700 mb:",
    "height850":":HGT:850 mb:", "height925":":HGT:925 mb:", "height1000":":HGT:1000 mb:", "height1070":":HGT:surface:", 
    "temp70":":TMP:70 mb:", "temp100":":TMP:100 mb:", "temp150":":TMP:150 mb:", "temp200":":TMP:200 mb:", "temp250":":TMP:250 mb:",
    "temp300":":TMP:300 mb:", "temp400":":TMP:400 mb:", "temp500":":TMP:500 mb:", "temp600":":TMP:600 mb:", "temp700":":TMP:700 mb:",
    "temp850":":TMP:850 mb:", "temp925":":TMP:925 mb:", "temp1000":":TMP:1000 mb:", "temp1070":":TMP:0.995 sigma level:",
    "relh100":":RH:100 mb:", "relh70":":RH:70 mb:", "relh150":":RH:150 mb:", "relh200":":RH:200 mb:", "relh250":":RH:250 mb:",
    "relh300":":RH:300 mb:", "relh400":":RH:400 mb:", "relh500":":RH:500 mb:", "relh600":":RH:600 mb:", "relh700":":RH:700 mb:",
    "relh850":":RH:850 mb:", "relh925":":RH:925 mb:", "relh1000":":RH:1000 mb:", "relh1070":":RH:0.995 sigma level:", 
    "p1070":":PRMSL:mean sea level:"
}


def last_synoptic_datetime(dt):

    hours=[6,12,18]

    synHour=0
    for hour in hours:
        if dt.hour >= hour:
            synHour=hour
        else:
            break

    return(dt.replace(hour=synHour,minute=0,second=0,microsecond=0))

def next_synoptic_datetime(dt):
            
    hours=[18,12,6]
        
    synHour=0
    for hour in hours:
        if dt.hour < hour:
            synHour=hour
        else:
            break

    return(dt.replace(hour=synHour,minute=0,second=0,microsecond=0))

def datetimes(dts):

    try:
        dtg=datetime.datetime.strptime(dts,DTFormat)
    except:
        msg="Time: {} invalid format (YYYYMMDDHHMMSSS)".format(dts)
        utils.error(LOG,msg,error_codes.EX_IOERR)

    try:
         initDTG=dtg.replace(minute=0,second=0,microsecond=0) # Last Hour floor e.g. 5:05:23 -> 5:00:00
         lastSynpDTG=last_synoptic_datetime(initDTG) 
         synpDelta=int((initDTG-lastSynpDTG).total_seconds() // 3600)
         datetimes={
             "initDTG":initDTG,
             "synpDTG":lastSynpDTG,
             "MIRS_FRWD_HOURS":synHourDeltas[synpDelta]['forward'],
             "MIRS_BKWD_HOURS":synHourDeltas[synpDelta]['backward']
          }
    except:
        msg="Problem determining datetimes"
        utils.error(LOG,msg,error_codes.EX_IOERR)

    return(datetimes)

def grib2pack(gribDate,gribTime,gribFhour,gribFile,wgribExe,bin2packExe,logDir):

    gribString="{}{}_F{}".format(gribDate,gribTime,gribFhour)
    LOG.info("Extracting binary fields from grib file: {}".format(gribFile))
    for field in gfsGribFields:

        pattern=gfsGribFields[field]
        binFile="{}_{}.bin".format(gribString,field)
    
        # Extract grids from grib files via WGRIB (needs to be replaced with grib_api module) 
        commandList=[wgribExe]
        commandArgs=[]
        commandArgs.extend(["-match",pattern,"-no_header",gribFile,"-order","raw","-bin",binFile])
        commandID=field
        stdoutFile=os.path.join(logDir,"wgrib_{}_{}.log".format(gribString,field))
        stderrFile=os.path.join(logDir,"wgrib_{}_{}.err".format(gribString,field))
        LOG.debug("Executing: {}".format(" ".join(commandList+commandArgs)))
        if not utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile):
             LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
             LOG.warning("See sub-process log file: {}".format(stdoutFile))
             LOG.warning("See sub-process error file: {}".format(stderrFile))
             return(False)

    # Combine binanry files to pack file
    commandList=[bin2packExe]
    commandArgs=[]
    commandArgs.extend([gribDate,gribTime,gribFhour])
    commandID="bin2pack"
    stdoutFile=os.path.join(logDir,"bin2pack_{}.log".format(gribString))
    stderrFile=os.path.join(logDir,"bin2pack_{}.err".format(gribString))
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    if not utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile):
         LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
         LOG.warning("See sub-process log file: {}".format(stdoutFile))
         LOG.warning("See sub-process error file: {}".format(stderrFile))
         return(False)

    return(True)

def getInputVars(filepath,column,value):

     # Fix for bad column name 'DELTA T' with space
     names=['DATE','TIME','DELTA_T','LATITUDE','LONGITUDE',\
            'DIRECTION','SPEED','RMAX','VMAX','NAME','BOGUS']

     if column not in names:
        msg="Column {} does not exist in short term track input table: {}".format(column,filepath)
        utils.error(LOG,msg,error_codes.EX_IOERR)
        
     if os.path.isfile(filepath):
         with open(filepath) as f:
            inputDF = pd.read_table(f, sep='\s+', index_col=0, header=None, names=names, skiprows=3, parse_dates={"DATETIME":[0,1]})

         inputDF=inputDF.drop('BOGUS',axis=1)
         rows=inputDF.loc[inputDF[column] == value].to_dict(orient="index") 
     else:
        msg="File doesn't exist: {}".format(filepath)
        utils.error(LOG,msg,error_codes.EX_IOERR)

     return(rows)

def getInputAnalysisVars(filepath):

     rows=getInputVars(filepath,'DELTA_T',0)

     analDTG=rows.keys()[0] # Assume only one
     analysis={}
     for col in rows[analDTG]:
        analysis[col]=rows[analDTG][col]
     analysis['DTG']=analDTG
 
     return(analysis)
