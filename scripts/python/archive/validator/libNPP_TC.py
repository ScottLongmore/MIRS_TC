#!/usr/bin/python
"""
libAMSU_TC.py - library of AMSU_TC data file filter routines 
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

# Local modules
import error_codes
from setup_nde_logging import setup_logging

# Error Handing Routines 
def error(LOG,msg="Unexpected Error:",code=1):
    LOG.exception(msg)
    sys.exit(code)

LOG = logging.getLogger(__name__)

# Filter Routines

def adeckValid(input):

    adeckRE=re.compile(input['re'])

    stormIds={}
    oldFiles=[]
    for file in input['files']:
        try: 
            match=re.match(adeckRE,file)
            fields=match.groupdict()
            stormId=fields['stormId']
            DTG=datetime.datetime.strptime(fields['DTG'],"%Y%m%d%H%M")
        except:
            msg="Problem extracting fields from adeckfile {}".format(file)
            error(LOG,msg,error_codes.EX_IOERR)

        if stormId in stormIds:
            if DTG > stormIds[stormId]['DTG']:
                oldFiles.append(stormIds[stormId]['file'])
                stormIds[stormId]['file']=file
                stormIds[stormId]['DTG']=DTG
            else:
                oldFiles.append(file)
        else:
           stormIds[stormId]={}
           stormIds[stormId]['file']=file
           stormIds[stormId]['DTG']=DTG

    for file in oldFiles:
        input['files'].pop(file,None)

    return

def gfsValid(input):

    gfsRE=re.compile(input['re'])

    lastDTG=datetime.datetime(1970,1,1,0,0,0)
    lastFile=None
    oldFiles=[]
    for file in input['files']:
        try: 
            match=re.match(gfsRE,file)
            fields=match.groupdict()
            DTGstr="{}{}".format(fields['gdate'],fields['hour'])
            DTG=datetime.datetime.strptime(DTGstr,"%Y%m%d%H")
        except:
            msg="Problem extracting fields from GFS GRIB file {}".format(file)
            error(LOG,msg,error_codes.EX_IOERR)

        if DTG > lastDTG:
            if lastFile:
                oldFiles.append(lastFile)
            lastFile=file
            lastDTG=DTG
        else:
            oldFiles.append(file)

    for file in oldFiles:
        input['files'].pop(file,None)

    return


