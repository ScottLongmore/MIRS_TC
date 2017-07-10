#!/usr/bin/python
"""
NPP_TC_DateTimes.py - initialized needed NPP_TC datetimes 
"""
    
# Stock modules
import sys
import os
import re
import datetime
import optparse 
import operator
import collections
import json
import importlib
import logging
import traceback
import csv

# Local modules
import error_codes
from setup_nde_logging import setup_logging
import datetime_lib

# Error Handing Routines 
def error(LOG,msg="Unexpected Error:",code=1):
    LOG.exception(msg)
    sys.exit(code)

# Constants
DTFormat="%Y%m%d%H%M%S%f"

mirs={}

# Log
LOG = logging.getLogger(__file__) #create the logger for this file
logging_setup = 'NPP_TC'
setup_logging(logging_setup)


# Read Command Line Arguments 
options = optparse.OptionParser()
options.add_option("-d", "--initDT", dest="initDT", help="Input DateTime")
options.add_option("-m", "--mirs", dest="mirs", help="MIRS data hours")
options.add_option("-l", "--list", dest="list", help="List of date/time variables")


try:
    (opts, args) = options.parse_args()
    listFilename=opts.list
    if opts.initDT: 
        try:
            initDT=datetime.datetime.strptime(opts.initDT,DTFormat)
        except:
            msg="Time: {} invalid format YYYYMMDDHHMMSSS".format(opts.initDT)
            error(LOG,msg,error_codes.EX_IOERR)
    else:
        initDT=datetime.datetime.now()

    try:
       mirs_fh=open(opts.mirs,'r')
       for rows in csv.DictReader(mirs_fh):
           mirs[int(rows['synHourDelta'])]={
                                  'frwdHour':rows['frwdHour'],
                                  'bkwdHour':rows['bkwdHour']
                                 }
    except:
       msg="Unable to open/process MIRS data hours file: {}".format(opts.mirs)
       error(LOG,msg,error_codes.EX_IOERR)

except:
    msg="Syntax: python {} -d <datetime> -m <MIRS data hours> -l <output list file>".format(os.path.basename(__file__))
    error(LOG,msg,error_codes.EX_USAGE)

    
try: 

# Determine last synoptic time and MIRS data time range 

    initHourDT=initDT.replace(minute=0,second=0,microsecond=0) # Last Hour floor e.g. 5:05:23 -> 5:00:00
    lastSynDT=datetime_lib.last_synoptic_datetime(initHourDT) 
    synDelta=int((initHourDT-lastSynDT).total_seconds() // 3600)

    listFH = open(listFilename,'w')
    listFH.write("INIT_DATETIME={}\n".format(initHourDT.strftime(DTFormat))) 
    listFH.write("SYNP_DATETIME={}\n".format(lastSynDT.strftime(DTFormat)))
    listFH.write("MIRS_FRWD_HOURS={}\n".format(mirs[synDelta]['frwdHour']))
    listFH.write("MIRS_BKWD_HOURS={}\n".format(mirs[synDelta]['bkwdHour']))
    listFH.close()

except:
    msg="Error calculating/writing date/times to ".format(listFilename)
    error(LOG,msg,error_codes.EX_USAGE)
