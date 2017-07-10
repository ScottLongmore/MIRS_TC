#!/usr/bin/python
"""
npp_tc_cases.py - Runs npp_tc.py for a series of datetimes 

example:
npp_tc_cases.py -c <config.json> 
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
from subprocess import call

# Local modules
import error_codes
from setup_nde_logging import setup_logging
import dirRE

# Error Handing Routines 
def error(LOG,msg="Unexpected Error:",code=1):
    LOG.exception(msg)
    sys.exit(code)

# Variables
timeFormat="%Y%m%d%H%M%S%f"
runDelta=datetime.timedelta(seconds=300) # Run Delta look forward N seconds to capture currently modified files
config_keys=['script','datetime_start','datetime_end','timedelta_increment']

# Log
LOG = logging.getLogger(__file__) #create the logger for this file
logging_setup = "MIRS_TC" 
setup_logging(logging_setup)


# Read Command Line Arguments 
options = optparse.OptionParser()
options.add_option("-c", "--config", dest="config", help="Configuration File")


try:
    (opts, args) = options.parse_args()
    config_filename=opts.config

except:
    msg='Syntax: python npp_tc_cases.py -c <config.json>' 
    error(LOG,msg,error_codes.EX_USAGE)

# Read Configuration File
try:
    LOG.info("Processing JSON config file: {}".format(config_filename)) 
    config=json.load(open(config_filename),object_pairs_hook=collections.OrderedDict)
    if not all(atts in config for atts in config_keys):
       msg="Required attributes {} not all found in config file: {}".format(config_keys,config_filename)
       error(LOG,msg,error_codes.EX_IOERR)

    if not os.path.isfile(config['script']):
       msg="Script file does not exist: {}".format(config['script'])
       error(LOG,msg,error_codes.EX_IOERR)
    
    try:
        config['startDT']=datetime.datetime.strptime(config['datetime_start'],timeFormat)
        config['endDT']=datetime.datetime.strptime(config['datetime_end'],timeFormat)
        config['delta']=datetime.timedelta(seconds=int(config['timedelta_increment']))
    except:
       msg="datetime_start/end invalid format {}, or timedelta_increment (seconds) not valid".format(timeFormat)
       error(LOG,msg,error_codes.EX_IOERR)

    if config['startDT'] > config['endDT']:
       msg="datetime_start is greater than datetime_end"
       error(LOG,msg,error_codes.EX_IOERR)

except:
    msg="Error in JSON config file: {}".format(config_filename)
    error(LOG,msg,error_codes.EX_IOERR)

currDT=config['startDT']
while (currDT <= config['endDT']):

   command="python {} -c {} -l {} -t {}".format(config['script'],config['config'],config['listFile'],currDT.strftime(timeFormat)) 
   try: 
       os.system(command)
       LOG.info("{}, EXECUTED".format(command))
   except: 
       LOG.info("{}, FAILED".format(command))

   currDT=currDT+config['delta']
