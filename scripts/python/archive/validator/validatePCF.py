#!/usr/bin/python
"""
validatePCF.py - Validates PCF, creates new PCF with changes, if needed 

example:
python validatePCF.py -i <current PCF> -o <new PCF> 
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

# Local modules
import error_codes
from setup_nde_logging import setup_logging

# Error Handing Routines 
def error(LOG,msg="Unexpected Error:",code=1):
    LOG.exception(msg)
    sys.exit(code)

# Variables
timeFormat="%Y%m%d%H%M%S%f"
runDelta=datetime.timedelta(seconds=300) # Run Delta look forward N seconds to capture currently modified files
config_keys=['library','inputs']
input_keys=['dirs','re','file_key']
file_keys=collections.OrderedDict()

# Log
LOG = logging.getLogger(__file__) #create the logger for this file
logging_setup = 'NPP_TC'
setup_logging(logging_setup)


# Read Command Line Arguments 
options = optparse.OptionParser()
options.add_option("-c", "--config", dest="config", help="Configuration JSON File")
options.add_option("-i", "--input", dest="input", help="Input PCF File")
options.add_option("-o", "--output", dest="output", help="Output File List")


try:
    (opts, args) = options.parse_args()
    config_filename=opts.config
    input_filename=opts.input
    output_filename=opts.output

except:
    msg='Syntax: python validatePCF.py -c <config file> -i <input PCF file> -o <output PCF file>'
    error(LOG,msg,error_codes.EX_USAGE)

# Read Configuration File
try:
    LOG.info("Processing JSON config file: {}".format(config_filename)) 
    config=json.load(open(config_filename),object_pairs_hook=collections.OrderedDict)
    if not all(atts in config for atts in config_keys):
       msg="Required attributes {} not all found in config file: {}".format(config_keys,config_filename)
       error(LOG,msg,error_codes.EX_IOERR)
    for input in config['inputs']:
        if not all(atts in config['inputs'][input] for atts in input_keys):
            msg="Required input attributes {} for input: {} not all found in config file: {}".format(input_keys,input,config_filename)
            error(LOG,msg,error_codes.EX_IOERR)
        config['inputs'][input]['files']={}
        file_keys[config['inputs'][input]['file_key']]=input
    try:
        module=importlib.import_module(config['library'])
    except:
        msg="Error importing library {} in config file: {}".format(config['library'],config_filename)
        error(LOG,msg,error_codes.EX_IOERR)

except:
    msg="Error in JSON config file: {}".format(config_filename)
    error(LOG,msg,error_codes.EX_IOERR)

# Process original PCF
try:
    pcf=open(input_filename,"r")
except:
    msg="Unable to open input PCF file: {}".format(input_filename)
    error(LOG,msg,error_codes.EX_IOERR)


config['params']=collections.OrderedDict()
try:
    for line in pcf:
       if re.match("\s*\S+=\S+",line):
           paramKey,value=line.split('=',1) 
           paramKey=paramKey.lstrip()
           value=value.rstrip()

           if paramKey in file_keys:
    
               inputKey=file_keys[paramKey]
               input=config['inputs'][inputKey]
    
               path,file = os.path.split(value)
               if os.path.isfile(value) and re.match(input['re'], file):
                   input['files'][file]={}
                   input['files'][file]['name']=value
               else:
                   msg="{} input file: {} does not exist/invalid filename format".format(paramKey,value)
                   error(LOG,msg,error_codes.EX_IOERR)
           else:
               config['params'][paramKey]=value
         
except:
    msg="Problem reading {} input: {}".format(inputKey,paramKey)
    error(LOG,msg,error_codes.EX_IOERR)


for inputKey in config['inputs']:

    input=config['inputs'][inputKey]

    if 'validate' in input:
       method=input['validate']
       args=[input]
       try: 
           getattr(module,method)(*args)
       except:
           msg="Problem in {} input {} method".format(inputKey,method)
           error(LOG,msg,error_codes.EX_IOERR)

# Create Input Data/File List 
try:
    LOG.info("Creating data list file: {}".format(output_filename)) 
    out = open(output_filename, "w")

    for paramKey in config['params']:
        out.write("{}={}\n".format(paramKey,config['params'][paramKey]))
    
    for inputKey in config['inputs']:
        input=config['inputs'][inputKey]
        for file in input['files']:
            out.write("{}={}\n".format(input['file_key'],file)) 
    out.close()

except:
    msg="Unable to write PCF {}".format(output_filename)
    error(LOG,msg,error_codes.EX_IOERR)

