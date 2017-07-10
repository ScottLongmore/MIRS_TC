#!/usr/bin/python
"""
mirs_tc.py - Master wrapper script for NPP_TC/AMSU_TC.sh
             Will become the master script in v2.0 

example:
mirs_tc.py -c <config.json> -l <input list file> -t <time:YYYYMMDDHHMMSS>
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
timeFormat="%Y%m%d%H%M%S"
runDelta=datetime.timedelta(seconds=300) # Run Delta look forward N seconds to capture currently modified files
config_keys=['library','script','work_root','input_list_file','meta','inputs']
meta_keys=['ROOTDIR','algorithmLogMessageContext','algorithmLogMessageError','algorithmLogMessageWarn','WGRIB','PYTHON']
input_keys=['dirs','re','delta','output_key']

# Log
LOG = logging.getLogger(__file__) #create the logger for this file
logging_setup = "MIRS_TC" 
setup_logging(logging_setup)


# Read Command Line Arguments 
options = optparse.OptionParser()
options.add_option("-c", "--config", dest="config", help="Configuration File")
options.add_option("-t", "--time", dest="time", help="Time <YYYYMMDDHHMMSS>")
options.add_option("-l", "--list", dest="list", help="Input List File")


try:
    (opts, args) = options.parse_args()
    config_filename=opts.config
    list_filename=opts.list
    time=opts.time

except:
    msg='Syntax: python mirs_tc.py -c <config.json> -l <input list file> -t <time:YYYYMMDDHHMMSS>'
    error(LOG,msg,error_codes.EX_USAGE)

if time: 
    try:
        currDTG=datetime.datetime.strptime(time,timeFormat)
        endDTG=currDTG
    except:
        msg="Time: {} invalid format YYYYMMDDHHMMSS".format('time')
        error(LOG,msg,error_codes.EX_IOERR)
else:
        currDTG=datetime.datetime.now()
        endDTG=currDTG+runDelta

# Read Configuration File
try:
    LOG.info("Processing JSON config file: {}".format(config_filename)) 
    config=json.load(open(config_filename),object_pairs_hook=collections.OrderedDict)
    if not all(atts in config for atts in config_keys):
       msg="Required attributes {} not all found in config file: {}".format(config_keys,config_filename)
       error(LOG,msg,error_codes.EX_IOERR)
    if not all(atts in config['meta'] for atts in meta_keys):
       msg="Required meta attributes {} not all found in config file: {}".format(meta_keys,config_filename)
       error(LOG,msg,error_codes.EX_IOERR)
    for input in config['inputs']:
        if not all(atts in config['inputs'][input] for atts in input_keys):
            msg="Required input attributes {} for input: {} not all found in config file: {}".format(input_keys,input,config_filename)
            error(LOG,msg,error_codes.EX_IOERR)
    try:
        module=importlib.import_module(config['library'])
    except:
        msg="Error importing library {} in config file: {}".format(config['library'],config_filename)
        error(LOG,msg,error_codes.EX_IOERR)

except:
    msg="Error in JSON config file: {}".format(config_filename)
    error(LOG,msg,error_codes.EX_IOERR)


# Retrieve input files
for inputKey in config['inputs']:

    input=config['inputs'][inputKey]
    inputFiles={}

    try:
        delta=datetime.timedelta(seconds=input['delta'])
        startDTG=endDTG-delta
    except:
        msg="Problem with {} delta: {} in config {}".format(inputKey,input['delta'],config_filename)
        error(LOG,msg,error_codes.EX_IOERR)

    for entry in input['dirs']: 

        entry=str(entry)
        dirs=collections.OrderedDict();
        if os.path.isfile(entry):
           dirFH = open(entry,"r")
           for line in dirFH:
               dir=line.rstrip('\n')
               dirs[dir]=dir
           dirFH.close()
        elif os.path.isdir(entry):
           dirs[entry]=entry
        elif re.search("\^.+\$",entry):
           entryRE=dirRE.dirRE(entry)
           for dir in (entryRE.getDirs()):
               dirs[dir]=dir
        else: 
           msg="{} dir entry {} neither dir, dir file, or dir RE".format(inputKey,entry)
           error(LOG,msg,error_codes.EX_IOERR)

	files={}
        for dir in dirs: 

            try:
                for file in os.listdir(dir):
                    filename=os.path.join(dir,file)
                    if re.match(input['re'], file):
                        fileDTG=datetime.datetime.fromtimestamp(os.path.getmtime(filename))
                        if fileDTG >= startDTG: 
                           fileKey=str(file)
                           files[fileKey]={}
                           files[fileKey]['name']=filename
                           files[fileKey]['modTime']=fileDTG
            except:
                msg="Problem reading {} input directory: {}".format(inputKey,dir)
                error(LOG,msg,error_codes.EX_IOERR)


        if 'method' in input:
            method=input['method']
            args=[input,endDTG,files]
            try: 
                files=getattr(module,method)(*args)
                inputFiles.update(files)
            except:
                msg="Problem in {} input {} method".format(inputKey,method)
                error(LOG,msg,error_codes.EX_IOERR)

    input['files']=collections.OrderedDict(sorted(inputFiles.iteritems(), key=lambda x: x[1]['time']))

# Create working directory root
try:
    workDir=os.path.join(config['work_root'],currDTG.strftime("%Y%m%d%H%M"))
    LOG.info("Creating working directory: {}".format(workDir)) 
    if not os.path.isdir(workDir):
       os.makedirs(workDir)
except:
    msg="Unable to create working directory: {}".format(workDir)
    error(LOG,msg,error_codes.EX_IOERR)

# Link data files to working directory and create Input Data/File List
listFile=os.path.join(workDir,config['input_list_file'])
print listFile
try:
    LOG.info("Creating data list file: {}".format(listFile)) 
    out = open(listFile, "w")

    out.write("{}={}\n".format("working_directory",workDir)) 
    startDTGstr=startDTG.strftime(timeFormat)
    out.write("{}={}0\n".format("job_coverage_start",startDTGstr)) 
    endDTGstr=endDTG.strftime(timeFormat)
    out.write("{}={}0\n".format("job_coverage_end",endDTGstr)) 
    for meta_key in meta_keys:
        out.write("{}={}\n".format(meta_key,config['meta'][meta_key])) 
    
    for inputKey in config['inputs']:
        input=config['inputs'][inputKey]
        for file in input['files']:
            out.write("{}={}\n".format(input['output_key'],file)) 
          
            workFile=os.path.join(workDir,file) 
            LOG.info("Linking {} to {}".format(input['files'][file]['name'],workFile))
            os.symlink(input['files'][file]['name'],workFile)
    
    out.close()
except:
    msg="Unable to write PCF {}".format(listFile)
    error(LOG,msg,error_codes.EX_IOERR)

# Run the script
try:
    LOG.info("Changing directory to {}".format(workDir))
    currDir=os.getcwd()
    os.chdir(workDir)
    command=[config['script'], workDir, currDTG.strftime("%Y%m%d%H%M%S0") ]
    LOG.info("Running: {}".format(' '.join(command)))
    status = call(command)
    LOG.info("Finished: {}".format(' '.join(command)))
    os.chdir(currDir)
except:
    msg="Unable to call subprocess {} ".format(' '.join(command))
    error(LOG,msg,error_codes.EX_IOERR)
