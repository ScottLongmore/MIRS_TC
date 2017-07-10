#!/usr/bin/python
"""
mirs_tc.py - Master control driver for MIRS TC algorithms 

example:
mirs_tc.py -c <config.json> -l <input list file> -t <time:YYYYMMDDHHMMSS>
"""
    
# System modules
import sys
import os
import re
import datetime
import argparse 
import operator
import collections
import json
import importlib
import logging
import traceback
from subprocess import call
import ConfigParser
import pprint

# Set local module paths
try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"lib"))
    sys.path.insert(2,os.path.join(parentPath,"pool"))
    sys.path.insert(3,os.path.join(parentPath,"pool","plugins"))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local modules
import setup_logging
import error_codes
import utils
import MIRS_TC 
import pool.pool as pool_api

# Setup Logging using logging templates
LOG = logging.getLogger("MIRS_TC") #create the logger for this file
setup_logging.setup_logging("MIRS_TC","MIRS_TC.LOG")
LOG.info("Starting: {}".format(__file__)) 

# Variables
timeFormat="%Y%m%d%H%M%S"
runDelta=datetime.timedelta(seconds=300) # Run Delta look forward N seconds to capture currently modified files
pp=pprint.PrettyPrinter(indent=4)

# Read command line arguments 
options = argparse.ArgumentParser()
options.add_argument("-c", "--config", dest="config", help="MIRS-TC Configuration File")
options.add_argument("-i", "--input", dest="input", help="Input Specification File")
try:
    args = options.parse_args()
except:
    msg='Syntax: python mirs_tc.py -c <config.json> -i <input spec file>'
    utils.error(LOG,msg,error_codes.EX_USAGE)

# Read master MIRS-TC JSON configuration file
try:
    LOG.info("Processing JSON config file: {}".format(args.config)) 
    config=json.load(open(args.config),object_pairs_hook=collections.OrderedDict)
    #
    # TODO: schema validation here
    #
except:
    msg="Error in JSON config file: {}".format(args.config)
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Read input specification file
inputs={}
try:
    LOG.info("Reading input specifation file: {} using module: {}".format(args.input,config['IOHandling']['inputModule'])) 
    inputModule=importlib.import_module(config['IOHandling']['inputModule'])
    args=[args.input]
    inputs=getattr(inputModule,config['IOHandling']['inputMethod'])(*args)
except:
    msg="Problem retrieving input specifations using {}:{}".format(config['IOHandling']['inputModule'],config['IOHandling']['inputMethod']) 
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Validate input parameters
LOG.info("Validating input specifation properties")
msg=utils.schemaValidate(inputs,config['inputs']['schema'])
if msg:
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Determine date/times information 
LOG.info("Setting date/time variables")
config['datetimes']=MIRS_TC.datetimes(inputs["job_coverage_end"])

# Create/update working sub-directories
workDir=inputs['working_directory']
for workSubDirName in config['workSubDirs']:
    workSubDir=os.path.join(workDir,config['workSubDirs'][workSubDirName])
    config['workSubDirs'][workSubDirName]=workSubDir # Replace absolute path to config workSubDir
    try:
        LOG.info("Creating subdirectory: {}".format(workSubDir))
        os.makedirs(workSubDir)
    except:
        msg="Unable to create sub-directory: {}".format('workSubDir')
        utils.error(LOG,msg,error_codes.EX_IOERR)

# Directory aliases
dataDir=config['workSubDirs']['dataDir']
modelDir=config['workSubDirs']['modelDir']
logDir=config['workSubDirs']['logDir']
outputDir=config['workSubDirs']['outputDir']

# Update/validate config parameters with inputs parameters
for progname in config['programs']:
   program=config['programs'][progname]
   if 'exe' in program:
       program['exe']=os.path.join(inputs['SYS_DIR'],program['exe'])
   if 'configs' in program:
       for configname in program['configs']:
           program['configs'][configname]=os.path.join(inputs['SYS_DIR'],program['configs'][configname])

# Validate data files, move data files, initilize meta-data object arrays, and filter data files
metadata={} # dict of dataset object lists
for dataname in config['inputs']['datasets']: 

    dataset=config['inputs']['datasets'][dataname]
    dataModule=importlib.import_module(dataset['module'])
    metadata[dataname]=[]
    try:
        datafiles=inputs[dataset['inputKey']]
        if not isinstance(datafiles,list):
            datafiles=[datafiles]
        for datafile in datafiles:
            if os.path.isfile(datafile) and re.match(dataModule.regexp,datafile):
                utils.moveFile(workDir,datafile,dataDir) # Move data files to dataDir
                metadata[dataname].append(getattr(dataModule,dataset['dataMethod'])(*[datafile]))
            else:
                msg="{} input file: {} does not exist/invalid filename format".format(dataset,datafile)
                utils.error(LOG,msg,error_codes.EX_IOERR)


    except:
        msg="Error validating dataset: {}".format(dataset)
        utils.error(LOG,msg,error_codes.EX_IOERR)
    LOG.info("Initalized [{}] meta-data objects for dataset {}".format(len(metadata[dataname]),dataname))

    # Filter data files 
    if "filterMethod" in config['inputs']['datasets'][dataname]:
        metadata[dataname]=getattr(dataModule,dataset['filterMethod'])(*[metadata[dataname]]) # Return a dataset object or list of dataset objects
        LOG.info("Filtered meta-data objects for dataset {} is now [{}]".format(dataname,len(metadata[dataname])))

# Running wgrib2 and bin2pack - convert GFS grib file to pack files
#
#   Inputs: <grib file> 
#   Outputs: <pack file> 
#
os.chdir(modelDir) # change to model directory

gfsYear=metadata['gfs'][0].getProperty('runDTG').strftime("%y")
gfsMonth=metadata['gfs'][0].getProperty('runDTG').strftime("%m")
gfsDay=metadata['gfs'][0].getProperty('runDTG').strftime("%d")
gfsHour=metadata['gfs'][0].getProperty('runDTG').strftime("%H")
gfsDate=gfsYear+gfsMonth+gfsDay
gfsFhour="{0:03d}".format(metadata['gfs'][0].getProperty('fhour'))
gfsGribFile=os.path.join(dataDir,metadata['gfs'][0].getProperty('filename'))

if not MIRS_TC.grib2pack(gfsDate,gfsHour,gfsFhour,gfsGribFile,inputs['WGRIB'],config['programs']['bin2pack']['exe'],logDir):
    msg="Problem converting to pack file from grib file: {}".format(gfsGribFile)
    utils.error(LOG,msg,error_codes.EX_IOERR)

pcode,pday=utils.ddhh2pack(int(gfsDay),int(gfsHour))
gfsPackFile="G{}{}_{}{}{:02d}_PACK.DAT".format(gfsFhour,gfsYear,pcode,gfsMonth,pday)
if os.stat(gfsPackFile).st_size == 0: # Verify GFS pack file
    msg="GFS pack file {} either doesn't exist or is zero size".format(gfsPackFile)
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Running pool.update - create pool database for MIR-TC data files 
#
#   Inputs: <MIRS_TC_POOL.ini> 
#   Outputs: <databaseDir>/meta_info.json 
#
os.chdir(workDir) # change to working directory
#os.symlink(config['programs']['pool']['configs']['pluginDir'],os.path.join(workDir,"plugins"))

poolConfig=pool_api.get_config_cascade([config['programs']['pool']['configs']['poolConfig']]) 
poolConfig.set("database","location",os.path.join(workDir,'database'))
poolConfig.set("file_gather","gather_dir",dataDir) 

#for section in poolConfig._sections:
#    print("Section: {}".format(section))
#    for key,value in dict(poolConfig.items(section)).iteritems():
#        print("\t{} : {}".format(key,value))

pool_api.create(poolConfig)
pool_api.update(poolConfig)

# Start main tropical cyclone (adeck) processing loop
LOG.info("Starting main tropical cyclone processing loop")
for adeck in metadata['adeck']:

    stormId=adeck.getProperty('stormId')
    LOG.info("Processing TC: {}".format(stormId))

    # Create and CD to storm directory
    stormDir=os.path.join(workDir,stormId)
    utils.workDir(stormDir) 

    # Run ShortTermTrack -  outputs track and forecast track for TC by hour 
    #
    #   Inputs: adeck [a<stormId>.dat] file 
    #   Outputs: ShortTermTrack [<stormId].inp] file
    #
    adeckFile=adeck.getProperty('filename')
    adeckSTTfile="a{}.dat".format(stormId)
    utils.linkFile(dataDir,adeckFile,stormDir,adeckSTTfile) # Link adeck file
    commandList=[config['programs']['shortTermTrack']['exe']]
    commandArgs=[]
    commandArgs.extend([adeckSTTfile,config['datetimes']['synpDTG'].strftime("%Y%m%d%H"),'CURR'])
    commandID="shortTermTrack"
    stdoutFile=os.path.join(logDir,"shortTermTrack_{}.log".format(stormId))
    stderrFile=os.path.join(logDir,"shortTermTrack_{}.err".format(stormId))
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    status=utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)

    afdeckInputFile="{}.inp".format(stormId)
    if os.path.isfile(afdeckInputFile)==False and status==False:
        LOG.info("ShortTermTrack failed with current synoptic time, trying with 12 hours previous")
        commandArgs=[]
        commandArgs.extend([adeckSTTfile,config['datetimes']['synpDTG'].strftime("%Y%m%d%H"),'M12H'])
        status=utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)
        if os.path.isfile(afdeckInputFile)==False and status==False:
            LOG.warning("ShortTermTrack failed with 12 hours previous, skipping to next storm")
            continue

    # Run afdeck - extracts TC coordinates  
    #
    #   Inputs: ShortTermTrack [<stormId>.inp] file 
    #   Outputs: COORDINATES file 
    #
    commandList=[config['programs']['afdeck']['exe']]
    commandArgs=[]
    commandArgs.extend([afdeckInputFile])
    commandID="afdeck"
    stdoutFile=os.path.join(logDir,"afdeck_{}.log".format(stormId))
    stderrFile=os.path.join(logDir,"afdeck_{}.err".format(stormId))
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    status=utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)

    coordinateFile="COORDINATES" 
    if not status or not os.path.exists(coordinateFile):
        LOG.warning("afdeck failed, skipping to next storm")
        continue 

    # Retreive Short Term Track 00Z analsys values
    analysis=MIRS_TC.getInputAnalysisVars(afdeckInputFile)
    for var in analysis:
        print("{} : {}".format(var,analysis[var]))

