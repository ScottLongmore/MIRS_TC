#!/usr/bin/python
"""
  plgnMIRS_TC.py - plug-in methods: TASKS, WORK, PURGE
"""

# Stock modules
import sys
import os
import re
import copy
import logging
import traceback
import datetime
import collections
import operator
import subprocess
import shutil
import pprint

# Set local module paths
try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"..","lib"))
    mirstcPath=os.path.join(parentPath,"..","mirs_tc")
    sys.path.insert(2,mirstcPath)
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local modules
import utils
import error_codes
import fileAction
import dirRE
import MIRS_TC

LOG = logging.getLogger(__name__)  # create the logger for this file

# Parameters
ISODTSFormat = "%Y%m%dT%H%M%S"
NDEFormat="%Y%m%d%H%M%S0"

def PURGE(config, tasks):
    """
    Removes tasks with DTS older than backward search datetime

    Parameters
    ----------
    config : dict
    tasks : dict

    Returns
    currentTasks : dict
        dictionary of current tasks to run
    """
    currentTasks = []
    for task in tasks:
        taskDTG = datetime.datetime.strptime(task['DTS'], ISODTSFormat)
        if taskDTG > config['meta']['bkwdDTG']:
            currentTasks.append(task)

    return(currentTasks)


def TASKS(config):
    """
    Generates list of tasks to run

    Parameters
    ----------
    config : dict
    """
    meta = config['meta']
    adeck = config['inputs']['adeck']
    gfs = config['inputs']['gfs']
    img = config['inputs']['mirs_atms_img']
    snd = config['inputs']['mirs_atms_snd']

    LOG.info("Creating MIRS_TC tasks")

    endDTG=meta['runDTG'].replace(minute=0,second=0) # 
    startDTG=endDTG-datetime.timedelta(seconds=meta['bkwdDelta'])
    tasks = []

    # Determine if run has already been completed
    if 'runs' in config:
       if endDTG.strftime(ISODTSFormat) in config['runs']:
          LOG.info("Run already executed: {}, skipping".format(endDTG.strftime(ISODTSFormat)))
          return(tasks)
    else:
       config['runs']=[]
    
    # Retrieve files (adeck,gfs,mirs)
    FA = fileAction.fileAction(config)

    # Get adeck files 
    filenames = FA.findInputFiles(['adeck'])['adeck']
    adeckFiles=[]
    for filename in filenames:
        filetime = datetime.datetime.fromtimestamp(os.path.getmtime(filename))
        if filetime > config['meta']['bkwdDTG']: 
            adeckFiles.append(filename)

    # Get GFS file 
    filenames = FA.findInputFiles(['gfs'])['gfs']
    latestDTG=config['meta']['bkwdDTG']
    gfsFile=None
    for filename in filenames:
        m=re.match(gfs['re'],os.path.basename(filename))
        fields=m.groupdict()
        gfsDTG=datetime.datetime.strptime(fields['runDTG'],"%Y%m%d%H")
        if gfsDTG > latestDTG:
            latestDTG=gfsDTG
            gfsFile=filename

    # Get MIRS ATMS IMG files
    filenames = FA.findInputFiles(['mirs_atms_img'])['mirs_atms_img']
    imgFiles=[]
    for filename in filenames:
        filetime = datetime.datetime.fromtimestamp(os.path.getmtime(filename))
        if filetime > config['meta']['bkwdDTG']: 
            imgFiles.append(filename)

    # Get MIRS ATMS SND files
    filenames = FA.findInputFiles(['mirs_atms_snd'])['mirs_atms_snd']
    sndFiles=[]
    for filename in filenames:
        filetime = datetime.datetime.fromtimestamp(os.path.getmtime(filename))
        if filetime > config['meta']['bkwdDTG']: 
            sndFiles.append(filename)

    if adeckFiles and gfsFile and imgFiles and sndFiles:
        records={
           "DTS":endDTG.strftime(ISODTSFormat),
           "job_coverage_start":startDTG.strftime(NDEFormat),
           "job_coverage_end":endDTG.strftime(NDEFormat),
           "adeck":adeckFiles,
           "gfs":gfsFile,
           "mirs_atms_img":imgFiles,
           "mirs_atms_snd":sndFiles
        }
        tasks.append(records)

    LOG.info("Number of Tasks: {}".format(len(tasks)))

    return(tasks)


def WORK(config, task):
    """
    The work to be done for each task

    Parameters
    ----------
    config : dict
    task : dict

    Returns
    -------
    status : Boolean
    """
    status=True
    pp=pprint.PrettyPrinter(indent=4)

    # config alaises
    meta = config['meta']
    adeck = config['inputs']['adeck']
    gfs = config['inputs']['gfs']
    img = config['inputs']['mirs_atms_img']
    snd = config['inputs']['mirs_atms_snd']
    plugin=config['plugin']
    nde=plugin['NDE']
    mirs_tc=plugin['MIRS_TC']
   
    LOG.info("Preparing MIRS_TC task: {} enviornment".format(task['DTS']))

    taskDTG=datetime.datetime.strptime(task['DTS'],ISODTSFormat)

    # Create work directory 
    workDir=os.path.join(str(config['workDirRoot']),task['DTS'])
    if not utils.workDir(workDir):
       LOG.warning("Unable to create working directory:  {}, exiting".format(workDir))
       status=False
       return(status)
 
    # Change to working directory
    if not utils.cdDir(workDir):
       LOG.warning("Unable to change to working directory: {}, exiting".format(workDir))
       status=False
       return(status)

    # Create NDE Process Control File (PCF):
    pcfFile = os.path.join(workDir,nde['PCF'])
    LOG.info("Creating NDE process control file: {}".format(pcfFile))
    try:
        pcfFH = open(pcfFile, "w")
        nde['PCFattrs']['working_directory']=workDir
        nde['PCFattrs']['job_coverage_start']=task['job_coverage_start']
        nde['PCFattrs']['job_coverage_end']=task['job_coverage_end']
        for attr in nde['PCFattrs']:
            pcfFH.write("{}={}\n".format(attr,nde['PCFattrs'][attr]))
    except:
       LOG.warning("Unable to create PCF: {}".format(pcfFile))
       status=False
       return(status)

    # Link adeck files to working directory, add entries to PCF
    LOG.info("Linking adeck files")
    for adeckFilename in task['adeck']:
        try:
            adeckPath,adeckFile=os.path.split(adeckFilename)
            adeckDTG = datetime.datetime.fromtimestamp(os.path.getmtime(adeckFilename))
    
            # NDE adeck naming convention (source and timestamp)
            m=re.match(adeck['re'],adeckFile)
            fields=m.groupdict()
            basinId=fields['basinId']
            source="nhc" if basinId in ['al','ep'] else "jtwc"
            adeckNDEFile="{}_{}.{}".format(source,adeckFile,adeckDTG.strftime("%Y%m%d%H%M"))
    
            utils.linkFile(adeckPath,adeckFile,workDir,adeckNDEFile)
            pcfFH.write("{}={}\n".format(adeck['PCFkey'],adeckNDEFile))
        except:
            LOG.warning("Problem linking adeck file: {)".format(adeckFilename))
            status=False
            return(status)

    # Link gfs file 
    LOG.info("Linking gfs files")
    try:
        gfsPath,gfsFile=os.path.split(task['gfs'])
        m=re.match(gfs['re'],gfsFile)
        fields=m.groupdict()
        gfsDTG=datetime.datetime.strptime(fields['runDTG'],"%Y%m%d%H")
        gfsNDEFile="gfs.t{}z.pgrb2.1p00.f{}.{}".format(gfsDTG.strftime("%H"),fields['fhour'],gfsDTG.strftime("%Y%m%d"))
    
        utils.linkFile(gfsPath,gfsFile,workDir,gfsNDEFile)
        pcfFH.write("{}={}\n".format(gfs['PCFkey'],gfsNDEFile))
    except:
        LOG.warning("Problem linking gfs file: {)".format(task['gfs']))
        status=False
        return(status)

    # Link mirs img/snd files
    LOG.info("Linking mirs files")
    for imgFilename in task['mirs_atms_img']:
        try:
            imgPath,imgFile=os.path.split(imgFilename)
            utils.linkFile(imgPath,imgFile,workDir,imgFile)
            pcfFH.write("{}={}\n".format(img['PCFkey'],imgFile))
        except:
            LOG.warning("Problem linking mirs image file: {)".format(imgFilename))
            status=False
            return(status)
 
    for sndFilename in task['mirs_atms_snd']:
        try:
            sndPath,sndFile=os.path.split(sndFilename)
            utils.linkFile(sndPath,sndFile,workDir,sndFile)
            pcfFH.write("{}={}\n".format(snd['PCFkey'],sndFile))
        except:
            LOG.warning("Problem linking mirs sounding file: {)".format(sndFilename))
            status=False
            return(status)
 
    # Close PCF
    pcfFH.close()

    # Create sub log dir
    subLogDir=os.path.join(config['logDir'],taskDTG.strftime(ISODTSFormat))
    utils.workDir(subLogDir)

    # Link file/dirs 
    os.symlink(os.path.join(workDir,nde["LOG"]),os.path.join(subLogDir,nde["LOG"]))
    os.symlink(os.path.join(workDir,nde["PSF"]),os.path.join(subLogDir,nde["PSF"]))
    os.symlink(os.path.join(workDir,nde["logDir"]),os.path.join(subLogDir,nde["logDir"]))
    os.symlink(os.path.join(workDir,nde["outputDir"]),os.path.join(subLogDir,nde["outputDir"]))

    # Execute MIRS_TC algorithm driver
    LOG.info("Running mirs_tc.py")
    commandList=[nde['PCFattrs']['PYTHON'],mirs_tc['exe']]
    commandArgs=['-c',mirs_tc['configs']['mirs_tc'],'-i',pcfFile]
    commandId="mirs_tc"
    stdoutFile=os.path.join(subLogDir,"{}.stdout".format(commandId))
    stderrFile=os.path.join(subLogDir,"{}.stderr".format(commandId))
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    status=utils.execute(commandList,commandArgs,commandId,config['logDir'],stdoutFile=stdoutFile,stderrFile=stderrFile)
    status=True

    # Check return status
    if not status:
        LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
        LOG.warning("See sub-process log file: {}".format(stdoutFile))
        LOG.warning("See sub-process error file: {}".format(stderrFile))
        status=False
        return(status)

    # Add completed run
    config['runs'].append(taskDTG.strftime(ISODTSFormat))

    return(status)

