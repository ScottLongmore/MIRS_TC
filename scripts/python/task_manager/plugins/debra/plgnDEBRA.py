#!/usr/bin/python
"""
  plgnDEBRA.py - plug-in methods: TASKS, WORK, PURGE
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
import zipfile
import shutil

# Local modules
import error_codes
import fileAction
import dirRE

LOG = logging.getLogger(__name__)  # create the logger for this file


# Parameters
DTSFormat = "%Y_%j_%H%M"
ISODTSFormat = "%Y%m%dT%H%M%S"

channelIDs=['IR_016','IR_039','IR_087','IR_097','IR_108','IR_120','IR_134','VIS006','VIS008','WV_062','WV_073']
binSuffixes=['BIN','LAT','LON']


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
    inputs = config['inputs']['MSG']

    tasksLogFile = os.path.join(config['logDir'],"{}_TASKS_{}.log".format(config['name'], meta['runDTG'].strftime(ISODTSFormat)))
    tasksLogFH = open(tasksLogFile, "a")

    # Find task files
    FA = fileAction.fileAction(config)
    inputFiles = FA.findInputFiles(['MSG'])
    records = {}
    filepaths = inputFiles['MSG']
    for filepath in filepaths:

        m = re.match(inputs['re'], os.path.basename(filepath))
        fields = m.groupdict()
        DTS = fields['DTS']
        DTG = datetime.datetime.strptime(DTS, DTSFormat)
        ISODTS = DTG.strftime(ISODTSFormat)
        id = filepath 

        if id not in records:
            records[id] = collections.OrderedDict()

        records[id]['DTS'] = ISODTS 
        records[id]['file'] = filepath

        tasksLogFH.write("DTS {} FILE: {}\n".format(ISODTS,filepath))

    ids = records.keys()
    ids.sort()
    ids.reverse()

    tasks = []
    for id in ids:
        tasks.append(records[id])

    tasksLogFH.write("Number of Tasks: {}".format(len(tasks)))

    # Remove any older tasks than backward search datetime
    tasks = PURGE(config, tasks)

    tasksLogFH.write("Number of Tasks: {}".format(len(tasks)))


    tasksLogFH.close()
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
    meta = config['meta']
    inputs = config['inputs']['MSG']
    plugin=config['plugin']

    workLogFile = os.path.join(config['logDir'],"{}_WORK_{}.log".format(config['name'], meta['runDTG'].strftime(ISODTSFormat)))
    workLogFH = open(workLogFile, "a")

    taskDTG=datetime.datetime.strptime(task['DTS'],ISODTSFormat)

    # Create work directory 
    workDir=os.path.join(str(config['workDirRoot']),task['DTS'])
    tdfDir=str(config['workDirRoot'])
    if not os.path.exists(workDir):
       LOG.info('Creating working directory: {}'.format(workDir))
       try:
           os.makedirs(workDir)
       except:
           LOG.warning("Unable to create work dir {}".format(workDir))
           status=False
           return(status)

    # CD to work directory
    try:
        LOG.info('Changing to working directory: {}'.format(workDir))
        os.chdir(workDir)
    except:
        LOG.warning("Unable to change to work dir: {}".format(workDir))
        status=False
        return(status)

    # Open MSG zipfile 
    workLogFH.write("DTS {} FILE: {}\n".format(task['DTS'],task['file']))
    try:
        msgFH = open(task['file'], 'rb')
        ZFH = zipfile.ZipFile(msgFH)
    except:
        LOG.warning("Unable to open MSG zipfile: {}".format(task['file']))
        status=False
        return(status)

    # Extract and verify all MSG channel files and convert XPIFs to binary files
    for channelID in channelIDs:
        try:
            channelString="{}_MSG03_{}".format(taskDTG.strftime(DTSFormat),channelID)
            channelFile="{}.XPIF".format(channelString)
            ZFH.extract(channelFile, workDir)
        except:
            LOG.warning("Unable to extract MSG channel file: {}".format(channelFile))
            status=False
            return(status)

        commandList=[plugin['XPIFToBinExe']]
        commandArgs=[channelFile]
        commandID=channelString
        if not execute(commandList,commandArgs,commandID,workDir):
            LOG.warning("Execute failed for {}".format(plugin['XPIFToBinExe']))
            status=False
            return(status)

        for sfx in binSuffixes:
            binFile=os.path.join(workDir,"{}.{}".format(channelString,sfx))
            if not os.path.isfile(binFile):
                LOG.warning("{} output binary file {} not found".format(plugin['XPIFToBinExe'],binFile))
                status=False
                return(status)

    # Convert binary files to TDF file
    commandList=[plugin['BinToTDFScp']]
    commandArgs=[workDir,taskDTG.strftime("%Y"),taskDTG.strftime("%j"),taskDTG.strftime("%H%M"),workDir,workDir]
    commandID=taskDTG.strftime(DTSFormat)
    if not execute(commandList,commandArgs,commandID,workDir):
        LOG.warning("Execute failed for {}".format(plugin['BinToTDFScp']))
        status=False
        return(status)

    tdfFile="{}.SEVIRI.m10.tdf".format(taskDTG.strftime("%Y%m%d.%H%M"))
    tdfFilePath=os.path.join(workDir,tdfFile)
    if not os.path.isfile(tdfFilePath):
        LOG.warning("{} terrascan file {} not found".format(plugin['BinToTDFScp'],tdfFilePath))
        status=False
        return(status)

    # Run DEBRA processing script 
    commandList=[plugin['DEBRAScp']]
    commandArgs=[tdfFile]
    commandID=taskDTG.strftime(DTSFormat)
    if not execute(commandList,commandArgs,commandID,workDir):
        LOG.warning("Execute failed for {}".format(plugin['DEBRAScp']))
        status=False
        return(status)

    # Save TDF file
    try:
        LOG.info('Moving TDF file {} to {}'.format(tdfFilePath,tdfDir))
        shutil.move(tdfFilePath,tdfDir)
    except:
        LOG.warning("Unable to move TDF file {} to {}".format(tdfFilePath,tdfDir))
        status=False
        return(status)

    # Remove working directory
    fileSaveRE="^.+.(stderr|stdout)$"
    try:
        LOG.info('Removing working files') 
        for file in os.listdir(workDir):
            if not re.match(fileSaveRE,file):
                os.remove(file)
    except:
        LOG.warning("Problem removing work files")
        status=False
        return(status)

    msgFH.close()

    workLogFH.close()

    status = True 
    return(status)

def execute(commandList,commandArgs,commandID,workDir):

    command=commandList[-1]

    commandLine=[]
    commandLine.extend(commandList)
    commandLine.extend(commandArgs)

    status=True
    try:
        base, ext =os.path.splitext(os.path.basename(command))
        outFile=os.path.join(workDir,"{}_{}.stdout".format(base,commandID))
        errFile=os.path.join(workDir,"{}_{}.stderr".format(base,commandID))
        outfh=open(outFile,'w')
        errfh=open(errFile,'w')
        LOG.info("STDOUT File: {}".format(outFile))
        LOG.info("STDERR File: {}".format(errFile))
        LOG.info("Subprocess Executing: {}".format(" ".join(commandLine)))
        retcode = subprocess.call(commandLine, stdout=outfh, stderr=errfh)
        outfh.close()
        errfh.close()
        if(retcode < 0):
            LOG.warning("Subprocess: {} terminated by signal {}".format(command,retcode))
            status=False
            return(status)
        elif(retcode > 0):
            LOG.warning("Subprocess: {} returned with exit code {}".format(command,retcode))
            status=False
            return(status)
        else:
            LOG.info("Subprocess: {} completed normally".format(command))
    except OSError as e:
        LOG.warning("Subprocess: {} failed".format(command))
        status=False
        return(status)

    return(status)

