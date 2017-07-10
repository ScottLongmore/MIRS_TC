# Stock modules
"""utils module."""

import sys
import os
import re
import psutil
import datetime
from dateutil import rrule 
from dateutil import parser
import operator
import collections
import json
import jsonschema
import logging
import types
import struct
import resource
import subprocess

# Local modules
import error_codes
import dirRE

LOG = logging.getLogger(__name__)

DTGFRMTS = collections.OrderedDict(
           {
           'Y':{
               'format':'%Y',
               'rule':rrule.YEARLY,
               're':'\d{4}',
               'printf':'%04d'
               },
           'y':{
               'format':'%y',
               'rule':rrule.YEARLY,
               're':'\d{2}',
               'printf':'%02d'
               },
           'm':{
               'format':'%m',
               'rule':rrule.MONTHLY,
               're':'\d{2}',
               'printf':'%02d'
               },
           'w':{
               'format':'%w',
               'rule':rrule.WEEKLY,
               're':'\d{1}',
               'printf':'%d'
               },
           'j':{
               'format':'%j',
               'rule':rrule.DAILY,
               're':'\d{3}',
               'printf':'%03d'
               },
           'd':{
               'format':'%d',
               'rule':rrule.DAILY,
               're':'\d{2}',
               'printf':'%02d'
               },
           'H':{
               'format':'%H',
               'rule':rrule.HOURLY,
               're':'\d{2}',
               'printf':'%02d'
               },
           'M':{
               'format':'%M',
               'rule':rrule.MINUTELY,
               're':'\d{2}',
               'printf':'%02d'
               },
           'S':{
               'format':'%S',
               'rule':rrule.MINUTELY,
               're':'\d{2}',
               'printf':'%02d'
               }
           })
  
def splitall(path):
    """splitall: recursively creates map of directory heirarchy from directory regular expression.

    Args:
        path : directory path string 

    Returns:
        allparts : list of subdirectories
    """
    allparts = []
    while 1:
        parts = os.path.split(path)
        if parts[0] == path:  # sentinel for absolute paths
            allparts.insert(0, parts[0])
            break
        elif parts[1] == path: # sentinel for relative paths
            allparts.insert(0, parts[1])
            break
        else:
            path = parts[0]
            allparts.insert(0, parts[1])
    return allparts

def ddhh2pack(day,hour):
    """ddhh2pack: convert day/hour to GFS pack format.

    Args:
        day : 2 digit integer 
        hour : 2 digit integer

    Returns:
        pack : string 
    """
    if hour == 0:
        pcode = 'X'
        pday = day
    elif hour == 6:
        pcode = 'Y'
        pday = day
    elif hour == 12:
        pcode = 'X'
        pday = day + 50
    elif hour == 18:
        pcode = 'Y'
        pday = day + 50
    else:
        msg = "Hour: {} is not a synoptic time (0,6,12,18)".format(hour)
        error(LOG, msg, error_codes.EX_IOERR)

    return(pcode,pday)

def pack2hhdd(pcode,pday):
    """pack2hhdd: convert GFS pack format to hour/day.

    Args:
        pcode : pack code string 
        pday : pack day string 

    Returns:
        day : integer 
        hour: integer
    """
    if pcode == 'X' and pday < 50:
       hour=0 
    elif pcode == 'Y' and pday < 50:
       hour=6
    elif pcode == 'X' and pday > 50:
       hour=12
       day=pday-50
    elif pcode == 'Y' and pday > 50:
       hour=18
       day=pday-50
    else:
        msg="Pack code: {} /day: {} not valid".format(pcode,pday)
        error(LOG,msg,error_codes.EX_IOERR)

    return(day,hour)

def DTGrangeToStrings(startDTG,endDTG,formats):
    """return range of datetimes as strings using format.

    Args:
        startDTG : datetime 
        endDTG : datetime 

    Returns:
        DTGstrs : string list
    """
    DTGstrs={}
    try:
        if startDTG > endDTG:
           msg="Start DTG {} is greater than end DTG {}".format(startDTG,endDTG)
           error(LOG,msg,error_codes.EX_IOERR)

        for key,value in formats.iteritems():
            startStr=startDTG.strftime(value)
            endStr=endDTG.strftime(value)
            DTGstrs[key]=[]
            for i in xrange(int(startStr),int(endStr)+1):
                string=str(i).zfill(len(startStr))
                DTGstrs[key].append(string)

    except:
        msg="DTGrangeToStrings unknown problem"
        error(LOG,msg,error_codes.EX_IOERR)

    return(DTGstrs)
           

def DTGintervalList(startDTG,endDTG,iterDelta):
    """return range of datetimes at interval timeDelta.

    Args:
        startDTG : datetime 
        endDTG : datetime 

    Returns:
        DTGstrs : string list
    """
    try:

        if startDTG > endDTG:
           msg="Start DTG {} is greater than end DTG {}".format(startDTG,endDTG)
           error(LOG,msg,error_codes.EX_IOERR)

        currDTG=startDTG
        DTGs=[]
        while currDTG <= endDTG:
            DTGs.append(currDTG)
            currDTG = currDTG + iterDelta 

    except:
        msg="DTGintervalList unknown problem"
        error(LOG,msg,error_codes.EX_IOERR)

    return(DTGs)

def listToRE(list):
    """create list RE.

    Args:
        list : string list 

    Returns:
        RE : string 
    """
    try:
        RE="({})".format('|'.join(list))
    except: 
        msg="listToRe: Problem with {}".format(list)
        error(LOG,msg,error_codes.EX_DATAERR)

    return(RE)

def repStrTmpl(tmplString,strings,ldelim='%',rdelim=''):
    """
    repStrTmpl - replace a template string with a set of strings/REs and return the new string/RE.

    Args:
        tmplString : template string 
        strings : list
        ldelim : char
        rdelim : char

    Returns:
        RE : newString 
    """
    try:
        strTypes=(types.StringType, types.UnicodeType)
        newString=tmplString
        REflag=False
        for strKey in strings:
    
            strValue=strings[strKey]
            tmpl="{}{}{}".format(ldelim,strKey,rdelim)
            if isinstance(strValue,strTypes):
                newString=newString.replace(tmpl,strValue)
            elif isinstance(strValue, list):
                unique=list(set(strValue))
                if re.search(tmpl,newString):
                    newString=newString.replace(tmpl,listToRE(unique))
                    REflag=True
            else:
                msg="repStrTmpl: {} value not string or list of strings".format(strKey)
                warning(LOG,msg)

        if REflag:
             newString="^{}$".format(newString)

    except:
        msg="repStrTmpl: unknown data error"
        error(LOG,msg,error_codes.EX_DATAERR)

    return(newString)

def repDTfrmtToRE(string):
    """
    repDTfrmtToRE - replace a datetime format specs with corresponding RE's 

    Args:
        string : string 

    Returns:
        RE : new regular expression string 
    """

    RE=string 
    for dtgfrmt in DTGFRMTS:
        if DTGFRMTS[dtgfrmt]['format'] in RE:
            RE=RE.replace(DTGFRMTS[dtgfrmt]['format'],DTGFRMTS[dtgfrmt]['re'])

    return(RE)

def repDTfrmtToPrintf(string):
    """
    repDTfrmtToPrintf - replace a datetime format specs with corresponding printf spec 

    Args:
        string : string 

    Returns:
        pfSpec : new printf spec 
    """

    pfSpec=string 
    for dtgfrmt in DTGFRMTS:
        if DTGFRMTS[dtgfrmt]['format'] in pfSpec:
            pfSpec=pfSpec.replace(DTGFRMTS[dtgfrmt]['format'],DTGFRMTS[dtgfrmt]['printf'])

    return(pfSpec)
             
def parseDirEntries(entries):
    """
    parse a list of directory entries (dir, dir file, or dirRE) and return expanded dir list.

    Parameters
    ----------
    entries : list 

    Returns
    -------
    dirs : list 
    """
    dirs=[]
    for entry in entries: 

        entry=str(entry)
        if os.path.isfile(entry):
           dirFH = open(entry,"r")
           for line in dirFH:
               dir=line.rstrip('\n')
               if os.path.isdir(dir):
                   dirs.append(dir)
               else:
                   msg="Dir file: {} dir not valid: {} ".format(entry,dir)
                   error(LOG,msg,error_codes.EX_IOERR)
           dirFH.close()
        elif os.path.isdir(entry):
           dirs.append(entry)
        elif re.search("\^.+\$",entry):
           entryRE=dirRE.dirRE(entry)
           for dir in (entryRE.getDirs()):
               dirs.append(dir) 
        else: 
           msg="Dir entry {} neither dir, dir file, or dir RE".format(entry)
           error(LOG,msg,error_codes.EX_IOERR)

    return(dirs)

def getProcesses(commandRE):
    """
    Return a dictonary of process objects for a given command line regular expression. Return None if process not found.

    Parameters
    ----------
    commandRE : string

    Returns
    -------
    processes : dict
    """
    procAttrs=['username', 'pid', 'cmdline', 'create_time', 'cpu_percent', 'terminal', 'ppid', 'cwd', 'nice', 'status', \
               'cpu_times', 'open_files', 'name', 'num_threads', 'exe', 'uids', 'gids', 'memory_percent','parent','children']
    processes={}
    for proc in psutil.process_iter():
        try:
            pinfo = proc.as_dict(attrs=procAttrs)
            cmdline=" ".join(pinfo['cmdline'])
        except psutil.NoSuchProcess:
            pass
        else:
            if re.search(commandRE,cmdline):
                processes[pinfo['pid']]=proc

    return(processes)

def readBinaryFileHeader(headerDef,filePath):
    """
    readBinaryFileHeader - Reads a binary file using the size/format definitions in a header dictionary.

    Adds a values array for each header attribute and returns true on success
    header attributes: size= # of bytes, format = python unpack format definition
    header={
            "string":{ "size":4, format:"4s",
            "integer":{ "size":4, format: "i"
           }
    Parameters
    ----------
    headerDef : string 
    filePath : string

    Returns
    -------
    boolean 
    """
    try:
         FH=open(filePath,'rb')
    except:
         LOG.warning("Unable to read binary file: {}".format(filePath))
         return(False)

    try:
        for attr in headerDef:
            size=headerDef[attr]['size']
            format=headerDef[attr]['format']
            buffer = FH.read(size)
            headerDef[attr]['values'] = list(struct.unpack(format,buffer))
        FH.close()
    except:
         LOG.warning("Problem reading binary file: {}".format(filePath))
         return(False)

    return(True)


def warning(LOG,msg="Unexpected Warning:"):
    """warning - append message to a log object."""
    LOG.warn(msg)

def error(LOG, msg="Unexpected Error:", code=1):
    """error - append message to a log object and throw an error."""
    LOG.exception(msg)
    sys.exit(code)

def memory_usage():
    """
    return the current usage of the process.

    Returns
    -------
    resource.getrusage : float
        RAM usage in MB
    """
    return float(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss ) / 1024.

def execute(commandList,commandArgs,commandID,logDir,stdoutFile='',stderrFile=''):
    """
    execute - builds the argument list for a subprocess system call within a given 
              working directory with stdout/err log files 

    Returns
    -------
    status - boolean whether it execute properly
    """

    command=commandList[-1]

    commandLine=[]
    commandLine.extend(commandList)
    commandLine.extend(commandArgs)

    status=True
    try:
        base, ext =os.path.splitext(os.path.basename(command))

        if not stdoutFile: 
           stdoutFile=os.path.join(logDir,"{}_{}.stdout".format(base,commandID))
        if not stderrFile:
           stderrFile=os.path.join(logDir,"{}_{}.stderr".format(base,commandID))

        outfh=open(stdoutFile,'w')
        errfh=open(stderrFile,'w')
        LOG.debug("STDOUT File: {}".format(stdoutFile))
        LOG.debug("STDERR File: {}".format(stderrFile))
        LOG.debug("Subprocess Executing: {}".format(" ".join(commandLine)))
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
            LOG.debug("Subprocess: {} completed normally".format(command))
    except OSError as e:
        LOG.warning("Subprocess: {} failed".format(command))
        status=False
        return(status)

    return(status)

def workDir(workPath):
    """
    workDir - creates and changes to working directory 

    Returns
    -------
    status - boolean whether it executed properly
    """

    if not os.path.exists(workPath):
        LOG.debug('Creating working directory: {}'.format(workPath))
        try:
            os.makedirs(workPath)
        except:
            msg = "Unable to create work dir {}".format(workPath)
            error(LOG, msg, error_codes.EX_IOERR)

    # CD to work directory
    try:
        LOG.debug('Changing to working directory: {}'.format(workPath))
        os.chdir(workPath)
    except:
        msg = "Unable to change to work dir: {}".format(workPath)
        error(LOG, msg, error_codes.EX_IOERR)

    return

def moveFile(sourceDir,filename,destDir,newFilename=None):

    if not os.path.exists(destDir):
        msg="Destination directory doesn't exist: {}".format(destDir)
        error(LOG,msg,error_codes.EX_CONFIG)

    sourceFile=os.path.join(sourceDir,filename)
    destFile=""
    if not os.path.isfile(sourceFile):
        msg="Source file doesn't exist: {}".format(sourceFile)
        error(LOG,msg,error_codes.EX_CONFIG)
    try:
        if newFilename is None:
             destFile=os.path.join(destDir,filename)
        else:
             destFile=os.path.join(destDir,newFilename)
        os.rename(sourceFile,destFile)
    except:
        msg="Unable to move file: {} to {}".format(sourceFile,destFile)
        error(LOG,msg,error_codes.EX_CONFIG)

    return(destFile)

def linkFile(sourceDir,filename,destDir,newFilename=None):

    if not os.path.exists(destDir):
        msg="Destination directory doesn't exist: {}".format(destDir)
        error(LOG,msg,error_codes.EX_CONFIG)

    sourceFile=os.path.join(sourceDir,filename)
    destFile=""
    if not os.path.isfile(sourceFile):
        msg="Source file doesn't exist: {}".format(sourceFile)
        error(LOG,msg,error_codes.EX_CONFIG)
    try:
        if newFilename is None:
            destFile=os.path.join(destDir,filename)
        else:
            destFile=os.path.join(destDir,newFilename)
        os.symlink(sourceFile,destFile)
    except:
        msg="Unable to link file: {} to {}".format(sourceFile,destFile)
        error(LOG,msg,error_codes.EX_CONFIG)

    return(destFile)

def schemaValidate(inputDict,schema):

    inputJson=json.loads(json.dumps(inputDict))
    if schema["$schema"] == "http://json-schema.org/draft-04/schema#":
        v = jsonschema.Draft4Validator(schema)
    elif schema["$schema"] == "http://json-schema.org/draft-03/schema#":
        v = jsonschema.Draft3Validator(schema)
    else:
        msg="Schema needs to be a recoginized meta-schema [Draft 3, Draft 4]"
        error(LOG,msg,error_codes.EX_IOERR)

    msg=None
    errs = sorted(v.iter_errors(inputDict), key=lambda e: e.path)
    if errs:
        msg = ""
        for err in errs:
            msg += err.message

    return(msg)


def schemaValidate(inputDict,schema):

    inputJson=json.loads(json.dumps(inputDict))
    if schema["$schema"] == "http://json-schema.org/draft-04/schema#":
        v = jsonschema.Draft4Validator(schema)
    elif schema["$schema"] == "http://json-schema.org/draft-03/schema#":
        v = jsonschema.Draft3Validator(schema)
    else:
        msg="Schema needs to be a recoginized meta-schema [Draft 3, Draft 4]"
        error(LOG,msg,error_codes.EX_IOERR)

    msg=None
    errs = sorted(v.iter_errors(inputDict), key=lambda e: e.path)
    if errs:
        msg = ""
        for err in errs:
            msg += err.message

    return(msg)

