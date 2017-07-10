# Stock modules
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
import logging
import types
import struct
import resource

# Local modules
import error_codes
import dirRE

LOG = logging.getLogger(__name__)

DTGFRMTS = collections.OrderedDict(
         {
         'Y':{
             'format':'%Y',
             'rule':rrule.YEARLY
             },
         'y':{
             'format':'%y',
             'rule':rrule.YEARLY
             },
         'm':{
             'format':'%m',
             'rule':rrule.MONTHLY
             },
         'w':{
             'format':'%w',
             'rule':rrule.WEEKLY
             },
         'j':{
             'format':'%j',
             'rule':rrule.DAILY
             },
         'd':{
             'format':'%d',
             'rule':rrule.DAILY
             },
         'H':{
             'format':'%H',
             'rule':rrule.HOURLY
             },
         'M':{
             'format':'%M',
         'rule':rrule.MINUTELY
             },
         'S':{
             'format':'%S',
         'rule':rrule.MINUTELY
             }
         }
         )

def splitall(path):
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

    try:
        RE="({})".format('|'.join(list))
    except: 
        msg="listToRe: Problem with {}".format(list)
        error(LOG,msg,error_codes.EX_DATAERR)

    return(RE)

def repStrTmpl(tmplString,strings,ldelim='%',rdelim=''):
    """
    repStrTmpl - replace a template string with a set of strings/REs and return the new string/RE
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
      
def parseDirEntries(entries):
    """
    parseDirEntries - parse a list of directory entries (dir, dir file, or dirRE) 
                      and return expanded dir list
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
    Returns a dictonary of process objects for a given command line regular
    expression. Returns None if process not found

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
    readBinaryFileHeader - Reads a binary file using the size/format definitions in a header dictionary
                           Adds a values array for each header attribute and returns true on success
                           header attributes: size= # of bytes, format = python unpack format definition
                           header={
                                   "string":{ "size":4, format:"4s",
                                   "integer":{ "size":4, format: "i"
                                  }
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
    """
    warning - append message to a log object 
    """
    LOG.warn(msg)

def error(LOG, msg="Unexpected Error:", code=1):
    """
    error - append message to a log object and throw an error
    """
    LOG.exception(msg)
    sys.exit(code)

def memory_usage():
    """
    returns the current memory usage of the process

    Returns
    -------
    resource.getrusage : float
        RAM usage in MB
    """
    return float(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss ) / 1024.
