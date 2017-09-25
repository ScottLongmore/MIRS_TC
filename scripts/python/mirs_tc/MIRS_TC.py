#!/usr/bin/python
"""
MIRS_TC - library of MIRS_TC routines 
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
import shutil
import json
import jsonschema
import numpy
import pandas as pd
import math
import pprint


# Local modules
import error_codes
import utils

# Pretty Printer
pp=pprint.PrettyPrinter(indent=4)

# Logging
LOG = logging.getLogger(__name__)

DTFormat="%Y%m%d%H%M%S%f"
mirsTimeQueryOffsets={
   0:{"after":0,"before":21600},
   1:{"after":3600,"before":21600},
   2:{"after":7200,"before":21600},
   3:{"after":10800,"before":21600},
   4:{"after":14400,"before":21600},
   5:{"after":18000,"before":21600}
}

basins={
   "al":"Atlantic",
   "ep":"Eastern Pacific",
   "cp":"Central Pacific",
   "wp":"Western Pacific",
   "io":"Indian Ocean",
   "sh":"Southern Hemisphere"
}

gfsGribFields={
    "u70":":UGRD:70 mb:", "u100":":UGRD:100 mb:", "u150":":UGRD:150 mb:", "u200":":UGRD:200 mb:", "u250":":UGRD:250 mb:", 
    "u300":":UGRD:300 mb:", "u400":":UGRD:400 mb:", "u500":":UGRD:500 mb:", "u600":":UGRD:600 mb:", "u700":":UGRD:700 mb:", 
    "u850":":UGRD:850 mb:", "u925":":UGRD:925 mb:", "u1000":":UGRD:1000 mb:", "u1070":":UGRD:0.995 sigma level:",
    "v70":":VGRD:70 mb:", "v100":":VGRD:100 mb:", "v150":":VGRD:150 mb:", "v200":":VGRD:200 mb:", "v250":":VGRD:250 mb:",
    "v300":":VGRD:300 mb:", "v400":":VGRD:400 mb:", "v500":":VGRD:500 mb:", "v600":":VGRD:600 mb:", "v700":":VGRD:700 mb:",
    "v850":":VGRD:850 mb:", "v925":":VGRD:925 mb:", "v1000":":VGRD:1000 mb:", "v1070":":VGRD:0.995 sigma level:",
    "height70":":HGT:70 mb:", "height100":":HGT:100 mb:", "height150":":HGT:150 mb:", "height200":":HGT:200 mb:", "height250":":HGT:250 mb:",
    "height300":":HGT:300 mb:", "height400":":HGT:400 mb:", "height500":":HGT:500 mb:", "height600":":HGT:600 mb:", "height700":":HGT:700 mb:",
    "height850":":HGT:850 mb:", "height925":":HGT:925 mb:", "height1000":":HGT:1000 mb:", "height1070":":HGT:surface:", 
    "temp70":":TMP:70 mb:", "temp100":":TMP:100 mb:", "temp150":":TMP:150 mb:", "temp200":":TMP:200 mb:", "temp250":":TMP:250 mb:",
    "temp300":":TMP:300 mb:", "temp400":":TMP:400 mb:", "temp500":":TMP:500 mb:", "temp600":":TMP:600 mb:", "temp700":":TMP:700 mb:",
    "temp850":":TMP:850 mb:", "temp925":":TMP:925 mb:", "temp1000":":TMP:1000 mb:", "temp1070":":TMP:0.995 sigma level:",
    "relh100":":RH:100 mb:", "relh70":":RH:70 mb:", "relh150":":RH:150 mb:", "relh200":":RH:200 mb:", "relh250":":RH:250 mb:",
    "relh300":":RH:300 mb:", "relh400":":RH:400 mb:", "relh500":":RH:500 mb:", "relh600":":RH:600 mb:", "relh700":":RH:700 mb:",
    "relh850":":RH:850 mb:", "relh925":":RH:925 mb:", "relh1000":":RH:1000 mb:", "relh1070":":RH:0.995 sigma level:", 
    "p1070":":PRMSL:mean sea level:"
}

erad=6371.0 # Radius of Earth
dtr=math.pi/180.0 # Degrees to radians conversion



def last_synoptic_datetime(dt):

    hours=[6,12,18]

    synHour=0
    for hour in hours:
        if dt.hour >= hour:
            synHour=hour
        else:
            break

    return(dt.replace(hour=synHour,minute=0,second=0,microsecond=0))

def next_synoptic_datetime(dt):
            
    hours=[18,12,6]
        
    synHour=0
    for hour in hours:
        if dt.hour < hour:
            synHour=hour
        else:
            break

    return(dt.replace(hour=synHour,minute=0,second=0,microsecond=0))

def datetimes(jcsDTS,jceDTS):

    try:
        jcsDTG=datetime.datetime.strptime(jcsDTS,DTFormat)
        jceDTG=datetime.datetime.strptime(jceDTS,DTFormat)
    except:
        msg="Time: {} invalid format (YYYYMMDDHHMMSSS)".format(jcsDTS)
        utils.error(LOG,msg,error_codes.EX_IOERR)

    try:
        initDTG=jceDTG.replace(minute=0,second=0,microsecond=0) # Last Hour floor e.g. 5:05:23 -> 5:00:00
        lastSynpDTG=last_synoptic_datetime(initDTG) # 0,6,12,18 
        synpDelta=int((initDTG-lastSynpDTG).total_seconds() // 3600) # Synoptic Hour Delta
        datetimes={
            "jcsDTG":jcsDTG,
            "jceDTG":jceDTG,
            "initDTG":initDTG,
            "synpDTG":lastSynpDTG,
            "mirsSecsAfter":mirsTimeQueryOffsets[synpDelta]['after'],
            "mirsSecsBefore":mirsTimeQueryOffsets[synpDelta]['before']
        }
    except:
        msg="Problem determining datetimes"
        utils.error(LOG,msg,error_codes.EX_IOERR)

    return(datetimes)

def datalinks(metadata,datasetKeys):

    # Fill datalinks with references to metadata objects by dataname and link value
    dataLinks={}
    for dataname in datasetKeys:
        datasetKey=datasetKeys[dataname] #Value containing object property key that links metadata (e.g. ATMS 'startDT')
        for dataset in metadata[dataname]:
            linkID=dataset.get(datasetKey) #Unique value (linkID) common between dataset files such as 'startDT' e.g. 20170601120000
            # print("dataname: {} datasetKey: {} linkID: {} ".format(dataname,datasetKey,linkID))
            if linkID not in dataLinks: 
               dataLinks[linkID]={}
            dataLinks[linkID][dataname]=dataset
            #print("linkID: {} dataname: {} filename: {}".format(linkID,dataname,dataLinks[linkID][dataname].get("filename")))

    #for linkID in dataLinks:
    #    print dataLinks[linkID].keys()
    #    for dataname in dataLinks[linkID]:
    #         print("linkID: {} dataname: {} filename: {}".format(linkID,dataname,dataLinks[linkID][dataname].get("filename")))

    # Keep only datalinks that have references to all datasets
    deleteLinks=[]
    for linkID in dataLinks:
        for dataname in datasetKeys: 
            if dataname not in dataLinks[linkID]:
                 deleteLinks.append(linkID)

#        dataset=dataLinks[linkID]
#        if not all(dataname in dataset for dataname in datasetKeys):
#            deleteLinks.append(linkID)

    for linkID in deleteLinks:
        #print("Deleting: {}".format(linkID))
        dataLinks.pop(linkID,None)

    return(dataLinks)



def grib2pack(gribDate,gribTime,gribFhour,gribFile,wgribExe,bin2packExe,logDir):

    gribString="{}{}_F{}".format(gribDate,gribTime,gribFhour)
    LOG.info("Extracting binary fields from grib file: {}".format(gribFile))
    for field in gfsGribFields:

        pattern=gfsGribFields[field]
        binFile="{}_{}.bin".format(gribString,field)
    
        # Extract grids from grib files via WGRIB (needs to be replaced with grib_api module) 
        commandList=[wgribExe]
        commandArgs=[]
        commandArgs.extend(["-match",pattern,"-no_header",gribFile,"-order","raw","-bin",binFile])
        commandID=field
        stdoutFile=os.path.join(logDir,"wgrib_{}_{}.log".format(gribString,field))
        stderrFile=os.path.join(logDir,"wgrib_{}_{}.err".format(gribString,field))
        LOG.info("Running wgrib2")
        LOG.debug("Executing: {}".format(" ".join(commandList+commandArgs)))
        if not utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile):
             LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
             LOG.warning("See sub-process log file: {}".format(stdoutFile))
             LOG.warning("See sub-process error file: {}".format(stderrFile))
             return(False)

    # Combine binanry files to pack file
    commandList=[bin2packExe]
    commandArgs=[]
    commandArgs.extend([gribDate,gribTime,gribFhour])
    commandID="bin2pack"
    stdoutFile=os.path.join(logDir,"bin2pack_{}.log".format(gribString))
    stderrFile=os.path.join(logDir,"bin2pack_{}.err".format(gribString))
    LOG.info("Running bin2pack")
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    if not utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile):
         LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
         LOG.warning("See sub-process log file: {}".format(stdoutFile))
         LOG.warning("See sub-process error file: {}".format(stderrFile))
         return(False)

    return(True)

def getInputVars(filepath,column,value):

     # Fix for bad column name 'DELTA T' with space
     names=['DATE','TIME','DELTA_T','LATITUDE','LONGITUDE',\
            'DIRECTION','SPEED','RMAX','VMAX','NAME','BOGUS']

     if column not in names:
        msg="Column {} does not exist in short term track input table: {}".format(column,filepath)
        utils.error(LOG,msg,error_codes.EX_IOERR)
        
     if os.path.isfile(filepath):
         with open(filepath) as f:
            inputDF = pd.read_table(f, sep='\s+', index_col=0, header=None, names=names, skiprows=3, parse_dates={"DATETIME":[0,1]})

         inputDF=inputDF.drop('BOGUS',axis=1)
         rows=inputDF.loc[inputDF[column] == value].to_dict(orient="index") 
     else:
        msg="File doesn't exist: {}".format(filepath)
        utils.error(LOG,msg,error_codes.EX_IOERR)

     return(rows)

def getInputAnalysisVars(filepath):

     rows=getInputVars(filepath,'DELTA_T',0)

     varDTG=rows.keys()[0] # Assume only one
     vars={}
     for col in rows[varDTG]:
        vars[col]=rows[varDTG][col]
     vars['DTG']=varDTG

     if vars['LONGITUDE'] > 180: 
        vars['LONGITUDE']=(360-vars['LONGITUDE'])*-1 
 
     return(vars)

def getLinkFilenames(dataLink, inputDatasets, inputDatasetKey, inputFilePaths, outputDataname):

    # Get dataset filenames, linkIDs
    dataFileLinks={} 
    for dataset in inputDatasets:
        dataFileLinks[dataset.get('filename')]=dataset.get(inputDatasetKey)

    # Match input filenames to filenames 
    outputFilenames=[]
    for inputFilePath in inputFilePaths:
        inputFile=os.path.basename(inputFilePath)
        if inputFile in dataFileLinks:
            try:
                linkID=dataFileLinks[inputFile]
                outputFile=dataLink[linkID][outputDataname].get('filename')
                outFilename=os.path.join(dataLink[linkID][outputDataname].get('path'),outputFile)
                outputFilenames.append(outFilename)
                LOG.info("Linked Files input: {} output: {}".format(inputFile,outputFile))
            except:
                LOG.warning("No matching input file: {} in output: {}".format(inputFile,outputDataname))

    return(outputFilenames)

def extractTarFiles(tarCommand,tarFile,stringID,logDir):

    # Extract files from tar file
    LOG.info("Extracting {} data files from archive file: {}".format(stringID,tarFile))
    commandList=[tarCommand]
    commandArgs=[]
    commandArgs.extend(["--warning=no-timestamp","--strip-components=1","-xvf",tarFile])
    commandID="tar_{}".format(stringID)
    stdoutFile=os.path.join(logDir,"{}.stdout".format(commandID))
    stderrFile=os.path.join(logDir,"{}.stderr".format(commandID))
    LOG.debug("Executing: {}".format(" ".join(commandList+commandArgs)))
    if not utils.execute(commandList,commandArgs,commandID,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile):
         LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
         LOG.warning("See sub-process log file: {}".format(stdoutFile))
         LOG.warning("See sub-process error file: {}".format(stderrFile))
         return(False)
 
    return(True)

def poolType(value,dtype):

    pValue=None
    if dtype=='int':
        pValue=int(value)
    elif dtype=='float':
        pValue=float(value)
    else:
        msg="Unknown Pool Type: {}".format(dtype)
        utils.error(LOG,msg,error_codes.EX_IOERR)
 
    return(pValue)
   
def readPoolTextFile(filename): 

    try:
        fh=open(filename,"r")
        rank=int(fh.readline().rstrip().split(":")[1])
        dimsString=fh.readline().rstrip().split(":")[1]
        dims=map(int,dimsString.split(","))
        dtype=fh.readline().rstrip().split(":")[1]
        nRecords=reduce(operator.mul, dims, 1)
         
        records=[]
        for line in fh:
            fields=line.rstrip().split(',')
            record=map(int,fields[:-1]) # convert indexes from string to int
            record.append(poolType(fields[-1],dtype))
            records.append(record)

        records=map(list,zip(*records)) # flip dimenstions
    except:
        LOG.warning("Problem reading pool text file: {}".format(filename))
       
    ptfDict={
        'rank':rank,
        'dims':dims,
        'type':dtype, 
        'records':records
    }
      
    return(ptfDict)

def writePoolTextFile(ptfDict,filename):

    try:
        newFH=open(filename,"w")
        newFH.write("rank:{:03d}\n".format(ptfDict['rank']))
        newFH.write("dimensions:{}\n".format(','.join(map(lambda x:"{:09d}".format(x), ptfDict['dims']))))
        newFH.write("type:{}\n".format(ptfDict['type']))

        records=numpy.array(ptfDict['records'][-1]).reshape(ptfDict['dims'])
        for index,npValue in numpy.ndenumerate(records):
            idxStr=','.join(map(lambda i:"{:09d}".format(i),index))
            value=npValue.astype(ptfDict['type'])
            newFH.write("{},{:015.5f}\n".format(idxStr,value))
    
        newFH.close()

    except:
        LOG.warning("Problem writing pool text file: {}".format(filename))
        return False 

    return True

def createTimeFieldFiles(timeFilename):

    ptfDict=readPoolTextFile(timeFilename) # index:records[0], epoch seconds record[1]

    timeVars={
        "years":"%Y",
        "months":"%m",
        "days":"%d",
        "hours":"%H",
        "minutes":"%M",
        "seconds":"%S"
    }
    timeFiles={}
    timeFHs={}
    for timeVar in timeVars: 
       timeFiles[timeVar]="{}.txt".format(timeVar)
       timeFHs[timeVar]=open(timeFiles[timeVar],"w")
       timeFHs[timeVar].write("rank:{:03d}\n".format(ptfDict['rank']))
       timeFHs[timeVar].write("dimensions:{}\n".format(','.join(map(lambda x:"{:09d}".format(x), ptfDict['dims']))))
       timeFHs[timeVar].write("type:int\n")

    records=ptfDict['records'] 
    for rn in xrange(0,ptfDict['dims'][-1]):
        DTG=datetime.datetime.fromtimestamp(float(records[1][rn]))
        for timeVar in timeVars:
            timeFHs[timeVar].write("{:09d},{:09d}\n".format(records[0][rn],int(DTG.strftime(timeVars[timeVar]))))

    for timeVar in timeVars: 
       timeFHs[timeVar].close()

    return True 

def scaleOffsetThreshPoolTextFile(scale,offset,lowThresh,highThresh,inputMissing,outputMissing,filename,newfilename):

    try:
        ptfDict=readPoolTextFile(filename) 

        values=ptfDict['records'][-1]
        for index in xrange(len(values)):
            values[index]=scaleOffsetThreshValue(values[index],scale,offset,lowThresh,highThresh,inputMissing,outputMissing)

        ptfDict['type']='float'
        writePoolTextFile(ptfDict,newfilename) 

    except:
        LOG.warning("Problem with scaleOffsetThreshPoolTextFile") 
        return False

    return True

def scaleOffsetThreshValue(value,scale,offset,lowThresh,highThresh,inputMissing,outputMissing):

    newValue=None
    if value != inputMissing:
        newValue=(value*scale)+offset
        if newValue < lowThresh or newValue > highThresh:
            newValue = outputMissing
    else:
        newValue=outputMissing

    return(newValue)
           

def createCOORTIMES(coordFile,timeFile,satIdent,instrIdent,coortimeFile):
   
    try:
        with open(coortimeFile,'wb') as wfd:
            for f in [coordFile,timeFile]:
                with open(f,'rb') as fd:
                   shutil.copyfileobj(fd, wfd)
            wfd.write("{}\n{}\n".format(satIdent,instrIdent))
    except:
        return False
 
    return True
