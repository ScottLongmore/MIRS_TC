#!/usr/bin/python
"""
libAMSU_TC.py - library of AMSU_TC data file filter routines 
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

# Local modules
import error_codes
from setup_nde_logging import setup_logging

LOG = logging.getLogger(__name__)

# Filter Routines

def adeck(input,time,files):
    adeckCARQRE=re.compile(".*CARQ,\s+0,.*")

    delta=datetime.timedelta(seconds=input['delta'])
    endDTG=time
    startDTG=time-delta

    newfiles={}
    for file in files:
   
        print "\tADECK: {}".format(file)
        lastDTG=startDTG
        fh=open(files[file]['name'],"r")
        for line in fh:
            if re.match(adeckCARQRE,line):
                basin,stormId,DTGstr,rest = re.split(',\s+',line,3)
                fileDTG=datetime.datetime.strptime(DTGstr,"%Y%m%d%H")
                if fileDTG > lastDTG and fileDTG<=endDTG:
                     lastDTG=fileDTG

         
        if(lastDTG > startDTG):
            newfiles[file]={}
            newfiles[file]['time']=lastDTG
            newfiles[file]['modTime']=files[file]['modTime']
            newfiles[file]['name']=files[file]['name']

    return(newfiles)

def gfspack(input,time,files):
    # G00014_X0278_PACK.DAT
    gfsRE=re.compile("^G000(?P<yy>\d{2})_(?P<pcode>[X|Y])(?P<mm>\d{2})(?P<dd>\d{2})_PACK\\.DAT$")

    delta=datetime.timedelta(seconds=input['delta'])
    endDTG=time
    startDTG=time-delta

    newfiles={}
    lastDTG=startDTG
    lastFile=""
    for file in files:

        match=re.match(gfsRE,file)
        groups=match.groupdict()
        day=int(groups['dd'])
        if groups['pcode'] == 'X' and day < 50:
           groups['HH']='00'
        if groups['pcode'] == 'Y' and day < 50:
           groups['HH']='06'
        if groups['pcode'] == 'X' and day > 50:
           groups['HH']='12'
           day=day-50
        if groups['pcode'] == 'Y' and day > 50:
           groups['HH']='18'
           day=day-50
        groups['dd']="{0:02d}".format(day)

        fileDTGstr="".join(operator.itemgetter('yy','mm','dd','HH')(groups))
        fileDTG=datetime.datetime.strptime(fileDTGstr,"%y%m%d%H")
        # print "GFS {} {} {} {}".format(file,fileDTG,startDTG,endDTG)
        if fileDTG > lastDTG and fileDTG<=endDTG:
           lastDTG=fileDTG
           lastFile=file

    # print "LAST {} {} {} {}".format(lastFile,lastDTG,startDTG,endDTG)
    newfiles[lastFile]={}
    newfiles[lastFile]['time']=lastDTG
    newfiles[lastFile]['modTime']=files[lastFile]['modTime']
    newfiles[lastFile]['name']=files[lastFile]['name']

    return(newfiles)

def atms(input,time,files):
    # NPR-MIRS-SND_v9_NPP_s201210271859486_e201210271900203_c20121220022941.nc
    amsuRE=re.compile(input['re'])

    delta=datetime.timedelta(seconds=input['delta'])
    endDTG=time
    startDTG=time-delta

    newfiles={}
    for file in files:

        match=re.match(amsuRE,file)
        groups=match.groupdict()
        fileDTGstr=groups['sDTG']
        fileDTG=datetime.datetime.strptime(fileDTGstr,"%Y%m%d%H%M%S")
        if fileDTG>=startDTG and fileDTG<=endDTG:
            newfiles[file]={}
            newfiles[file]['time']=fileDTG
            newfiles[file]['modTime']=files[file]['modTime']
            newfiles[file]['name']=files[file]['name']

    return(newfiles)
