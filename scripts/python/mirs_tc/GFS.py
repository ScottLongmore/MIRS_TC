#!/usr/bin/python
"""
GFS.py - GFS GRIB2 class 
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
import utils
from dataset import dataset

LOG = logging.getLogger(__name__)

# gfs.t18z.pgrb2.1p00.f000.20120818
regexp="^gfs\\.t(?P<hour>\\d{2})z\\.pgrb2\\.1p00\\.f(?P<fhour>\\d{3})\\.(?P<runDTG>\\d{8})$"
schema={
    "$schema": "http://json-schema.org/draft-04/schema#",
    "type":"object",
    "properties": {
        "hour":{ "type":"string" },
        "fhour":{ "type":"string" },
        "runDTG":{ "type":"string" },
    }
}

class gfs(dataset):

    def __init__(self,filepath):

        self.regexp=regexp
        self.schema=schema

        super(gfs,self).__init__(filepath)

        self.properties['hour']=int(self.properties['hour'])
        self.properties['fhour']=int(self.properties['fhour'])
        DTS="{}{}".format(self.properties['runDTG'],self.properties['hour'])
        self.properties['runDTG']=datetime.datetime.strptime(DTS,"%Y%m%d%H")

def latestGFS(config,metadata,dataname):

    try:
        latestGFS=metadata[dataname].pop(0)
        for gfs in metadata[dataname]: 
    
            runDTG=gfs.get('runDTG')
            if runDTG > latestGFS.get('runDTG'):
                latestGFS=gfs
    except:
        msg="Problem filtering dataname object list".format(dataname)
        utils.error(LOG,msg,error_codes.EX_DATAERR)
    
    return([latestGFS])
 
