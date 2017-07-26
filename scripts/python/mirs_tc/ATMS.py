#!/usr/bin/python
"""
ATMS.py - MIRS SNPP ATMS class 
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

# NPR-MIRS-SND_v9_NPP_s201208180059460_e201208180100176_c20130308234921.nc
# NPR-MIRS-IMG_v9_NPP_s201208180058100_e201208180058416_c20130308234421.nc
regexp="^NPR-MIRS-(?P<type>SND|IMG)_(?P<version>\\w+)_(?P<satellite>\\w+)_s(?P<startDT>\\d{14,15})_e(?P<endDT>\\d{14,15})_c(?P<createDT>\\d{14,15})\\.nc$"
tmpl="^NPR-MIRS-{}_{}_{}_s{}_e{}_c{}.nc$"
schema={
    "$schema": "http://json-schema.org/draft-04/schema#",
    "type":"object",
    "properties": {
        "type":{ "type":"string" },
        "version":{ "type":"string" },
        "satellite":{ "type":"string" },
        "startDTG":{ "type":"string" },
        "endDTG":{ "type":"string" },
        "createDTG":{ "type":"string" }
    }
}
datasets={
    "mirs_atms_img":{
       "type":"IMG",
    },
    "mirs_atms_snd":{
       "type":"SND",
    }
}
dtFormat="%Y%m%d%H%M%S"

class atms(dataset):

    def __init__(self,filename):

        self.regexp=regexp
        self.schema=schema

        super(atms,self).__init__(filename)

        # Seconds or milliseconds
        for dt in ['startDT','endDT','createDT']:
            if len(self.properties[dt]) == 15:
                self.properties[dt]=datetime.datetime.strptime(self.properties[dt][:-1],dtFormat)
            else:
                self.properties[dt]=datetime.datetime.strptime(self.properties[dt],dtFormat)

        return


def filterAtms(config,metadata,dataname):
   
    filterAtms=[]
    dataset=config['datasets'][dataname]
    if dataset['datalink'] in config['datalinks']:

        datalink=config['datalinks'][dataset['datalink']]
        datasetKeys=datalink['datasetKeys']
        links=datalink['links']

        try:
            for atms in metadata[dataname]:
                linkID=atms.get(datasetKeys[dataname])
                if linkID in links: 
                    filterAtms.append(atms)
                else:
                    utils.moveFile(atms.get('path'),atms.get('filename'),config['dirs']['discard'])
                    LOG.info("Moving unmatched file: {} to discard dir: {}".format(atms.get('filename'),config['dirs']['discard']))
        except:
            msg="Error matching/filtering dataset: {}".format(dataset)
            utils.error(LOG,msg,error_codes.EX_IOERR)

    else:
       msg="config[\"datalinks\"][{}] definition required, see MIRS_TC.datalinks".format(dataset['datalink'])
       utils.error(LOG,msg,error_codes.EX_IOERR)

    return(filterAtms)
