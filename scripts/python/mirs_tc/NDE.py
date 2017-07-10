#!/usr/bin/python

# System modules
import sys
import os
import re
import datetime
import operator
import collections
import importlib
import logging
import traceback

# Local modules
import utils
import setup_logging
import error_codes

LOG = logging.getLogger(__name__)

PCFschema={
   "$schema": "http://json-schema.org/draft-04/schema#",
   "type":"object",
   "properties": {
      "working_directory":{
         "type":"string"
      },
      "job_coverage_start":{
         "type":"string"
      },
      "job_coverage_end":{
          "type":"string"
      },
      "algorithmLogMessageContext":{
           "type":"string"
      },
      "algorithmLogMessageError":{
           "type":"string"
      },
      "algorithmLogMessageWarn":{
           "type":"string"
      }
   } 
}

def getInput(PCFdata):

       PCFdict={} 
       if os.path.exists(PCFdata):
          PCFdict=_readPCF(PCFdata)
       elif isinstance(PCFdata,dict):
          PCFdict=PCFdata 
       else:
          msg="PCF data neither file or dictionary"
          utils.error(LOG,msg,error_codes.EX_IOERR)

       msg=utils.schemaValidate(PCFdict,PCFschema)
       if msg:
          utils.error(LOG,msg,error_codes.EX_IOERR)
       else:
          return(PCFdict)


def _readPCF(PCFfile):
   pcf={}
   try:
       with open(PCFfile,"r") as pcf_fh:
          for line in pcf_fh:
             line=line.rstrip()
             key,value=line.split('=') 
             if key not in pcf:
                pcf[key]=value
             else:
                if isinstance(pcf[key],list):
                    pcf[key].append(value)
                else:
                    firstValue=pcf[key]
                    pcf[key]=[]
                    pcf[key].append(firstValue)
                    pcf[key].append(value)

   except Exception, e:
       utils.error(LOG,str(e),error_codes.EX_IOERR)

   return(pcf)
             

