#!/usr/bin/python
"""
dataset.py - parent class for dataset objects 
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

LOG = logging.getLogger(__name__)

class dataset(object):

    def __init__(self,filename):

        self.properties={
            "filename":filename
        }

        try:
            match=re.match(self.regexp,filename)
            fields=match.groupdict()
            msg=utils.schemaValidate(fields,self.schema)
            if msg:
               utils.error(LOG,msg,error_codes.EX_IOERR)
            self.properties.update(fields)
        except:
            msg="Problem extracting properties from filename: {}".format(filename)



    def getProperty(self,prop):

        if prop in self.properties:
            return(self.properties[prop])
        else:
            msg="Property: {} not found".format(prop)
            utils.error(LOG,msg,error_codes.EX_IOERR)

    def getProperties(self):

        return(self.properties)


