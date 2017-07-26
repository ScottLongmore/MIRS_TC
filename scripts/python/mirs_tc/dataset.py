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

    def __init__(self,filepath):

        self.properties={}
        self._processFilepath(filepath)

    def get(self,prop):

        if prop in self.properties:
            return(self.properties[prop])
        else:
            msg="Property: {} not found".format(prop)
            utils.error(LOG,msg,error_codes.EX_IOERR)

    def getProperties(self):

        return(self.properties)


    def _processFilepath(self,filepath):

        # Check file existence
        if os.path.isfile(filepath):
            path,filename=os.path.split(filepath)
            if not path: # Just the filename, CWD is path, since it exists)
               path=os.getcwd()

            # Validate regexp/fields
            fields={}
            try:
                match=re.match(self.regexp,filename)
                fields=match.groupdict()
            except:
                msg="Filename: {} invalid filename format".format(filename)
                utils.error(LOG,msg,error_codes.EX_IOERR)

            # Validate file fields against dataset object schema
            msg=utils.schemaValidate(fields,self.schema)
            if msg:
                msg="Problem extracting properties from filename: {}".format(filename) + msg
                utils.error(LOG,msg,error_codes.EX_IOERR)

            self.properties['path']=path
            self.properties['filename']=filename
            self.properties.update(fields)

        else:
            msg="File: {} does't exist".format(filename)
            utils.error(LOG,msg,error_codes.EX_IOERR)

        return
