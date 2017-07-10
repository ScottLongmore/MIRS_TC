#!/usr/bin/python
'''
fileAction.py - Retrieves a list of files per input given directory and file specifications
                Performs specfied subroutines on found input files from specified subroutine library

                Library defined subroutines that take a dictionary an argument, add/remove/modify entries 
                in the dictionary and return true if the subroutine succeeds.

'''
    
# Stock modules
import sys
import os
import re
import datetime
import optparse 
import operator
import collections
import json
import importlib
import logging
import traceback
import copy
from subprocess import call

# Local modules
import dirRE
from utils import *
import error_codes

LOG = logging.getLogger(__name__)

# Class Variables
config_keys=['inputs']
input_keys=['dirs','re']

class fileAction(object):

    def __init__(self,fileActionOption):

        self.fileActionOption=fileActionOption

        # Determine if JSON file or dict
        if isinstance(fileActionOption,str) and os.path.isfile(fileActionOption):
             try:
                 self.config=json.load(open(fileActionOption),object_pairs_hook=collections.OrderedDict)
             except:
                 msg="Error in JSON config file: {}".format(fileActionOption)
                 error(LOG,msg,error_codes.EX_IOERR)
        elif isinstance(fileActionOption,dict):
            self.config=fileActionOption
        else:
            msg="file action option must be valid JSON file or dictionary"
            error(LOG,msg,error_codes.EX_IOERR)

        # Make sure config keys are defined...will be replaced with json schema
        if not all(atts in self.config for atts in config_keys):
            msg="Required attributes {} not all found in config data".format(config_keys)
            error(LOG,msg,error_codes.EX_IOERR)

        # Make sure input keys are defined...will be replaced with json schema 
        for input in self.config['inputs']:
            if not all(atts in self.config['inputs'][input] for atts in input_keys):
                msg="Required input attributes {} for {} not all found in config data".format(input_keys,input)
                error(LOG,msg,error_codes.EX_IOERR)

    def findInputFiles(self,inputs=[]):

        if not inputs:
             inputs=self.config['inputs'].keys()

        # Retrieve input files
        returnFiles={}
        for inputKey in inputs: 
    
            #print "\tINPUT: {}".format(inputKey)
            input=self.config['inputs'][inputKey]
    
            # Replace dir/re templates (%string) with replace section values to narrow input searches
            if self.config['replace']:
               # print json.dumps(self.config['replace'],indent=4)
               newDirs=[]
               for dir in input['dirs']:
                   newSubDirs=[]
                   for subdir in splitall(dir)[1:]:
                       newSubDirs.append(repStrTmpl(subdir,self.config['replace']))
                   newDir='/'+str('/'.join(newSubDirs))
                   newDirs.append(newDir)

                   #print "\t\tDIRS: {}".format(dir)
                   #print "\t\tNEWDIRS: {}".format(newDirs[-1])
               input['dirs']=newDirs
               #print "\t\tRE: {}".format(input['re'])

               input['re']=repStrTmpl(input['re'],self.config['replace'])
               #print "\t\tNEWRE: {}".format(input['re'])
            
            # Parse and retrieve file search directories 
            #print "RDIRS: {}".format(input['dirs'])
            input['dirs']=parseDirEntries(input['dirs']) 
            #print "NEWRDIRS: {}".format(input['dirs'])
    
            input['files']=collections.OrderedDict()
            files=input['files']
            for dir in input['dirs']: 
    
                # print "Dir: {}".format(dir) 
                try:
                    for file in sorted(os.listdir(dir)):
                        filepath=os.path.join(dir,file)
                        # print "\tFile: {}".format(filepath)
                        m=re.match(input['re'], file)
                        if m:
                            files[filepath]={}
                            files[filepath]['ctime']=datetime.datetime.fromtimestamp(os.path.getctime(filepath))
                            files[filepath]['mtime']=datetime.datetime.fromtimestamp(os.path.getmtime(filepath))
                            files[filepath]['atime']=datetime.datetime.fromtimestamp(os.path.getatime(filepath))
                except:
                    msg="Problem reading {} input directory: {}".format(inputKey,dir)
                    error(LOG,msg,error_codes.EX_IOERR)

            returnFiles[inputKey]=files.keys()
      
        return(returnFiles)

    def getInputFiles(self,inputs=[]):

        if not inputs:
             inputs=self.config['inputs'].keys()

        returnFiles={}
        for inputKey in inputs:
            returnFiles[inputKey]=self.config['inputs'][inputKey]['files'].keys()

        return(returnFiles)

    def actions(self,actions=[]):

        if not actions:
             actions=self.config['actions'].keys()

        for action in actions:

            try: 
                module=importlib.import_module(self.config['actions'][action]['module'])
                args=[self.config]
                files=getattr(module,action)(*args)
            except:
                msg="Problem in module: {} action: {}".format(self.config['actions'][action]['module'],action)
                error(LOG,msg,error_codes.EX_IOERR)



