#/usr/bin/python
'''
data2netCDF - creates a data instance from given module, class and data file,
              profiles data variables, and converts specified data variables to
              netCDF variables using conversion routines specified in config.
'''

# Stock Modules
import re
import os
import sys
import datetime
import logging
import traceback
import collections
import importlib
from inspect import getmembers, isfunction

# System Modules
import numpy as np

# Local Modules
import error_codes
from libconvert2netCDF import *
import libdata2netCDF

# --- create the logger for this file
LOG = logging.getLogger(__name__)

dataKey="data_names"
convertKey="conversion"

# Create dictionary of conversion functions in convert2netCDF
converters={}
for func in [o for o in getmembers(libdata2netCDF) if isfunction(o[1])]:
    converters[func[0]]=func[0]


class data2netCDF:
    """
    data2netCDF: dynamic wrapper class to translate module class data 
                 to CF netCDF convention formatted data 
    """
    
    def __init__(self, config, module_name, class_name, data_file):

        """
        Constructor: creates data class from data_file, translates/converts
                     variables to CF netCDF variables and stores them in
                     mixed type dictionary 
        
        """
        self.dataVarNames=[]
        self.ncVars=collections.OrderedDict()

        # Import data class and create list of class data variable names 
        module=importlib.import_module(module_name)
        module_class=getattr(module,class_name)
        self.data = module_class(data_file)
        self.dataVarNames=sorted(vars(self.data).iterkeys())
        
        # Create dictonary of variables that need to be converted
        ncVars=collections.OrderedDict() # list of data varaibles by netCDF variable name
        convertVars=collections.OrderedDict() # conversion routines and arguments by netCDF variable name 
        for varName in config["Variables"]:
            variable=config["Variables"][varName]
    
            if dataKey in variable:
                ncVars[varName]=variable[dataKey]
                if convertKey in variable:
                    convertVars[varName]=variable[convertKey]

        # Copy/Convert data varaibable(s) to netCDF variable 
        for ncVarName in ncVars:

           ncDataVarNames=ncVars[ncVarName]
           commonDataVarNames=list(set(self.dataVarNames).intersection(set(ncDataVarNames)))

           if sorted(ncDataVarNames) != sorted(commonDataVarNames):
               msg="Variable {} config/data mismatch/not found in {} attribute".format(ncVarName,dataKey)
               error(LOG,msg,error_codes.EX_CONFIG)
           else:
               # Copy variable directly to netCDF variable
               if ncVarName not in convertVars:
                   # Warning if more than more data argument
                   if not len(ncDataVarNames):
                       msg="Variable {} has >1 data variable in {} attribute, copying first".format(ncVarName,dataKey)
                   self.ncVars[ncVarName]=getattr(self.data,ncDataVarNames[0])
               else:
                   # Build function call command and arguments
                   func=convertVars[ncVarName][0]
                   args=[]
                   if func in converters:
                       # Append data variables
                       for ncDataVarName in ncDataVarNames:
                           args.append(getattr(self.data,ncDataVarName))
#                           print "{} {} {} {}".format(ncVarName,func,ncDataVarName,type(args[-1]))
                       # Append config arguments
                       for carg in convertVars[ncVarName][1:len(convertVars[ncVarName])]:
                           args.append(carg)
                       # Convert data variable(s) to CF netCDF formatted variable
#                      print "{} {}".format(ncVarName,func)
                       self.ncVars[ncVarName]=getattr(libdata2netCDF,converters[func])(*args)
#                       print "{} {} {}".format(ncDataVarName,type(self.ncVars[ncVarName]),self.ncVars[ncVarName])
                   else:
                       msg="Conversion function {} for variable {} not found in config file".format(func,ncVarName)
                       error(LOG,msg,error_codes.EX_CONFIG)
