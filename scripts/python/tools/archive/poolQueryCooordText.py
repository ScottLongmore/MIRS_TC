#!/usr/bin/python
"""
poolQueryCoordText.py - read pool query variable text files and outputs them by lat/lon coordinates 

example:
mirs_tc.py -c <config.json> -l <input list file> -t <time:YYYYMMDDHHMMSS>
"""
    
# System modules
import sys
import os
import re
import datetime
import argparse 
import operator
import collections
import json
import importlib
import logging
import traceback
import pprint
import numpy

pp=pprint.PrettyPrinter(indent=4)

# Set local module paths
try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"lib"))
    sys.path.insert(2,os.path.join(parentPath,"mirs_tc"))
    #print("System Path: {}".format(sys.path))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local modules
import setup_logging
import error_codes
import utils
import MIRS_TC 

# Setup Logging using logging templates
LOG = logging.getLogger("poolQueryCoordText") #create the logger for this file
setup_logging.setup_logging("poolQueryCoordText","pqct.log")
LOG.info("Starting: {}".format(os.path.basename(__file__))) 


# Read command line arguments 
options = argparse.ArgumentParser()
options.add_argument("-c", "--covariables", dest="covariables", help="Coordinate Variable Files e.g. lat.txt,lon.txt")
options.add_argument("-v", "--variables", dest="variables", help="Variable Files e.g. TPW.txt,LWP.txt,Qc.txt")
options.add_argument("-o", "--output", dest="output", help="Output Text File")
try:
    args = options.parse_args()
except:
    msg='Syntax: python poolQueryCoordText.py -i <input text file> <output text file>'
    utils.error(LOG,msg,error_codes.EX_USAGE)

# Read in lat,lon, variable list in

lonFile,latFile=args.covariables.split(',')

lonPTF=MIRS_TC.readPoolTextFile(lonFile)
latPTF=MIRS_TC.readPoolTextFile(latFile)
coords={}
indexes={}
for line in xrange(0,lonPTF['dims'][0]):
    
    idx=lonPTF['records'][0][line]
    lon=lonPTF['records'][1][idx]
    lat=latPTF['records'][1][idx]
    if lon not in coords:
        coords[lon]={}
    if lat not in coords[lon]:
        coords[lon][lat]={}
    coords[lon][lat]['index']=idx
    indexes[idx]={'lon':lon,'lat':lat}
                 

varFiles=args.variables.split(',')
varNames=[]
for varFile in varFiles:

    varName=os.path.basename(varFile).split('.')[0]
    varNames.append(varName)
    varPTF=MIRS_TC.readPoolTextFile(varFile)

    for line in xrange(0,varPTF['dims'][0]):
        idx=lonPTF['records'][0][line]
        var=varPTF['records'][-1][idx]
        lon=indexes[idx]['lon']
        lat=indexes[idx]['lat']
        coords[lon][lat][varName]=var

      
lons=sorted(coords)
for lon in lons:
   lats=sorted(coords[lon])
   for lat in lats:

       sys.stdout.write("{:015.5f},{:015.5f}".format(lon,lat))
       for varName in varNames:
           sys.stdout.write(",{:015.5f}".format(coords[lon][lat][varName]))
       sys.stdout.write("\n")
 
