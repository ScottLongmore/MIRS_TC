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
options.add_argument("-i", "--index", dest="index", help="Index/Lat/Lon Text File")
options.add_argument("-o", "--output", dest="output", help="Output Text File")
try:
    args = options.parse_args()
except:
    msg='Syntax: python poolQueryCoordText.py -i <input text file> <output text file>'
    utils.error(LOG,msg,error_codes.EX_USAGE)

lonFile,latFile=args.covariables.split(',')
varFiles=args.variables.split(',')
idxFile=args.index
outFile=args.output

# Read in lat,lon, variable list in
lonPTF=MIRS_TC.readPoolTextFile(lonFile)
latPTF=MIRS_TC.readPoolTextFile(latFile)
coords={}
indexes={}
idxFH=open(idxFile,"w")
for line in xrange(0,lonPTF['dims'][0]):
    
    idx=lonPTF['records'][0][line]
    lon=lonPTF['records'][1][idx]
    lat=latPTF['records'][1][idx]
    if lon not in coords:
        coords[lon]={}
    if lat not in coords[lon]:
        coords[lon][lat]={}
    #coords[lon][lat]['index']=idx
    indexes[idx]={'lon':lon,'lat':lat}
    idxFH.write("{:09d} {:015.5f} {:015.5f}\n".format(idx,lon,lat))

idxFH.close()
                 
varNames=[]
for varFile in varFiles:

    varName=os.path.basename(varFile).split('.')[0]
    varNames.append(varName)
    varPTF=MIRS_TC.readPoolTextFile(varFile)

    for idx,record in enumerate(list(zip(*varPTF['records']))): 

        vidx=record[0]
        if vidx in indexes:
           lon=indexes[vidx]['lon']
           lat=indexes[vidx]['lat']
        else: 
           LOG.warning("Variable: {} index: {} not found in lat/lon indexes, skipping".format(varname,idx))
           continue 
        
        #sys.stdout.write("Indexes: {} {}\n".format(idx,vidx))

        if varPTF['rank'] == 2:
           sidx=record[1]
        elif varPTF['rank'] == 1:
           sidx=-999
        else: 
           LOG.warning("Variable: {} rank: {} not defined, skipping".format(varname,varPTF['rank']))
           continue 

        if sidx not in coords[lon][lat]:
            coords[lon][lat][sidx]={}

        coords[lon][lat][sidx][varName]=record[-1]

        #print("{} {} {} {} {}".format(lon,lat,varName,sidx,coords[lon][lat][varName][sidx]))

      
outFH=open(outFile,"w")

# Header
outFH.write("{:7s} {:6s} ".format("Lon","Lat"))
outFH.write("{:4s} ".format("Idx2"))
for varName in varNames:
    outFH.write("{:10s} ".format(varName))
outFH.write("\n")

# Data
lons=sorted(coords)
for lon in lons:
   lats=sorted(coords[lon])
   for lat in lats:
       sidxs=sorted(coords[lon][lat])
       for sidx in sidxs:
            outFH.write("{:07.2f} {:06.2f} ".format(lon,lat))
            if sidx == -999:
                outFH.write("---- ".format(sidx))
            else:
                outFH.write("{:04d} ".format(sidx))
            for varName in varNames:
               if varName in coords[lon][lat][sidx]:
                   outFH.write("{:010.5f} ".format(coords[lon][lat][sidx][varName]))
               else:
                   outFH.write("{} ".format('-'*10))
            outFH.write("\n")

outFH.close()
