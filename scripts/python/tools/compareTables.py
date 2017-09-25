#!/usr/bin/python
"""
compareTables.py - compare 2 tables output by poolQueryCoordText 

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
LOG = logging.getLogger("compareTables") #create the logger for this file
setup_logging.setup_logging("compareTables","ct.log")
LOG.info("Starting: {}".format(os.path.basename(__file__))) 


# Read command line arguments 
options = argparse.ArgumentParser()
options.add_argument("-1", "--table1", dest="table1", help="table 1 file")
options.add_argument("-2", "--table2", dest="table2", help="table 2 file")
options.add_argument("-o", "--output", dest="output", help="output table file")
try:
    args = options.parse_args()
except:
    msg='Syntax: python compareTable -1 <table1> -2 <table2>'
    utils.error(LOG,msg,error_codes.EX_USAGE)

tbl1File=args.table1
tbl2File=args.table2
outFile=args.output

tbl1FH=open(tbl1File,"r")
tbl2FH=open(tbl2File,"r")

outFH=open(outFile,"w")

hFields1=re.split(" +",tbl1FH.readline().rstrip())
hFields2=re.split(" +",tbl2FH.readline().rstrip())
outFH.write("{:7s} {:6s} {:4s} ".format(hFields1[0],hFields1[1],hFields1[2]))
for hField1 in (hFields1[3:]):

    outFH.write("{:10s} ".format(hField1+"1"))
    outFH.write("{:10s} ".format(hField1+"2"))

outFH.write("\n")

for line1 in tbl1FH:

    line2=tbl2FH.readline()

    fields1=line1.rstrip().split(' ')
    fields2=line2.rstrip().split(' ')

    outFH.write("{} {} {} ".format(fields1[0],fields1[1],fields1[2]))
    for idx in xrange(3,len(fields1)):

         outFH.write("{} ".format(fields1[idx]))
         outFH.write("{} ".format(fields2[idx]))

    outFH.write("\n")

outFH.close()
