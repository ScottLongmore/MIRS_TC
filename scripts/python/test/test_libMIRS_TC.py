#!/usr/bin/python
"""
libMIRS_TC.py - MIRS TC library 
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
import numpy
import pandas as pd

# Local modules
import error_codes
from setup_nde_logging import setup_logging
import libMIRS_TC

LOG = logging.getLogger(__name__)

inputDF=libMIRS_TC.readShortTermTrackInputFile("./al072015.inp")

#print(inputDF.to_string())
inputDict=inputDF.loc[inputDF['DELTA_T'] == 0].to_dict(orient='index')

for DT in inputDict:
    print "{}".format(DT.isoformat()),
    for col in inputDict[DT]:
       print " {}:{}".format(col,inputDict[DT][col]),
    print("\n")
