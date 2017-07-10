#!/usr/bin/python
"""
MIRS_TC_lib.py - python driver interface to libMIRS_TC
"""

# Stock modules
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
import csv

# Local modules
import error_codes
from setup_nde_logging import setup_logging
import libMIRS_TC 

# Error Handing Routines 
def error(LOG,msg="Unexpected Error:",code=1):
    LOG.exception(msg)
    sys.exit(code)

# Constants
DTFormat="%Y%m%d%H%M%S%f"

mirs={}

# Log
LOG = logging.getLogger(__file__) #create the logger for this file
logging_setup = 'MIRS_TC_lib'
setup_logging(logging_setup)


# Read Command Line Arguments
parser = argparse.ArgumentParser(prog='MIRS_TC_lib',description="python driver interface to libMIRS_TC")
subparser = parser.add_subparsers()

datetime_p = subparser.add_parser('datetime')
datetime_p.set_defaults(which='datetime')
datetime_p.add_argument("-d", "--initDT", dest="initDT", help="Input DateTime")
datetime_p.add_argument("-m", "--mirs", dest="mirs", help="MIRS data hours")
datetime_p.add_argument("-l", "--list", dest="list", help="List of date/time variables")

poolConfig_p = subparser.add_parser('poolConfig')
poolConfig_p.set_defaults(which='poolConfig')

args = vars(parser.parse_args())
if args['which'] == 'datetime':
    print("datetime")
elif args['which'] == 'poolConfig':
    print("poolConfig")
else:
    print("unknown")
