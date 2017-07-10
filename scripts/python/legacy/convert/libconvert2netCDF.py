#!/usr/bin/python
"""
libconvert2netCDF.py - error handlign, logging and utility routines. 
"""

# Stock modules
import sys
import os
import logging
import traceback
import datetime
import collections

# Site modules
import numpy as np

# Local modules
import error_codes
from setup_nde_logging import setup_logging

# Error Handing Routines 
def error(LOG,msg="Unexpected Error:",code=1):
    LOG.exception(msg)
    sys.exit(code)

