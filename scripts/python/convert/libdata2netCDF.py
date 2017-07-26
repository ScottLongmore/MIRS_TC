#!/usr/bin/python
"""
libdata2netCDF.py - library of data to netCDF variable conversion routines
"""

# Stock modules
import sys
import os
import logging
import traceback
import datetime
import collections
import unicodedata

# Site modules
import numpy as np

try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"lib"))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Loca modulels
import utils
import error_codes

LOG = logging.getLogger(__name__)

# Conversion Routines

# takes a datatime object, and format returns string
def time(time,format="%q"):
    time_string=time.strftime(format)
    return time_string 

def yyyymmddhh_time(year,month,day,hour,format="%q",offset=0):
   
    try: 
        time_string="{}{}{}{}0000".format(year,month,day,hour)
        dtg=datetime.datetime.strptime(time_string, "%Y%m%d%H%M%S")
        if offset:
           delta=datetime.timedelta(seconds=offset)
           if offset>0:
              dtg=dtg+delta
           else:
              dtg=dtg-delta
        new_time_string=dtg.strftime(format)
        return new_time_string
    except:
        msg="yyyymmddhh_time: invalid year:{} month:{} day:{} hour:{} or format:{}".format(year,month,day,hour,format)
        utils.error(LOG,msg,error_codes.EX_DATAERR)

def levels(levels):

    new_levels=[]
    for level in levels:
        new_levels.append(level)

    return new_levels


def latitudes(latitudes):

    new_latitudes=latitudes[0,:,0]
    for lat in new_latitudes:
        latitude(lat)
    return new_latitudes

def latitude(latitude):

    new_latitude=convert2float(latitude)
    if new_latitude < -90.0 or new_latitude > 90.0:
       msg="Latitude {} out of range -90<latitude<90".format(new_latitude)
       utils.error(LOG,msg,error_codes.EX_DATAERR)

    return new_latitude

def longitudes(longitudes):

    new_longitudes=longitudes[:,0,0]
    for lon in new_longitudes:
        longitude(lon)
    return new_longitudes

def longitude(longitude):
  
    new_longitude=convert2float(longitude)
    if longitude<0.0:
       new_longitude=new_longitude+360.0
    else:
       new_longitude=new_longitude

    if new_longitude < 0.0 or new_longitude > 360.0:
       msg="Longitude {} out of range 0<longitude<360".format(new_longitude)
       utils.error(LOG,msg,error_codes.EX_DATAERR)

    return new_longitude

def transpose2D(matrix):

    dims=list(matrix.shape)
    new_matrix=np.zeros(dims[::-1])
    new_matrix[:,:]=matrix[:,:].transpose()

    return new_matrix

def transpose3D(matrix):

    dims=list(matrix.shape)
    new_matrix=np.zeros(dims[::-1])
    for lev in range(0, dims[2]):
        new_matrix[lev,:,:]=matrix[:,:,lev].transpose()

    return new_matrix


def convert2float(value):
      
    value_type=type(value)
    if value_type is not float:
        if value_type.__module__ == 'numpy':
            new_value=value.astype(float)
            return new_value
        elif value_type is str:
            new_value=float(value)
            return new_value
        else:
            msg="Unable to convert {} type {} to float ".format(value,value_type)
            utils.error(LOG,msg,error_codes.EX_DATAERR)
    else:
        return value
