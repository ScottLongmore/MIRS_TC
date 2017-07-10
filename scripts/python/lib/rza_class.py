#/usr/bin/python

import numpy as np
from mw_base_class import *
import re
import os
import sys
#from datetime import datetime
import datetime
import time_subs as ts
import logging
import traceback
import error_codes

#  constants
MS2KNOTS = 1.943844492457398
LATKM = 111.

# RZA data definitions
varNames={
          "rza_p":"PRESSURE",
          "rza_d":"DENSITY",
          "rza_t":"TEMPERATURE",
          "rza_gw":"GRADIENT"
         }
dataNames={y:x for x,y in varNames.iteritems()}

#  create the logger for this file
LOGGER = logging.getLogger(__name__)

class rza(MWBase):
    """
    reads one RZA file and provides interface for accessing
    data from .XYA file

    class variables:
        # ATSF data
            ayear, amonth, aday - year, month,day
            ajday, ahour - julian day (from Jan 1st of current year)
            alat, alon - lat,lon 
            alatm12, alonm12 - lat,lon 12 hr before ahour
            adir - direction (degrees)
            aspeed - knots
            armw - radius of maximum winds  (nautical miles)
            avmx, avmxm12 - intensity, intensity 12 hr earlier (knots) 
            abasin, asnum, asyear, asname - basin, storm number, 2digit year, storm name
        # Swath data
            syear, sjday, smonth, sday, shour, smin, ssec - time
            slat, slon - center of the line
            clat, clon - extrapolated storm position at swath time
        # 2d variables
            rza_p,rza_d,rza_t,rza_gw - pressure,density,temperature,gradient wind 
        # time
            swath_time - swath time, type = python datetime object
        # converted variables
            storm_name,basin,storm_num - storm parameters. Could be read from XYA file
                or from external source (NOT implemented in this version)
            year_str  - year converted to string
            lon0,lat0 - storm position at swath_time. Always betww 0 and 360 deg. 
            
    """
    
    def __init__(self, data_file,all_year=2012,\
                    convert_to_knots = True):

        """
        input:
            required: data_file
            optional:
            all_year = 2012 
            convert_to_knots = True : XYA file has wind in m/s, this
                will convert it to knots
        output:
            class variables
            NOTE: lon0,lat0 - center of the storm. They are read  
                from estimated storm center from .XYA file (clon, clat)
                

        example:

            from rza_class import *

            #  instantiate rza class   
            d = rza(data_file.RZA)
        
        """
        #  read data
        self.read_rza(data_file)

        #  get storm parameters from line 1 of XYA file
        self.storm_name = self.asname
        self.basin = self.abasin
        self.storm_num = self.asnum
        self.year_str = str(self.ayear) 

                
        #  use storm center estimate recorded in line2 (clon, clat)
        #  make sure all lons are always between 0 and 360
        if self.clon <= 0:
            self.lon0 = self.clon+360
        else:
            self.lon0 = self.clon
        self.lat0 = self.clat
        
        #  create var_name--var dictionary to use for saving data
        self.var_dict = self.__var_dict()


    def read_rza(self, rza_file, convert_to_knots = True):

        """
        read ATMS RZA file
        
        input:
            required input: filename
            optional input: 
                 convert_to_knot = True by default

        return: creates class variables 
        """
      
        #  initialize array variables

        self.rza_p    = np.empty(0,dtype=np.float)
        self.rza_d    = np.empty(0,dtype=np.float)
        self.rza_t    = np.empty(0,dtype=np.float)
        self.rza_gw    = np.empty(0,dtype=np.float)

        #  open data file
        try:
            cur_file = open(rza_file,'r')
        except IOError:
            error_msg = 'Cannot open file '+ rza_file
            LOGGER.exception(error_msg)
            sys.exit(error_codes.EX_IOERR)

        
        #  read 1st header line
        # 01 20121027 201230118   29.8  -75.6   28.1  -76.9 034 010 075 065 065 AL1812  SANDY 
        try:
            first_line = cur_file.readline()
            anum_str,atime1_str,atime2_str,alat_str,alon_str,alatm12_str,alonm12_str,adir_str,\
                aspeed_str,armw_str,avmx_str,avmxm12_str,astorm_str,asname_str\
                 = first_line.split()
            self.ayear = int(atime1_str[0:4])
            self.amonth = int(atime1_str[4:6])
            self.aday = int(atime1_str[6:8])
            self.ajday = int(atime2_str[4:7])
            self.ahour = int(atime2_str[7:9])
            self.abasin = astorm_str[0:2]
            self.asnum = int(astorm_str[2:4])
            self.asyear = int(astorm_str[4:6])
            self.asname = asname_str

            anum,self.alat,self.alon,self.alatm12,self.alonm12,\
                    self.adir,self.aspeed,self.armw,self.avmx,self.avmxm12\
                = int(anum_str),float(alat_str),float(alon_str),float(alatm12_str),float(alonm12_str),\
                    int(adir_str),int(aspeed_str),int(armw_str),int(avmx_str),int(avmxm12_str)
        except:
            error_msg1 = 'Error encountered while reading first line of XYA file '+rza_file +'\n'
            error_msg2 = 'cannot read line: '+ first_line
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)

        #  read 2nd header line
        # 01 2012301 180856    29.70   -76.38    29.82   -75.58 
        try:
            second_line = cur_file.readline()
            snum_str, stime1_str,stime2_str,slat_str,slon_str,clat_str,clon_str\
                 = second_line.split()
            self.syear = int(stime1_str[0:4])
            self.sjday = int(stime1_str[4:7])
            self.shour = int(stime2_str[0:2])
            self.smin = int(stime2_str[2:4])
            self.ssec = int(stime2_str[4:6])

            snum, self.slat,self.slon,self.clat,self.clon\
             = int(snum_str), float(slat_str),float(slon_str),float(clat_str),float(clon_str)\
        
        except:
            error_msg1 = 'Error encountered while reading 2nd line of XYA file '+rza_file+'\n'
            error_msg2 = 'cannot read line: '+ second_line
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)
        

        #  calculate swath time
        try:
            #  calculate month and date from julian day
            self.smonth,self.sday = ts.julian_to_date(self.syear,self.sjday)
            self.swath_time = ts.dt_from_60s_timestamp(self.syear,self.smonth,self.sday,\
                self.shour,self.smin,self.ssec)
        except:
            error_msg1 = 'Error encountered while converting swath time from '+rza_file+'\n'
            error_msg2 = 'cannot convert to swath time: %d %d %d %d %d' % (self.syear, self.sjday,\
                    self.shour,self.smin,self.ssec)
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)

        #  read 3nd header line
        # AL1812 102718 10271808  r(km): 31    0.0  600.0 z(km): 21    0.0   20.0 lat= 29.82 lon= -75.59
        try:
            third_line = cur_file.readline()
            fields=third_line.split()
                 
            self.nRange=int(fields[4])
            self.minRange=float(fields[5])
            self.maxRange=float(fields[6])
            self.nHeight=int(fields[8])
            self.minHeight=float(fields[9])
            self.maxHeight=float(fields[10])

            self.resRange=(self.maxRange-self.minRange)/(float(self.nRange)-1.0)
            self.ranges=np.linspace(self.minRange,self.maxRange,self.nRange)
            self.resHeight=(self.maxHeight-self.minHeight)/(float(self.nHeight)-1.0)
            self.heights=np.linspace(self.minHeight,self.maxHeight,self.nHeight)

        except:
            error_msg1 = 'Error encountered while reading 3rd line of XYA file '+rza_file+'\n'
            error_msg2 = 'cannot read line: '+ third_line
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)

        
        #  read the rest of the file
        try:
            for read_line in iter(cur_file):

                #  check that line is not empty (I only saw empty lines at the end of file)
                if len(read_line) > 0:
                    fields = read_line.split()
                else: continue

                #  parse non-empty line
                if fields[0] in dataNames:
                   varName=dataNames[fields[0]]
                elif re.match('[-+]?\d+',fields[0]):
                    setattr(self,varName,np.append(getattr(self,varName),[float(i) for i in fields])) 
                else:
                    error_msg =  'Unexpected data in the file %s Will exit now'  % rza_file
                    raise ValueError(error_msg)
            
        except:
            error_msg =  'Error encountered while reading main data from XYA file '+rza_file
            LOGGER.exception(error_msg)
            sys.exit(error_codes.EX_DATAERR)

        cur_file.close()

        if convert_to_knots:
            #  convert to knots U,V at pressure levels and at the surface
            self.rza_gw = self.rza_gw *MS2KNOTS

        try: 
            #  reshape data

            #  2D variables
            for varName in varNames:
              setattr(self,varName,np.reshape(getattr(self,varName),(self.nRange,self.nHeight),order='F'))

        except:
            error_msg =  'Error encountered while reshaping '+rza_file
            LOGGER.exception(error_msg)
            sys.exit(error_codes.EX_DATAERR)

    def __var_dict(self):

        """
        construct variables dictionary for saving data

        input:
            requred: none
        output: 
            var_atms_dict: dictionary with var_names as keys, and vars as values
        """
        
        var_list=[self.ayear, self.amonth, self.aday, self.ajday, self.ahour, self.alat,\
                 self.alon, self.alatm12, self.alonm12, self.adir, self.aspeed, self.armw,\
                 self.avmx, self.avmxm12, self.abasin, self.asnum, self.asyear, self.asname,\
                 self.syear, self.sjday, self.smonth, self.sday, self.shour, self.smin, self.ssec, self.slat,\
                 self.slon, self.clat, self.clon,\
                 self.rza_p,self.rza_d,self.rza_t,self.rza_gw,\
                 self.swath_time,\
                 self.storm_name,self.basin,self.storm_num,self.year_str,\
                 self.lon0,self.lat0,\
                 self.slon,self.slat]

        var_names_list = ['ayear', 'amonth', 'aday', 'ajday', 'ahour', 'alat',\
                'alon', 'alatm12', 'alonm12', 'adir', 'aspeed', 'armw',\
                'avmx', 'avmxm12', 'abasin', 'asnum', 'asyear', 'asname',\
                'syear', 'sjday','smonth','sday', 'shour', 'smin', 'ssec', 'slat',\
                'slon', 'clat', 'clon',\
                'rza_p', 'rza_d','rza_t','rza_gw',\
                'rza_slp','rza_sclw','rza_st','rza_su','rza_sv',\
                'swath_time',\
                'storm_name','basin','storm_num','year_str',\
                'lon0','lat0',\
                'slon','slat']

        var_rza_dict = dict(zip(var_names_list,var_list))     
        
        return var_rza_dict 


