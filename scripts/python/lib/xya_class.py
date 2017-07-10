#/usr/bin/python

import numpy as np
from mw_base_class import *
import re
import os
import sys
from datetime import datetime
import time_subs as ts
import logging
import traceback
import error_codes

#import pdb
#import traceback
# --- error codes
#error_codes.EX_IOERR = 74
#error_codes.EX_DATAERR = 65

# --- constants
MS2KNOTS = 1.943844492457398
LATKM = 111.

# --- create the logger for this file
LOGGER = logging.getLogger(__name__)

class xya(MWBase):
    """
    reads one XYA file and provides interface for accessing
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
        # Coordinate variables
            xya_p, xya_lon, xya_lat
        # 3d variables
            xya_u,xya_v,xya_t,xya_z
        # Surface coordinates
            xya_slon, xya_slat
        # 2d surface variables
            xya_su, xya_sv, xya_st - U,V,Temp
            xya_slp,xya_sclw - Sea Level Pressure, Cooud Liquid Water
        # time
            swath_time - swath time, type = python datetime object
        # converted variables
            storm_name,basin,storm_num - storm parameters. Could be read from XYA file
                or from external source (NOT implemented in this version)
            year_str  - year converted to string
            lon0,lat0 - storm position at swath_time. Always betww 0 and 360 deg. 
            xya_lon,xya_lat - alat, alon with longitudes converted to be between 0 and 360 deg
            xya_slon,xya_slat - with longitudes converted to be between 0 and 360 deg
        # dictionaries
            p_levels_dict - key = string(p_levels in mb), and value = corresponding index
            self.var_dict - key = 'var_name' (string), and value = corresponding variable
        # subroutines
            get_pressure_slice(self,var3d,press_mb)  get 3d var slice at pressure level
            
            
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

            from xya_class import *

            # --- instantiate xya class   
            d = xya(data_file.XYA)
        
        """
        # --- read data
        self.read_xya(data_file)

        # --- create dictinary with key = string(p_levels in mb), and value = corresponding index 
        self.p_levels_str = self.__make_p_levels_str(self.xya_p)
        self.p_levels_dict = self.make_p_levels_dict(self.p_levels_str)

        # --- get storm parameters from line 1 of XYA file
        self.storm_name = self.asname
        self.basin = self.abasin
        self.storm_num = self.asnum
        self.year_str = str(self.ayear) 

                
        # --- use storm center estimate recorded in line2 (clon, clat)
        # --- make sure all lons are always between 0 and 360
        if self.clon <= 0:
            self.lon0 = self.clon+360
        else:
            self.lon0 = self.clon
        self.lat0 = self.clat
        
        
        # --- make sure all lons are always between 0 and 360
        lon_mask = self.xya_lon < 0
        self.lon = self.xya_lon
        self.lon[lon_mask] += 360.            
        self.lat = self.xya_lat           
       
        # --- make sure all surface lons are always between 0 and 360
        slon_mask = self.xya_slon < 0
        self.slon = self.xya_slon
        self.slon[slon_mask] += 360.            
        self.slat = self.xya_slat           
           
        # --- create var_name--var dictionary to use for saving data
        self.var_dict = self.__var_dict()


    def read_xya(self, xya_file, convert_to_knots = True):

        """
        read ATMS XYA file
        
        input:
            required input: filename
            optional input: 
                 convert_to_knot = True by default

        return: creates class variables 
        """
      
        # --- initialize array variables
        self.xya_p    = np.empty(0,dtype=np.float)
        self.xya_lat  = np.empty(0,dtype=np.float) 
        self.xya_lon  = np.empty(0,dtype=np.float) 
        self.xya_u   = np.empty(0,dtype=np.float)
        self.xya_v   = np.empty(0,dtype=np.float)
        self.xya_t   = np.empty(0,dtype=np.float)
        self.xya_z   = np.empty(0,dtype=np.float)
        self.xya_slat    = np.empty(0,dtype=np.float)
        self.xya_slon    = np.empty(0,dtype=np.float)
        self.xya_su  = np.empty(0,dtype=np.float)
        self.xya_sv  = np.empty(0,dtype=np.float)
        self.xya_st  = np.empty(0,dtype=np.float)
        self.xya_slp = np.empty(0,dtype=np.float)
        self.xya_sclw = np.empty(0,dtype=np.float)

        # --- open data file
        try:
            cur_file = open(xya_file,'r')
        except IOError:
            error_msg = 'Cannot open file '+ xya_file
            LOGGER.exception(error_msg)
            sys.exit(error_codes.EX_IOERR)

        
        # --- read 1st header line
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
            error_msg1 = 'Error encountered while reading first line of XYA file '+xya_file +'\n'
            error_msg2 = 'cannot read line: '+ first_line
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)


        # --- read 2nd header line
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
            error_msg1 = 'Error encountered while reading 2nd line of XYA file '+xya_file+'\n'
            error_msg2 = 'cannot read line: '+ second_line
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)
        

        # --- calculate swath time
        try:
            # --- calculate month and date from julian day
            self.smonth,self.sday = ts.julian_to_date(self.syear,self.sjday)
            self.swath_time = ts.dt_from_60s_timestamp(self.syear,self.smonth,self.sday,\
                self.shour,self.smin,self.ssec)
        except:
            error_msg1 = 'Error encountered while converting swath time from '+xya_file+'\n'
            error_msg2 = 'cannot convert to swath time: %d %d %d %d %d' % (self.syear, self.sjday,\
                    self.shour,self.smin,self.ssec)
            LOGGER.exception(error_msg1+error_msg2)
            sys.exit(error_codes.EX_DATAERR)
        

        
        # --- read the rest of the file
        try:
            prs_level_data = False
            sfc_level_data = False
            for read_line in iter(cur_file):

                # --- check that line is not empty (I only saw empty lines at the end of file)
                if len(read_line) > 0:
                    split_line = read_line.split()
                else: continue

                # --- parse non-empty line
                if split_line[0] == 'Pressure':
                    # --- header line for pressure level data
                    prs_level_data = True
                    # --- read pressure level
                    current_plev = split_line[-3]
                    # --- read 'Pressure level (Pa)=100000.0', where there is no space
                    # --- after '='
                    if re.match('.*=.*',current_plev):
                        current_plev = current_plev.split('=')[-1]
                    self.xya_p = np.append(self.xya_p,float(current_plev))
                    # --- read number of lat,lon points
                    nlat = int(split_line[-2].split('=')[-1])
                    nlon = int(split_line[-2].split('=')[-1])
                elif split_line[0] == 'Lat':
                    # --- header line with column names
                    pass
                elif split_line[0] == 'Surface':
                    # --- header line for surface data
                    prs_level_data = False
                    sfc_level_data = True
                elif re.match('[-+]?\d+',split_line[0]) and prs_level_data is True and sfc_level_data is False:
                    # --- read data for pressure level
                    self.xya_lat = np.append(self.xya_lat,float(split_line[0]))
                    self.xya_lon = np.append(self.xya_lon,float(split_line[1]))
                    self.xya_u = np.append(self.xya_u,float(split_line[2]))
                    self.xya_v = np.append(self.xya_v,float(split_line[3]))
                    self.xya_t = np.append(self.xya_t,float(split_line[4]))
                    self.xya_z = np.append(self.xya_z,float(split_line[5]))
                elif re.match('[-+]?\d+',split_line[0]) and prs_level_data is False and sfc_level_data is True:
                    # --- read data for surface
                    self.xya_slat = np.append(self.xya_slat,float(split_line[0]))
                    self.xya_slon = np.append(self.xya_slon,float(split_line[1]))
                    self.xya_su = np.append(self.xya_su,float(split_line[2]))
                    self.xya_sv = np.append(self.xya_sv,float(split_line[3]))
                    self.xya_st = np.append(self.xya_st,float(split_line[4]))
                    self.xya_slp = np.append(self.xya_slp,float(split_line[5]))
                    self.xya_sclw = np.append(self.xya_sclw,float(split_line[6]))
                else:
                    #print split_line
                    error_msg =  'Unexpected data in the file %s Will exit now'  % xya_file
                    raise ValueError(error_msg)
            
        except:
            error_msg =  'Error encountered while reading main data from XYA file '+xya_file
            LOGGER.exception(error_msg)
            sys.exit(error_codes.EX_DATAERR)

        cur_file.close()

        if convert_to_knots:
            # --- convert to knots U,V at pressure levels and at the surface
            self.xya_u = self.xya_u *MS2KNOTS
            self.xya_v = self.xya_v *MS2KNOTS
            self.xya_su = self.xya_su *MS2KNOTS
            self.ya_sv = self.xya_sv *MS2KNOTS

        try: 
            # --- reshape data
            npres = len(self.xya_p)  #number of pressure levels
            # --- 3d variables
            # --- MUST use order='F' to get correct fields!!!!! 
            self.xya_lat = np.reshape(self.xya_lat,(nlon,nlat,npres),order='F')
            self.xya_lon = np.reshape(self.xya_lon,(nlon,nlat,npres),order='F')
            self.xya_u = np.reshape(self.xya_u,(nlon,nlat,npres),order='F')
            self.xya_v = np.reshape(self.xya_v,(nlon,nlat,npres),order='F')
            self.xya_t = np.reshape(self.xya_t,(nlon,nlat,npres),order='F')
            self.xya_z = np.reshape(self.xya_z,(nlon,nlat,npres),order='F')
            

            # --- surface variables
            self.xya_slat = np.reshape(self.xya_slat,(nlon,nlat),order='F') 
            self.xya_slon = np.reshape(self.xya_slon,(nlon,nlat),order='F')
            self.xya_su = np.reshape(self.xya_su,(nlon,nlat),order='F')
            self.xya_sv = np.reshape(self.xya_sv,(nlon,nlat),order='F')
            self.xya_st = np.reshape(self.xya_st,(nlon,nlat),order='F')
            self.xya_slp = np.reshape(self.xya_slp,(nlon,nlat),order='F')
            self.xya_sclw = np.reshape(self.xya_sclw,(nlon,nlat),order='F')
        except:
            error_msg =  'Error encountered while reshaping '+xya_file
            LOGGER.exception(error_msg)
            sys.exit(error_codes.EX_DATAERR)

    def get_pressure_slice(self,var3d,press_mb,var_name):
        """
        returns slice of 3d class variables at given pressure level
        this sub assumes 3d var dimentions specific to XYA files
        input:
            required:
                3d class var
                pressure level
        """
        
        level = self.p_level_idx(press_mb)
        try:
            var3d_at_plev = var3d[:,:,level]
        except:
            error_msg =  'Cannot create slice of 3d var '+var_name+'\n var shape is '+ str(np.shape(var3d) )
            raise ValueError(error_msg)
            

        return var3d_at_plev 


    def __make_p_levels_str(self, p_levels):

        """
        returns arrays of p_levels as strings,
        output is used to create dictionary 
        of pressure levels and corresponding strings
        which are used for plotting
        (see __make_p_levels_dict in MWBase class) 
        input:
            required: 
                p_levels: list of pressure levels as floats
        output: p_levels_str - pressure levels strings array, 
            usually in units of mb, in a format convinient for
            display on plots


        # --- XYA pressure levels
        #    --- total of 12 pressure levels
        #   -0- Pressure level (Pa)= 100 00.0 nlat=61 nlon=61
        #   -1- Pressure level (Pa)= 150 00.0 nlat=61 nlon=61
        #   -2- Pressure level (Pa)= 200 00.0 nlat=61 nlon=61
        #   -3- Pressure level (Pa)= 250 00.0 nlat=61 nlon=61
        #   -4- Pressure level (Pa)= 300 00.0 nlat=61 nlon=61
        #   -5- Pressure level (Pa)= 400 00.0 nlat=61 nlon=61
        #   -6- Pressure level (Pa)= 500 00.0 nlat=61 nlon=61
        #   -7- Pressure level (Pa)= 600 00.0 nlat=61 nlon=61
        #   -8- Pressure level (Pa)= 700 00.0 nlat=61 nlon=61
        #   -9- Pressure level (Pa)= 850 00.0 nlat=61 nlon=61
        #   -10-Pressure level (Pa)= 925 00.0 nlat=61 nlon=61
        #   -11-Pressure level (Pa)=1000 00.0 nlat=61 nlon=61
        """



        # --- to make floats with one digit after comma
        # --- convert to mb and round
        p_levels_str = [str(round( float(  int(round(float(i),0))/100  ),1 )) for i in p_levels]

        return p_levels_str

    
    def __var_dict(self):

        """
        construct variables dictionary for saving data

        input:
            requred: none
        output: 
            var_atms_dict: dictionary with var_names as keys, and vars as values
        """
        
        var_list = [self.ayear, self.amonth, self.aday, self.ajday, self.ahour, self.alat,\
                self.alon, self.alatm12, self.alonm12, self.adir, self.aspeed, self.armw,\
                self.avmx, self.avmxm12, self.abasin, self.asnum, self.asyear, self.asname,\
            self.syear, self.sjday, self.smonth, self.sday, self.shour, self.smin, self.ssec, self.slat,\
                self.slon, self.clat, self.clon,\
            self.xya_p, self.xya_lon, self.xya_lat,\
                self.xya_u,self.xya_v,self.xya_t,self.xya_z,\
                self.xya_slp,self.xya_sclw,self.xya_st,self.xya_su,self.xya_sv,\
                self.xya_slat, self.xya_slon,\
                self.swath_time,\
                self.storm_name,self.basin,self.storm_num,self.year_str,\
                self.lon0,self.lat0,\
                self.lon,self.lat,\
                self.slon,self.slat]

        var_names_list = ['ayear', 'amonth', 'aday', 'ajday', 'ahour', 'alat',\
                'alon', 'alatm12', 'alonm12', 'adir', 'aspeed', 'armw',\
                'avmx', 'avmxm12', 'abasin', 'asnum', 'asyear', 'asname',\
            'syear', 'sjday','smonth','sday', 'shour', 'smin', 'ssec', 'slat',\
                'slon', 'clat', 'clon',\
            'xya_p', 'xya_lon', 'xya_lat',\
                'xya_u','xya_v','xya_t','xya_z',\
                'xya_slp','xya_sclw','xya_st','xya_su','xya_sv',\
                'xya_slat', 'xya_slon',\
                'swath_time',\
                'storm_name','basin','storm_num','year_str',\
                'lon0','lat0',\
                'lon','lat',\
                'slon','slat']

        var_xya_dict = dict(zip(var_names_list,var_list))     
        
        return var_xya_dict 


