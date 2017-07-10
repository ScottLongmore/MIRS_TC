#!/usr/bin/python
"""
data2netCDF.py - converts a data input file to CF netCDF using a JSON configuration
                 that specifies the data class reader, input and output variables, 
                 attributes and conversion routines. The parameter file is currently 
                 used to include external data not in the data input file.

example:
python data2netCDF.py -c xyaNetCDF.json -p npp_tc_params.json -i al182012_2012102312_NOAA15.XYA -o al182012_2012102312_NOAA15_XYA.nc 
python data2netCDF.py -c rzaNetCDF.json -p npp_tc_params.json -i al182012_2012102312_NOAA15.RZA -o al182012_2012102312_NOAA15_RZA.nc 
"""
    
# Stock modules
import sys
import os
import logging
import traceback
import datetime
import optparse 
import json
import collections

# Site modules
import numpy as np
from netCDF4 import Dataset

# Local modules
import error_codes
from libconvert2netCDF import *
from data2netCDF import * 
from setup_nde_logging import setup_logging

# Variables
timeFormat="%y%m%d%H%M%S%f"
outDTGFrmt="%Y%m%d%H%M%S%f"
cfDimVarAttrs=["standard_name","long_name","units","axis"]
cfVarAttrs=["standard_name","long_name","units","missing_value","valid_range"]

# Log
LOG = logging.getLogger(__file__) #create the logger for this file
logging_setup = 'MIRS_TC'
setup_logging(logging_setup)

# Read Command Line Arguments 
options = optparse.OptionParser()
options.add_option("-c", "--config", dest="config", help="Configuration File")
options.add_option("-p", "--params", dest="params", help="Params File")
options.add_option("-i", "--input",  dest="input", help="Input Data File")
options.add_option("-o", "--output", dest="output", help="Output netCDF File")

try:
    (opts, args) = options.parse_args()
    config_filename=opts.config
    params_filename=opts.params
    input_filename=opts.input
    output_filename=opts.output
except:
    msg='Syntax: python xya2netCDF.py -c <config file> -i <input XYA file> -o <output netCDF file>'
    error(LOG,msg,error_codes.EX_USAGE)

# Read Configuration File
try:
    LOG.info("Processing JSON config file: {}".format(config_filename)) 
    config=json.load(open(config_filename),object_pairs_hook=collections.OrderedDict)
except:
    msg="Error in JSON config file: {}".format(config_filename)
    error(LOG,msg,error_codes.EX_IOERR)

# Data section
try:
    data_module=config["Data"]["module"]
    data_class=config["Data"]["class"]
except:
    msg="Data[] section does not contain module/class attribues in config file: {}".format(config_filename)
    error(LOG,msg,error_codes.EX_CONFIG)

# Dimensions section
try:
    dimensions=config["Dimensions"].copy()
except:
    msg="Dimensions[] section not defined in config file: {}".format(config_filename)
    error(LOG,msg,error_codes.EX_CONFIG)

# Variables section
try:
    variables=config["Variables"].copy()
except:
    msg="Variables[] section not defined in config file:{}".format(config_filename)
    error(LOG,msg,error_codes.EX_CONFIG)

# Global Attributes section
try:
    attributes=config["Attributes"].copy()
except:
    msg="Attributes[] section not defined in config file:{}".format(config_filename)
    error(LOG,msg,error_codes.EX_CONFIG)

# Read Params File
try:
    LOG.info("Processing JSON params file: {}".format(params_filename)) 
    params=json.load(open(params_filename),object_pairs_hook=collections.OrderedDict)
except:
    msg="Error in JSON config file: {}".format(params_filename)
    error(LOG,msg,error_codes.EX_IOERR)

# Params Section
try:
    params=params["Params"].copy()
except:
    msg="Params[] section not defined in params file:{}".format(params_filename)
    error(LOG,msg,error_codes.EX_CONFIG)


# Create netCDF file
LOG.info("Creating netCDF file: {} from configuration information".format(output_filename)) 
try:
    rootGrp = Dataset(output_filename, 'w', format='NETCDF4')
    nc_variables={}
except:
    msg="Unable to open output netCDF file: {}".format(output_filename)
    error(LOG,msg,error_codes.EX_IOERR)

# Create Dimensions and Dimension Variables
dimVarDims={}
for dimName in dimensions:
    try:
        dimSize=dimensions[dimName]
        rootGrp.createDimension(dimName,dimSize)
    except:
        msg="Dimension {}={:d} invalid (name=size) in config file {}:".format(dimName,dimSize,config_filename)
        error(LOG,msg,error_codes.EX_CONFIG)

    try:
        variable=variables[dimName] 
    except:
        msg="Matching dimension {} variable not found in config file {}".format(dimName,config_filename)
        error(LOG,msg,error_codes.EX_CONFIG)
    try:
        nc_variables[dimName]=rootGrp.createVariable(dimName, \
                                                     variable["type"], \
                                                     dimensions=variable["dimensions"])
    except:
        msg="Invalid type/dimensions {} variable attributes in config file {}".format(dimName,config_filename)
        error(LOG,msg,error_codes.EX_CONFIG)

    for attrName in cfDimVarAttrs:
        try:
            nc_variables[dimName].setncattr(attrName,variable[attrName])

            if attrName == "axis" and variable[attrName] == "Z":
                if "positive" in variable: 
                    nc_variables[dimName].setncattr("positive",variable["positive"])
                else:
                    msg="Z Dimension variable {} must have positive attribute defined in config file {}".format(dimName,config_filename)
                    error(LOG,msg,error_codes.EX_CONFIG)
 
        except:
            msg="Mandatory variable {} attribute {} missing/invalid in config file {}".format(dimName,attrName,config_filename)
            error(LOG,msg,error_codes.EX_CONFIG)

    nDims=len(variable["dimensions"])
    if nDims in dimVarDims:
       dimVarDims[nDims].append(dimName)
    else:
       dimVarDims[nDims]=[dimName]
        
    variables.pop(dimName)

# Create Data Variables and Attributes
  
varDims={}
for varName in variables:

    variable=variables[varName]
    try:
        nc_variables[varName]=rootGrp.createVariable(varName, \
                                                     variable["type"], \
                                                     dimensions=variable["dimensions"], \
                                                     fill_value=variable["_FillValue"]) 
    except:
        msg="Invalid type/dimensions/_FillValue {} variable attributes in config file {}".format(dimName,config_filename)
        error(LOG,msg,error_codes.EX_CONFIG)

    for attrName in cfVarAttrs:
        try:
            nc_variables[varName].setncattr(attrName,variable[attrName])
        except:
            msg="Mandatory variable {} attribute {} missing/invalid in config file {}".format(varName,attrName,config_filename)
            error(LOG,msg,error_codes.EX_CONFIG)

  
    # Create varDims dict=[list] (dimSize=["var1","varN"]) of variables with 0-N dimensions
    nDims=len(variable["dimensions"])
    if nDims in varDims:
       varDims[nDims].append(varName)
    else:
       varDims[nDims]=[varName]


# Convert and Store Variables into netCDF

# Read data class file
LOG.info("Processing data input file {}".format(input_filename)) #this will be ignored by NDE
data = data2netCDF(config,data_module,data_class,input_filename)

#for dataVarName in data.dataVarNames:
#     print dataVarName

LOG.info("Writing netCDF file: {} from input data".format(output_filename)) 

for dim in sorted(dimVarDims):

    for varName in dimVarDims[dim]:
        if varName in data.ncVars:
           nc_variables[varName][:]=data.ncVars[varName]

for dim in sorted(varDims):

    for varName in varDims[dim]:
        if varName in data.ncVars:
            nc_variables[varName][:]=data.ncVars[varName]

# Hardwired section

netcdfDTGFrmt="%Y-%m-%dT%H:%M:%SZ"
rootGrp.setncattr("Metadata_Link",output_filename)
dtg=datetime.datetime.strptime(params["date_created"].encode("ascii","ignore"),outDTGFrmt)
rootGrp.setncattr("date_created",dtg.strftime(netcdfDTGFrmt))
dtg=datetime.datetime.strptime(params["time_coverage_start"].encode("ascii","ignore"),outDTGFrmt)
rootGrp.setncattr("time_coverage_start",dtg.strftime(netcdfDTGFrmt))
dtg=datetime.datetime.strptime(params["time_coverage_end"].encode("ascii","ignore"),outDTGFrmt)
rootGrp.setncattr("time_coverage_end",dtg.strftime(netcdfDTGFrmt))
rootGrp.setncattr("satellite_name",params["satellite_name"].encode("ascii","ignore"))
rootGrp.setncattr("instrument_name",params["instrument_name"].encode("ascii","ignore"))

# ATCF
atcfStormId="{}{}{}".format(data.data.abasin.upper(),data.data.asnum,data.data.ayear)
rootGrp.setncattr("atcf_storm_id",atcfStormId)
rootGrp.setncattr("atcf_storm_basin",data.data.abasin.upper())
rootGrp.setncattr("atcf_storm_number",data.data.asnum)
rootGrp.setncattr("atcf_storm_name",data.data.asname)
atcfDTGstr="{}{:02d}{:02d}{:02d}".format(data.data.ayear,data.data.amonth,data.data.aday,data.data.ahour)
atcfDTG=datetime.datetime.strptime(atcfDTGstr,"%Y%m%d%H")
rootGrp.setncattr("atcf_storm_date_time",atcfDTG.strftime(netcdfDTGFrmt))
rootGrp.setncattr("atcf_storm_latitude_degrees_north",data.data.alat)
alon=data.data.alon
if alon < 0.0:
    alon=alon+360.0
rootGrp.setncattr("atcf_storm_longitude_degrees_east",alon)
rootGrp.setncattr("atcf_storm_speed_knots",data.data.aspeed)
rootGrp.setncattr("atcf_storm_direction_degrees",data.data.adir)
rootGrp.setncattr("atcf_storm_intensity_knots",data.data.avmx)
rootGrp.setncattr("atcf_storm_radius_of_maximum_winds_nautical_miles",data.data.armw)

# mirs swath 
swathDTGstr="{}{:02d}{:02d}{:02d}{:02d}{:02d}".format(data.data.syear,data.data.smonth,data.data.sday,data.data.shour,data.data.smin,data.data.ssec)
swathDTG=datetime.datetime.strptime(swathDTGstr,"%Y%m%d%H%M%S")
rootGrp.setncattr("swath_date_time",swathDTG.strftime(netcdfDTGFrmt))
rootGrp.setncattr("swath_storm_extrapolated_latitude_degrees_north",data.data.clat)
clon=data.data.clon
if clon < 0.0:
    clon=clon+360.0
rootGrp.setncattr("swath_storm_extrapolated_longitude_degrees_east",clon)

# GFS

# Create Global Attributes
rootGrp.setncatts(attributes)

# Yet another hacked section 
if data_class == "xya":
    rootGrp.setncattr("cmd_data_type","grid")
    rootGrp.setncattr("geospatial_lat_units","degrees north")
    rootGrp.setncattr("geospatial_lon_units","degrees east")
    rootGrp.setncattr("geospatial_lat_min",data.ncVars["lats"][0])
    rootGrp.setncattr("geospatial_lat_max",data.ncVars["lats"][-1])
    rootGrp.setncattr("geospatial_lon_min",data.ncVars["lons"][0])
    rootGrp.setncattr("geospatial_lon_max",data.ncVars["lons"][-1])
elif data_class == "rza":
    rootGrp.setncattr("cmd_data_type","grid")
    rootGrp.setncattr("geospatial_radius_units","km")
    rootGrp.setncattr("geospatial_height_units","km")
    rootGrp.setncattr("geospatial_radius_min",data.ncVars["radius"][0])
    rootGrp.setncattr("geospatial_radius_max",data.ncVars["radius"][-1])
    rootGrp.setncattr("geospatial_height_min",data.ncVars["height"][0])
    rootGrp.setncattr("geospatial_height_max",data.ncVars["height"][-1])

rootGrp.close()
