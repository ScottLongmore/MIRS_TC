#!/usr/bin/python
"""
reads from command line XYA data file and (optional)
output_dir. If output_dir is not provided or does not
exist, will create plots in the current directory. 

Makes plots specified in plots_config.txt 

example:

python main_read_plot_xya.py al182012_2012102312_NOAA15.XYA plots

"""
    
import sys
import os
import plot_tools as pt
from xya_class import *
import logging
import traceback
import error_codes
from setup_nde_logging import setup_logging


LOGGER = logging.getLogger(__file__)#create the logger for this file

logging_setup = 'NPP_TC'

setup_logging(logging_setup)
plot_config_file = 'plots_config.txt'

# --- read input filename
try:
    input_filename = sys.argv[1]  # full path to filename
except (IndexError):
    error_msg = 'please provide XYA data filename; USAGE: python main_read_plot_xya.py data_file.XYA'
    LOGGER.exception(error_msg)
    sys.exit(error_codes.EX_USAGE) 

# --- determine output dir
try:
    output_dir = sys.argv[2]
except:
    output_dir = '.'
    
# --- make sure the requested output dir exists, else default to current dir
if not os.path.exists(output_dir):
    output_dir = '.'

LOGGER.info('Processing file '+input_filename)#this will be ignored by NDE
#print 'Processing file ', input_filename

# --- instanteniate atms class   
d = xya(input_filename)

#----------------START PLOTTING based on config file-------------------------------------------
"""
from config file select:
    plot_var
    p_lev (if p_lev is None: loop through all available pressure levels)

"""
pt.read_plot_config(plot_config_file,input_filename,d,\
    d.swath_time, d.xya_slon, d.xya_slat,d.storm_name,\
    plot_out_dir = output_dir,\
    lon0 = d.lon0, lat0 = d.lat0)

