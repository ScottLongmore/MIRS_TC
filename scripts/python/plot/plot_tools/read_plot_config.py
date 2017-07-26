#!/usr/bin/python

import numpy as np
import re
import os
from plot_data_lonlat import plot_data_lonlat
import sys
import logging
import traceback
import error_codes

LOGGER = logging.getLogger(__name__)#create the logger for this file

def read_plot_config(config_file_name,input_filename, d,\
        call_time, call_lon, call_lat,\
        call_storm_name,\
        call_add_lon = 4., call_add_lat = 4,\
        call_var = None,\
        var_name = None,\
        call_units = '',\
        call_storm_num = None,\
        call_storm_basin = None,\
        lon0 = None, lat0 = None,\
        plt_type = 'barbs',\
        barbs_subplot_type = 'scatter',\
        plot_out_dir = '.',\
        call_var2 = None,\
        call_var3 = None):


    """
    reads plot config file. 

    plotting data must be in the following format:
        for 3d variables:
            (lon,lat,pres)
        for 2d variables: 
            (lon,lat)

    input:
        required: 
            config_file: ascii config file. Must be written according to specifications
            d: class containing data to be plotted
        optional:
            
    """

    # --- lenght in km of 1 degree lat
    LATKM = 111.

    try:
        config_file = open(config_file_name,'r')
    except IOError:
        error_msg = 'Cannot open file '+ config_file_name
        LOGGER.exception(error_msg)
        sys.exit(error_codes.EX_IOERR)

    try:
        data_block = False
        for read_line in iter(config_file):
            strip_line = read_line.strip()
            if len(strip_line) > 1:
                # --- read non-empty line
                curr_line = strip_line.split(';')

                # --- parse non-empty line
                if data_block is False and re.match('^ *#',curr_line[0]):
                    # --- skip comments
                    continue

                elif data_block is False and curr_line[0] == 'data':
                    var_name = curr_line[1].strip()
                    num_id = curr_line[2].strip()
                    read_press = curr_line[3].strip()
                    press = [int(x) for x in read_press.split(",")]
                    data_block = True
                elif data_block:
                    if num_id == '1':   #plot_data_lonlat 
                         
                        call_time = d.var_dict[curr_line[0].strip()] 
                        for ipress in press:                                   
     
                            call_var = d.var_dict[curr_line[1].strip()] 
                            if ipress == 'none':
                                #MUST be 'none' for 2d variables
                                pass
                            else: 
                                #3d variables
                                call_var = d.get_pressure_slice(call_var,ipress,var_name)

                            call_lon = d.var_dict[curr_line[2].strip()]
                            call_lat = d.var_dict[curr_line[3].strip()]
                            call_storm_num = d.var_dict[curr_line[4].strip()]
                            call_storm_basin = d.var_dict[curr_line[5].strip()]


                            for ikw in curr_line[6:len(curr_line)]:
                                if re.match(' *add_lon', ikw): call_add_lon = float(ikw.split('=')[-1])
                                if re.match(' *add_lat', ikw): call_add_lat = float(ikw.split('=')[-1])
                                if re.match(' *storm_name', ikw): 
                                    call_storm_name = d.var_dict[ikw.split('=')[-1].strip()]
                                if re.match(' *units', ikw): call_units = ikw.split('=')[-1].strip()
                                if re.match(' *pos', ikw): call_pos = ikw.split('=')[-1].strip()
                                if re.match(' *rmax', ikw): call_rmax = ikw.split('=')[-1].strip()
                                if re.match(' *plot_var2', ikw): 
                                    plot_var2 = d.var_dict[ikw.split('=')[-1].strip()]
                                    call_var2= d.get_pressure_slice(plot_var2,ipress,'var2')
                                if re.match(' *plot_var3', ikw): 
                                    plot_var3 = d.var_dict[ikw.split('=')[-1].strip()]
                                    call_var3= d.get_pressure_slice(plot_var3,ipress,'var3')
                                if re.match(' *plt_type', ikw): 
                                    plt_type = ikw.split('=')[-1].strip()
                                if re.match(' *barbs_subplot_type', ikw): 
                                    barbs_subplot_type = ikw.split('=')[-1].strip()
                               
                                if ipress != 'none':
                                    call_p_lev = ipress
                                else:
                                    call_p_lev = 'none'

                                if re.match(' *savefig', ikw): 
                                    savefig_str = ikw.split('=')[-1]    
                                    if savefig_str == 'True': call_savefig = True
                                    else: call_savefig = False
                                if re.match(' *showfig', ikw): 
                                    show_str = ikw.split('=')[-1]    
                                    if show_str == 'True': call_show = True
                                    else: call_show = False
                                if re.match(' *plot_out_dir', ikw): plot_out_dir = ikw.split('=')[-1].strip()
                    
                            LOGGER.info( "plotting variable "+var_name.strip()+', pressure_level = %d mb' % call_p_lev)
                            try: 
                                plot_data_lonlat(call_time,call_var,call_lon,call_lat,\
                                            call_storm_num, call_storm_basin,\
                                            lon0 = lon0, lat0 = lat0,\
                                            add_lon = call_add_lon, add_lat = call_add_lat,\
                                            storm_name = call_storm_name,\
                                            p_lev = call_p_lev,var_name = var_name,\
                                            units = call_units,\
                                            filename_var = var_name,\
                                            plot_var2 = call_var2,\
                                            plot_var3 = call_var3,\
                                            plt_type = plt_type,\
                                            plot_out_dir = plot_out_dir,\
                                            input_filename = input_filename,\
                                            barbs_subplot_type = barbs_subplot_type,\
                                            savefig = True,showfig =False)
                            except:
                                error_msg = 'unable to make plot using values from config file'
                                raise ValueError(error_msg)
                            data_block = False

                    #-----------------------------------------------------------------------------------
                    else: 
                        data_block = False
                else: 
                    # ---error in config file
                    raise ValueError('Incorrect data format in config file '+config_file_name)

            else:
                # --- skip empty lines
                continue
    except:
        error_msg = 'Error encountered while reading config file '+config_file_name
        LOGGER.exception(error_msg)
        sys.exit(error_codes.EX_CONFIG)  
    
    config_file.close()
