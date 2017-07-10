#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colorbar import *
import os
from plot_tools_misc import make_plt_title,plot_map, plot_hur_symb, make_filename_fom_input_file
from matplotlib import rc

def plot_data_lonlat(time_0,plot_var,lons,lats,\
            storm_num,storm_basin,\
            lon0 = None, lat0 = None,\
            add_lon = 0, add_lat = 0,\
            filename_var = 'None',\
            storm_name='None',\
            plot_var2 = None,\
            plot_var3 = None,\
            plot_hur_symbol = True,\
            plt_type = 'barbs',\
            savefig = True,\
            showfig = False,\
            plot_out_dir = ".",\
            scatter_marker_size = 100,\
            cmap='jet',edgecolor='none',marker='o', dpi = 400,\
            colorbar_data = True, clf = True,title = True,\
            p_lev =None,var_name = None,\
            save_file_format = 'png',\
            input_filename = 'plot.png',\
            cbar_format = None,\
            barbs_subplot_type = 'none',\
            vmin = None, vmax = None,units = 'none',side_units = False, text_units = True):


    """
    creates map
    overplots hurricane position
    overplots data
    input:
        required: time_0,plot_var,lons,lats,
            storm_num,storm_basin,\
        optional: 
            lon0 = None, lat0 = None,\
            add_lon = 0, add_lat = 0,\
            filename_var = 'None',\
            storm_name='None',\
            plot_var2 = None,\
            plot_var3 = None,\
            plot_hur_symbol = True,\
            plt_type = 'barbs'; could also be 'scatter' or 'contour'
            savefig = True,\
            showfig = False,\
            plot_out_dir = ".",\
            scatter_marker_size = 100,\
            cmap='jet',edgecolor='none',marker='o', dpi = 400,\
            colorbar_data = True, clf = True,title = True,\
            p_lev =None,var_name = None,\
            save_file_format = 'png',\
            input_filename = 'plot.png',\
            cbar_format = None,\
            barbs_subplot_type == 'none',\
            vmin = 'none', vmax = 'none',units = 'none',side_units = False, text_units = True)
            
    output: makes plot
    """
    
    # --- specify font for plots
    rc('font',  family='sans-serif', style='normal', variant='normal',
       stretch='normal')

    # --- get plot min/max
    plt_min = np.nanmin(plot_var)
    plt_max = np.nanmax(plot_var)

    # --- find min values of data+bt lon,lats 
    lo_min = np.min(lons)
    la_min = np.min(lats)
    lo_max = np.max(lons)
    la_max = np.max(lats)

    # --- create map
    map = plot_map(lo_min,la_min,lo_max,la_max,\
                 add_lon = add_lon, add_lat = add_lat)
  
    if plot_hur_symbol:
        # --- plot hurricane symbol on the map
        h_map = plot_hur_symb(map,lon0, lat0)
   
    x_map, y_map = map(lons, lats) #get lons, lats projected to plot space  

    if plt_type == 'scatter':
        cs = map.scatter(x_map,y_map,c=plot_var,cmap=cmap,
            edgecolor=edgecolor, marker=marker, vmin = plt_min, vmax = plt_max,s = scatter_marker_size)

    elif plt_type == 'contour':
        cs = map.contourf(x_map,y_map, plot_var, vmin = plt_min, vmax = plt_max)
       
    elif plt_type == 'barbs':

        barbs_subplot_type == 'none'
        if barbs_subplot_type == 'scatter':
            # --- plot plot_var as scatter plot
            cs = map.scatter(x_map,y_map,c=plot_var,cmap=cmap,\
                edgecolor=edgecolor, marker=marker,\
                vmin = plt_min, vmax = plt_max,s = scatter_marker_size)
            cs.set_clim(plt_min, plt_max)      # without this sets lower limit to zero
            cbar_format = '%6.0f'
        elif barbs_subplot_type == 'contour':       
            # --- plot plot_var as contour plot
            cs = map.contourf(x_map,y_map, plot_var, vmin = plt_min, vmax = plt_max,zorder = 85)
            cbar_format = '%6.0f'
        elif barbs_subplot_type == 'none':
            # --- plotting 2nd variables in color for barbs plot is not requested" 
            pass
        else:
            raise ValueError( 'Invalid barbs_subplot_type requested')

        # --- create barbs plot
        barbs = map.barbs(x_map[::4,1::4],y_map[::4,1::4],plot_var2[::4,1::4],plot_var3[::4,1::4],\
                length=5.5,flagcolor='r',linewidth=0.6, color = ['k'],zorder = 90)

            
    else:
        raise ValueError( 'Unknow plot type requested')

    if title:
        make_plt_title(time_0,storm_name = storm_name, var_name = var_name,p_lev = p_lev)
        p_title = make_plt_title(time_0,storm_name = storm_name,\
            title_type = 'two_lines',\
            var_name = var_name,p_lev = p_lev)
   
    if colorbar_data:
        # --- create colorbar
        try:
            cbar = plt.colorbar(cs, format = cbar_format)
        except: cbar = plt.colorbar(cs)
 
        if units != 'none':
            # --- will add units to the side
            if side_units: cbar.set_label(units)
            if text_units:
               #plt.figtext(hor, vert,  units,
               plt.figtext(0.80, 0.91,  units,
                   color='black', weight='roman',
                   size='large')

    # --- create output filename
    save_plot_filename = make_filename_fom_input_file(input_filename,p_lev = p_lev, var_name = var_name)
    full_save_plot_filename = plot_out_dir+os.sep+save_plot_filename

    if savefig:
        plt.savefig(full_save_plot_filename, dpi=dpi)
    if showfig:
        plt.show()

    if clf: plt.clf()

    return
