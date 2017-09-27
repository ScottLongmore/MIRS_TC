#!/usr/bin/python


import numpy as np
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import os
import re

def plot_map(min_lon, min_lat, max_lon, max_lat,\
        grid_step_lon = 5, grid_step_lat = 5,\
        min_lat_width = 15., min_lon_width = 15.,\
        projection = 'cyl', lat_0=0, lon_0=0, resolution='h',\
        add_lon = 0, add_lat = 0):

    """
    instantiate a map plot using an Equidistant Cylindrical Projection,\
    using crude('c') resolution coasts, adding
    add_lon, add_lat to th emax/min lon/lat values

    input:
        required: min_lon, min_lat, max_lon, max_lat
        optional: 
            projection = 'cyl', lat_0=0, lon_0=0, resolution='c'
            add_lon = 0, add_lat = 0 - add these to max/min lon/lat to make plot look
                better
        accepts following Basemap keywords: projection, resolution,
                lat_0, lon_0
    output:  map

    """
    # --- set figure size
    plt.figure(num=None, figsize=(11.5, 8.5), dpi=80, facecolor='w', edgecolor='k')

    # --- create map
    map = Basemap(projection=projection, lat_0=lat_0, lon_0=lon_0, resolution=resolution,
        llcrnrlat=min_lat - abs(add_lat), llcrnrlon=min_lon - abs(add_lon),\
        urcrnrlat=max_lat + abs(add_lat), urcrnrlon=max_lon + abs(add_lon))

    # --- decorate map
    map.drawcoastlines(zorder=100)
    map.drawmapboundary(zorder=101)
    map.drawparallels(np.arange(-85.0, 85.0, grid_step_lat), labels=[1,0,0,0], zorder=102) # draw parallels
    map.drawmeridians(np.arange(-180.0, 360.0, grid_step_lon),labels=[0,0,0,1], zorder=103) # draw meridians

    return map


def plot_hur_symb(map, lon0, lat0, zorder=149):

    """
    plot hurricane symbol on existing map

    input:
        required:  - map
                   - lon0, lat0  : hurricane position
        optional:
            hurrican symbol parameters:
                 zorder1=149, zorder2 = 150,s = 150, c = 'w', edgecolor='w' 
    output:

    """

    # --- plot white cirle at hurricane position
    x0_map, y0_map = map(lon0, lat0) #get lons, lats projected to plot space 
    #print  'hurricane position is ', x0_map, y0_map
    map.scatter(x0_map,y0_map,c = 'w',\
        s = 150,marker='o',zorder = zorder,\
        edgecolor = 'w')
    map.scatter(x0_map,y0_map,c = 'k',\
        s = 38,marker='o',zorder = zorder,\
        edgecolor = 'k')
    map.scatter(x0_map,y0_map,c = 'w',\
        s = 6,marker='o',zorder = zorder+2,\
        edgecolor = 'w')
    
    plt.text(x0_map,y0_map,"S", zorder = zorder+1, fontsize = 16,\
        horizontalalignment='center',verticalalignment='center')
    return map


def make_plt_title(time_0,var_name = 'none',p_lev = 'none',storm_name = 'none',\
        title_type = 'reg'):
    
    """
    create plot title
    input:
        required: time_0 - datetime object, plot time
        optional: var_name = 'none',p_lev = 'none',storm_name = 'none'
                title_type: reg; two_lines
    output: none   
    
    """

    # --- define time string (minutes only) in the title
    time_string_min = "%04d-%02d-%02d_%02d%02d"  %\
         (int(time_0.year),int(time_0.month),int(time_0.day),int(time_0.hour),int(time_0.minute))


    # --- define pressure string in the title
    if p_lev == 'none':
        p_lev_string = ' '
    elif p_lev == 'Sfc':
        p_lev_string = str(p_lev)
    else: p_lev_string = str(p_lev)+'mb ' 


    # --- define var_name string in the title
    if var_name == 'none':
        var_name_string = ' '
    else:
        var_name_string = var_name+' ' 


    if title_type == 'reg':
        # --- make regular plot title
        title = plt.title(''.join([storm_name,' ',var_name_string,p_lev_string,time_string_min]))
        return title
    elif title_type == 'two_lines':
        # --- make more complicated plot title
        title_str1 =storm_name+' '+var_name_string
        title_str2 =p_lev_string+time_string_min

        plt.suptitle(title_str1, fontsize = 18,\
            horizontalalignment='center',verticalalignment='top')
        plt.title(title_str2,fontsize = 17)
        return  None  #2nd return
    else:
        print 'requested title type is not available,'
        print 'plot title will not be created'
        return None

def make_filename_fom_input_file(input_file,p_lev = None,var_name = None):
    """
    take input filename, strip the extention, add _p_lev.png
    """
    input_file_basename = os.path.basename(input_file)
    input_file_corename = os.path.splitext(input_file_basename)[0]

    # --- define var_name string in the title
    if var_name is None:
        var_name_string = ''
    else:
        # --- replace spaces by '_'
        var_name_string = re.sub(' ', '_', var_name)
        var_name_string = var_name_string+'_' 
    
    # --- define pressure string for the title
    if p_lev == 'none':
        p_lev_string = ' '
    elif p_lev == 'Sfc':
        p_lev_string = str(p_lev)
    else: p_lev_string = str(p_lev)+'mb'

    save_plot_filename = input_file_corename+'_'+var_name_string+p_lev_string+'.png'
    
    return save_plot_filename 






