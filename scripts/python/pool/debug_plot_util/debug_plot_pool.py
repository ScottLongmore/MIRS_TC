from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np
import argparse
import sys
import os
import re
import json
import logging
import h5py
from pool import pool
from pool import bounding_box 

LOGGER = logging.getLogger(__name__)

GRID_CELL_REGEX = re.compile("(?P<lon>[\d]+)_(?P<lat>[\d]+).json")
META_INFO_FILENAME = "meta_info.json"

class GridSpacing(object):
    def __init__(self, x, y, t):
        self.x = x
        self.y = y
        self.t = t
        
class LineSegments(object):
    def __init__(self):
        self.x = []
        self.y = []

def prepare_plot():
    plt.clf()
    m = Basemap(projection='cyl', lat_0=0, lon_0=0, resolution='c', 
    llcrnrlat=-90, llcrnrlon=-180,
    urcrnrlat=90, urcrnrlon=180)
    
    m.drawcoastlines()
    m.drawparallels(np.arange(-90.0, 90.0, 10.0), labels=[1,0,0,0])
    m.drawmeridians(np.arange(-180.0, 180.0, 10.0), labels=[0,0,0,1])
    m.drawparallels(np.arange(-90,90,20),labels=[1,1,0,0])
    m.drawmeridians(np.arange(-180,180,20),labels=[0,0,0,1])
    
    return m

def finish_plot(output_filename=None):
    if output_filename is not None:
        plt.savefig(output_filename)
        return
        
    plt.show()
    
def get_spacing_from_meta_info(pool_dir):
    meta_info_filename = os.path.join(pool_dir, META_INFO_FILENAME)
    try:
        with open(meta_info_filename, "r") as in_file:
            meta_info = json.load(in_file)
    except ValueError:
        LOGGER.exception("Corrupt data found in meta data file: %s", meta_info_filename)
        sys.exit(1)

    spacing = GridSpacing(meta_info["lon_spacing_degrees"], meta_info["lat_spacing_degrees"], meta_info["time_spacing_seconds"])
    return spacing
    
def add_grid_cell_contents_to_database_files(grid_filename, database_files):
    with open(grid_filename, "r") as in_file:
        filename_list = json.load(in_file)

    for filename in filename_list:
        database_files[filename] = 0
    
def read_cells_from_time_dir(time_dir, grid_spacing):
    segments = []
    database_files = {}

    for root, dirs, files in os.walk(time_dir):
        for filename in files:
            match = GRID_CELL_REGEX.match(filename)
            if match is None:
                LOGGER.warning("Found bad grid filename: %s", os.path.join(root, filename))
                continue
            
            add_grid_cell_contents_to_database_files(os.path.join(root, filename), database_files)
            
            lon_index = int(match.group("lon"))
            lat_index = int(match.group("lat"))
            lon = (lon_index*grid_spacing.x)-180
            lat = (lat_index*grid_spacing.y)-90
            
            segment = LineSegments()
            #lower left
            segment.x.append(lon)
            segment.y.append(lat)
            #lower right
            segment.x.append(lon+grid_spacing.x)
            segment.y.append(lat)
            #upper right
            segment.x.append(lon+grid_spacing.x)
            segment.y.append(lat+grid_spacing.y)            
            #upper left
            segment.x.append(lon)
            segment.y.append(lat+grid_spacing.y)
            #lower left
            segment.x.append(lon)
            segment.y.append(lat)
            #upper right
            segment.x.append(lon+grid_spacing.x)
            segment.y.append(lat+grid_spacing.y)
            segments.append(segment)
            LOGGER.debug("Found lon, lat: %f, %f with indices: %d, %d", lon, lat, lon_index, lat_index)
        break

    return segments, database_files
    
def plot_hdf5_file(map, filename, lat_var_name="Latitude", lon_var_name="Longitude", color='r'):
    in_file = h5py.File(filename, "r")
    lons = in_file[lon_var_name]
    lats = in_file[lat_var_name]
    
    x, y = map(lons, lats)
    map.scatter(x, y, c=color, lw=0)
    in_file.close()
    
def plot_bounding_box(map, config):
    boxes, start_time, end_time = bounding_box.get_bounding_boxes_from_config(config)
    for box in boxes:
        x = []
        y = []
        #lower left
        x.append(box.left)
        y.append(box.bottom)
        #lower right
        x.append(box.right)
        y.append(box.bottom)
        #upper right
        x.append(box.right)
        y.append(box.top)
        #upper left
        x.append(box.left)
        y.append(box.top)
        #lower left
        x.append(box.left)
        y.append(box.bottom)
        #upper right
        x.append(box.right)
        y.append(box.top)
        x_projected, y_projected = map(x, y)
        map.plot(x_projected, y_projected, c='green')
        
def plot_database_grid(map, pool_dir, pool_time_dir, show_database_files=False):
    time_dir = os.path.join(pool_dir, pool_time_dir)
    grid_spacing = get_spacing_from_meta_info(pool_dir)
    segments, database_files = read_cells_from_time_dir(time_dir, grid_spacing)
    
    for segment in segments:
        x, y = map(segment.x, segment.y)
        map.plot(x, y, c="cyan", marker=None)
    
    if not show_database_files:
        return
        
    for filename in database_files.keys():
        #TODO: make type of reader configurable
        #TODO: make var names configurable
        plot_hdf5_file(map, filename, lat_var_name="Latitude", lon_var_name="Longitude", color='r')

def gen_plots(pool_dir, output_filename=None, pool_time_dirs=None, logging_level="debug", show_database_files=False, configs=None, h5filename=None):
    setup_logging(logging_level)
    
    map = prepare_plot()
    if pool_time_dirs is not None:
        for pool_time_dir in pool_time_dirs:
            plot_database_grid(map, pool_dir, pool_time_dir, show_database_files=show_database_files)
        
    if configs is not None:
        config = pool.get_config_cascade(configs)
        plot_bounding_box(map, config)
        
    if h5filename is not None:
        plot_hdf5_file(map, h5filename, lat_var_name="Latitude", lon_var_name="Longitude", color='b')
    
    finish_plot(output_filename=output_filename)
    
def setup_logging(level="debug"):
    level_dict = {"debug":logging.DEBUG, "info":logging.INFO, "warning":logging.WARNING, "error":logging.ERROR, "critical":logging.CRITICAL}
    logging.basicConfig(level=level_dict[level])

def main():
    parser = argparse.ArgumentParser(description='Generates debug plots for a pool and related files.')
    parser.add_argument("pool_dir", help="Pool directory.")
    parser.add_argument("--output_filename", "-o", help="Saves the generated figure to the specified filename rather than showing it.")
    parser.add_argument("--time_dirs", "-t", nargs="*", help="Pool database time directories whose grid cells will be plotted on the map")
    parser.add_argument("--logging_level", "-l", default="debug", help="The logging level to use when printing messages")
    parser.add_argument("--show_database_files", "-d", action="store_true", help="Plot the lons/lats of files in the database")
    parser.add_argument("--configs", "-c", nargs="*", help="Show the bounding box generated from the passed list of config files")
    parser.add_argument("--show_hdf5", "-hd", help="Show lat/lon points from the passed hdf5 file.")
    args = parser.parse_args()
    
    gen_plots(args.pool_dir, output_filename=args.output_filename, pool_time_dirs=args.time_dirs, logging_level=args.logging_level, show_database_files=args.show_database_files, configs=args.configs, h5filename=args.show_hdf5)    
    
if __name__ == "__main__":
    main()