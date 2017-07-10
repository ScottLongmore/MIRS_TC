from netCDF4 import Dataset
import numpy as np
import logging
from datetime import datetime
import pdb

from pool import time_util as tu
from pool import config_sections as cfg

LOGGER = logging.getLogger(__name__)

class NucapsDataNavigationIterator(object):
    def __init__(self, lats, lons, times, time_scale):
        self.i = 0
        
        self.lats = lats
        self.lons = lons
        self.times = times
        self.time_scale = time_scale
        
    def __iter__(self):
        return self
        
    def next(self):
        if self.i >= self.lats.shape[0]:
            raise StopIteration
            
        lat = self.lats[self.i]
        lon = self.lons[self.i]
        time = self.times[self.i]*self.time_scale
        
        self.i += 1
            
        return lat, lon, time, (self.i-1)
                    

class NucapsData(object):
    def __init__(self, config, filename):
        LOGGER.info("Initializing NucapsData instance from file %s.", filename)
        self.filename = filename
        self.nucaps_file = Dataset(filename, "r")
        
        self.lats_var_name = config.get("nucaps", "lats_var_name")
        self.lons_var_name = config.get("nucaps", "lons_var_name")
        self.time_var_name = config.get("nucaps", "time_var_name")
        self.time_scale = config.getfloat("nucaps", "time_scale")        
        
        LOGGER.info("Finished initializing NucapsData instance from file %s.", filename)
    
    def __contains__(a, b):
        return b in a.nucaps_file.variables
        
    def __getitem__(obj, k):
        return np.array(obj.nucaps_file[k])
        
    def get_var_names(self):
        return self.nucaps_file.variables.keys()
    
    #Required by the plugin
    def close(self):
        self.nucaps_file.close()

    #Required by the plugin
    def get_navigation_iterator(self, config):
        LOGGER.info("Generating NucapsData navigation iterator for file %s.", self.filename)
        
        it = NucapsDataNavigationIterator(self.nucaps_file[self.lats_var_name], self.nucaps_file[self.lons_var_name], self.nucaps_file[self.time_var_name], self.time_scale)
        
        LOGGER.info("Finished generating NucapsData navigation iterator for file %s.", self.filename)
        return it                      
    
    #Required by the plugin
    def get_var_at_footprint_index(self, config, var_name, i):
        data = self.nucaps_file[var_name]
        
        if len(data.shape) == 0:
            return data[()]

        return data[i]
        
    def _copy_meta_data_into_dict(self, data_obj):
        meta_data = data_obj.ncattrs()
    
        meta_data_dict = {}
        for attr_name in meta_data:
            meta_data_dict[attr_name] = getattr(data_obj, attr_name)
            
        return meta_data_dict
        
    #Required by the plugin    
    def get_global_meta_data(self, config):
        return self._copy_meta_data_into_dict(self.nucaps_file)
        
    #Required by the plugin    
    def get_var_meta_data(self, config, var_name):
        return self._copy_meta_data_into_dict(self.nucaps_file[var_name])
        
        
#Required by the plugin
def get_data(config, filename):
    LOGGER.info("Retrieving NucapsData object from file %s.", filename)
    data = NucapsData(config, filename)
    LOGGER.info("Finished retrieving NucapsData object from file %s.", filename)   
    return data