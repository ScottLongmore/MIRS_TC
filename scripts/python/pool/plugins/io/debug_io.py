import pickle
import numpy as np
import logging
from datetime import datetime
from pool import bounding_box as bb
import pdb

from pool import time_util as tu

LOGGER = logging.getLogger(__name__)

class DebugDataNavigationIterator(object):
    def __init__(self, lats, lons, times):
        self.i = 0
        self.j = 0
        
        self.lats = lats
        self.lons = lons
        self.times = times
        
    def __iter__(self):
        return self
        
    def next(self):
        if self.j >= self.lats.shape[1]:
            raise StopIteration
            
        lat = self.lats[self.i, self.j]
        lon = self.lons[self.i, self.j]
        time = self.times[self.j]
        
        indices = (self.i, self.j)
        
        self.i += 1
        if self.i >= self.lats.shape[0]:
            self.i = 0
            self.j += 1
            
        return lat, lon, time, indices

class DebugData(object):
    def __init__(self, filename):
        LOGGER.info("Initializing DebugData instance from file %s.", filename)
        self.filename = filename
        with open(filename, "rb") as in_file:
            self.data_dict = pickle.load(in_file)
            time_data = self.data_dict["times"]
            
            LOGGER.info("Converting debug datetimes to timestamps.")
            timestamps = np.zeros(len(time_data))
            for i, d in zip(range(len(time_data)), time_data):
                timestamps[i] = tu.get_seconds_since_epoch(d)
                
            self.data_dict["timestamps"] = timestamps
        LOGGER.info("Finished initializing DebugData instance from file %s.", filename)
            
    def contains(a, b):
        raise b in a.data_dict
    
    def __contains__(a, b):
        raise b in a.data_dict
        
    def getitem(obj, k):
        return obj.data_dict[k]
        
    def __getitem__(obj, k):
        return obj.data_dict[k]
        
    def get_var_names(self):
        return self.data_dict.keys()
        
    def close(self):
        pass

    def get_global_meta_data(self, config):
        return {"foo": "bar"}
        
    def get_var_meta_data(self, config, var_name):
        return {"foobar": "buzz"}        
        
    def get_navigation_iterator(self, config):
        LOGGER.info("Retrieving DebugData navigation from file %s.", self.filename)
        it = DebugDataNavigationIterator(self["lats"], self["lons"], self["timestamps"])
        LOGGER.info("Finished retrieving DebugData navigation from file %s.", self.filename)
        return it
        
    def get_var_at_footprint_index(self, config, var_name, indices):
            data = self[var_name]
            i, j = indices
            
            if len(data.shape) == 2:
                return data[i, j]
            elif len(data.shape) == 1:
                return data[j]
            else:
                raise ValueError("Found debug variable: %s with dimension %d. Only 1D and 2D variables are allowed." % (var_name, len(data.shape)))
        
    
def get_data(config, filename):
    LOGGER.info("Retrieving DebugData object from file %s.", filename)
    data = DebugData(filename)
    LOGGER.info("Finished retrieving DebugData object from file %s.", filename)   
    return data
    