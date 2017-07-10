import itertools
import logging
import numpy as np

from pool.nc_hdf_adapter import AdapterFile

LOGGER = logging.getLogger(__name__)
class NDimenNavigationIterator(object):
    def __init__(self, lats, lons, times, time_scale, subsample_step_size):
        self.lats = lats
        self.lons = lons
        self.times = times
        self.time_scale = time_scale
        
        shape = lats.shape
        xranges = []
        for dimen in shape:
            xranges.append(xrange(0, dimen, subsample_step_size))
        
        self.nd_iterator = itertools.product(*xranges)
        
    def __iter__(self):
        return self
        
    def next(self):
        indices = self.nd_iterator.next()
            
        lat = self.lats[indices]
        lon = self.lons[indices]
        time = self.times[indices]*self.time_scale        
            
        return lat, lon, time, indices

class SciData(object):
    def __init__(self, config, filename, var_creation_plugins):
        LOGGER.info("Initializing SciData instance from file %s.", filename)
        self.filename = filename
        
        self.using_hdf = False
        if config.getboolean("nc_hdf_io", "use_hdf"):
            self.using_hdf = True
        
        self.in_file = AdapterFile(filename, self.using_hdf)
        self.generated_var_dict = {}
        self.generated_var_meta_data_dict = {}
        self._init_generated_vars(config, var_creation_plugins)
        
        self.lats_var_name = config.get("nc_hdf_io", "lats_var_name")
        self.lons_var_name = config.get("nc_hdf_io", "lons_var_name")
        self.time_var_name = config.get("nc_hdf_io", "time_var_name")
        self.time_scale = config.getfloat("nc_hdf_io", "time_scale")        
        
        LOGGER.info("Finished initializing SciData instance from file %s.", filename)
        
    def _init_generated_vars(self, config, var_creation_plugins):
        for plugin in var_creation_plugins:
            vars = plugin.generate_vars(self.in_file, config)
            for var_name, var_data, var_meta_data in vars:
                self.generated_var_dict[var_name] = var_data
                self.generated_var_meta_data_dict[var_name] = var_meta_data    
    
    #Required by the plugin
    def __contains__(self, b):
        return (b in self.in_file or b in self.generated_var_dict)
    
    #Required by the plugin
    def __getitem__(self, k):
        #When the object is used like a dictionary, it should be safe to
        #use the returned array after the SciData instance is closed.
        #Currently, a call to this method shouldn't happen more than once
        #per variable (and only for shared variables), so it's OK just to
        #make a copy of it.  If this changes, then a caching system should
        #be developed.

        return np.array(self._get_item_without_copy(k))
        
    def _get_item_without_copy(self, k):
        if k in self.generated_var_dict:
            return self.generated_var_dict[k]
        
        return self.in_file[k]       
    
    #Required by the plugin
    def close(self):
        self.in_file.close()

    #Required by the plugin
    def get_navigation_iterator(self, config):
        LOGGER.info("Generating SciData navigation iterator for file %s.", self.filename)
        
        subsample_step_size = 1
        if config.has_option("nc_hdf_io", "subsample_step_size"):
            subsample_step_size = config.getint("nc_hdf_io", "subsample_step_size")
        
        it = NDimenNavigationIterator(self._get_item_without_copy(self.lats_var_name), 
            self._get_item_without_copy(self.lons_var_name), 
            self._get_item_without_copy(self.time_var_name), self.time_scale, 
            subsample_step_size)
        
        LOGGER.info("Finished generating SciData navigation iterator for file %s.", self.filename)
        return it                      
    
    #Required by the plugin
    def get_var_at_footprint_index(self, config, var_name, indices):
        if var_name in self.generated_var_dict:
            data = self.generated_var_dict[var_name]
        else:
            data = self.in_file[var_name]
        
        if len(data.shape) == 0:
            return data[()]

        return data[indices]
        
    #Required by the plugin    
    def get_global_meta_data(self, config):
        return self.in_file.get_global_meta_data()
        
    #Required by the plugin    
    def get_var_meta_data(self, config, var_name):
        if var_name in self.generated_var_meta_data_dict:
            return self.generated_var_meta_data_dict[var_name]
    
        return self.in_file.get_var_meta_data(var_name)
        
        
#Required by the plugin
def get_data(config, filename, var_creation_plugins):
    LOGGER.info("Retrieving SciData object from file %s.", filename)
    
    data = SciData(config, filename, var_creation_plugins)
    
    LOGGER.info("Finished retrieving SciData object from file %s.", filename)   
    return data