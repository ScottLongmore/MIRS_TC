import h5py
import numpy as np
import logging

LOGGER = logging.getLogger(__name__)

class RaobDataNavigationIterator(object):
    def __init__(self, lats, lons, times):
        self.lats = lats
        self.lons = lons
        self.times = times
        
        #raob files only have one sounding in them
        self.returned_value = False
        
    def __iter__(self):
        return self
        
    def next(self):
        if self.returned_value:
            raise StopIteration 

        self.returned_value = True
        return self.lats[()], self.lons[()], self.times[()], (0)

class RaobData(object):

    def __init__(self, config, filename):
        LOGGER.info("Initializing RaobData instance from file %s.", filename)
        self.filename = filename
        self.raob_file = h5py.File(filename, "r")
        
        self.lats_var_name = config.get("raob", "lats_var_name")
        self.lons_var_name = config.get("raob", "lons_var_name")
        self.time_var_name = config.get("raob", "time_var_name")    
        LOGGER.info("Finished initializing RaobData instance from file %s.", filename)
    
    def close(self):
        self.raob_file.close()
        
    def __contains__(a, b):
        return b in a.raob_file
        
    def __getitem__(obj, k):
        return np.array(obj.raob_file[k])
    
    def get_navigation_iterator(self, config):
        LOGGER.info("Generating RaobData navigation iterator for file %s.", self.filename)
        
        it = RaobDataNavigationIterator(self.raob_file[self.lats_var_name], self.raob_file[self.lons_var_name], self.raob_file[self.time_var_name])
        
        LOGGER.info("Finished generating RaobData navigation iterator for file %s.", self.filename)
        return it       
    
    def get_var_at_footprint_index(self, config, var_name, i):
        data = self.raob_file[var_name]
        
        if len(data.shape) == 0:
            return data[()]
        elif len(data.shape) == 1:
            return data[:]
        
        raise ValueError("Encountered variable: %s with dimension: %d. Only scalars and 1D variables supported." % (var_name, len(data.shape)))
        
    def _copy_meta_data_into_dict(self, meta_data):
        meta_data_dict = {}
        for k,v in meta_data.items():
            meta_data_dict[k] = v
            
        return meta_data_dict
        
    def get_global_meta_data(self, config):
        return self._copy_meta_data_into_dict(self.raob_file.attrs)
        
    def get_var_meta_data(self, config, var_name):
        return self._copy_meta_data_into_dict(self.raob_file[var_name].attrs)
    
def get_data(config, filename):
    LOGGER.info("Retrieving RaobData object from file %s.", filename)
    data = RaobData(config, filename)
    LOGGER.info("Finished retrieving RaobData object from file %s.", filename)   
    return data