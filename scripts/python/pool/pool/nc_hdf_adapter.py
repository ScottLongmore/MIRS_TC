import netCDF4
import h5py

class AdapterFile(object):
    def __init__(self, filename, should_use_hdf):
        self.filename = filename
        self.using_hdf = should_use_hdf
        
        if self.using_hdf:
            self._in_file = h5py.File(self.filename, "r")
        else:
            self._in_file = netCDF4.Dataset(self.filename, "r")
            self._in_file.set_auto_scale(False)
            
    def __contains__(self, b):
        if self.using_hdf:
            return b in self._in_file
        else:
            return b in self._in_file.variables
            
    def __getitem__(self, k):
        try:
            return self._in_file[k]
        except IndexError:
            raise KeyError("Could not find var name: %s in file: %s." % (k, self.filename))
        
    def close(self):
        self._in_file.close()
        
    def _copy_nc_meta_data_into_dict(self, data_obj):
        meta_data = data_obj.ncattrs()
    
        meta_data_dict = {}
        for attr_name in meta_data:
            meta_data_dict[attr_name] = getattr(data_obj, attr_name)
            
        return meta_data_dict

    def _copy_hdf_meta_data_into_dict(self, data_obj):
        meta_data = data_obj.attrs
    
        meta_data_dict = {}
        for k,v in meta_data.items():
            meta_data_dict[k] = v
            
        return meta_data_dict
        
    def get_global_meta_data(self):
        if self.using_hdf:
            return self._copy_hdf_meta_data_into_dict(self._in_file)
        else:
            return self._copy_nc_meta_data_into_dict(self._in_file)
            
    def get_var_meta_data(self, var_name):
        if self.using_hdf:
            return self._copy_hdf_meta_data_into_dict(self._in_file[var_name])
        else:
            return self._copy_nc_meta_data_into_dict(self._in_file[var_name])
