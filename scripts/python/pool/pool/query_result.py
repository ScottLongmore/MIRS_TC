import tarfile
import logging
import os
import cira_txt_array as ctxt
import h5py
import numpy as np

LOGGER = logging.getLogger(__name__)
LOGGER.addHandler(logging.NullHandler())

class QueryResult(object):
    def __init__(self, filename, data_extension='.txt'):
        self.variable_data = {}

        if not os.path.isfile(filename):
            raise IOError("No such file: "+filename)
    
        LOGGER.info("Attempting to read file: "+filename+" as a .tar file.")
        try:
            self._init_from_tar(filename, data_extension=data_extension)
            return
        except:
            LOGGER.info("Could not read file: "+filename+" as a .tar file.")
            if LOGGER.isEnabledFor(logging.DEBUG):
                LOGGER.exception("Could not read file: "+filename+" as a .tar file.")
            
        LOGGER.info("Attempting to read file: "+filename+"as an hdf5 file.")
        try:
            self._init_from_hdf5(filename)
            return
        except:
            LOGGER.info("Could not read file: "+filename+" as hdf5 file.")
            if LOGGER.isEnabledFor(logging.DEBUG):
                LOGGER.exception("Could not read file: "+filename+" as a hdf5 file.")            
        
        raise IOError("File: "+filename+" is corrupt or has an unkown file format.")
            
    def _init_from_tar(self, tar_filename, data_extension):        
        tar = tarfile.open(tar_filename, "r")
        for member in tar.getmembers():
            if member.name.endswith(data_extension):
                var_name = member.name[:-len(data_extension)]
                var_name = os.path.basename(var_name)
                try:
                    psuedo_file = tar.extractfile(member)
                    self.variable_data[var_name] = ctxt.read_array_from_file(psuedo_file)
                except (IOError, ValueError, IndexError):
                    LOGGER.exception("Could not read tar archive file: "+member.name+" skipping archive file.")
                    continue
                    
    def _init_from_hdf5(self, filename):
        with h5py.File(filename) as in_file:
            in_file.visititems(self._add_hdf5_item_to_var_data)
            
    def _add_hdf5_item_to_var_data(self, item_name, item):
        if not isinstance(item, h5py.Dataset):
            return
            
        self.variable_data[item_name] = np.copy(item)
        