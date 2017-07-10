import h5py
from pool import config_sections as cfg
import numpy as np
import logging

LOGGER = logging.getLogger(__name__)

def write(config, selected_data):
    LOGGER.info("Writing query result to hdf5 file.")
    output_filename = config.get(cfg.QUERY_SEC, "output_file")

    config_output_filename = output_filename+".cfg"
    LOGGER.info("Writing config file: %s", config_output_filename)
    with open(config_output_filename, "w") as config_out:
        config.write(config_out)
    LOGGER.info("Wrote config file: %s", config_output_filename)
    
    LOGGER.info("Opening hdf5 file %s for write.", output_filename)
    out_file = h5py.File(output_filename, "w")
    
    LOGGER.info("Adding global meta data to hdf5 file.")
    if selected_data.global_meta_data is not None:
        for name, value in selected_data.global_meta_data.iteritems():
            out_file.attrs[name] = value
        
        #add attributes that specify which file the meta data came from
        out_file.attrs["query_global_meta_data_src"] = selected_data.global_meta_data_src
        out_file.attrs["query_variable_meta_data_src"] = selected_data.var_meta_data_src
        #add attribute that specifies which variables were missing while performing the query 
        out_file.attrs["query_missing_variables"] = ",".join(selected_data.get_missing_var_names())
    
    for var_name, data_collection in selected_data.var_data.iteritems():
        if selected_data.is_var_name_missing(var_name):
            LOGGER.debug("Skipping creation of hdf5 dataset for missing var: %s", var_name)
            continue
            
        LOGGER.debug("Creating hdf5 dataset for var: %s", var_name)
        
        data_array = np.array(data_collection)
        out_file.create_dataset(var_name, data=data_array)
        
        if var_name in selected_data.var_meta_data:
            for name, value in selected_data.var_meta_data[var_name].iteritems():
                out_file[var_name].attrs[name] = value
        
        LOGGER.debug("Finished creating hdf5 dataset for var: %s", var_name)
        
    for var_name, data in selected_data.shared_var_data.iteritems():
        if selected_data.is_var_name_missing(var_name):
            LOGGER.debug("Skipping creation of hdf5 dataset for missing var: %s", var_name)
            continue    
    
        LOGGER.debug("Creating hdf5 dataset for shared var: %s", var_name)
        out_file.create_dataset(var_name, data=data)
        
        if var_name in selected_data.shared_var_meta_data:
            for name, value in selected_data.shared_var_meta_data[var_name].iteritems():
                out_file[var_name].attrs[name] = value        
        LOGGER.debug("Finished creating hdf5 dataset for shared var: %s", var_name)
    
    out_file.close()
    LOGGER.info("Finished writing query result to hdf5 file.")
    