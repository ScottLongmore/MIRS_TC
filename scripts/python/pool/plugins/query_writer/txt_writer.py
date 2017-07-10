from pool import cira_txt_array as ctxt
from pool import config_sections as cfg
import numpy as np
import os
import logging

LOGGER = logging.getLogger(__name__)

DEFAULT_OUTPUT_EXTENSION = ".txt"

def write(config, query_results):
    LOGGER.info("Writing query result to txt files.")
    output_dir = config.get(cfg.QUERY_SEC, "txt_output_dir")
    output_extension = DEFAULT_OUTPUT_EXTENSION
    if config.has_option(cfg.QUERY_SEC, "txt_output_extension"):
        config.get(cfg.QUERY_SEC, "txt_output_extension")
        
    if not os.path.isdir(output_dir):
        os.mkdir(output_dir)
    
    for var_name, data_collection in query_results.var_data.iteritems():
        if query_results.is_var_name_missing(var_name):
            LOGGER.debug("Skipping creation of txt file for missing var: %s", var_name)
            continue
    
        LOGGER.debug("Creating txt file for var: %s", var_name)
        data_array = np.array(data_collection)
        var_filename = os.path.join(output_dir, var_name+output_extension)
        ctxt.write_array_to_file(var_filename, data_array)
        
        LOGGER.debug("Finished creating txt file for var: %s", var_name)
        
    for var_name, data in query_results.shared_var_data.iteritems():
        LOGGER.debug("Creating txt file for shared var: %s", var_name)
        
        var_filename = os.path.join(output_dir, var_name+output_extension)
        ctxt.write_array_to_file(var_filename, data)
        
        LOGGER.debug("Finished creating txt file for shared var: %s", var_name)
    LOGGER.info("Finished writing query result to txt files.")
    