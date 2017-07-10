import numpy as np
import os
import logging
import tarfile
import StringIO

from pool import cira_txt_array as ctxt
from pool import config_sections as cfg

LOGGER = logging.getLogger(__name__)

DEFAULT_OUTPUT_EXTENSION = ".txt"
TAR_INNER_FOLDER = "query_results"
CONFIG_FILENAME = "query_results/query_config.cfg"

def write(config, selected_data):
    LOGGER.info("Writing query result to txt files.")
    output_tar_filename = config.get(cfg.QUERY_SEC, "tar_output_filename")
    output_extension = DEFAULT_OUTPUT_EXTENSION
    if config.has_option(cfg.QUERY_SEC, "txt_output_extension"):
        config.get(cfg.QUERY_SEC, "txt_output_extension")
        
    tar_archive = tarfile.open(output_tar_filename, "w")
    
    _write_config_to_tar(config, tar_archive)
    
    for var_name, data_collection in selected_data.var_data.iteritems():
        if selected_data.is_var_name_missing(var_name):
            LOGGER.debug("Skipping creation of txt file for missing var: %s", var_name)
            continue
    
        LOGGER.debug("Creating txt file for var: %s", var_name)
        data_array = np.array(data_collection)
        var_filename = os.path.join(TAR_INNER_FOLDER, var_name+output_extension)
        _write_array_to_tar(var_filename, tar_archive, data_array)
        
        LOGGER.debug("Finished creating txt file for var: %s", var_name)
        
    for var_name, data in selected_data.shared_var_data.iteritems():
        LOGGER.debug("Creating txt file for shared var: %s", var_name)
        
        var_filename = os.path.join(TAR_INNER_FOLDER, var_name+output_extension)
        _write_array_to_tar(var_filename, tar_archive, data)
        
        LOGGER.debug("Finished creating txt file for shared var: %s", var_name)
    tar_archive.close()
    LOGGER.info("Finished writing query result to txt files.")
    
def _write_config_to_tar(config, tar_archive):
    LOGGER.info("Wrting query config to %s.", CONFIG_FILENAME)
    psuedo_file = StringIO.StringIO()
    config.write(psuedo_file)
    psuedo_file.seek(0)
    info = tarfile.TarInfo(name=CONFIG_FILENAME)
    info.size=len(psuedo_file.buf)
    tar_archive.addfile(tarinfo=info, fileobj=psuedo_file)
    LOGGER.info("Finished wrting query config to %s.", CONFIG_FILENAME)
    
def _write_array_to_tar(filename, tar_archive, array):
    psuedo_file = StringIO.StringIO()
    ctxt.write_array_to_file(psuedo_file, array)
    psuedo_file.seek(0)
    info = tarfile.TarInfo(name=filename)
    info.size=len(psuedo_file.buf)
    tar_archive.addfile(tarinfo=info, fileobj=psuedo_file)
