import imp
import os
import logging
import config_sections as cfg
import pdb

DEFAULT_PLUGIN_ROOT_DIR = "plugins"

LOGGER = logging.getLogger(__name__)

def load(config, plugin_name, should_log=True, optional=False):
    if should_log:
        LOGGER.info("Started loading %s plugin.", plugin_name)
    
    if optional and not config.has_option(cfg.PLUGINS_SEC, plugin_name):
        return None
    
    path = config.get(cfg.PLUGINS_SEC, plugin_name)
        
    return load_from_path(config, plugin_name, path, should_log)
    
    
def load_from_path(config, plugin_name, path, should_log=True):
    plugins_root_dir = DEFAULT_PLUGIN_ROOT_DIR
    if config.has_option(cfg.PLUGINS_SEC, "plugins_root_dir"):
        plugins_root_dir = config.get(cfg.PLUGINS_SEC, "plugins_root_dir")
        
    plugin_module_name, file_extension = os.path.splitext(path)
    full_path = os.path.join(plugins_root_dir, path)
    
    if should_log:
        LOGGER.info("Attempting to load %s plugin with module name %s and path %s.", plugin_name, plugin_module_name, full_path)
    
    plugin_module = imp.load_source(plugin_module_name, full_path)
    
    if should_log:
        LOGGER.info("Finished loading %s plugin.", plugin_name)    
    
    return plugin_module
    
    