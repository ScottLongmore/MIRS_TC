import json
import os
from pool import config_sections as cfg

CONFIG_SECTION = "option_checker"
VALID_OPTIONS_FILENAME = "valid_options.json"

def check(config):
    section_to_check = config.get(CONFIG_SECTION, "section_to_check")
    option_to_check = config.get(CONFIG_SECTION, "option_to_check")
    
    option_value = config.get(section_to_check, option_to_check).strip()
    
    database_dir = config.get(cfg.DATABASE_SEC, "location").strip()
    
    valid_options_path = os.path.join(database_dir, VALID_OPTIONS_FILENAME)
    with open(valid_options_path, "r") as in_file:
        valid_options_list = json.load(in_file)
        
    for valid_option in valid_options_list:
        if option_value == valid_option.strip():
            return
            
    raise ValueError("Can not perform update. The config option: ["+
        section_to_check+"], "+option_to_check+": with value: "+option_value+
        " does not match any entry in "+valid_options_path+
        ".  If this is a valid value, then it should be added to the "+
        VALID_OPTIONS_FILENAME+" file.")