import logging
import re
import os
import pdb
import time
from pool import config_sections as cfg

LOGGER = logging.getLogger(__name__)
SECONDS_IN_A_DAY = 3600*24

def gather_files(config):
    LOGGER.info("Starting basic file gather.")
    gather_dirs = config.get(cfg.FILE_GATHER_SEC, "gather_dir").split(",")
    
    for gather_dir in gather_dirs:
        gather_dir = gather_dir.strip()
        if not os.path.isdir(gather_dir):
            raise IOError("Could not find gather dir: %s" % (gather_dir))
    
    regex_str = config.get(cfg.FILE_GATHER_SEC, "regex").strip()
    
    regex = re.compile(regex_str)
    
    gathered_files = []
    discarded_file_count = 0

    past_seconds_to_gather = None
    if config.has_option(cfg.FILE_GATHER_SEC, "ignore_files_older_than_X_days"):
        past_seconds_to_gather = config.getint(cfg.FILE_GATHER_SEC, "ignore_files_older_than_X_days")*SECONDS_IN_A_DAY
        current_time = time.time()
    
    for gather_dir in gather_dirs:
        gather_dir = gather_dir.strip()
        LOGGER.info("Gathering files from dir: %s using regex: %s", gather_dir, regex_str)        

        for root, dirs, files in os.walk(gather_dir, followlinks=True):
            for name in files:
                if regex.match(name) is not None:
                    LOGGER.debug("Found filename: %s that matches regex.", name)
                    file_path = os.path.join(root, name)
                    
                    if past_seconds_to_gather is not None and current_time - os.path.getmtime(file_path) > past_seconds_to_gather:
                        discarded_file_count += 1
                        LOGGER.debug("Ignoring file: %s that is older than %d days.", name, past_seconds_to_gather//SECONDS_IN_A_DAY)             
                        continue
                    
                    gathered_files.append(file_path)
                else:
                    discarded_file_count += 1
                    LOGGER.debug("Found filename: %s that does not match regex.", name)
    
    LOGGER.info("Finished basic file gather and found: %d matching files and ignored: %d files.", len(gathered_files), discarded_file_count)
    return gathered_files