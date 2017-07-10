import logging
import re
import os
from datetime import date

from pool import config_sections as cfg

LOGGER = logging.getLogger(__name__)

DEFAULT_DIR_REGEX_STR = "\A(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})"

def gather_files(config):
    LOGGER.info("Starting sorted dir file gather.")
    gather_dir = config.get(cfg.FILE_GATHER_SEC, "gather_dir").strip()
    
    if not os.path.isdir(gather_dir):
        raise IOError("Could not find gather dir: %s" % (gather_dir))
    
    regex_str = config.get(cfg.FILE_GATHER_SEC, "regex").strip()
    regex = re.compile(regex_str)
    
    dir_regex = _get_dir_regex(config)
    
    sub_dir_map = _get_subdir_map(gather_dir, dir_regex)
    sorted_dates = _get_sorted_dates(config, sub_dir_map)        

    file_list = []
    for d in sorted_dates:
        sub_dir = sub_dir_map[d]
        _add_files_from_dir(sub_dir, regex, file_list)
    
    LOGGER.info("Finished sorted dir file gather.")
    return file_list
    
def _get_dir_regex(config):
    dir_regex_str = DEFAULT_DIR_REGEX_STR
    if config.has_option(cfg.FILE_GATHER_SEC, "dir_regex"):
        dir_regex_str = config.get(cfg.FILE_GATHER_SEC, "dir_regex").strip()
    return re.compile(dir_regex_str)

def _get_subdir_map(gather_dir, dir_regex):
    subdir_map = {}
    for root, dirs, files in os.walk(gather_dir, followlinks=True):
        for dir in dirs:
            match = dir_regex.match(dir)
            if match is None:
                continue
                
            year, month, day = int(match.group("year")), int(match.group("month")), int(match.group("day"))
            dir_date = date(year=year, month=month, day=day)
            
            subdir_map[dir_date] = os.path.join(root, dir)
            
        return subdir_map

def _get_sorted_dates(config, sub_dir_map):
    dates = sub_dir_map.keys()
    dates.sort()
    if config.getboolean(cfg.FILE_GATHER_SEC, "sort_newest_to_oldest"):
        dates.reverse()

    return dates
    
def _add_files_from_dir(sub_dir, regex, file_list):
    for root, dirs, files in os.walk(sub_dir, followlinks=True):
        for file in files:
            if regex.match(file) is not None:
                file_list.append(os.path.join(root, file))