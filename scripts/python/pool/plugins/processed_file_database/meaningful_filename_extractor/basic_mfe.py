import logging
import os
import re
import pdb

from pool import config_sections as cfg

LOGGER = logging.getLogger(__name__)

class BasicMFE(object):
    def __init__(self, config):
        LOGGER.info("Initializing BasicMFE")
        self.regex_str = config.get(cfg.FILE_GATHER_SEC, "regex").strip()
        self.regex = re.compile(self.regex_str)
        LOGGER.info("Finished initializing BasicMFE")
        
    def extract(self, filename):
        LOGGER.debug("Extracting meaningful info from filename :%s", filename)
        basename = os.path.basename(filename)
        match = self.regex.match(basename)
        LOGGER.debug("Using regex: %s to extract info from basename: %s", self.regex_str, basename)
        if match is None:
            raise ValueError("Tried to extract meaningful sections of filename: %s with basename: %s which does not match regex: %s" % (filename, basename, self.regex_str))
        
        if self.regex.groups == 0:
            LOGGER.debug("No groups in regex.  Entire basename is meaningful.")
            LOGGER.debug("Returning basename: %s for filename %s:", basename, filename)
            return basename
        
        groups = match.groups()
        
        if len(groups) != self.regex.groups:
            raise ValueError("Regex %s found %d groups in basename: %s. Needed %d groups. Please examine filename: %s" % (self.regex_str, len(groups), basename, self.regex.groups(), filename))
        
        extracted_info = "".join(groups)
        LOGGER.debug("Found meaningful info: %s in basename: %s", extracted_info, basename)
        
        LOGGER.debug("Finished extracting meaningful info from filename : %s", filename)
        return extracted_info
        
        
    
def get_meaningful_filename_extractor(config):
    LOGGER.info("Retrieving BasicMFE object.")
    mfe = BasicMFE(config)
    LOGGER.info("Finished retrieving BasicMFE object.")
    return mfe