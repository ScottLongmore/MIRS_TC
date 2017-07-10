import ConfigParser
import pdb

LOGGING_SEC = "logging"
PLUGINS_SEC = "plugins"
FILE_GATHER_SEC = "file_gather"
DATABASE_SEC = "database"
QUERY_SEC = "query"

def copy_config(src):
    dst = ConfigParser.RawConfigParser()
    for section in src.sections():
        dst.add_section(section)
        
        for pair in src.items(section):
            name, value = pair
            dst.set(section, name, value)
            
    return dst