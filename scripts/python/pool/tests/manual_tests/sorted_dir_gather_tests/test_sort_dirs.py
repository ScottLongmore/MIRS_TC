import logging
import ConfigParser
import pdb

from pool import plugin_loader

def main():
    logging.basicConfig(level=logging.DEBUG)
    config = ConfigParser.RawConfigParser()
    config.read("tests/manual_tests/sorted_dir_gather_tests/test.cfg")
    sort_plugin = plugin_loader.load(config, "file_gather_plugin", should_log=False)
    
    file_list = sort_plugin.gather_files(config)
    for filename in file_list:
        print filename
    

if __name__ == "__main__":
    main()