import logging

def setup_logging(program_name):
    logging.basicConfig(\
        level=logging.DEBUG,\
        format='%(asctime)s | '+program_name+' | %(name)s | %(levelname)s: %(message)s',\
        datefmt='%Y%m%d%H%M%S'\
    )
