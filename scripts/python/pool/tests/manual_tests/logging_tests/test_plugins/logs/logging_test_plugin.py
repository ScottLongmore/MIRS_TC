import logging

def setup_logging(config):
    logging.basicConfig(format="Foo! %(levelname)s - %(message)s", level=logging.DEBUG)