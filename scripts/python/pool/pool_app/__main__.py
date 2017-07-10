import sys
import logging
from pool import pool

LOGGER = logging.getLogger(__name__)
        
if __name__ == "__main__":
    pool.main(sys.argv)