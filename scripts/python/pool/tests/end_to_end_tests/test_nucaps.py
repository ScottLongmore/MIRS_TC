import unittest
import ConfigParser
import copy
import os
import shutil
import sys
import pdb
import json
from subprocess import call
#sys.path.append(os.getcwd())

BASE_DIR = "tests/end_to_end_tests/nucaps/"
CONSTANTS_CONFIG = os.path.join(BASE_DIR, "configs/constants.cfg")
UPDATE_CONFIG = os.path.join(BASE_DIR, "configs/update.cfg")
POOL_DIR = os.path.join(BASE_DIR, "output/pool_nucaps/")
META_INFO_FILENAME = os.path.join(POOL_DIR, "meta_info.json")

class TestNucapsCreate(unittest.TestCase):

    def setUp(self):
        call(["python", "pool", "create", CONSTANTS_CONFIG])
        

    def test_for_correct_meta_data_grid_spacing(self):
        with open(META_INFO_FILENAME, "r") as in_file:
            meta_info = json.load(in_file)
        self.assertEquals(meta_info["lat_spacing_degrees"], 1)
        self.assertEquals(meta_info["lon_spacing_degrees"], 1)
        self.assertEquals(meta_info["time_spacing_seconds"], 3600)
        
    def tearDown(self):
        shutil.rmtree(POOL_DIR)
        
class TestNucapsUpdate(unittest.TestCase):

    def setUp(self):
        call(["python", "pool_app", "create", CONSTANTS_CONFIG])
        call(["python", "pool_app", "update", CONSTANTS_CONFIG, UPDATE_CONFIG])
        

    def test_for_correct_time_dir_gen(self):
        time_dir = os.path.join(POOL_DIR, "403553")
        self.assertTrue(os.path.isdir(time_dir))
        
    def tearDown(self):
        shutil.rmtree(POOL_DIR)

