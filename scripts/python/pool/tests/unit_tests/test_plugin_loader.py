import unittest
import ConfigParser
import copy
import os
import sys
sys.path.append(os.getcwd())

import pool.plugin_loader as loader
import pool.config_sections as cfg

TEST_PLUGIN_ROOT_DIR = "tests/unit_tests/test_plugins/"
TEST_PLUGIN_NAME = "foo_plugin"
TEST_PLUGIN_PATH = "foo/plugin.py"

class TestPluginLoader(unittest.TestCase):

    def setUp(self):
        self.config = ConfigParser.SafeConfigParser()
        self.config.add_section(cfg.PLUGINS_SEC)
        self.config.set(cfg.PLUGINS_SEC, "plugins_root_dir", TEST_PLUGIN_ROOT_DIR)
        self.config.set(cfg.PLUGINS_SEC, TEST_PLUGIN_NAME, TEST_PLUGIN_PATH)

    def test_call_func_from_loaded_plugin(self):
        plugin = loader.load(self.config, TEST_PLUGIN_NAME)
        
        val = plugin.return_10()
        self.assertEquals(val, 10)
        

