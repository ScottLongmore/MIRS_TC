import unittest
import glob
import os
import sys
import imp
import pdb

def build_test_suite(root_path):
    search_string = os.path.join(root_path, 'test_*.py')

    suite = unittest.TestSuite()
    for testcase in glob.glob(search_string):
        modname = os.path.basename(testcase)
        modname = modname[:-len('.py')]
        module = imp.load_source(modname, testcase)
        suite.addTest(unittest.TestLoader().loadTestsFromModule(module))    
    return suite
    
if __name__ == "__main__":
    runner = unittest.TextTestRunner()
    suite = build_test_suite(sys.argv[1])
    runner.run(suite)