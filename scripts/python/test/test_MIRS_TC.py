# System
import sys
import os
import unittest
import json
import logging
import pprint

try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"lib"))
    sys.path.insert(2,os.path.join(parentPath,"mirs_tc"))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local
import MIRS_TC 
import setup_logging
 
class TestMIRS_TC(unittest.TestCase):
 
    def setUp(self):
        LOG = logging.getLogger(__file__) #create the logger for this file
        setup_logging.setup_logging("MIRS_TC","MIRS_TC.LOG")
        self.pp=pprint.PrettyPrinter(indent=4)
 
    def test_scaleOffsetThreshPoolTextFile(self):
        
        scale=0.01
        offset=0.0
        lowThresh=0.0
        highThresh=500.0
        inputMissing=-999.0
        outputMissing=-999.0
        filename="LWP.txt"
        newfilename="new_LWP.txt"
        # Test LWP
        status=MIRS_TC.scaleOffsetThreshPoolTextFile(scale,offset,lowThresh,highThresh,inputMissing,outputMissing,filename,newfilename)
        self.assertTrue(status)
 
 
if __name__ == '__main__':
    unittest.main()
