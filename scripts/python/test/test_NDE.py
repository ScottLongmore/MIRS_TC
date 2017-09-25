# System
import unittest
import json
import logging
import pprint

try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"lib"))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local
import NDE
import setup_logging
 
class TestNDE(unittest.TestCase):
 
    def setUp(self):
        LOG = logging.getLogger(__file__) #create the logger for this file
        setup_logging.setup_logging("testNDE")
        self.pp=pprint.PrettyPrinter(indent=4)
        pcfFile="NPP_TC.PCF"
        self.pcfObj=NDE.PCF(pcfFile)
 
    def test_PCF(self):
        self.assertIsInstance(self.pcfObj, object)

    def test_getDict(self):
        pcfDict=self.pcfObj.getDict()
        self.assertIsInstance(pcfDict,dict)
        self.pp.pprint(pcfDict)
         
 
 
if __name__ == '__main__':
    unittest.main()
