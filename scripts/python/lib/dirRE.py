#!/usr/bin/python

# Stock modules
import sys
import os
import re
import datetime
import optparse
import operator
import collections
import json
import importlib
import logging
import traceback

LOG = logging.getLogger(__name__)

class dirRE(object):

   def __init__(self,dirREstring):

       self.dirREstring=dirREstring
       self.dirTiers=self.dirREstring.split(os.sep)
       self.dirTree={}

       self._createDirTree(self.dirTiers,1,'/',self.dirTree)

   def getDirs(self):
       
       dirs=[]
       self._createDirs(self.dirTree,'/','/',dirs)
       return(dirs)

   def _createDirTree(self,dirTiers,tier,dir,dirTree):
       dirPattern="^\^(.+)\$$"
  
       dirTier=dirTiers[tier]
       #   print "{} : {} ".format(dirTier,tier)
       dirMatch=re.match(dirPattern,dirTier)
       subdirs=[]

       if dirMatch:
           subdirPattern=dirMatch.group(1)
           for subEntry in os.listdir(dir):
               subdirMatch=re.match(subdirPattern,subEntry)
	       if subdirMatch:
                    cwdir=os.path.join(dir,subEntry)
	            if os.path.isdir(cwdir): 
                       subdirs.append(subEntry) 
                    else:
                       LOG.warning("Sub-directory match not a directory, ignoring")
       else:
           cwdir=os.path.join(dir,dirTier)
           if os.path.isdir(cwdir):
              subdirs.append(dirTier) 
           else:
              LOG.warning("Dir RE: {} not a sub-directory, or directory reg-expression")
              return(1)
    
       for subdir in subdirs:
           if tier < len(dirTiers)-1:
                dirTree[subdir]={}
                subdirTree=dirTree[subdir];
                cwdir=os.path.join(dir,subdir)
                self._createDirTree(dirTiers,tier+1,cwdir,subdirTree)
                #print "{} {} {}".format(tier,subdir,type(dirTree[subdir]))
           else:
                dirTree[subdir]=subdir
                #print "{} {} {} last".format(tier,subdir,type(dirTree[subdir]))

       return


   def _createDirs(self,dirTree,rootpath,path,dirs):
    
       tierDirs=dirTree.keys()
       for tierDir in tierDirs:
           if isinstance(dirTree[tierDir], dict): 
               subpath=os.path.join(path,tierDir)
               self._createDirs(dirTree[tierDir],rootpath,subpath,dirs)
           else:
               subpath=os.path.join(path,tierDir)
               dirs.append(subpath) 
               # path=rootpath
