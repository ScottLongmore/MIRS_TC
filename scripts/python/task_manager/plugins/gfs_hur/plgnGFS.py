#!/usr/bin/python
"""
  plgnGFS.py - plug-in methods: TASKS, WORK, PURGE
"""

# Stock modules
import sys
import os
import re
import copy
import logging
import traceback
import datetime
import collections
import operator
import subprocess

# Local modules
import error_codes
import fileAction
import dirRE

LOG = logging.getLogger(__name__)  # create the logger for this file

fcsts = range(0, 127, 6)

# Parameters
DTSFormat = "%Y%m%d%H%M%S"
ISODTSFormat = "%Y%m%dT%H%M%S"


def PURGE(config, tasks):
    """
    Removes tasks with DTS older than backward search datetime

    Parameters
    ----------
    config : dict
    tasks : dict

    Returns
    currentTasks : dict
        dictionary of current tasks to run
    """
    currentTasks = []
    for task in tasks:
        taskDTG = datetime.datetime.strptime(task['DTS'], ISODTSFormat)
        if taskDTG > config['meta']['bkwdDTG']:
            currentTasks.append(task)
    return(currentTasks)


def TASKS(config):
    """
    Generates list of tasks to run

    Parameters
    ----------
    config : dict
    """
    meta = config['meta']
    inputs = config['inputs']['grib2']

    tasksLogFile = os.path.join(config['logDir'],
                                "{}_TASKS_{}.log".format(config['name'], meta['runDTG'].strftime(ISODTSFormat)))
    tasksLogFH = open(tasksLogFile, "a")

    # Find task files
    FA = fileAction.fileAction(config)
    inputFiles = FA.findInputFiles(['grib2'])
    records = {}
    filepaths = inputFiles['grib2']
    for filepath in filepaths:

        m = re.match(inputs['re'], os.path.basename(filepath))
        fields = m.groupdict()
        DTS = fields['DTS'] + '0000'
        DTG = datetime.datetime.strptime(DTS, DTSFormat)
        fcst = fields['fcst']
        file_id = "_".join([DTS, fcst])

        if file_id not in records:
            records[file_id] = collections.OrderedDict()
            records[file_id]['fcst'] = []
            records[file_id]['files'] = []

        records[file_id]['DTS'] = DTG.strftime(ISODTSFormat)
        records[file_id]['fcst'].append(fcst)
        records[file_id]['files'].append(filepath)

    ids = records.keys()
    ids.sort()
    ids.reverse()

    tasks = []
    for file_id in ids:
        tasks.append(records[file_id])
    # Remove any older tasks than backward search datetime
    tasks = PURGE(config, tasks)

    tasksLogFH.close()
    return(tasks)


def WORK(config, task):
    """
    The work to be done for each task

    Parameters
    ----------
    config : dict
    task : dict

    Returns
    -------
    status : Boolean
    """
    status = False
    return(status)
