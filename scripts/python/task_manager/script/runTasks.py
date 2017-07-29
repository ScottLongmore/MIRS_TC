# -*- coding: utf-8 -*-
"""
runTasks.py - creates and runs tasks from a json configuration file, which specifies the plugin task, work, and purge methods
              for generating, executing, and removing tasks. Completed tasks are written to a task list json file for future runs
              and in the event of a system failure.
"""

# Stock modules
import sys
import os
import psutil
import datetime
import argparse
import collections
import copy
import json
import jsonschema
import importlib
import logging
import pprint
import traceback


# Set local module paths
try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"..","lib"))
    sys.path.insert(2,os.path.join(parentPath,"plugins"))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local modules
import setup_logging
import error_codes
import utils
import libTask

__author__ = 'Scott Longmore'
__copyright__ = 'Copyright 2015'
__version__ = '1.0dev1'
__license__ = 'BSD 3-clause'
__maintainer__ = 'Scott Longmore'
__email__ = 'scott.longmore@colostate.edu'

# Variables
runDTG = datetime.datetime.utcnow()
ISODTSFormat = "%Y%m%dT%H%M%S"
schema_task = json.load(open('schema_task.json', 'r'), object_pairs_hook=collections.OrderedDict)
pp=pprint.PrettyPrinter(indent=4)

# Read Command Line Arguments
options = argparse.ArgumentParser(prog='runTasks')
options.add_argument('-c', '--config', dest='config', help='Configuration File')
options.add_argument('-l', '--log', dest='log', help='Log File')
try:
    args = options.parse_args()
    config_filename = args.config
    logFile = args.log
except:
    print('Syntax: python runTasks.py -c <config.json> -l <log file>')
    sys.exit(1)

# setup logging
LOG = logging.getLogger('runTasks')  # create the logger for this file
setup_logging.setup_logging("runTasks",logFile)

# Determine if process is running, or in zombie state
cpid = os.getpid()
#commandRE = "^{}$".format(" +".join([sys.executable] + sys.argv))
commandRE = "^{}.*{}.*$".format(sys.executable,config_filename)
procs = utils.getProcesses(commandRE)
if cpid in procs:
    del procs[cpid]
if len(procs) > 0:
    for pid in procs:
        if procs[pid].status() != psutil.STATUS_ZOMBIE:
            LOG.info("Process {} ({}) is running, exiting".format(__file__, pid))
            sys.exit(0)
        else:
            LOG.info("Process {} ({}) is in zombie state, terminating".format(__file__, pid))
            procs[pid].kill()

# Read and Validate Configuration File
try:
    LOG.info("Processing JSON config file: {}".format(config_filename))
    config = json.load(open(config_filename), object_pairs_hook=collections.OrderedDict)

    validator = jsonschema.Draft4Validator(schema_task)
    errs = sorted(validator.iter_errors(config), key=lambda e: e.path)

    if errs:
        msg = ""
        for err in errs:
            msg += err.message
        utils.error(LOG, msg, error_codes.EX_IOERR)

except:
    msg = "Error in JSON config file: {}".format(config_filename)
    utils.error(LOG, msg, error_codes.EX_IOERR)

# Load library module
try:
    module = importlib.import_module(config['plugin']['module'])
    purgeMethod = config['plugin']['purge']
    tasksMethod = config['plugin']['tasks']
    workMethod = config['plugin']['work']
except:
    msg = "Unable to load module: {}".format(config['plugin']['module'])
    utils.error(LOG, msg, error_codes.EX_IOERR)

# Determine run, backward search DTG and iteration delta
meta = config['meta']
meta['runDTG'] = runDTG
meta['bkwdDTG'] = meta['runDTG']-datetime.timedelta(seconds=meta['bkwdDelta'])
LOG.info("Run datetime: {}".format(meta['runDTG'].strftime(ISODTSFormat)))
LOG.info("Backward search datetime: {}".format(meta['bkwdDTG'].strftime(ISODTSFormat)))

# Add config replace section, add DTG strings
config['replace'] = {}
config['replace'].update(utils.DTGrangeToStrings(meta['bkwdDTG'], meta['runDTG'], meta['DTGfrmts']))

# Copy master config to workConfig
workConfig = copy.deepcopy(config)
primeTasksKey = workConfig['primeTasksKey']

# Read task list json file
LOG.info("Reading processed tasks JSON file: {}".format(config['completeTaskFile']))
readTasks = libTask.read_tasks_json_file(config['completeTaskFile'])

# Purge unneeded tasks using plug-in purge method
try:
    args = [workConfig, readTasks]
    completeTasks = getattr(module, purgeMethod)(*args)
    del readTasks[:]
except:
    msg = "Problem in module: {} purge method: {}".format(module, purgeMethod)
    utils.error(LOG, msg, error_codes.EX_IOERR)

# Create task list from plug-in tasks method
try:
    LOG.info("Creating new tasks list")
    args = [workConfig]
    createdTasks = getattr(module, tasksMethod)(*args)
except:
    msg = "Problem in module: {} tasks method: {}".format(module, tasksMethod)
    utils.error(LOG, msg, error_codes.EX_IOERR)

# Compare created and completed tasks lists, put difference into task list
tasks = libTask.new_tasks(completeTasks, createdTasks)
del createdTasks[:]

# Iterate through tasks, newest to oldest, insert into completeTask list
# and update complete task list file, after each complete task (incase of system failure)

executedTasks = []
incompleteTasks = []
LOG.info("Running Tasks...")
while tasks:

    LOG.info("Tasks in queue: {}".format(len(tasks)))
    LOG.info("{}".format(libTask.print_tasks_keys_values(tasks, primeTasksKey)))

    # Get latest task
    task = tasks.pop(0)

    # Execute task
    try:
        args = [workConfig, task]
        status = getattr(module, workMethod)(*args)
        
        if status:
            LOG.info("Task: {} completed".format(libTask.print_tasks_keys_values([task], primeTasksKey)))
            executedTasks.append(task)
        else:
            LOG.warning("Problem completing task: {}, dequeing task and continuing to next task".format(libTask.print_tasks_keys_values([task], primeTasksKey)))
            incompleteTasks.append(task)

    except:
        LOG.warning("Unable to complete task, dequeing task and continuing to next task")
        traceback.print_exc(file=sys.stdout)
        incompleteTasks.append(task)

    # Determine if any new tasks are available
    try:
        LOG.info("Searching for new tasks")
        args = [workConfig]
        createdTasks = getattr(module, tasksMethod)(*args)
    except:
        msg = "Problem in module: {} tasks routine: {}".format(module, tasksMethod)
        utils.error(LOG, msg, error_codes.EX_IOERR)

    # Compare complete, created, executed and current tasks lists, prepend new tasks to task list
    workTasks = libTask.new_tasks(completeTasks, createdTasks)
    del createdTasks[:]
    workTasks = libTask.new_tasks(executedTasks, workTasks)
    workTasks = libTask.new_tasks(incompleteTasks, workTasks)
    workTasks = libTask.new_tasks(tasks, workTasks)
    LOG.info("Adding tasks: {}".format(libTask.print_tasks_keys_values(workTasks, primeTasksKey)))
    libTask.prepend_tasks(tasks, workTasks)
    del workTasks[:]

    # Update completed tasks list json file
    LOG.info("Updating completed tasks list JSON file: {}".format(config['completeTaskFile']))
    writeTasks = executedTasks + completeTasks
    writeTasks = libTask.sort_tasks(writeTasks, primeTasksKey, workConfig['sortReverseOption'])
    libTask.write_tasks_json_file(config['completeTaskFile'], writeTasks)
    del writeTasks[:]
    LOG.info("Current memory usage: %.2f" % utils.memory_usage())

LOG.info("Tasks Executed:")
LOG.info("{}".format(libTask.print_tasks_keys_values(executedTasks, primeTasksKey)))
LOG.info("Completed")
