#!/usr/bin/python
"""
libTask.py - library of routines for handling task...maybe come a module
"""

# Stock modules
import sys
import os
import logging
import collections
import operator
import itertools
import json
import jsonschema

# Local modules
import utils
import error_codes

# change to the current working directory in cron
executable_path = os.path.abspath(__file__)
current_working_directory = os.path.dirname(executable_path)
os.chdir(current_working_directory)

LOG = logging.getLogger(__name__)

schema_completed = json.load(open("./schema_completed.json", 'r'),
                             object_pairs_hook=collections.OrderedDict)


def read_tasks_json_file(jsonFile):
    """
    Read Task list JSON file

    Parameters
    ----------
    jsonFile : string
        json file path

    Returns
    -------
    tasks : dict
        returns json dict if completed tasks
    """
    try:
        LOG.info("Processing task list JSON file: {}".format(jsonFile))
        tasks = json.load(open(jsonFile, 'r'), object_pairs_hook=collections.OrderedDict)
        validator = jsonschema.Draft4Validator(schema_completed)
        errs = sorted(validator.iter_errors(tasks), key=lambda e: e.path)
        if errs:
            msg = ''
            for err in errs:
                msg += err.message
            utils.error(LOG, msg, error_codes.EX_IOERR)
    except:
        LOG.info("Unable to open/process: {} task list json file".format(jsonFile))
        tasks = [] # CS -- What if there is no completed file? The return fails.
    return(tasks)


def write_tasks_json_file(jsonFile, tasks):
    """
    Write New Processed Image Files

    Parameters
    ----------
    jsonFile : string
        completed task json file path
    tasks : dict

    Returns
    -------
    None : NoneType
    """
    # print jsonFile
    # print tasks
    try:
        taskFH = open(jsonFile, "w")
        taskFH.write(json.dumps(tasks, indent=4, skipkeys=True))
        taskFH.close()
    except:
        msg = "Unable to open/write: {} task list json file".format(jsonFile)
        utils.error(LOG, msg, error_codes.EX_IOERR)
    return()


def sort_tasks(tasks, keyOption, reverseOption):

    try:
        sortedTasks = sorted(tasks, key=operator.itemgetter(keyOption), reverse=reverseOption)
    except:
        msg = "Unable to sort task list"
        utils.error(LOG, msg, error_codes.EX_IOERR)
    return(sortedTasks)


def compare_tasks(tasks1, tasks2):

    try:
        diffList = list(itertools.ifilterfalse(lambda x: x in tasks1, tasks2)) \
                 + list(itertools.ifilterfalse(lambda x: x in tasks2, tasks1))
    except:
        msg = "Unable to compare task lists"
        utils.error(LOG, msg, error_codes.EX_IOERR)
    return(diffList)


def new_tasks(tasks, updatedTasks):

    try:
        newTasks = list(itertools.ifilterfalse(lambda x: x in tasks, updatedTasks))
    except:
        msg = "Unable to get new task lists"
        utils.error(LOG, msg, error_codes.EX_IOERR)
    return(newTasks)


def prepend_tasks(tasks, prependTasks):

    try:
        tasks[:0] = prependTasks
    except:
        msg = "Unable to prepend task lists"
        utils.error(LOG, msg, error_codes.EX_IOERR)
    return(True)


def print_tasks(tasks):

    print(json.dumps(tasks, indent=4))
    sys.stdout.flush()
    return()


def print_tasks_keys_values(tasks, key):

    values = []
    for task in tasks:
        values.append(task[key])
    return(values)
