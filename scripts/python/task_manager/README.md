======
Task Automation, Queue, and Operational System (TAQOS)
======
:Description:  TAQOS is an algorithm automation framework to build tasks for algorithms 
and execute those algorithms with supplied task data.  
:Keywords: python, task, automation, queue, framework, JSON  
:Version:1.0.1  
:Last Edit: 2016-05-12  
:Author: Scott Longmore, Cooperative Institute for Research in the Atmosphere (CIRA)  
:Contact: Scott.Longmore@colostate.edu  

**Development Team**  
* Scott Longmore - project lead, developer  
* Chris Slocum - developer

**Documentation**  
The framework currently utilized 3 main components
* runTasks.py - the automation program the calls configuration specified task plugin module routines
* [name].json - the JSON configuration for the plugin module
* plgn[name].py - the plug-in module that defines:

                TASKS - task creation - builds tasks e.g. data files, parameters, etc 
                        inputs: config (dictionary from configuration JSON, and runTasks meta data)
                        output: list of tasks (dictionaries)
                WORK  - task algorithm execution - setups and runs algorithm(s) using task data
                        inputs: config (dictionary) and task (dictionary) 
                        output: True on success, False on fail 
                PURGE - purges tasks in <name>_completed.json file, could be used to purge old data as well. 

**Directory Structure**
* scripts/ - hosts the runTasks.py, libTasks.py and linked python utility libraries
* lib/ - utility libraries including:
     - fileAction.py (file search regular expression module) 
     - dirRE.py (directory search regular expression module, used by fileAction.py) 
* etc/ - hosts the JSON schema validation files for runTasks configuration and completed files
* plugins/ - hosts examples of plugin modules (plgn<name>.py)and configuration files (<name>.json)