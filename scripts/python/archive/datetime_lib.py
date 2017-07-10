#!/usr/bin/python

'''
Utility subroutines based on the datetime module
'''

import os
import datetime

def last_synoptic_datetime(dt):

    hours=[6,12,18]

    synHour=0
    for hour in hours:
        if dt.hour >= hour:
            synHour=hour
        else:
            break

    return(dt.replace(hour=synHour,minute=0,second=0,microsecond=0))

def next_synoptic_datetime(dt):
            
    hours=[18,12,6]
        
    synHour=0
    for hour in hours:
        if dt.hour < hour:
            synHour=hour
        else:
            break

    return(dt.replace(hour=synHour,minute=0,second=0,microsecond=0))



