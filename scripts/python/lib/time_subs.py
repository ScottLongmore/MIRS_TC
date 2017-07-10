#!/usr/bin/python

import os
from datetime import timedelta, datetime

def get_modification_date(filename):
    """
    function: get_modification_date
        determines the modification time of a file with the given filename
    input:
        string filename: filename
    output:
        datetime modification_time: the modification time of the file
    """
    t = os.path.getmtime(filename)
    return datetime.fromtimestamp(t)
    
def get_hms_from_seconds(seconds):
    """
    function: get_hms_from_seconds
        coverts the given number of seconds to hours, minutes, and seconds
    input:
        int seconds: the number of seconds to convert
    output:
        int hours: the number of hours that fit evenly into the given number of seconds
        int minutes: the remaining minutes after the hours have been subtracted from the given seconds
        int seconds: the remaining seconds after the hours and minutes have been subtracted from the given seconds
    """
    seconds = seconds
    hour = seconds/(60*60)
    
    remainder = seconds - hour*(60*60)
    minute = remainder/(60)
    remainder = remainder - minute*60
    
    return hour, minute, remainder

def julian_to_date(year, jday=1):
    """
    #### WORKS with Python 2.4 #######
    function: julian_to_date
      Converts a julian date to a calendar date
    input:  
      int year : year 
      int jday : julian date
    output:
      int month: month
      int day  : day
    exception:
      ValueError : Raised if year < 0 or jday > number_of_days_in_a_year or jday < 1
      TypeError : Raised if year or jday are not integers
    """
    
    if year < 0:
        raise ValueError("Year Value Can Not Be Negative: %04d" % (year))
         
    max_jday = (datetime(year, 12, 31) - datetime(year, 1, 1)).days+1
    
    if jday <= 0:
        raise ValueError("Julian Day Value (jday) Can Not Be Less Than 1: %d" % (jday))
    elif jday > max_jday:
        raise ValueError("Julian Day Value (jday) Can Not Be Greater Than %03d: %03d" % (max_jday, jday))    
    
    date = datetime(year, 1, 1) + timedelta(jday - 1)
    return date.month, date.day

def dt_from_60s_timestamp(year,month,day,hour,min,sec):
    """
    make sure seconds are between 0 and 59;
    and create datetime object from (year,month,day,hour,min,sec)
    in .XYA (and possibly .ret) files
    11:15:00 sometimes is recorded as 11:14:60
    input:  
      int year,month,day,hour,min,sec : year,month,day,hour,min,sec
    output:
      datetime object dt_time: datetime
    """
    if sec == 60:
        sec = 59
        add_time = timedelta(0,1)
        dt_time = datetime(year,month,day,hour,min,sec)+add_time
    else:
        dt_time = datetime(year,month,day,hour,min,sec)
    return dt_time

 
