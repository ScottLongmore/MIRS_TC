import os
from datetime import datetime

EPOCH = datetime(year=1970, month=1, day=1)

def get_seconds_since_epoch(d):
    return timedelta_total_seconds(d - EPOCH)
    
def timedelta_total_seconds(timedelta):
    return (
        timedelta.microseconds + 0.0 +
        (timedelta.seconds + timedelta.days * 24 * 3600) * 10 ** 6) / 10 ** 6
        
def get_mtime_from_path(path):
    t = os.path.getmtime(path)
    return datetime.fromtimestamp(t)    