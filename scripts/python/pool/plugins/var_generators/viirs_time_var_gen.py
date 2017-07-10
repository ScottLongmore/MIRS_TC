import numpy as np
from datetime import datetime
from datetime import timedelta
from pool import time_util as tu

VIIRS_EPOCH = datetime(1958, 1, 1)
#Some viirs files are in IET rather than UTC, IET is UTC + 5

def generate_vars(in_file, config):
    lat_var = in_file[config.get("viirs_time_var_gen", "lat_var_name")]
    mid_time = in_file[config.get("viirs_time_var_gen", "mid_time_var_name")]
    
    time_zone_offset = 0
    if config.has_option("viirs_time_var_gen", "time_zone_offset"):
        time_zone_offset = config.getint("viirs_time_var_gen", "time_zone_offset")
    tz_offset = timedelta(hours=time_zone_offset)
    
    scanline_count = lat_var.shape[0]//mid_time.shape[0]
    
    time_var = np.zeros(lat_var.shape)
    for i in xrange(lat_var.shape[0]):
        time_index = i//scanline_count
        td = timedelta(microseconds=int(mid_time[time_index]))
        sample_date = td + VIIRS_EPOCH + tz_offset
        time_var[i] = tu.get_seconds_since_epoch(sample_date)
        
    meta_data = {"missing_val":np.NaN}
    var_list = []
    var_list.append( ("viirs_time", time_var, meta_data) )
    
    return var_list