from datetime import datetime
from datetime import timedelta
import numpy as np

from pool import bounding_box as bb

NUCAPS_SCANLINE_LEN = 30
RIGHT_OF_NADIR_INDEX = 15

def generate_vars(in_file, config):
    lat_var = in_file[config.get("nucaps_var_gen", "lat_var_name")]
    lon_var = in_file[config.get("nucaps_var_gen", "lon_var_name")]
    time_var = in_file[config.get("nucaps_var_gen", "time_var_name")]
    should_use_negative_west = config.getboolean("nucaps_var_gen", "use_negative_west_lons")

    swath_time = np.zeros(lat_var.shape)
    swath_lat = np.zeros(lat_var.shape)
    swath_lon = np.zeros(lon_var.shape)
    
    for i in xrange(lat_var.shape[0]):    
        scanline_index = int(i/NUCAPS_SCANLINE_LEN)
        nadir_right_index = scanline_index*NUCAPS_SCANLINE_LEN + RIGHT_OF_NADIR_INDEX
        nadir_left_index = nadir_right_index - 1
        
        lon1 = lon_var[nadir_right_index]
        lon2 = lon_var[nadir_left_index]
        
        lat1 = lat_var[nadir_right_index]
        lat2 = lat_var[nadir_left_index]
        
        time1 = time_var[nadir_right_index]
        time2 = time_var[nadir_left_index]
        
        if not _is_data_valid(lon1, lon2, lat1, lat2, time1, time2):
            swath_time[i] = np.NaN
            swath_lat[i] = np.NaN
            swath_lon[i] = np.NaN
            continue
        
        if should_use_negative_west:
            lon1 = bb.convert_lon_to_negative_west(lon1)
            lon2 = bb.convert_lon_to_negative_west(lon2)
        else:
            lon1 = bb.convert_lon_to_0_to_360(lon1)
            lon2 = bb.convert_lon_to_0_to_360(lon2)
            
        swath_lon[i] = (lon1 + lon2)/2.0
        swath_lat[i] = (lat1 + lat2)/2.0
        swath_time[i] = (time1 + time2)/2.0
    
    meta_data = {"missing_val":np.NaN}
    return [("ScanLine_Center_Time", swath_time, meta_data),
        ("ScanLine_Center_Lon", swath_lon, meta_data),
        ("ScanLine_Center_Lat", swath_lon, meta_data)]
            
def _is_data_valid(lon1, lon2, lat1, lat2, time1, time2):
    if not _is_lon_valid(lon1):
        return False
        
    if not _is_lon_valid(lon2):
        return False
        
    if not _is_lat_valid(lat1):
        return False
        
    if not _is_lat_valid(lat2):
        return False
        
    if not _is_time_valid(time1):
        return False
        
    if not _is_time_valid(time2):
        return False
        
    return True

def _is_lon_valid(lon):
    if lon < -181 or lon > 361 or np.isnan(lon):
        return False
        
    return True

def _is_lat_valid(lat):
    if lat < -91 or lat > 91:
        return False
        
    return True

def _is_time_valid(time):
    if time < 0 or np.isnan(time):
        return False
        
    return True