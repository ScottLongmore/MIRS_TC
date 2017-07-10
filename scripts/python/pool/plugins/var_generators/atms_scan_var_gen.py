import numpy as np
from datetime import datetime
from datetime import timedelta
import pdb

from pool import time_util as tu
from pool import bounding_box as bb

def generate_vars(in_file, config):
    year_var = in_file[config.get("atms_var_gen", "year_var_name")]
    month_var = in_file[config.get("atms_var_gen", "month_var_name")]
    day_var = in_file[config.get("atms_var_gen", "day_var_name")]

    utc_var = in_file[config.get("atms_var_gen", "utc_var_name")]

    lat_var = in_file[config.get("atms_var_gen", "lat_var_name")]
    lon_var = in_file[config.get("atms_var_gen", "lon_var_name")]
    should_use_negative_west = config.getboolean("atms_var_gen", "use_negative_west_lons")

    time_var = np.zeros(lat_var.shape)
    swath_lat = np.zeros(lat_var.shape)
    swath_lon = np.zeros(lon_var.shape)

    for i in xrange(lat_var.shape[0]):
        year = year_var[i]
        month = month_var[i]
        day = day_var[i]
        utc = utc_var[i]
        center_lon = _get_scanline_center_lon(i, lon_var, should_use_negative_west)
        center_lat = _get_scanline_center_lat(i, lat_var)

        if not _is_scanline_data_valid(year, month, day, utc, center_lon, center_lat):
            time_var[i] = np.NaN
            swath_lat[i] = np.NaN
            swath_lon[i] = np.NaN
            continue

        base_dt = datetime(year=year, month=month, day=day)
        td = timedelta(seconds=utc)

        dt = base_dt + td
        time_var[i] = tu.get_seconds_since_epoch(dt)

        swath_lat[i] = center_lat
        swath_lon[i] = center_lon

    meta_data = {"missing_val":np.NaN}
    return [("atms_time", time_var, meta_data),
        ("ScanLine_Center_Time", time_var, meta_data),
        ("ScanLine_Center_Lon", swath_lon, meta_data),
        ("ScanLine_Center_Lat", swath_lat, meta_data)]

def _is_scanline_data_valid(year, month, day, utc, center_lon, center_lat):
    if year < 0 or month < 0 or day < 0 or utc < 0 or year > 3000 or month > 12 or day > 31 or utc > 87000 or np.isnan(utc):
        return False
    elif not _is_lon_valid(center_lon):
        return False
    elif not _is_lat_valid(center_lat):
        return False

    return True

def _is_lon_valid(lon):
    if lon < -181 or lon > 361 or np.isnan(lon):
        return False

    return True

def _is_lat_valid(lat):
    if lat < -91 or lat > 91 or np.isnan(lat):
        return False

    return True

def _get_scanline_center_lon(sample_index, lon_var, should_use_negative_west):
    lon1, lon2 = _get_coord_samples(sample_index, lon_var)

    if not _is_lon_valid(lon1) or not _is_lon_valid(lon2):
        return np.NaN

    if should_use_negative_west:
        lon1 = bb.convert_lon_to_negative_west(lon1)
        lon2 = bb.convert_lon_to_negative_west(lon2)
    else:
        lon1 = bb.convert_lon_to_0_to_360(lon1)
        lon2 = bb.convert_lon_to_0_to_360(lon2)

    return (lon1+lon2)/2.0

def _get_scanline_center_lat(sample_index, lat_var):
    lat1, lat2 = _get_coord_samples(sample_index, lat_var)
    return (lat1+lat2)/2.0

def _get_coord_samples(sample_index, coord_var):
    coord1 = np.NaN
    coord2 = np.NaN

    center_index = coord_var.shape[1]/2
    if coord_var.shape[1] % 2 == 0:
        coord1 = coord_var[sample_index, center_index]
        coord2 = coord_var[sample_index, center_index-1]
    else:
        coord1 = coord_var[sample_index, center_index]
        coord2 = coord1

    return coord1, coord2
