from datetime import datetime
from datetime import timedelta
import config_sections as cfg
import time_util as tu
import numpy as np
import pdb

#***CONSTANTS***
DEGREE_TO_KM_CONVERSION_FACTOR = 111.1
COMPARISON_EPSILON = 0.001
DEFAULT_TIME_FORMAT = "%Y-%m-%d-%H-%M-%S"

class Rectangle:
    def __init__(self, left, right, bottom, top):
        if left > right:
            raise ValueError("Left must be less than right. Left: %f Right: %f" % (left, right))
        if bottom > top:
            raise ValueError("Top must be lower than bottom. Top: %f, Bottom: %f" % (top, bottom))
    
        self.left = left
        self.right = right
        self.bottom = bottom
        self.top = top
        
        self.width = right - left
        self.height = top - bottom
        
        self.center_x = self.left + (self.width/2.0)
        self.center_y = self.right + (self.height/2.0)
    
    def is_intersecting(self, rect):
        if self.left <= rect.right and self.right >= rect.left and \
        self.bottom <= rect.top and self.top >= rect.bottom:
            return True

        return False
    
    def is_intersecting_rectangles(self, other_rects):
        for rect in other_rects:
            if self.is_intersecting(rect):
                return True
            
        return False
        
    def is_point_in_rect(self, x, y):
        point_is_in_rect = False
        if self.left <= x and self.right >= x and self.bottom <= y and self.top >= y:
            point_is_in_rect = True
            
        return point_is_in_rect
        
def convert_lon_to_0_to_360(lon):
    if lon > 360 + COMPARISON_EPSILON:
        raise ValueError("%f is greater than 360" % (lon))
    elif lon < -180 - COMPARISON_EPSILON:
        raise ValueError("%f is less than -180" % (lon))
    
    if lon < 0:
        lon = 360 + lon
        
    assert not np.isnan(lon)
    assert lon > 0 - COMPARISON_EPSILON
    assert lon < 360 + COMPARISON_EPSILON        
    
    return lon 
    
def convert_lon_to_negative_west(lon):
    if lon > 360 + COMPARISON_EPSILON:
        raise ValueError("%f is greater than 360" % (lon))
    elif lon < -180 - COMPARISON_EPSILON:
        raise ValueError("%f is less than -180" % (lon))
    
    if lon > 180:
        lon = lon - 360
    
    return lon

def _convert_wraparound_lon(lon):
    if lon > 180:
        lon -= 360
    elif lon < -180:
        lon += 360
        
    return lon
    
def get_rectangles_from_lat_lon_area(left_lon, right_lon, bottom_lat, top_lat):
    
    bottom_lat = max(-90, bottom_lat)
    bottom_lat = min(90, bottom_lat)
    top_lat = max(-90, top_lat)
    top_lat = min(90, top_lat)
    
    if bottom_lat > top_lat:
        raise ValueError("Bottom lat must be lower than top lat. Bottom lat: %f Top lat: %f" % (bottom_lat, top_lat))
    if right_lon > 360 + COMPARISON_EPSILON:
        raise ValueError("Right lon must be less than 360 degrees. Right lon: %f Left lon: %f" % (right_lon, left_lon))
    if right_lon < -360 - COMPARISON_EPSILON:
        raise ValueError("Right lon must be greater than -360 degrees. Right lon: %f Left lon: %f" % (right_lon, left_lon))
    if left_lon > 360 + COMPARISON_EPSILON:
        raise ValueError("Left lon must be less than 360 degrees. Right lon: %f Left lon: %f" % (right_lon, left_lon))
    if left_lon < -360 - COMPARISON_EPSILON:
        raise ValueError("Left lon must be greater than -360 degrees. Right lon: %f Left lon: %f" % (right_lon, left_lon))
    if right_lon - left_lon > 360:
        raise ValueError("Area too big, right_lon - left_lon must be less than 360.  Right lon: %f Left lon: %f" % (right_lon, left_lon))

    rectangles = []
    
    left_lon = _convert_wraparound_lon(left_lon)
    right_lon = _convert_wraparound_lon(right_lon)
    
    height = top_lat - bottom_lat
    
    #need to split the area into two rectangles
    if right_lon < left_lon:
        rect = Rectangle(-180, right_lon, bottom_lat, top_lat)
        rectangles.append(rect)
    
        rect = Rectangle(left_lon, 180, bottom_lat, top_lat)
        rectangles.append(rect)
    else:
        rect = Rectangle(left_lon, right_lon, bottom_lat, top_lat)
        rectangles.append(rect)
        
    return rectangles
    
def is_point_in_boxes(boxes, x, y):
    for box in boxes:
        if box.is_point_in_rect(x, y):
            return True
            
    return False
    
def are_rectangles_overlapping(rectangles_a, rectangles_b):
    for rect_a in rectangles_a:
        if rect_a.is_intersecting_rectangles(rectangles_b):
            return True
        
    return False
    
def get_bounding_boxes_from_config(config):
    time_format = DEFAULT_TIME_FORMAT
    if config.has_option(cfg.QUERY_SEC, "time_format"):
        time_format = config.get(cfg.QUERY_SEC, "time_format")
    
    query_time = datetime.strptime(config.get(cfg.QUERY_SEC, "time"), time_format)
    after_delta = timedelta(seconds = config.getint(cfg.QUERY_SEC, "seconds_after"))
    before_delta = timedelta(seconds = config.getint(cfg.QUERY_SEC, "seconds_before"))
    
    start_time = query_time - before_delta
    end_time = query_time + after_delta
    
    center_lat = config.getfloat(cfg.QUERY_SEC, "center_lat")
    center_lon = config.getfloat(cfg.QUERY_SEC, "center_lon")
    width = config.getfloat(cfg.QUERY_SEC, "width_lon")
    height = config.getfloat(cfg.QUERY_SEC, "height_lat")
    
    boxes = get_rectangles_from_lat_lon_area(center_lon - width/2.0, center_lon + width/2.0, center_lat - height/2.0, center_lat + height/2.0)
    
    start_timestamp = tu.get_seconds_since_epoch(start_time)
    end_timestamp = tu.get_seconds_since_epoch(end_time)
    
    return boxes, start_timestamp, end_timestamp
