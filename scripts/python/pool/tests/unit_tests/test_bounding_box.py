import unittest
import pdb
import ConfigParser
import copy
from datetime import datetime
from datetime import timedelta

import pool.config_sections as cfg
import pool.bounding_box as bb
import pool.time_util as tu

LEFT = 0
RIGHT = 1
BOTTOM = 2
TOP = 3

class TestBoundingBoxFromLatLonArea(unittest.TestCase):

    def setUp(self):
        #map bounds include lons from -180 to 180 degrees, and lats from -90 to 90 degrees
        #box longitudes are allowed to wrap around one hemisphere (180 degrees to the right or left of the map bounds)
        self.no_wraparound_box = (-10, 20, -30, 45) #box that sits inside map bounds without wrapping around
        self.wraparound_right_box = (100, 200, 20, 60)
        self.wraparound_left_box = (-210, 50, -10, 50)
        
        self.bottom_above_top_box = (-10, 20, 33, -5)
        self.bottom_below_valid_bounds_box = (-10, 20, -91, 20)
        self.bottom_above_valid_bounds_box = (-10, 20, 91, 92)       
        self.top_below_valid_bounds_box = (-10, 20, -92, -91)
        self.top_above_valid_bounds_box = (-10, 20, 20, 91)
        
        self.left_above_valid_bounds_box = (361, 50, 20, 60)
        self.left_below_valid_bounds_box = (-361, 50, 20, 60)
        
        self.right_above_valid_bounds_box = (20, 361, 20, 60)
        self.right_below_valid_bounds_box = (20, -361, 20, 60)
        
        self.too_large_box = (-181, 181, 20, 50)


    def test_no_wraparound_box_returns_one_box(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.no_wraparound_box)
        self.assertEquals(len(boxes), 1)
        
    def test_no_wraparound_box_returns_box_with_correct_borders(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.no_wraparound_box)
        box = boxes[0]
        
        self.assertAlmostEquals(box.left, self.no_wraparound_box[LEFT])
        self.assertAlmostEquals(box.right, self.no_wraparound_box[RIGHT])
        self.assertAlmostEquals(box.bottom, self.no_wraparound_box[BOTTOM])
        self.assertAlmostEquals(box.top, self.no_wraparound_box[TOP])
        
    def test_no_wraparound_box_returns_box_with_correct_dimen(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.no_wraparound_box)
        box = boxes[0]        
        self.assertAlmostEqual(box.width, self.no_wraparound_box[RIGHT] - self.no_wraparound_box[LEFT])
        self.assertAlmostEqual(box.height, self.no_wraparound_box[TOP] - self.no_wraparound_box[BOTTOM])
        
    def test_wraparound_right_box_returns_two_boxes(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_right_box)
        self.assertEquals(len(boxes), 2)

    def test_wraparound_right_box_returns_box_extending_to_left_border(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_right_box)
        box = boxes[0]

        self.assertAlmostEquals(box.left, -180)
        self.assertAlmostEquals(box.right, self.wraparound_right_box[RIGHT] - 360)
        
    def test_wraparound_right_box_returns_box_extending_to_right_border(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_right_box)
        box = boxes[1]

        self.assertAlmostEquals(box.left, self.wraparound_right_box[LEFT])
        self.assertAlmostEquals(box.right, 180)
        
    def test_wraparound_right_box_preserves_top_and_bottom(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_right_box)
        box0 = boxes[0]
        box1 = boxes[1]

        self.assertAlmostEquals(box0.top, self.wraparound_right_box[TOP])
        self.assertAlmostEquals(box0.bottom, self.wraparound_right_box[BOTTOM])
        self.assertAlmostEquals(box1.top, self.wraparound_right_box[TOP])
        self.assertAlmostEquals(box1.bottom, self.wraparound_right_box[BOTTOM])

    def test_wraparound_left_box_returns_two_boxes(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_left_box)
        self.assertEquals(len(boxes), 2)

    def test_wraparound_left_box_returns_box_extending_to_left_border(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_left_box)
        box = boxes[0]

        self.assertAlmostEquals(box.left, -180)
        self.assertAlmostEquals(box.right, self.wraparound_left_box[RIGHT])

    def test_wraparound_left_box_returns_box_extending_to_right_border(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_left_box)
        box = boxes[1]

        self.assertAlmostEquals(box.left, self.wraparound_left_box[LEFT] + 360)
        self.assertAlmostEquals(box.right, 180)         
        
    def test_wraparound_left_box_preserves_top_and_bottom(self):
        boxes = bb.get_rectangles_from_lat_lon_area(*self.wraparound_left_box)
        box0 = boxes[0]
        box1 = boxes[1]

        self.assertAlmostEquals(box0.top, self.wraparound_left_box[TOP])
        self.assertAlmostEquals(box0.bottom, self.wraparound_left_box[BOTTOM])
        self.assertAlmostEquals(box1.top, self.wraparound_left_box[TOP])
        self.assertAlmostEquals(box1.bottom, self.wraparound_left_box[BOTTOM])

    def test_bottom_above_top_raises_value_error(self):
        with self.assertRaises(ValueError):
            bb.get_rectangles_from_lat_lon_area(*self.bottom_above_top_box)
            
    def test_bottom_below_valid_bounds_clamps_bottom(self):
        box = bb.get_rectangles_from_lat_lon_area(*self.bottom_below_valid_bounds_box)[0]

        self.assertAlmostEquals(box.bottom, -90)

    def test_bottom_above_valid_bounds_clamps_bottom(self):
        box = bb.get_rectangles_from_lat_lon_area(*self.bottom_above_valid_bounds_box)[0]

        self.assertAlmostEquals(box.bottom, 90)  

    def test_top_below_valid_bounds_clamps_top(self):
        box = bb.get_rectangles_from_lat_lon_area(*self.top_below_valid_bounds_box)[0]

        self.assertAlmostEquals(box.top, -90)  

    def test_top_above_valid_bounds_clamps_top(self):
        box = bb.get_rectangles_from_lat_lon_area(*self.top_above_valid_bounds_box)[0]

        self.assertAlmostEquals(box.top, 90) 

    def test_right_lon_below_valid_bounds_raises_value_error(self):
        with self.assertRaises(ValueError):
            bb.get_rectangles_from_lat_lon_area(*self.right_below_valid_bounds_box)  

    def test_right_lon_above_valid_bounds_raises_value_error(self):
        with self.assertRaises(ValueError):
            bb.get_rectangles_from_lat_lon_area(*self.right_above_valid_bounds_box)  

    def test_left_lon_above_valid_bounds_raises_value_error(self):
        with self.assertRaises(ValueError):
            bb.get_rectangles_from_lat_lon_area(*self.left_above_valid_bounds_box)   

    def test_left_lon_below_valid_bounds_raises_value_error(self):
        with self.assertRaises(ValueError):
            bb.get_rectangles_from_lat_lon_area(*self.left_below_valid_bounds_box)      

    def test_too_large_box_raises_value_error(self):
        with self.assertRaises(ValueError):
            bb.get_rectangles_from_lat_lon_area(*self.too_large_box) 

class TestBoundingBoxFromConfig(unittest.TestCase):
    def setUp(self):
        
        self.above_delta = timedelta(seconds=3600)
        self.below_delta = timedelta(seconds=3600*24)
        self.base_time = datetime(year=2015, month=10, day=1, hour=5, minute=33, second=1)
    
        self.base_config = ConfigParser.SafeConfigParser()
        self.base_config.add_section(cfg.QUERY_SEC)
        self.base_config.set(cfg.QUERY_SEC, "time", self.base_time.strftime(bb.DEFAULT_TIME_FORMAT))
        self.base_config.set(cfg.QUERY_SEC, "seconds_after", "%d" % (self.above_delta.days*3600*24 + self.above_delta.seconds))
        self.base_config.set(cfg.QUERY_SEC, "seconds_before", "%d" % (self.below_delta.days*3600*24 + self.below_delta.seconds))
        self.base_config.set(cfg.QUERY_SEC, "center_lat", "0")
        self.base_config.set(cfg.QUERY_SEC, "center_lon", "0")
        self.base_config.set(cfg.QUERY_SEC, "height_lat", "10")
        self.base_config.set(cfg.QUERY_SEC, "width_lon", "20")
        
        
        config_with_time_format_base_time = datetime(year=2015, month=10, day=1) 
        self.config_with_time_format = cfg.copy_config(self.base_config)
        self.config_with_time_format.set(cfg.QUERY_SEC, "time", config_with_time_format_base_time.strftime("%Y_%m_%d"))
        self.config_with_time_format.set(cfg.QUERY_SEC, "time_format", "%Y_%m_%d")
        
        self.config_with_time_format_start_time = config_with_time_format_base_time - self.below_delta
        self.config_with_time_format_end_time = config_with_time_format_base_time + self.above_delta
        
    def test_box_bounds_are_correct(self):
        boxes, start_time, end_time = bb.get_bounding_boxes_from_config(self.base_config)
        box = boxes[0]
        
        self.assertAlmostEquals(box.left, -10)
        self.assertAlmostEquals(box.right, 10)
        self.assertAlmostEquals(box.top, 5)
        self.assertAlmostEquals(box.bottom, -5)
        
    def test_default_time_specification_produces_correct_time(self):
        boxes, start_time, end_time = bb.get_bounding_boxes_from_config(self.base_config)
        
        self.assertEquals(start_time, tu.get_seconds_since_epoch(self.base_time - self.below_delta))
        self.assertEquals(end_time, tu.get_seconds_since_epoch(self.base_time + self.above_delta))       
        
    def test_time_format_specification_produces_correct_time(self):
        boxes, start_time, end_time = bb.get_bounding_boxes_from_config(self.config_with_time_format)
        
        config_start_time = tu.get_seconds_since_epoch(self.config_with_time_format_start_time)
        config_end_time = tu.get_seconds_since_epoch(self.config_with_time_format_end_time)
        
        self.assertEquals(start_time, config_start_time)
        self.assertEquals(end_time, config_end_time)
        