import unittest
import time_subs

class TimeSubsTests(unittest.TestCase):
    
    def test_jdaytodate_negativeyear_throwsexception(self):
        self.assertRaises(ValueError, time_subs.julian_to_date, -1, 200)
        
    def test_jdaytodate_jdaylessthanone_throwsexception(self):
        self.assertRaises(ValueError, time_subs.julian_to_date, 2013, 0)
        
    def test_jdaytodate_leapyearjdaygreaterthan366_throwsexception(self):
        self.assertRaises(ValueError, time_subs.julian_to_date, 2012, 367)
        
    def test_jdaytodate_leapyearjday365_yieldsdec30(self):
        month, day = time_subs.julian_to_date(2016, 365)
        self.assertTrue(month == 12)
        self.assertTrue(day == 30)
        
    def test_jdaytodate_commonyearjdaygreaterthan365_throwsexception(self):
        self.assertRaises(ValueError, time_subs.julian_to_date, 2013, 366)
        
    def test_jdaytodate_leapyearleapday_yieldsfeb29(self):
        month, day = time_subs.julian_to_date(2016, 60)
        self.assertTrue(month == 2)
        self.assertTrue(day == 29)
        
    def test_jdaytodate_commonyearjday60_yieldsmarch1(self):
        month, day = time_subs.julian_to_date(2015, 60)
        self.assertTrue(month == 3)
        self.assertTrue(day == 1)
        
    def test_jdaytodate_leapyearandcommonyearinjan_yieldssamedate(self):
        month_leap, day_leap = time_subs.julian_to_date(2016, 20)
        month, day = time_subs.julian_to_date(2015, 20)
        
        self.assertTrue(month == month_leap)
        self.assertTrue(day == day_leap)
        
    def test_jdaytodate_leapyearlastday_yieldsdec31(self):
        month, day = time_subs.julian_to_date(2016, 366)
        self.assertTrue(month == 12)
        self.assertTrue(day == 31)
        
    def test_jdaytodate_commonyearlastday_yieldsdec31(self):
        month, day = time_subs.julian_to_date(2011, 365)
        self.assertTrue(month == 12)
        self.assertTrue(day == 31)

    def test_jdaytodate_commonyearfirstday_yieldsjan1(self):
        month, day = time_subs.julian_to_date(2017, 1)
        self.assertTrue(month == 1)
        self.assertTrue(day == 1)
    
    def test_jdaytodate_leapyearfirstday_yieldsjan1(self):
        month, day = time_subs.julian_to_date(2020, 1)
        self.assertTrue(month == 1)
        self.assertTrue(day == 1)
        
if __name__ == '__main__':
    unittest.main()