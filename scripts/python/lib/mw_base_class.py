#!/usr/bin/python

import numpy as np

class MWBase(object):
    """
    base class for use with MW data classes (AMSU, ATMS)
    """

    def __init__(self):
        pass

    def p_level_idx(self, p_level_str):

        """
        returns index for pressure level in mb

        input:
            required: 
                p_levels: list of floats
                p_level_str: string
            example: idx = p_level([ 100.0, 125.0, 150.0, 175.0, 200.0 ], '200')
        output: index
        """
        
        # --- check if requested pressure level is available
        p_level_str_0 = str(round(float(p_level_str),1))
        if p_level_str_0 in self.p_levels_dict.keys():
            index = int(self.p_levels_dict[p_level_str_0])
        else:
            error_msg_levels = ""
            error_msg_line1 = "Requested pressure level %s is not available.\n" % p_level_str
            error_msg_line2 = "Available pressure levels are (mb):\n"
            integer_p_levels = map(float,self.p_levels_dict.keys())
            for level in np.sort(integer_p_levels):
                error_msg_levels = "%s%s\n" % (error_msg_levels,level)
                error_msg = ''.join([error_msg_line1,error_msg_line2,error_msg_levels])
            raise ValueError(error_msg)

        return index


    def make_p_levels_dict(self, p_levels_str):
              
        """
        returns dictionary with 
            keys:pressure levels in mb (strings)
            values: corresponding pressure level index
        input:
            required: 
                p_levels_str: list of pressure levels as strings
        output: dictionary
        """
        ln = len(p_levels_str)
        values = np.linspace(0,ln, ln,endpoint=False)
        p_levels_dict = dict(zip(p_levels_str,values))
        
        return p_levels_dict

