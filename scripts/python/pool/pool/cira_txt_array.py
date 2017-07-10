import numpy as np
import sys
import gzip
import pdb

RANK_HEADER_ID = "rank:"
DIMENSIONS_HEADER_ID = "dimensions:"
TYPE_ID_HEADER_ID = "type:"
COLUMN_SEPERATOR = ","

TYPE_ID_FLOAT = "float"
TYPE_ID_INT   = "int"

RANK_OUTPUT_FORMAT = "%s%03d\n"
INDEX_OUTPUT_FORMAT = "%09d"
FLOAT_OUTPUT_FORMAT = "%015.5f"
FLOAT_NAN_OUTPUT_FORMAT = "%15.5f"
INT_OUTPUT_FORMAT   = "%09d"

def get_io_file(file, mode="w", zip=False):
    """Return a file to read/write to as well as boolean indicating whether the calling routine should be responsible for closing the file. 
    Determines if the passed file argument is a string or file-like object. If the file argument is a string, then the file is opened."""
    io_file = None
    should_close = True
    if isinstance(file, basestring):
        if zip:
            io_file = gzip.open(file, mode)
        else:
            io_file = open(file, mode)
    else:
        io_file = file
        should_close= False
        
    return io_file, should_close

def read_array_from_file(file, zip=False):
    """Return a numpy array from a file assumed to contain data in a simple text format used by companion function write_array_to_file. The file argument may be a string or a file like object."""
    in_file, should_close = get_io_file(file, mode="r", zip=zip)
    
    rank_line = in_file.readline()
    rank_val = parse_rank_header_line(rank_line)
    
    #TODO: check to make sure the rank is equal to the number of dimensions 
    dimensions_line = in_file.readline()
    dimensions = parse_dimensions_header_line(dimensions_line)
    
    #TODO: check to make sure the type matches a known type
    type_id_line = in_file.readline()
    type_id = parse_type_id_header_line(type_id_line)
    
    array_type = np.float64
    if type_id == TYPE_ID_INT:
        array_type = np.int32
    
    data_array = np.zeros(dimensions, dtype=array_type)
    
    parse_array(data_array,in_file)
    
    if should_close:
        in_file.close()
    
    return data_array
    
def parse_array(data_array, in_file):
    """Read each element of the passed array from the passed file."""
    rank = len(data_array.shape)

    index_list = []
    for i in range(0, rank):
        index_list.append(0)
    
    for line in in_file:
        line = line.strip()
        split_line = line.split(COLUMN_SEPERATOR)
        
        index = 0
        for elem in split_line:
            elem = elem.strip()
            if index < rank:
                index_list[index] = int(elem)
            elif data_array.dtype == np.int32:           
                data_array[tuple(index_list)] = int(elem)
            else:
                data_array[tuple(index_list)] = float(elem)
        
            index += 1
    
def parse_rank_header_line(rank_line):
    """Return the rank of the array from the rank header line."""
    rank_line = rank_line.strip()
    rank_val = int(rank_line[len(RANK_HEADER_ID):])
    
    return rank_val
    
def parse_dimensions_header_line(dimensions_line):
    """Return the dimensions of the array from the dimension header line."""
    dimensions_line = dimensions_line.strip()
    dimensions_list_string = dimensions_line[len(DIMENSIONS_HEADER_ID):]
    split_line = dimensions_list_string.split(COLUMN_SEPERATOR)
    
    dimensions = []
    for elem in split_line:
        elem_val = int(elem)
        dimensions.append(elem_val)
        
    return dimensions
    
def parse_type_id_header_line(type_id_line):
    """Return the type ID of the array from the type ID header line."""
    type_id_line = type_id_line.strip()
    type_id = type_id_line[len(TYPE_ID_HEADER_ID):]
    
    return type_id
    
def write_array_to_file(file, array, zip=False):
    """Write the passed array to a file for later use using a simple text format. The file argument may be a string or file-like object."""
    out_file, should_close = get_io_file(file, mode="w", zip=zip)
    
    rank = array.ndim
    rank_out_line = RANK_OUTPUT_FORMAT % (RANK_HEADER_ID,rank)
    out_file.write(rank_out_line)
    
    out_file.write(DIMENSIONS_HEADER_ID)
    shape = array.shape
    index = 0
    for elem in shape:
        out_file.write(INDEX_OUTPUT_FORMAT % (elem))
        
        if index < len(shape) -1:
            out_file.write(",")
        
        index += 1
    out_file.write("\n")
        
    type = TYPE_ID_FLOAT
    if np.issubdtype(array.dtype, np.int):
        type = TYPE_ID_INT
    
    out_file.write(TYPE_ID_HEADER_ID)
    out_file.write(type)
    out_file.write("\n")
    
    b = np.ndenumerate(array)
    for position,value in b:
        for pos in position:
            out_file.write(INDEX_OUTPUT_FORMAT % (pos))
            out_file.write(",") 
        
        if np.issubdtype(array.dtype, np.int):
            out_file.write(INT_OUTPUT_FORMAT % value)
        elif array.dtype.type is np.string_:
            out_file.write(value)
        else:
            if np.isnan(value) or np.isinf(value):
                out_file.write(FLOAT_NAN_OUTPUT_FORMAT % value)
            else:
                out_file.write(FLOAT_OUTPUT_FORMAT % value)
        
        out_file.write("\n")
    
    if should_close:
        out_file.close()
    
