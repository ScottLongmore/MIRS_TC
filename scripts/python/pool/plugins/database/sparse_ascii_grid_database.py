import os
import sys
import logging
from pool import config_sections as cfg
from pool import database_plugin as dp
import json
from itertools import izip
from pool import time_util as tu
from datetime import datetime
from datetime import timedelta
import re
import shutil
import pdb

LOGGER = logging.getLogger(__name__)

DATABASE_VERSION = 2
DEFAULT_LON_SPACING_DEGREES = 1
DEFAULT_LAT_SPACING_DEGREES = 1
DEFAULT_TIME_SPACING_SECONDS = 60*60

DEFAULT_TIME_INDEX_SCALE_HIERARCHY = (10000, 1000, 1)

META_INFO_FILENAME = "meta_info.json"

DEFAULT_BACKUP_DAYS_TO_KEEP = 7
DEFAULT_BACKUP_FREQ_SECONDS = 3600*24
BACKUP_DATE_FORMAT = "%Y-%m-%d-%H-%M-%S"
BACKUP_FILENAME_RE_STRING = r"@DIR_NAME_(?P<date>\d{4}-\d{2}-\d{2}-\d{2}-\d{2}-\d{2}).tar.gz"

_cached_path_existence_lookups = {}

def _path_exists(path):
    if path in _cached_path_existence_lookups:
        LOGGER.debug("Found cached path existence lookup.")
        return True

    if os.path.isdir(path):
        _cached_path_existence_lookups[path] = 1
        return True

    return False

def _ensure_dir_exists(path):
    LOGGER.debug("Ensuring path: %s exists.", path)
    if _path_exists(path):
        LOGGER.debug("The path: %s already exists, returning.", path)
        return

    LOGGER.debug("The path: %s does not exist.  Making new dir.", path)
    os.makedirs(path)
    _cached_path_existence_lookups[path] = 1 #only care about the keys, use 1 to save space
    LOGGER.debug("Finished ensuring path exists.")


class GridCell(object):
    def __init__(self, database_dir, x_index, y_index, t_index, time_index_scale_hierarchy):
        LOGGER.debug("Initializing grid cell with indices x: %d y %d t: %d", x_index, y_index, t_index)
        self.is_closed = False
        self.modified = False

        self.x_index = x_index
        self.y_index = y_index
        self.t_index = t_index

        self.filenames = {}

        t_index_dirs = []
        for scale in time_index_scale_hierarchy:
            t_index_str = "%d" % (t_index//scale)
            t_index_dirs.append(t_index_str)

        space_index_str = "%d_%d.json" % (x_index, y_index)
        self.time_path = os.path.join(database_dir, *t_index_dirs)
        self.file_path = os.path.join(self.time_path, space_index_str)

        self._read_grid_cell()

        LOGGER.debug("Finished initializing grid cell.")

    def _read_grid_cell(self):
        LOGGER.debug("Attempting to read grid cell with indices x: %d y %d t: %d from disc.", self.x_index, self.y_index, self.t_index)
        try:
                filename_list = _get_filenames_from_cell_file(self.file_path)

                self.filenames = dict(izip(filename_list, filename_list))
        except IOError:
            LOGGER.debug("Could not find grid cell file at path: %s with index x: %d y: %d t: %d. Assuming grid cell is new.", self.file_path, self.x_index, self.y_index, self.t_index)
        except ValueError:
            LOGGER.exception("Corrupt data found in grid cell file: %s", self.file_path)
            sys.exit(1)

        LOGGER.debug("Finished reading grid cell from disc.")

    def _write_grid_cell(self):
        LOGGER.debug("Writing grid cell x: %d y: %d t:%d to disc.", self.x_index, self.y_index, self.t_index)
        _ensure_dir_exists(self.time_path)

        with open(self.file_path, "w") as out_file:
            json.dump(self.filenames.keys(), out_file)
        LOGGER.debug("Finished writing grid cell to disc.")

    def get_filenames(self):
        LOGGER.debug("Retrieving filenames from grid cell x: %d y: %d t:%d.", self.x_index, self.y_index, self.t_index)
        if self.is_closed:
            raise ValueError("Attempted to retrieve filenames from closed grid cell." % (filename))

        LOGGER.debug("Finished retrieving filenames from grid cell.")
        return self.filenames.keys()


    def add_filename(self, filename):
        LOGGER.debug("Adding filename: %s to grid cell x: %d y: %d t:%d.", filename, self.x_index, self.y_index, self.t_index)
        if self.is_closed:
            raise ValueError("Attempted to add file: %s to closed grid cell." % (filename))

        if filename in self.filenames:
            return

        self.modified = True
        self.filenames[filename] = filename
        LOGGER.debug("Finished adding filename to grid cell.")

    def close(self):
        LOGGER.debug("Closing grid cell x: %d y: %d t:%d", self.x_index, self.y_index, self.t_index)
        if self.is_closed:
            raise ValueError("Attempted to close previously closed grid cell.")

        self.is_closed = True
        if not self.modified:
            return

        self._write_grid_cell()

        LOGGER.debug("Finished closing grid cell")

def _get_filenames_from_cell_file(grid_filename):
    with open(grid_filename, "r") as in_file:
        return json.load(in_file)


def _get_t_index_from_dir_name(dir_name):
    return int(dir_name)

def _get_space_indices_from_cell_filename(filename):
    root = os.path.splitext(filename)[0]
    split_string = root.split("_")

    x_index = int(split_string[0])
    y_index = int(split_string[1])

    return x_index, y_index


class QueryIndexExtents(object):
    def __init__(self, database, box, start_timestamp, end_timestamp):
        LOGGER.info("Initializing QueryIndexExtents.")
        self.min_x = database._get_x_index(box.left)
        self.max_x = database._get_x_index(box.right)
        self.min_y = database._get_y_index(box.bottom)
        self.max_y = database._get_y_index(box.top)

        self.min_t = database._get_t_index(start_timestamp)
        self.max_t = database._get_t_index(end_timestamp)
        LOGGER.info("Extents: min_x: %d max_x: %d min_y: %d max_y: %d min_t: %d max_t: %d", self.min_x, self.max_x, self.min_y, self.max_y, self.min_t, self.max_t)
        LOGGER.info("Finished initializing QueryIndexExtents.")

    def t_is_inside(self, t_index, t_index_factor):
        if t_index <= self.max_t//t_index_factor and t_index >= self.min_t//t_index_factor:
            return True

        return False

    def space_indices_are_inside(self, x_index, y_index):
        if x_index <= self.max_x and x_index >= self.min_x and y_index <= self.max_y and y_index >= self.min_y:
            return True

        return False

def _t_index_in_extents_group(extents, t_index, t_index_factor):
    for extent in extents:
        if extent.t_is_inside(t_index, t_index_factor):
            return True

    return False

def _space_indices_in_extents_group(extents, x_index, y_index):
    for extent in extents:
        if extent.space_indices_are_inside(x_index, y_index):
            return True

    return False

class SparseAsciiGridDatabase(dp.Database):
    def __init__(self, config):
        LOGGER.info("Initializing sparse ascii grid database v%d.", DATABASE_VERSION)
        self.database_dir = config.get(cfg.DATABASE_SEC, "location").strip()

        if not os.path.isdir(self.database_dir):
            raise dp.DatabaseIOError("Database location: %s does not exist. Please create first." % (self.database_dir))

        self._read_meta_info()

        self.grid_cells = {}

        LOGGER.info("Finished initializing sparse ascii grid database v%d.", DATABASE_VERSION)

    def _read_meta_info(self):
        LOGGER.info("Reading meta data from database.")
        meta_info_filename = os.path.join(self.database_dir, META_INFO_FILENAME)
        try:
            with open(meta_info_filename, "r") as in_file:
                self.meta_info = json.load(in_file)
        except ValueError:
            LOGGER.exception("Corrupt data found in meta data file: %s", meta_info_filename)
            sys.exit(1)

        if self.meta_info["version"] != DATABASE_VERSION:
            raise ValueError("Database version: %d on disc does not match software version: %d" % (self.meta_info["version"], DATABASE_VERSION))

        self.x_spacing = self.meta_info["lon_spacing_degrees"]
        self.y_spacing = self.meta_info["lat_spacing_degrees"]
        self.t_spacing = self.meta_info["time_spacing_seconds"]
        self.time_index_scale_hierarchy = self.meta_info["time_index_scale_hierarchy"]

        LOGGER.info("Finished reading meta data from database.")

    def _gather_filenames_for_query(self, index_extents):
        LOGGER.info("Gathering filenames from database dir: %s", self.database_dir)
        filenames = {}

        self._search_directory_layer_for_filenames(index_extents, filenames, self.database_dir, 0)

        LOGGER.info("Finished gathering filenames from database dir: %s", self.database_dir)
        return filenames.keys()

    def _search_directory_layer_for_filenames(self, index_extents, filenames, search_dir, search_layer):
        #search directories
        for root, dirs, files in os.walk(search_dir):
            for dir_name in dirs:
                try:
                    t_index = _get_t_index_from_dir_name(dir_name)
                except ValueError:
                    #invalid directory name, so just skip it
                    continue

                if not _t_index_in_extents_group(index_extents, t_index, self.time_index_scale_hierarchy[search_layer]):
                    continue

                new_search_dir = os.path.join(search_dir, dir_name)
                if search_layer < len(self.time_index_scale_hierarchy) -1:
                    self._search_directory_layer_for_filenames(index_extents, filenames, new_search_dir, search_layer+1)
                else:
                    self._search_time_dir_for_grid_cells_within_bounds(index_extents, filenames, new_search_dir)

            break

    def _search_time_dir_for_grid_cells_within_bounds(self, index_extents, filenames, search_dir):
        LOGGER.info("Searching dir: %s for cell files within bounds.", search_dir)
        file_count = 0
        for sub_root, sub_dirs, sub_files in os.walk(search_dir):
            for cell_filename in sub_files:
                x_index, y_index = _get_space_indices_from_cell_filename(cell_filename)
                if not _space_indices_in_extents_group(index_extents, x_index, y_index):
                    continue

                LOGGER.debug("Found in bounds cell file x: %d y: %d filename: %s", x_index, y_index, cell_filename)

                cell_path = os.path.join(sub_root, cell_filename)
                filenames_from_cell = _get_filenames_from_cell_file(cell_path)

                for filename in filenames_from_cell:
                    file_count += 1
                    filenames[filename] = 0 #Don't really care about the values, just the keys, so use 0 to save space

            break
        LOGGER.info("Finished searching dir: %s and found: %d cell files within space bounds.", search_dir, file_count)

    def backup(self, config):
        LOGGER.info("Performing backup.")
        backup_freq_seconds = DEFAULT_BACKUP_FREQ_SECONDS
        if config.has_option(cfg.DATABASE_SEC, "backup_freq_seconds"):
            backup_freq_seconds = config.getint(cfg.DATABASE_SEC, "backup_freq_seconds")

        most_recent_dt = self._get_most_recent_backup_date()
        now = datetime.now()
        td = timedelta(backup_freq_seconds)

        if most_recent_dt is None or now - most_recent_dt > td:
            date_suffix = datetime.now().strftime(BACKUP_DATE_FORMAT)
            database_dir, database_root = self._get_database_dir_and_root()
            backup_filename = os.path.join(database_root, database_dir+"_"+date_suffix)
            LOGGER.info("Making backup of %s, in tar.gz file: %s", self.database_dir, backup_filename)
            shutil.make_archive(backup_filename, "gztar", self.database_dir)

        LOGGER.info("Finished backup.")

    def _get_most_recent_backup_date(self):
        backup_files = self._find_backup_files()
        most_recent_dt = None
        for backup_filename, backup_date in backup_files:
            if most_recent_dt is None or backup_date > most_recent_dt:
                most_recent_dt = backup_date

        return most_recent_dt

    def _get_database_dir_and_root(self):
        head, tail = os.path.split(self.database_dir.rstrip("\\/"+os.path.sep))
        tail = tail.rstrip("\\/"+os.path.sep)

        return tail, head

    def _find_backup_files(self):
        database_dir, database_root = self._get_database_dir_and_root()

        backup_filename_re_string_with_dir_name = BACKUP_FILENAME_RE_STRING.replace("@DIR_NAME", database_dir)
        backup_filename_re = re.compile(backup_filename_re_string_with_dir_name)

        backups = []

        for root, dirs, files in os.walk(database_root):
            for filename in files:
                match = backup_filename_re.match(filename)
                if match is None:
                    LOGGER.debug("File: %s doesn't match backup format: %s, skipping file.", filename, backup_filename_re_string_with_dir_name)
                    continue

                LOGGER.debug("File: %s matches backup format: %s, checking date.", backup_filename_re_string_with_dir_name)
                backup_date_string = match.groupdict()["date"]
                backup_date = datetime.strptime(backup_date_string, BACKUP_DATE_FORMAT)

                backup_info = (os.path.join(root, filename), backup_date)
                backups.append(backup_info)

            break

        return backups

    def _clean_backups(self, config):
        number_of_backup_days_to_keep = config.getint(cfg.DATABASE_SEC, "number_of_backup_days_to_keep")
        if number_of_backup_days_to_keep < 0:
            LOGGER.info("Clean not performed due to number_of_backup_days_to_keep < 0: %d.", number_of_backup_days_to_keep)
            return
        td = timedelta(days=number_of_backup_days_to_keep)

        backup_files = self._find_backup_files()
        now = datetime.now()
        for backup_filename, backup_date in backup_files:
            if now - backup_date > td:
                LOGGER.info("Deleting backup file: %s older than %d days.", backup_filename, number_of_backup_days_to_keep)
                os.remove(backup_filename)
                LOGGER.info("Finished deleting backup file: %s.", backup_filename)

    def _clean_database(self, config):
        number_of_days_to_keep = config.getint(cfg.DATABASE_SEC, "number_of_days_to_keep")
        if number_of_days_to_keep < 0:
            LOGGER.info("Clean not performed due to number_of_backup_days_to_keep < 0: %d.", number_of_backup_days_to_keep)
            return

        td = timedelta(days=number_of_days_to_keep)
        oldest_date_to_keep = datetime.now() - td
        LOGGER.info("Searching for directories older than %s to delete.", oldest_date_to_keep.isoformat())
        self._recursively_delete_old_db_dirs(self.database_dir, oldest_date_to_keep)

    def _recursively_delete_old_db_dirs(self, search_dir, oldest_date_to_keep):
        LOGGER.info("Searching %s for old directories to delete.", search_dir)
        for root, dirs, files in os.walk(search_dir):
            for dirname in dirs:
                full_path = os.path.join(root, dirname)
                self._recursively_delete_old_db_dirs(full_path, oldest_date_to_keep)

                mod_time = datetime.fromtimestamp(os.path.getmtime(full_path))
                LOGGER.debug("Found dir: %s with mod time: %s.", full_path, mod_time.isoformat())
                if mod_time < oldest_date_to_keep:
                    LOGGER.info("Removing old dir: %s with mod time: %s", full_path, mod_time.isoformat())
                    shutil.rmtree(full_path)
            break
        LOGGER.info("Done searching %s for old directories to delete.", search_dir)

    def clean(self, config):
        LOGGER.info("Performing clean.")
        self._clean_backups(config)
        self._clean_database(config)
        LOGGER.info("Finished clean.")


    def query(self, config, boxes, start_timestamp, end_timestamp):
        LOGGER.info("Performing query.")

        index_extents = []

        for box in boxes:
            extents = QueryIndexExtents(self, box, start_timestamp, end_timestamp)
            index_extents.append(extents)

        filenames = self._gather_filenames_for_query(index_extents)

        LOGGER.info("Found %d filenames in database.", len(filenames))

        LOGGER.info("Finished query.")
        return filenames

    def add_file(self, config, filename, lat, lon, time):
        LOGGER.debug("Adding %s to database.", filename)

        x_index, y_index, t_index = self._get_indices(lon, lat, time)

        grid_cell = self._get_grid_cell(x_index, y_index, t_index)
        grid_cell.add_filename(filename)

        LOGGER.debug("Finished adding %s to database.", filename)

    def _get_indices(self, x, y, t):
        x_index = self._get_x_index(x)
        y_index = self._get_y_index(y)
        t_index = self._get_t_index(t)

        return x_index, y_index, t_index

    def _get_x_index(self, x):
        return int((x+180)/self.x_spacing)

    def _get_y_index(self, y):
        return int((y+90)/self.y_spacing)

    def _get_t_index(self, t):
        return int(t/self.t_spacing)

    def _has_grid_cell(self, x_index, y_index, t_index):
        indices = (x_index, y_index, t_index)
        if indices in self.grid_cells:
            LOGGER.debug("Found pre-existing grid cell.")
            return self.grid_cells[indices]

        return False

    def _get_grid_cell(self, x_index, y_index, t_index):
        LOGGER.debug("Retrieving grid cell x: %d y: %d t: %d", x_index, y_index, t_index)
        indices = (x_index, y_index, t_index)
        if indices in self.grid_cells:
            LOGGER.debug("Found pre-existing grid cell.")
            return self.grid_cells[indices]

        LOGGER.debug("Creating new grid cell.")
        grid_cell = GridCell(self.database_dir, x_index, y_index, t_index, self.time_index_scale_hierarchy)
        self.grid_cells[indices] = grid_cell

        LOGGER.debug("Finished retrieving grid cell x: %d y: %d t: %d", x_index, y_index, t_index)
        return grid_cell

    def flush(self, config):
        self.close(config)

    def close(self, config):
        LOGGER.debug("Closing all open grid cells")
        for cell in self.grid_cells.values():
            cell.close()
        self.grid_cells = {}
        LOGGER.debug("Finished closing all open grid cells")

def _get_time_index_scale_hierarchy_from_config(config):
    if not config.has_option(cfg.DATABASE_SEC, "time_index_scale_hierarchy"):
        return DEFAULT_TIME_INDEX_SCALE_HIERARCHY

    scales_list = []
    scale_strings = config.get(cfg.DATABASE_SEC, "time_index_scale_hierarchy").split(",")
    for scale_string in scale_strings:
        scale = int(scale_string.strip())
        scales_list.append(scale)

    return tuple(scales_list)

#Required by the plugin
def create_database(config):
    LOGGER.info("Starting create_database.")
    database_dir = config.get(cfg.DATABASE_SEC, "location").strip()
    LOGGER.info("Creating database at location: {}".format(database_dir))

    LOGGER.debug("Checking to see if database already exists.")
    if os.path.isdir(database_dir):
        raise dp.DatabaseIOError("Database location: %s already exists. Please change location or delete existing database first." % (database_dir))

    LOGGER.info("Making database directory.")
    os.mkdir(database_dir)

    _write_meta_info(config, database_dir)

    LOGGER.info("Finished create_database.")

def _write_meta_info(config, database_dir):
    LOGGER.info("Writing meta data info to dir: %s", database_dir)
    meta_info_path = os.path.join(database_dir, META_INFO_FILENAME)

    lon_spacing_degrees = DEFAULT_LON_SPACING_DEGREES
    if config.has_option(cfg.DATABASE_SEC, "grid_spacing_lon"):
        lon_spacing_degrees = config.getfloat(cfg.DATABASE_SEC, "grid_spacing_lon")

    lat_spacing_degrees = DEFAULT_LAT_SPACING_DEGREES
    if config.has_option(cfg.DATABASE_SEC, "grid_spacing_lat"):
        lat_spacing_degrees = config.getfloat(cfg.DATABASE_SEC, "grid_spacing_lat")

    time_spacing_seconds = DEFAULT_TIME_SPACING_SECONDS
    if config.has_option(cfg.DATABASE_SEC, "grid_spacing_seconds"):
        time_spacing_seconds = config.getint(cfg.DATABASE_SEC, "grid_spacing_seconds")

    time_index_scale_hierarchy = _get_time_index_scale_hierarchy_from_config(config)

    #TODO: Control grid spacing with config options
    meta_info = {"version": DATABASE_VERSION,
        "lon_spacing_degrees": lon_spacing_degrees,
        "lat_spacing_degrees": lat_spacing_degrees,
        "time_spacing_seconds": time_spacing_seconds,
        "time_index_scale_hierarchy": time_index_scale_hierarchy}

    with open(meta_info_path, "w") as out_file:
        json.dump(meta_info, out_file)

    LOGGER.info("Finished writing meta info")

#Required by the plugin
def open_database(config):
    return SparseAsciiGridDatabase(config)
