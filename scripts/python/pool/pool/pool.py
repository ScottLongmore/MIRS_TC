import logging
import ConfigParser
import plugin_loader
import config_sections as cfg
import bounding_box as bb
import sys
import math
import numpy as np
import pdb

LOGGER = logging.getLogger(__name__)
DEFAULT_WARN_ON_MISSING_FRACTION_THRESHOLD = 0.7

class QueryResults(object):
    def __init__(self):
        self._filenames = None
        self._var_data = {}
        self._meta_data = {}
        self._global_meta_data = {}
        self._global_meta_data_src = ""

    def __contains__(self, k):
        return k in self._var_data

    def __getitem__(self, k):
        return self._var_data[k]

    def iter(self):
        return self._var_data.iterkeys()

    def items(self):
        return self._var_data.items()

    def iteritems(self):
        return self._var_data.iteritems()

    def iterkeys(self):
        return self._var_data.iterkeys()

    def itervalues(self):
        return self._var_data.itervalues()

    def keys(self):
        return self._var_data.keys()

    def values(self):
        return self._var_data.values()

    @property
    def filenames(self):
        return self._filenames

    @property
    def var_data(self):
        return self._var_data

    @property
    def meta_data(self):
        return self._meta_data

    @property
    def global_meta_data(self):
        return self._global_meta_data

    @property
    def global_meta_data_src(self):
        return self._global_meta_data_src

class QueryResultsFromSelectedData(QueryResults):
    def __init__(self, selected_data):
        super(QueryResultsFromSelectedData, self).__init__()

        self._filenames = tuple(selected_data.filenames)

        for k, v in selected_data.var_data.iteritems():
            if selected_data.is_var_name_missing(k):
                continue

            self._var_data[k] = np.array(v)
            if k in selected_data.var_meta_data:
                self._meta_data[k] = selected_data.var_meta_data[k]

        for k, v in selected_data.shared_var_data.iteritems():
            if selected_data.is_var_name_missing(k):
                continue

            self._var_data[k] = np.array(v)
            if k in selected_data.shared_var_meta_data:
                self._meta_data[k] = selected_data.shared_var_meta_data[k]

        if selected_data.global_meta_data is not None:
            for k, v in selected_data.global_meta_data.iteritems():
                self._global_meta_data[k] = v

        if selected_data.global_meta_data_src is not None:
            self._global_meta_data_src = selected_data.global_meta_data_src


class SelectedData(object):
    def __init__(self, filenames, desired_var_names, desired_shared_var_names, missing_var_response_type):
        self.filenames = filenames
        self.var_names = desired_var_names
        self.shared_var_names = desired_shared_var_names

        self.is_shared_var_data_initialized = False

        self.global_meta_data = None
        self.global_meta_data_src = None

        self.var_data = {}
        self.var_meta_data = {}
        self.var_meta_data_src = None
        for name in desired_var_names:
            self.var_data[name] = []

        self.shared_var_data = {}
        self.shared_var_meta_data = {}

        #need to keep track of whether a variable was ever missing or present
        #to ensure that the data inside a query is consistent
        #if a variable appears in both dictionaries, then the query will be
        #inconsistent and the program will throw an error
        self._missing_var_dict = {}
        self._present_var_dict = {}

        missing_var_response_type = missing_var_response_type.strip().lower()
        missing_var_responses = {"ignore":None, "warn":self._warn_missing_var, "error":self._error_missing_var}
        self.missing_var_response = missing_var_responses[missing_var_response_type]

    def is_var_name_missing(self, var_name):
        return var_name in self._missing_var_dict

    def get_missing_var_names(self):
        return self._missing_var_dict.keys()

    def has_global_meta_data(self):
        return not self.global_meta_data is None

    def _set_global_meta_data(self, global_meta_data, filename):
        self.global_meta_data = global_meta_data
        self.global_meta_data_src = filename

    def has_var_meta_data(self, var_name):
        return var_name in self.var_meta_data

    def _set_var_meta_data(self, var_name, meta_data, filename):
        self.var_meta_data[var_name] = meta_data
        self.var_meta_data_src = filename

    def _add_var_data(self, name, data, filename):
        self.var_data[name].append(data)
        self._add_var_to_present_dictionary(name, filename)

    def has_shared_var_data(self, name):
        return name in self.shared_var_data

    def _add_shared_var_data(self, config, data, filename):
        for shared_var_name in self.shared_var_names:
            if self.has_shared_var_data(shared_var_name):
                continue

            try:
                s_data = data[shared_var_name]
                s_meta_data = data.get_var_meta_data(config, shared_var_name)
            except KeyError:
                self._handle_missing_var(shared_var_name, filename)

            self.shared_var_data[shared_var_name] = s_data
            self.shared_var_meta_data[shared_var_name] = s_meta_data

    def add_file_data(self, config, data, filename, boxes, start_timestamp, end_timestamp):
        if not self.has_global_meta_data():
            global_meta_data = data.get_global_meta_data(config)
            self._set_global_meta_data(global_meta_data, filename)

        self._add_shared_var_data(config, data, filename)

        navigation_iterator = data.get_navigation_iterator(config)
        self._add_footprint_data(config, data, filename, navigation_iterator, boxes, start_timestamp, end_timestamp)

    def _add_var_to_present_dictionary(self, var_name, filename):
        self._present_var_dict[var_name] = True

        if var_name in self._present_var_dict and var_name in self._missing_var_dict:
            raise ValueError("The variable %s is missing in some of the files used for this query, but present in %s. Check the files used in this query for consistency." % (var_name, filename))

    def _handle_missing_var(self, var_name, filename):
        if self.missing_var_response is not None:
            self.missing_var_response(var_name, filename)
        self._missing_var_dict[var_name] = True

    def _add_footprint_data(self, config, data, filename, navigation_iterator, boxes, start_timestamp, end_timestamp):
        LOGGER.info("Started adding footprint data to query results")
        for lat, lon, time, indices in navigation_iterator:
            if _is_footprint_missing(lat, lon, time):
                continue

            if time < start_timestamp or time > end_timestamp:
                continue

            if not bb.is_point_in_boxes(boxes, lon, lat):
                continue

            for name in self.var_names:
                try:
                    var_data = data.get_var_at_footprint_index(config, name, indices)
                    self._add_var_data(name, var_data, filename)
                except KeyError:
                    self._handle_missing_var(name, filename)
                    continue

        for name in self.var_names:
            if not self.has_var_meta_data(name) and name in self._present_var_dict:
                var_meta_data = data.get_var_meta_data(config, name)
                self._set_var_meta_data(name, var_meta_data, filename)

        LOGGER.info("Finished adding footprint data to query results")

    def _warn_missing_var(self, var_name, filename):
        LOGGER.warning("Could not find variable: %s in file: %s" % (var_name, filename))

    def _error_missing_var(self, var_name, filename):
        raise ValueError("Could not find variable: %s in file: %s" % (var_name, filename))

class MissingDataCounter(object):
    def __init__(self, config):
        self.missing_footprint_count = 0
        self.total_footprint_count = 0
        self._init_warn_on_missing_fraction_threshold(config)

    def _init_warn_on_missing_fraction_threshold(self, config):
        self.warn_on_missing_fraction_threshold = DEFAULT_WARN_ON_MISSING_FRACTION_THRESHOLD
        if(config.has_option(cfg.LOGGING_SEC, "warn_on_missing_fraction_threshold")):
            self.warn_on_missing_fraction_threshold = config.getfloat(cfg.LOGGING_SEC, "warn_on_missing_fraction_threshold")

    def add_to_missing(self):
        self.missing_footprint_count += 1

    def add_to_total(self):
        self.total_footprint_count += 1

    def log_missing_fraction(self, filename):
        fraction_missing = self.missing_footprint_count/float(self.total_footprint_count)
        LOGGER.debug("File: %s had %6.2f%s missing data." % (filename, fraction_missing*100, '%'))
        if fraction_missing >= self.warn_on_missing_fraction_threshold:
            LOGGER.warning("%6.2f percent of file: %s was missing data." % (fraction_missing*100, filename))

    def reset_counter(self):
        self.missing_footprint_count = 0
        self.total_footprint_count = 0


def get_config_cascade(filenames):
    if len(filenames) <= 0:
        print "Need at least one config file to generate config cascade."
        help()
        sys.exit(1)

    config = ConfigParser.RawConfigParser()

    for filename in filenames:
        with open(filename, "r") as in_file:
            config.readfp(in_file)

    return config

def create(config):
    LOGGER.info("Starting create command.")

    database_plugin = plugin_loader.load(config, "database_plugin")
    database_plugin.create_database(config)

    LOGGER.info("Finishing create command.")

def _is_footprint_missing(lat, lon, time):
    if math.isnan(lat) or lat < -90.1 or lat > 90.1:
        return True

    if math.isnan(lon) or lon < -180.1 or lon > 180.1:
        return True

    if math.isnan(time) or time < 0:
        return True

    return False

def get_var_creation_plugins(config):
    plugins = []
    if config.has_option(cfg.PLUGINS_SEC, "var_generation_plugins"):
        for plugin_path in config.get(cfg.PLUGINS_SEC, "var_generation_plugins").split(","):
            plugin = plugin_loader.load_from_path(config, "var_generation_plugin", plugin_path)
            plugins.append(plugin)

    return plugins

def _init_update_plugins(config):
    file_gather_plugin = plugin_loader.load(config, "file_gather_plugin")
    file_reader_plugin = plugin_loader.load(config, "file_reader_plugin")
    database_plugin = plugin_loader.load(config, "database_plugin")
    if config.has_option(cfg.PLUGINS_SEC, "update_consistency_checker"):
        consistency_checker = plugin_loader.load(config, "update_consistency_checker")
        consistency_checker.check(config)
    processed_file_database_plugin = None
    if config.has_option(cfg.PLUGINS_SEC, "processed_file_database"):
        processed_file_database_plugin = plugin_loader.load(config, "processed_file_database")
    var_creation_plugins = get_var_creation_plugins(config)

    return file_gather_plugin, file_reader_plugin, database_plugin, processed_file_database_plugin, var_creation_plugins

def update(config):
    LOGGER.info("Starting update command.")

    file_gather_plugin, file_reader_plugin, database_plugin, processed_file_database_plugin, var_creation_plugins = _init_update_plugins(config)

    gathered_files = file_gather_plugin.gather_files(config)
    database = database_plugin.open_database(config)

    missing_data_counter = MissingDataCounter(config)

    if processed_file_database_plugin is not None:
        processed_file_database = processed_file_database_plugin.open_database(config)

    should_print_progress = True
    if config.has_option(cfg.LOGGING_SEC, "should_print_update_progress"):
        should_print_progress = config.getboolean(cfg.LOGGING_SEC, "should_print_update_progress")      
        
    progress = 0
    N_files = len(gathered_files)
    for filename in gathered_files:
        if processed_file_database_plugin is not None and filename in processed_file_database:
            LOGGER.debug("File: %s already in processed file database, skipping file.", filename)
            continue

        if should_print_progress:
            print "Adding file: ", filename," to database."
            print "Progress: ", progress, "/", N_files, " ", float(progress)/float(N_files)*100,'%'
            
        LOGGER.debug("Adding file: %s to database.", filename)
        progress += 1
        try:
            file_data = file_reader_plugin.get_data(config, filename, var_creation_plugins)
        except:
            LOGGER.exception("An error occurred while retrieving data object for file: %s. Skipping this file." % (filename))
            continue

        navigation_iterator = file_data.get_navigation_iterator(config)

        encountered_at_least_one_footprint = False
        try:
            for lat, lon, time, indices in navigation_iterator:
                missing_data_counter.add_to_total()
                if _is_footprint_missing(lat, lon, time):
                    missing_data_counter.add_to_missing()
                    continue

                encountered_at_least_one_footprint = True
                database.add_file(config, filename, lat, lon, time)
        except:
            LOGGER.exception("An error occurred while iterating over lats/lons/times in file: %s. Skipping this file." % (filename))
            file_data.close()
            database.flush(config)
            missing_data_counter.reset_counter()
            continue

        if processed_file_database_plugin is not None:
            if encountered_at_least_one_footprint:
                processed_file_database.add(config, filename)
            else:
                LOGGER.warning("Encountered file with no footprints: %s.  File was not added to processed file database." % (filename))

        file_data.close()
        database.flush(config)
        missing_data_counter.log_missing_fraction(filename)
        missing_data_counter.reset_counter()

    if processed_file_database_plugin is not None:
        processed_file_database.close(config)
    database.close(config)

    LOGGER.info("Finishing update command.")

def backup(config):
    LOGGER.info("Starting backup command.")
    file_gather_plugin, file_reader_plugin, database_plugin, processed_file_database_plugin, var_creation_plugins = _init_update_plugins(config)

    database = database_plugin.open_database(config)
    database.backup(config)
    database.close(config)

    LOGGER.info("Finished backup command.")

def clean(config):
    LOGGER.info("Starting clean command.")
    file_gather_plugin, file_reader_plugin, database_plugin, processed_file_database_plugin, var_creation_plugins = _init_update_plugins(config)

    database = database_plugin.open_database(config)
    database.clean(config)
    database.close(config)

    if processed_file_database_plugin is not None:
        processed_file_database = processed_file_database_plugin.open_database(config)
        processed_file_database.clean(config)
        processed_file_database.close(config)

    LOGGER.info("Finished clean command.")

def get_var_name_list_from_config(config, section, option):
    raw_var_list = config.get(section, option).split(',')
    var_list = []
    for var in raw_var_list:
        var_list.append(var.strip())

    return var_list

def _get_filenames_matching_query(config, boxes, start_timestamp, end_timestamp):
    LOGGER.info("Retrieving list of filenames matching query.")
    database_plugin = plugin_loader.load(config, "database_plugin")

    query_filename_converter = None
    if config.has_option(cfg.PLUGINS_SEC, "query_filename_converter"):
        query_filename_converter = plugin_loader.load(config, "query_filename_converter")

    database = database_plugin.open_database(config)
    filenames = database.query(config, boxes, start_timestamp, end_timestamp)
    database.close(config)

    if query_filename_converter is not None:
        filenames = query_filename_converter.convert_filenames(filenames, config)

    LOGGER.debug("Query found files: "+", ".join(filenames))

    _write_txt_filenames(config, filenames)

    LOGGER.info("Finished retrieving list of filenames matching query.")
    return filenames

def _gather_samples_matching_query(config, filenames, boxes, start_timestamp, end_timestamp):
    LOGGER.info("Retrieving samples matching query in list of filenames.")

    selected_data = _gather_selected_data_from_files(config, filenames, boxes, start_timestamp, end_timestamp)
    query_results = QueryResultsFromSelectedData(selected_data)

    if config.has_option(cfg.PLUGINS_SEC, "query_writer_plugin"):
        query_writer = plugin_loader.load(config, "query_writer_plugin")
        query_writer.write(config, selected_data)
    else:
        LOGGER.warning("No query_writer_plugin option specified.  Query variables will not be written.")
        print "No query_writer_plugin option specified.  Query variables will not be written."

    LOGGER.info("Finished retrieving samples matching query in list of filenames.")
    return query_results

def query_gather_filenames(config):
    LOGGER.info("Starting query_gather_filenames command.")
    boxes, start_timestamp, end_timestamp = bb.get_bounding_boxes_from_config(config)
    filenames = _get_filenames_matching_query(config, boxes, start_timestamp, end_timestamp)
    LOGGER.info("Finished query_gather_filenames command.")
    return filenames

def query_gather_samples(config, filenames):
    LOGGER.info("Starting query_gather_samples command.")
    boxes, start_timestamp, end_timestamp = bb.get_bounding_boxes_from_config(config)
    query_results = _gather_samples_matching_query(config, filenames, boxes, start_timestamp, end_timestamp)
    LOGGER.info("Finished query_gather_samples command.")
    return query_results

def _read_txt_filenames(query_input_list_filename):
    filenames = []
    with open(query_input_list_filename, "r") as in_file:
        for line in in_file:
            stripped_line = line.strip()
            if stripped_line.startswith("n_files"):
                continue

            if len(stripped_line) <= 0:
                continue

            filenames.append(stripped_line)

    return filenames

def query_gather_samples_from_list_file(config):
    LOGGER.info("Starting query_gather_samples_from_list_file command.")
    query_input_list_filename = config.get(cfg.QUERY_SEC, "query_input_list_file")
    filenames = _read_txt_filenames(query_input_list_filename)

    boxes, start_timestamp, end_timestamp = bb.get_bounding_boxes_from_config(config)
    query_results = _gather_samples_matching_query(config, filenames, boxes, start_timestamp, end_timestamp)
    LOGGER.info("Finished query_gather_samples_from_list_file command.")
    return filenames, query_results

def query(config):
    LOGGER.info("Starting query command.")

    boxes, start_timestamp, end_timestamp = bb.get_bounding_boxes_from_config(config)

    filenames = _get_filenames_matching_query(config, boxes, start_timestamp, end_timestamp)

    query_results = _gather_samples_matching_query(config, filenames, boxes, start_timestamp, end_timestamp)

    LOGGER.info("Finishing query command.")
    return filenames, query_results

def _write_txt_filenames(config, filenames):
    if config.has_option(cfg.QUERY_SEC, "query_txt_output_file"):
        output_filename = config.get(cfg.QUERY_SEC, "query_txt_output_file")
        LOGGER.info("Writing query filenames to: %s", output_filename)
        output_string = "\n".join(filenames)
        with open(output_filename, "w") as out_file:
            out_file.write("n_files:%06d\n" % (len(filenames)))
            out_file.write(output_string)
    else:
        LOGGER.warning("No query_txt_output_file option specified.  Query filenames will not be written to a file.")

def _gather_selected_data_from_files(config, filenames, boxes, start_timestamp, end_timestamp):
    LOGGER.info("Started gathering query results from files.")
    file_reader_plugin = plugin_loader.load(config, "file_reader_plugin")
    var_creation_plugins = get_var_creation_plugins(config)

    var_list = get_var_name_list_from_config(config, cfg.QUERY_SEC, "var_list")

    if config.has_option(cfg.QUERY_SEC, "shared_var_list"):
        shared_var_list = get_var_name_list_from_config(config, cfg.QUERY_SEC, "shared_var_list")
    else:
        LOGGER.warning("The shared_var_list option in %s was not specified.  No shared variables will be gathered in the query." % (cfg.QUERY_SEC))
        shared_var_list = []

    missing_var_response_type = "error"
    if config.has_option(cfg.QUERY_SEC, "missing_var_response_type"):
        missing_var_response_type = config.get(cfg.QUERY_SEC, "missing_var_response_type")

    selected_data = SelectedData(filenames, var_list, shared_var_list, missing_var_response_type)

    for filename in filenames:
        LOGGER.info("Gathering data points from file: %s", filename)
        data = file_reader_plugin.get_data(config, filename, var_creation_plugins)

        selected_data.add_file_data(config, data, filename, boxes, start_timestamp, end_timestamp)

        data.close()

    LOGGER.info("Finished gathering query results from files.")
    return selected_data

def secret(config):
    LOGGER.info("Starting secret.")
    print "IT'S A SECRET TO EVERYBODY."
    LOGGER.info("Finishing secret.")

def setup_logging(config):
    if config.has_option(cfg.PLUGINS_SEC, "logging_plugin"):
        logging_plugin = plugin_loader.load(config, "logging_plugin", False)
        logging_plugin.setup_logging(config)
        return
    elif config.has_option(cfg.LOGGING_SEC, "config_file"):
        logging_config_filename = config.get(cfg.LOGGING_SEC, "config_file")
        logging.fileConfig(logging_config_filename)
        LOGGER.info("Logger setup using config file: %s.", logging_config_filename)
        return

    level = None
    level_dict = {"debug":logging.DEBUG, "info":logging.INFO, "warning":logging.WARNING, "error":logging.ERROR, "critical":logging.CRITICAL}
    if config.has_option(cfg.LOGGING_SEC, "level"):
        level_str = config.get(cfg.LOGGING_SEC, "level").strip().lower()
        level = level_dict[level_str]
        LOGGER.info("Level: %s selected for default logging configuration.", level_str)

    logging.basicConfig(level=level)
    LOGGER.info("Set up default logging using basicConfig.")

def help_cmd(config=None):
    print "Help not yet implemented."

def main(args):
    commands = {"create":create, "update":update, "backup":backup, "query":query,
        "clean":clean, "help":help_cmd, "secret":secret,
        "query_gather_filenames":query_gather_filenames,
        "query_gather_samples":query_gather_samples_from_list_file}

    try:
        arg = args[1].strip().lower()
    except IndexError:
        print "Didn't receive any command line arguments."
        help_cmd()
        sys.exit(1)

    if arg == "help":
        help_cmd()
        sys.exit()

    config = get_config_cascade(args[2:])
    setup_logging(config)

    LOGGER.info("Launching pool command: %s", arg)

    if arg not in commands:
        print "Could not find command: ", arg
        help_cmd()
        sys.exit(1)

    return commands[arg](config)
