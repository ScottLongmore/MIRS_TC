import logging
import json
import os
from datetime import datetime
from datetime import timedelta
import shutil

from pool import database_plugin as dp
from pool import config_sections as cfg
from pool import time_util as tu
from pool import plugin_loader

DATABASE_VERSION = 1
DATABASE_NAME = "processed_file_database.json"
DEFAULT_COMMIT_FREQUENCY = 1000
DEFAULT_BACKUP_COUNT = 5
DEFAULT_TIME_FMT = "%Y-%m-%dT%H:%M:%S"

LOGGER = logging.getLogger(__name__)

class IdentityMFE(object):
    def extract(self, filename):
        return filename

class JsonProcessedFileDatabase(object):
    def __init__(self, config):
        LOGGER.info("Initializing JsonProcessedFileDatabase v%d.", DATABASE_VERSION)
        self.database_dir = config.get(cfg.DATABASE_SEC, "location").strip()

        self._add_count = 0
        self._commit_frequency = DEFAULT_COMMIT_FREQUENCY
        if config.has_option("json_pfd", "commit_frequency"):
            self._commit_frequency = config.getint("json_pfd", "commit_frequency")

        self.mfe = IdentityMFE()
        mfe_plugin = plugin_loader.load(config, "meaningful_filename_extractor", optional=True)
        if mfe_plugin is not None:
            self.mfe = mfe_plugin.get_meaningful_filename_extractor(config)

        if not os.path.isdir(self.database_dir):
            raise dp.DatabaseIOError("Database location: %s does not exist. Please create first." % (self.database_dir))

        self._time_fmt = DEFAULT_TIME_FMT
        if config.has_option("json_pfd", "time_format"):
            self._time_fmt = config.get("json_pfd", "time_format")

        pfd_path = os.path.join(self.database_dir, DATABASE_NAME)
        self._pfd_path = pfd_path
        self._pfd_dict = self._load_pfd_dict(pfd_path)

        self._backup_database(config, pfd_path)

        LOGGER.info("Finished initializing JsonProcessedFileDatabase v%d.", DATABASE_VERSION)

    def _load_pfd_dict(self, pfd_path):
        LOGGER.info("Starting load of pfd file: %s", pfd_path)

        dict = {}
        if not os.path.isfile(pfd_path):
            LOGGER.info("Pfd file: %s does not exist. Initializing with empty pfd.", pfd_path)
            return dict

        LOGGER.info("Found pre-existing pfd file.  Loading json dictionary.")
        with open(pfd_path, "r") as in_file:
            dict = json.load(in_file)

        LOGGER.info("Finished loading pfd file: %s", pfd_path)
        return dict

    def _backup_database(self, config, pfd_path):
        LOGGER.info("Backing up the pfd file.")
        backup_count = DEFAULT_BACKUP_COUNT
        if config.has_option("json_pfd", "backup_count"):
            backup_count = config.getint("json_pfd", "backup_count")

        if not os.path.isfile(pfd_path):
            LOGGER.info("No pfd file to backup. No backup performed.")
            return

        selected_backup_pfd_path = pfd_path+"_1"
        selected_backup_mtime = None
        for i in xrange(1, backup_count+1):
            current_backup_path = "%s_%d" % (pfd_path, i)
            if not os.path.isfile(current_backup_path):
                selected_backup_pfd_path = current_backup_path
                break

            current_m_time = tu.get_mtime_from_path(current_backup_path)

            if selected_backup_mtime is None or current_m_time < selected_backup_mtime:
                selected_backup_pfd_path = current_backup_path
                selected_backup_mtime = current_m_time

        LOGGER.info("Copying pfd file: to %s.", selected_backup_pfd_path)
        shutil.copy(pfd_path, selected_backup_pfd_path)
        LOGGER.info("Finished backing up the pfd file.")

    def __contains__(self, filename):
        extracted_filename = self.mfe.extract(filename)
        return extracted_filename in self._pfd_dict

    def _dump_dict(self):
        LOGGER.info("Writing pfd dictionary to json file: %s", self._pfd_path)
        with open(self._pfd_path, "w") as out_file:
            json.dump(self._pfd_dict, out_file)
        LOGGER.info("Finished writing pfd dictionary to json file: %s", self._pfd_path)

    def clean(self, config):
        LOGGER.info("Starting clean.")
        number_of_days_to_keep = config.getint("json_pfd", "number_of_days_to_keep")
        if number_of_days_to_keep < 0:
            LOGGER.info("Clean not performed due to number_of_days_to_keep < 0: %d.", number_of_days_to_keep)
            return

        td = timedelta(days=number_of_days_to_keep)

        now = datetime.now()
        entries_to_delete = []
        for entry, date_string in self._pfd_dict.iteritems():
            entry_date = datetime.strptime(date_string, self._time_fmt)
            if now - entry_date > td:
                entries_to_delete.append(entry)

        for entry in entries_to_delete:
            del self._pfd_dict[entry]

        LOGGER.info("Finished clean.")

    def close(self, config):
        LOGGER.info("Closing JsonProcessedFileDatabase.")
        self._dump_dict()
        LOGGER.info("Finished closing JsonProcessedFileDatabase.")

    def add(self, config, filename):
        extracted_filename = self.mfe.extract(filename)

        LOGGER.info("Attempting to add %s to processed file database.", filename)
        if extracted_filename in self._pfd_dict:
            LOGGER.info("File: %s already in processed file database, skipping file.", filename)
            return

        self._pfd_dict[extracted_filename] = datetime.now().strftime(self._time_fmt)

        self._add_count += 1
        if self._add_count % self._commit_frequency == 0:
            LOGGER.info("Added %d files to processed file database. Commiting changes to database every %d files.", self._add_count, self._commit_frequency)
            self._dump_dict()
            LOGGER.info("Commited changes to processed file database.")
        LOGGER.info("Finished adding %s to processed file database.", filename)

def open_database(config):
    LOGGER.info("Opening JsonProcessedFileDatabase.")
    db = JsonProcessedFileDatabase(config)
    LOGGER.info("Finished opening JsonProcessedFileDatabase.")
    return db
