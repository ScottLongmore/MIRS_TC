import logging
import sqlite3
import os
import shutil

from pool import database_plugin as dp
from pool import config_sections as cfg 
from pool import time_util as tu
from pool import plugin_loader

DATABASE_VERSION = 1
DATABASE_NAME = "processed_file_database.db"
DEFAULT_COMMIT_FREQUENCY = 1000
DATABASE_TABLE_NAME = "processed_files"
DEFAULT_BACKUP_COUNT = 5

LOGGER = logging.getLogger(__name__)

class IdentityMFE(object):
    def extract(self, filename):
        return filename

class SQLite3ProcessedFileDatabase(object):
    def __init__(self, config):
        LOGGER.info("Initializing SQLite3ProcessedFileDatabase v%d.", DATABASE_VERSION)
        self.database_dir = config.get(cfg.DATABASE_SEC, "location").strip()
        
        self._add_count = 0
        self._commit_frequency = DEFAULT_COMMIT_FREQUENCY
        if config.has_option("sqlite3_pfd", "commit_frequency"):
            self._commit_frequency = config.getint("sqlite3_pfd", "commit_frequency")
        
        self.mfe = IdentityMFE()
        mfe_plugin = plugin_loader.load(config, "meaningful_filename_extractor", optional=True)
        if mfe_plugin is not None:
            self.mfe = mfe_plugin.get_meaningful_filename_extractor(config)
        
        if not os.path.isdir(self.database_dir):
            raise dp.DatabaseIOError("Database location: %s does not exist. Please create first." % (self.database_dir))

        pfd_path = os.path.join(self.database_dir, DATABASE_NAME)
        self._backup_database(config, pfd_path)
            
        self.db_connection = sqlite3.connect(pfd_path)
        self.cursor = self.db_connection.cursor()
        self._ensure_table_exists()

        LOGGER.info("Finished initializing SQLite3ProcessedFileDatabase v%d.", DATABASE_VERSION)
    
    def _ensure_table_exists(self):
        tablename = DATABASE_TABLE_NAME
        LOGGER.info("Ensuring that the processed file database table: %s exists.", tablename)
        found_names = self.cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='processed_files'")
        for name in found_names:
            LOGGER.info("Table already exists.")
            return
        LOGGER.info("Table does not exist.")    
        self._create_table()
    
    def _create_table(self):
        LOGGER.info("Creating the processed file database table.")
        self.cursor.execute("CREATE TABLE processed_files (filename text)")
        LOGGER.info("Finished creating the processed file database table.")
        
    def _backup_database(self, config, pfd_path):
        LOGGER.info("Backing up the pfd file.")
        backup_count = DEFAULT_BACKUP_COUNT
        if config.has_option("sqlite3_pfd", "backup_count"):
            backup_count = config.getint("sqlite3_pfd", "backup_count")
            
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
        return self._contains_extraced_filename(extracted_filename)
        
    def _contains_extraced_filename(self, extracted_filename):
        LOGGER.info("Checking if %s is in processed file database.", extracted_filename)
        val = (extracted_filename,)
        contains_filename = self.cursor.execute("SELECT filename FROM processed_files WHERE filename=(?)", val)
        
        for i in contains_filename:
            LOGGER.info("%s is in processed file database.", extracted_filename)
            return True
            
        LOGGER.info("%s is not in processed file database.", extracted_filename)    
        return False
        
    def close(self, config):
        LOGGER.info("Closing SQLite3ProcessedFileDatabase.")
        self.db_connection.commit()
        self.db_connection.close()
        LOGGER.info("Finished closing SQLite3ProcessedFileDatabase.")
        
    def add(self, config, filename):
        extracted_filename = self.mfe.extract(filename)
    
        LOGGER.info("Attempting to add %s to processed file database.", filename)
        if self._contains_extraced_filename(extracted_filename):
            LOGGER.info("File: %s already in processed file database, skipping file.", filename)
            return
            
        val = (extracted_filename,)
        self.cursor.execute("INSERT INTO processed_files VALUES (?)", val)
        self._add_count += 1
        if self._add_count % self._commit_frequency == 0:
            LOGGER.info("Added %d files to processed file database. Commiting changes to database every %d files.", self._add_count, self._commit_frequency)
            self.db_connection.commit()
            LOGGER.info("Commited changes to processed file database.")
        LOGGER.info("Finished adding %s to processed file database.", filename)
        
def open_database(config):
    LOGGER.info("Opening SQLite3ProcessedFileDatabase.")
    db = SQLite3ProcessedFileDatabase(config)
    LOGGER.info("Finished opening SQLite3ProcessedFileDatabase.")
    return db
    