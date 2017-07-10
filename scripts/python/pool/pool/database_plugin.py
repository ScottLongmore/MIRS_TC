class DatabaseIOError(Exception):
    pass
    
class Database(object):
    def add_files(self, config, filenames, reader_plugin):
        raise NotImplementedError()
        
    def query(self, config):
        raise NotImplementedError()
        
    def close(self, config):
        raise NotImplementedError()