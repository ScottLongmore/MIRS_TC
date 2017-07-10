def convert_filenames(filenames, config):
    old_string = config.get("path_replacer", "old_string")
    new_string = config.get("path_replacer", "new_string")

    for i in xrange(len(filenames)):
        filenames[i] = filenames[i].replace(old_string, new_string)
        
    return filenames