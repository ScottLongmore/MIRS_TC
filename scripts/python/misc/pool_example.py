import pool.pool as pool_api

#read the config from a file (it can be a template)
config = pool_api.get_config_cascade(["your_filename.ini"])

#alter the options you care about
config.set("file_gather", "gather_dir", "some_dir/")

#create the database if it doesn't exist
db_dir = config.get("database", "location")
if not os.path.isdir(db_dir):
    pool_api.create(config)

#populate the database with meta-data about files in the "gather_dir"
pool_api.update(config)

#The query consists of two steps:
#step 1) Ask the database for a list of filenames that (probably) intersect your
#        query bounding box.  This list will be written to a text file.
#step 2) Open each of these files and collect all the samples that intersect 
#        your bounding box and place them in a hdf5 or tar file.

#To perform both step 1) and step 2) use the following command:
#This command will return a list of filenames the query found as well as a 
#QueryResults instance.  A QueryResults instance works like a dictionary and 
#contains all the samples the query discovered indexed by the variable name.
filenames, query_results = pool_api.query(config)

#If you have a database but you don't want to perform step 2) at all or if you'd
#like to filter the filenames prior to giving them to step 2), then you can 
#perform step 1) separately using the following command:
filenames = query_gather_filenames(config)

#If you decided not to create a database and you just have a list of filenames,
#you can just perform step 2) using the following command:
query_results = query_gather_samples(config, ["foo.nc", "bar.nc"])



