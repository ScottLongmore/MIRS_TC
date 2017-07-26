#!/usr/bin/python
"""
mirs_tc.py - Master control driver for MIRS TC algorithms 

example:
mirs_tc.py -c <config.json> -l <input list file> -t <time:YYYYMMDDHHMMSS>
"""
    
# System modules
import sys
import os
import re
import datetime
import argparse 
import operator
import collections
import json
import importlib
import logging
import traceback
import ConfigParser
import pprint

pp=pprint.PrettyPrinter(indent=4)

# Set local module paths
try:
    exePath=os.path.dirname(os.path.abspath(__file__))
    parentPath,childDir=os.path.split(exePath)
    sys.path.insert(1,os.path.join(parentPath,"lib"))
    sys.path.insert(2,os.path.join(parentPath,"pool"))
    sys.path.insert(3,os.path.join(parentPath,"pool","plugins"))
except:
   print "Unable to load local library paths"
   sys.exit(1)

# Local modules
import setup_logging
import error_codes
import utils
import MIRS_TC 
import pool.pool as pool_api

# Setup Logging using logging templates
LOG = logging.getLogger("MIRS_TC") #create the logger for this file
setup_logging.setup_logging("MIRS_TC","MIRS_TC.LOG")
LOG.info("Starting: {}".format(__file__)) 


# Read command line arguments 
options = argparse.ArgumentParser()
options.add_argument("-c", "--config", dest="config", help="MIRS-TC Configuration File")
options.add_argument("-i", "--input", dest="input", help="Input Specification File")
try:
    args = options.parse_args()
except:
    msg='Syntax: python mirs_tc.py -c <config.json> -i <input spec file>'
    utils.error(LOG,msg,error_codes.EX_USAGE)

# Read master MIRS-TC JSON configuration file
try:
    LOG.info("Processing JSON config file: {}".format(args.config)) 
    config=json.load(open(args.config),object_pairs_hook=collections.OrderedDict)
    #
    # TODO: schema validation here
    #
except:
    msg="Error in JSON config file: {}".format(args.config)
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Read input specification file
try:
    LOG.info("Reading input specifation file: {} using module: {}".format(args.input,config['inputs']['module'])) 
    inputModule=importlib.import_module(config['inputs']['module'])
    args=[args.input]
    config['inputs'].update(getattr(inputModule,config['inputs']['method'])(*args))
except:
    msg="Problem retrieving input specifations using {}:{}".format(config['inputs']['module'],config['inputs']['method']) 
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Validate input parameters
LOG.info("Validating input specifation properties")
msg=utils.schemaValidate(config['inputs'],config['schemas']['inputs'])
if msg:
    utils.error(LOG,msg,error_codes.EX_IOERR)

# Determine date/times information 
LOG.info("Setting date/time variables")
config['datetimes']=MIRS_TC.datetimes(config['inputs']["job_coverage_start"],config['inputs']["job_coverage_end"])

# Create/update working sub-directories
workDir=config['inputs']['working_directory']
utils.replaceStructValues(config['dirs'],'^.+$',utils.prependAbsPath,[workDir])
for subDir in config['dirs']:
    utils.workDir(subDir)

# Directory aliases
dataDir=config['dirs']['data']
discardDir=config['dirs']['discard']
modelDir=config['dirs']['model']
poolDir=config['dirs']['pool']
logDir=config['dirs']['log']
outputDir=config['dirs']['output']

# Update/validate config parameters with inputs parameters
# Add system root path to sub paths
for filename in config['files']:
   utils.replaceStructValues(config['files'][filename],'^.+$',utils.prependAbsPath,[config['inputs']['SYS_DIR']])
for progname in config['programs']:
   utils.replaceStructValues(config['programs'][progname],'^(exe|configs)$',utils.prependAbsPath,[config['inputs']['SYS_DIR']])

# Validate, create, and initilize metadata datafile object arrays, move files to data directory 
metadata={} # dict of dataset object lists for (ATCF/ADECK, GFS, MIRS)
for dataname in config['datasets']: 

    dataset=config['datasets'][dataname]
    dataset['dataModule']=importlib.import_module(dataset['module'])
    metadata[dataname]=[]
    try:
        datafiles=config['inputs'][dataset['inputKey']]
        if not isinstance(datafiles,list):
            datafiles=[datafiles]
        for datafile in datafiles:
            datapath=utils.moveFile(workDir,datafile,dataDir) # Move data files to dataDir
            metadata[dataname].append(getattr(dataset['dataModule'],dataset['dataMethod'])(*[datapath]))
    except:
        msg="Error creating/validating dataset objects: {}".format(dataset)
        utils.error(LOG,msg,error_codes.EX_IOERR)

    LOG.info("Initalized [{}] meta-data objects for dataset {}".format(len(metadata[dataname]),dataname))

# Create metadata link map for configuration specified datasets (mirs_atms_img/snd) 
for dataname in config['datalinks']: 
    datasetKeys=config['datalinks'][dataname]['datasetKeys']
    config['datalinks'][dataname]['links']=MIRS_TC.datalinks(metadata,datasetKeys)
    LOG.info("Linked meta-data objects for {}".format(dataname))

# Run metadata object filter methods
for dataname in metadata: 
    dataset=config['datasets'][dataname]
    if "filterMethod" in dataset:
        metadata[dataname]=getattr(dataset['dataModule'],dataset['filterMethod'])(*[config,metadata,dataname]) # Return a dataset object or list of dataset objects
        LOG.info("Filtered meta-data objects for dataset {} is now [{}]".format(dataname,len(metadata[dataname])))

# metadata aliases
adecks=metadata['adeck']
gfs=metadata['gfs'][0] #latest GFS file
mirs_atms_snd=metadata['mirs_atms_snd']
mirs_atms_img=metadata['mirs_atms_img']

# Running wgrib2 and bin2pack - convert GFS grib file to pack files
#
#   Inputs: <grib file> 
#   Outputs: <pack file> 
#

gfsYear=gfs.get('runDTG').strftime("%y")
gfsMonth=gfs.get('runDTG').strftime("%m")
gfsDay=gfs.get('runDTG').strftime("%d")
gfsHour=gfs.get('runDTG').strftime("%H")
gfsDate=gfsYear+gfsMonth+gfsDay
gfsFhour="{0:03d}".format(gfs.get('fhour'))
gfsGribFile=os.path.join(dataDir,gfs.get('filename'))

pcode,pday=utils.ddhh2pack(int(gfsDay),int(gfsHour))
gfsPackFile="G{}{}_{}{}{:02d}_PACK.DAT".format(gfsFhour,gfsYear,pcode,gfsMonth,pday)

utils.cdDir(modelDir) # change to model directory

if os.path.isfile(gfsPackFile):
   LOG.info("GFS pack file: {}  already exists, continuing".format(gfsPackFile))
else:
   LOG.info("Creating gfs pack file: {}".format(gfsPackFile))
   if not MIRS_TC.grib2pack(gfsDate,gfsHour,gfsFhour,gfsGribFile,config['inputs']['WGRIB'],config['programs']['bin2pack']['exe'],logDir):
       msg="Problem converting to pack file from grib file: {}".format(gfsGribFile)
       utils.error(LOG,msg,error_codes.EX_IOERR)
   
   if os.stat(gfsPackFile).st_size == 0: # Verify GFS pack file
       msg="GFS pack file {} either doesn't exist or is zero size".format(gfsPackFile)
       utils.error(LOG,msg,error_codes.EX_IOERR)

   
utils.cdDir(workDir) # change to working directory

# Running pool_api.update - create MIRS satellite/instrument dataset pools (geo-cross-reference databases) 
#
#   Inputs:  etc/<dataset>.ini 
#   Outputs: <poolDir>/meta_info.json 
for satname in config['satellites']:

    sat=config['satellites'][satname] 
    pool=sat['pool']
    poolDataDir=os.path.join(poolDir,pool['dataset'])
    pool['config']=config['files']['pools'][pool['dataset']]

    pool['cfgParser']=pool_api.get_config_cascade([pool['config']])
    pool['cfgParser'].set("file_gather","gather_dir",dataDir) 
    pool['cfgParser'].set("database","location",poolDataDir)
    pool['cfgParser'].set("query","time",config['datetimes']['initDTG'].strftime("%Y-%m-%d-%H-%M-%S"))

    if os.path.isdir(poolDataDir):
        LOG.info("Satellite/instrument/dataset pool: {}/{}/{}, already exists, skipping".format(satname,sat['instrument'],pool['dataset']))
    else:
        LOG.info("Creating satellite/instrument/dataset pool: {}/{}/{}".format(satname,sat['instrument'],pool['dataset']))
        pool_api.create(pool['cfgParser'])
        LOG.info("Running pool_api.update")
        pool_api.update(pool['cfgParser'])

# Start main tropical cyclone (adeck) processing loop
#
LOG.info("Starting main tropical cyclone processing loop")
for adeck in adecks:

    stormId=adeck.get('stormId')
    LOG.info("Processing TC: {}".format(stormId))

    # Create and CD to storm directory
    stormDir=os.path.join(workDir,stormId)
    utils.workDir(stormDir) 
    utils.cdDir(stormDir)

    # Run ShortTermTrack -  outputs track and forecast track for TC by hour 
    #
    #   Inputs: adeck [a<stormId>.dat] file 
    #   Outputs: ShortTermTrack [<stormId].inp] file
    #
    adeckFile=adeck.get('filename')
    adeckSTTfile="a{}.dat".format(stormId)
    utils.linkFile(dataDir,adeckFile,stormDir,adeckSTTfile) # Link adeck file
    commandList=[config['programs']['shortTermTrack']['exe']]
    commandArgs=[]
    commandArgs.extend([adeckSTTfile,config['datetimes']['synpDTG'].strftime("%Y%m%d%H"),'CURR'])
    commandId="shortTermTrack"
    stdoutFile=os.path.join(logDir,"{}_{}.stdout".format(commandId,stormId))
    stderrFile=os.path.join(logDir,"{}_{}.stderr".format(commandId,stormId))
    LOG.info("Running shortTermTrack")
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)

    afdeckInputFile="{}.inp".format(stormId)
    if os.path.isfile(afdeckInputFile)==False and status==False:
        LOG.info("ShortTermTrack failed with current synoptic time, trying with 12 hours previous")
        commandArgs=[]
        commandArgs.extend([adeckSTTfile,config['datetimes']['synpDTG'].strftime("%Y%m%d%H"),'M12H'])
        status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)
        if os.path.isfile(afdeckInputFile)==False and status==False:
            LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
            LOG.warning("See sub-process log file: {}".format(stdoutFile))
            LOG.warning("See sub-process error file: {}".format(stderrFile))
            LOG.warning("ShortTermTrack failed with 12 hours previous, skipping to next storm")
            continue

    # Run afdeck - extracts TC coordinates  
    #
    #   Inputs: ShortTermTrack [<stormId>.inp] file 
    #   Outputs: COORDINATES file 
    #
    afdeck=config['programs']['afdeck']
    commandList=[afdeck['exe']]
    commandArgs=[]
    commandArgs.extend([afdeckInputFile])
    commandId="afdeck"
    stdoutFile=os.path.join(logDir,"{}_{}.stdout".format(commandId,stormId))
    stderrFile=os.path.join(logDir,"{}_{}.stderr".format(commandId,stormId))
    LOG.info("Running afdeck")
    LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
    status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)

    # Check return status
    if not status or not os.path.exists(afdeck['outputs']['coordFile']):
        LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
        LOG.warning("See sub-process log file: {}".format(stdoutFile))
        LOG.warning("See sub-process error file: {}".format(stderrFile))
        LOG.warning("afdeck failed, skipping to next storm")
        continue 

    # Retreive Short Term Track 00Z analsys values
    stormVars=MIRS_TC.getInputAnalysisVars(afdeckInputFile)
    #for stormname in stormVars:
    #    print("{} : {}".format(stormname,stormVars[stormname]))

    #
    # Start satelite/instrument processing loop
    #  
    LOG.info("Starting storm {} satellite/instrument processing loop".format(stormId))
    for satname in config['satellites']:

        sat=config['satellites'][satname] 
        satId=sat['ident']
        instr=sat['instrument']
        pool=sat['pool']
        LOG.info("Processing satellite/instrument: {}/{}".format(satname,instr))

        # Create and CD to satellite directory
        satDir=os.path.join(stormDir,satname)
        utils.workDir(satDir) 
        utils.cdDir(satDir)

        # Running pool query - query MIRS satellite/instrumment dataset pools to determine if data near TC lat/lon bounding box and time 
        #
        #   Inputs: <MIRS_TC_POOL.ini> 
        #   Outputs: <poolDir>/<sat>/results.tar
        #
	LOG.info("Querying satellite/instrument/dataset pool: {}/{}/{} ".format(satname,instr,pool['dataset']))
        skipFlag=False
        poolFiles={}
        query_results={}

        queryFile=os.path.join(satDir,"{}_query.txt".format(pool['dataset']))
        tarFile=os.path.join(satDir,"{}.tar".format(pool['dataset']))
        pool['cfgParser'].set("query","center_lat",stormVars['LATITUDE'])
        pool['cfgParser'].set("query","center_lon",stormVars['LONGITUDE'])
        pool['cfgParser'].set("query","time",stormVars['DTG'].strftime("%Y-%m-%d-%H-%M-%S"))
        pool['cfgParser'].set("query","seconds_after",config['datetimes']['mirsSecsAfter'])
        pool['cfgParser'].set("query","seconds_before",config['datetimes']['mirsSecsBefore'])
        pool['cfgParser'].set("query","var_list",",".join(config['datasets'][pool['dataset']]['variables'])) # First dataset
        pool['cfgParser'].set("query","query_txt_output_file",queryFile)
        pool['cfgParser'].set("query","tar_output_filename",tarFile)

        poolFiles[pool['dataset']], query_results = pool_api.query(pool['cfgParser'])

        # Extract files from pool tar file
        if not MIRS_TC.extractTarFiles(config['inputs']['TAR'],tarFile,"{}_{}_{}".format(stormId,satId,pool['dataset']),logDir):
            LOG.warning("Problem extracting files from tar file: {}".format(tarFile))
            next

        outprefix="{}_{}_{}".format(stormId,stormVars['DTG'].strftime("%Y%m%d%H"),satId)
        # Verify and rename output variable text files
        varFiles={}
        for varname in config['datasets'][pool['dataset']]['variables']:
            varFiles[varname]="{}.{}".format(varname,config['exts']['txt'])
            varFilename=os.path.join(satDir,varFiles[varname])
            if not os.path.isfile(varFilename):
                LOG.warning("Query failed for dataset: {}, variable file: {} does not exist".format(pool['dataset'],variableFile))
                skipFlag=True
                next 

        # Check for error in sub-loop
        if skipFlag:
	    LOG.warning("Problem querying pool: {}, skiping to next satellite".format(pool['dataset']))
            continue

        # Query Pool for MIRS linked datasets (e.g. mirs_atms_snd)
        for poolname in pool['datasets']:
	    LOG.info("Determining files from linked dataset: {}/{}/{} ".format(satname,instr,poolname))

            # Get corresponding linked data files
            dataLinkName=config['datasets'][pool['dataset']]['datalink']
            dataLink=config['datalinks'][dataLinkName]
            links=dataLink['links']
            inputDataset=metadata[pool['dataset']]
            inputDatasetKey=dataLink['datasetKeys'][pool['dataset']]
            inputFiles=poolFiles[pool['dataset']]
            poolFiles[poolname]=MIRS_TC.getLinkFilenames(links, inputDataset, inputDatasetKey, inputFiles, poolname)

            if len(poolFiles[poolname]) != len(poolFiles[pool['dataset']]):
	       LOG.warning("Number of files doesn't match between {} and {} queries".format(pool['dataset'],poolname))
               skipFlag=True
               next

            # Query linked data files
	    LOG.info("Querying satellite/instrument/dataset linked pool: {}/{}/{} ".format(satname,instr,poolname))
            queryFile=os.path.join(satDir,"{}_query.txt".format(poolname))
            tarFile=os.path.join(satDir,"{}.tar".format(poolname))
            pool['cfgParser'].set("query","var_list",",".join(config['datasets'][poolname]['variables'])) 
            pool['cfgParser'].set("query","shared_var_list",",".join(config['datasets'][poolname]['shared_variables'])) # First dataset
            pool['cfgParser'].set("query","query_txt_output_file",queryFile)
            pool['cfgParser'].set("query","tar_output_filename",tarFile)
            query_results = pool_api.query_gather_samples(pool['cfgParser'], poolFiles[poolname])

            # Extract files from pool tar file
            if not MIRS_TC.extractTarFiles(config['inputs']['TAR'],tarFile,"{}_{}_{}".format(stormId,satId,poolname),logDir):
                LOG.warning("Problem extracting files from tar file: {}, skipping to next satellite".format(tarFile))
                next
 
            # Verify output variable text files
            for varname in config['datasets'][poolname]['variables']+config['datasets'][poolname]['shared_variables']:
                varFiles[varname]="{}.{}".format(varname,config['exts']['txt'])
                varFilename=os.path.join(satDir,varFiles[varname])
                if not os.path.isfile(varFilename):
                    LOG.warning("Query failed for dataset: {}, variable file: {} does not exist".format(poolname,variableFile))
                    skipFlag=True
                    next 
        # Check for error in sub-loop 
        if skipFlag:
	    LOG.warning("Problem querying pool: {}, skipping to next satellite".format(poolname))
            continue

        # Running satcenter - determine if TC center is within range for satellite scanline center 
        #
        #   Inputs: COORDINATES (TC coordinates) 
        #           <years,months,days,hours,minutes,seconds>.txt 
        #           <Scanline_Center_<Lat,Lon>.txt
        #   Outputs: TIMES
        #
        satcenter=config['programs']['satcenter']

        # Create time fields files from pool epoch text file (e.g. ScanLine_Center_Times.txt) 
        MIRS_TC.createTimeFieldFiles(satcenter['timeFile'])

        # Link COORDINATES files to current dir
        utils.linkFile(stormDir,afdeck['outputs']['coordFile'],satDir,afdeck['outputs']['coordFile'])

        commandList=[satcenter['exe']]
        commandArgs=[]
        commandArgs.extend([stormId,config['datetimes']['synpDTG'].strftime("%Y%m%d%H"),satId])
        commandId="satcenter"
        stdoutFile=os.path.join(logDir,"{}_{}_{}.stdout".format(commandId,stormId,satId))
        stderrFile=os.path.join(logDir,"{}_{}_{}.stderr".format(commandId,stormId,satId))
        LOG.info("Running satcenter")
        LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
        status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)
        
        # Check return status
	if not status:
            LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
            LOG.warning("See sub-process log file: {}".format(stdoutFile))
            LOG.warning("See sub-process error file: {}".format(stderrFile))
            LOG.warning("satcenter exited with status: {}, skipping to next satellite".format(status))
            continue 

        # Verify TIMES file 
        if not os.path.isfile(satcenter['outputs']['timesFile']):
            LOG.warning("TIMES file does not exist, skipping to next satellite")
            continue 

        # Running oparet - run oparet model 
        #
        #   Inputs: COORTIMES
        #           oparet.cfg - input variable thresholds
        #           oparet_params.cfg - parameters, coeff file names
        #           pmn_<instr>.inp.coef r34_<instr>.inp.coef r50_<instr>.inp.coef r64_<instr>.inp.coef vmx_<instr>.inp.coef
        #           latitude.txt longitude.txt pressure_layers.txt temperature_profile.txt water_vapor_profile.txt liquid_water_path.txt tpw.xt qc.txt
        #           AVN.TXT - GFS pack file
        #   Outputs: <sat ident>.AFX <sat ident>.XYA <sat ident>.RZA <sat ident>.FIX <sat ident>.LOC
        #            oparet.log
        #

        oparet=config['programs']['oparet']

	# Create COORTIMES file from, COORDINATES, TIMES, satellite and instrument identifiers
        status=MIRS_TC.createCOORTIMES(afdeck['outputs']['coordFile'], satcenter['outputs']['timesFile'], satId, instr.upper(), oparet['inputs']['COORTIMES'])
 
        # Check return status
	if not status:
            LOG.warning("Unable to create COORTIMES file: {}".format(oparet['inputs']['COORTIMES']))
            continue 
        
        # Link oparat param, coeff configs and files
        opCfgDir,opCfgFile=os.path.split(oparet['configs']['oparet'])
        utils.linkFile(opCfgDir,opCfgFile,satDir)
        instrCoeffDir=os.path.join(oparet['configs']['coeffsDir'],instr)
        utils.linkFile(instrCoeffDir,oparet['inputs']['param'],satDir)
        for coeffFile in oparet['inputs']['coeffs']:
            utils.linkFile(instrCoeffDir,coeffFile,satDir)

        # Link GFS pack file
        utils.linkFile(modelDir,gfsPackFile,satDir,oparet['inputs']['gfsPackFile'])

        # Link MIRS data files
        for mirsFile in oparet['inputs']['mirs']:
            utils.linkFile(satDir,mirsFile,satDir,oparet['inputs']['mirs'][mirsFile])
        
        # Run oparet model
        commandList=[oparet['exe']]
        commandArgs=[]
        commandId="oparet"
        stdoutFile=os.path.join(logDir,"{}_{}_{}.stdout".format(commandId,stormId,satId))
        stderrFile=os.path.join(logDir,"{}_{}_{}.stderr".format(commandId,stormId,satId))
        LOG.info("Running oparet")
        LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
        status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)

        # Check return status
	if not status:
            LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
            LOG.warning("See sub-process log file: {}".format(stdoutFile))
            LOG.warning("See sub-process error file: {}".format(stderrFile))
            LOG.warning("oparet exited with status: {}, skipping to next satellite".format(status))
            continue 

        # Verify oparet output files
        oparetFiles={}
        for opext in oparet['outputs']['exts']: 
            oparetFiles[opext]="{}.{}".format(satId,opext)

            oparetFilename=os.path.join(satDir,oparetFiles[opext])
            if not os.path.isfile(oparetFilename):
                LOG.warning("oparet file {} does not exist".format(oparetFile))
                skipFlag=True
                next 

        # Check for error in sub-loop 
        if skipFlag:
	    LOG.warning("Problem running {}, skipping to next satellite".format(oparet['exe']))
            continue

        # Check oparet log file
        if os.path.isfile(oparet['outputs']['LOG']):
            utils.moveFile(satDir,oparet['outputs']['LOG'],logDir,"{}_{}_{}.log".format(commandId,stormId,satId))
        else:
            LOG.warning("oparet log file {} does not exist, skipping to next satellite".format(oparetFile))
            continue

        # Product prefix/suffixes 
        productPfx="{}-{}".format(config['products']['ident'],stormId)
        productSfx="{}_{}_s{}_e{}_c{}".format(config['products']['version'],sat['productId'],
                                              config['datetimes']['jcsDTG'].strftime(config['products']['DTFormat']),
                                              config['datetimes']['jceDTG'].strftime(config['products']['DTFormat']),
                                              datetime.datetime.now().strftime(config['products']['DTFormat']))

        # Link AFX/FIX products to output directory
        oparetAFXFile="{}_{}.AFX".format(productPfx,productSfx)
        oparetFixFile="{}_{}_{}_FIX".format(productPfx,productSfx,adeck.get('jtwcId'))
        utils.linkFile(satDir,oparetFiles['AFX'],outputDir,oparetAFXFile)
        utils.linkFile(satDir,oparetFiles['AFX'],outputDir,oparetFixFile)

        # Generate netCDF files for XYA/RZA files 
        #
        #   Inputs: XYA/RZA files
        #   Outputs: netCDF files

        for opext in ['XYA','RZA']:

            ncFile="{}-{}_{}.nc".format(productPfx,opext,productSfx)
            convertParams={
              "Params":{
                "date_created":datetime.datetime.now().strftime(config['products']['DTFormat']),
                "time_coverage_start":config['datetimes']['jcsDTG'].strftime(config['products']['DTFormat']),
                "time_coverage_end":config['datetimes']['jceDTG'].strftime(config['products']['DTFormat']),
                "satellite_name":sat['productId'],
                "instrument_name":instr,
                "Metadata_Link":ncFile
               }
            }
            convertParamsFile="{}_params.json".format(opext)
            with open(convertParamsFile,'w') as fh:
               json.dump(convertParams, fh)
          
            # Run convert2netCDF.py 
            convert=config['programs']['convert2NetCDF'] 
            commandList=[config['inputs']['PYTHON'],convert['exe']]
            commandArgs=['-c',convert['configs'][opext],'-p',convertParamsFile, '-i', oparetFiles[opext], '-o', ncFile]
            commandId="convert2NetCDF"
            stdoutFile=os.path.join(logDir,"{}_{}_{}_{}.stdout".format(commandId,opext,stormId,satId))
            stderrFile=os.path.join(logDir,"{}_{}_{}_{}.stderr".format(commandId,opext,stormId,satId))
            LOG.info("Running convert2netCDF")
            LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
            status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)
    
            # Check return status
	    if not status:
                LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
                LOG.warning("See sub-process log file: {}".format(stdoutFile))
                LOG.warning("See sub-process error file: {}".format(stderrFile))
                LOG.warning("{} exited with status: {}, skipping to next satellite".format(convert['exe'],status))
                skipFlag=True
                next
    
            # Check nc files
            if os.path.isfile(ncFile):
                utils.linkFile(satDir,ncFile,outputDir)
            else:
                LOG.warning("{} does not exist, skipping to next satellite".format(ncFile))
                skipFlag=True
                next

        # Check for error in sub-loop 
        if skipFlag:
	    LOG.warning("Problem running {}, skipping to next satellite".format(convert['exe']))
            continue

        # Generate XYA plots 
        #
        #   Inputs: oparet XYA file 
        #   Outputs: PNG files at different pressure levels

        plot=config['programs']['plot']

        # Link plot config file
        plotCfgDir,plotCfgFile=os.path.split(plot['configs']['plot'])
        utils.linkFile(plotCfgDir,plotCfgFile,satDir)

        # Run plotting code 
        commandList=[config['inputs']['PYTHON'],plot['exe']]
        commandArgs=[oparetFiles['XYA']]
        commandId="plot"
        stdoutFile=os.path.join(logDir,"{}_{}_{}.stdout".format(commandId,stormId,satId))
        stderrFile=os.path.join(logDir,"{}_{}_{}.stderr".format(commandId,stormId,satId))
        LOG.info("Running plot")
        LOG.info("Executing: {}".format(" ".join(commandList+commandArgs)))
        status=utils.execute(commandList,commandArgs,commandId,logDir,stdoutFile=stdoutFile,stderrFile=stderrFile)
        
        # Check return status
	if not status:
            LOG.warning("Problem executing {}".format(" ".join(commandList+commandArgs)))
            LOG.warning("See sub-process log file: {}".format(stdoutFile))
            LOG.warning("See sub-process error file: {}".format(stderrFile))
            LOG.warning("{} exited with status: {}, skipping to next satellite".format(convert['exe'],status))
            continue

        # Verify plot output image files, rename/link to product output directory
        plotFiles=utils.regexpFiles(plot['outputs']['plotRE'],satDir)
        if plotFiles:
            for plotFile in plotFiles:
                imageProductFile="{}-{}-{}_{}.{}".format(productPfx,plotFiles[plotFile]['var'],plotFiles[plotFile]['level'],productSfx,plotFiles[plotFile]['ext'])
                utils.linkFile(satDir,plotFile,outputDir,imageProductFile)
        else:
            LOG.warning("No plot files found, continuing to next satellite".format(status))
            continue

        # End Satellite Loop
        LOG.info("Completed Processing satellite/instrument: {}/{}".format(satname,instr))

    # End Storm Loop
    LOG.info("Completed Processing TC: {}".format(stormId))

# Finished
LOG.info("Finishing: {}".format(__file__)) 

