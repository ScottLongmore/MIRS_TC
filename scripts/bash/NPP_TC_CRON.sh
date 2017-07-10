#!/bin/bash

ROOTDIR="/data/longmore/Projects/NDE/ops/NPP_TC/"
PYTHONDIR="/home/longmore/local/epd-7.3-1-rh5-x86_64/"
ROOTWORKDIR="/data2/NPP_TC/runs/rt/"

INITDATE=`date -u  +%Y%m%d%H`
MIRS_TC_EXE="${ROOTDIR}/scripts/python/npp_tc/npp_tc.py"
MIRS_TC_CFG="${ROOTDIR}/etc/NPP_TC_RT.json"
LOGFILE="${ROOTDIR}/log/NPP_TC.${INITDATE}.LOG"

PYTHON="${PYTHONDIR}/bin/python"
export PYTHONPATH="${PYTHONDIR}:${ROOTDIR}/scripts/python/common"

# python npp_tc.py -c NPP_TC_RT.json -t 201408252200000
COMMAND="${PYTHON} ${MIRS_TC_EXE} -c ${MIRS_TC_CFG} -t ${INITDATE}00" 
${COMMAND} >& ${LOGFILE}
