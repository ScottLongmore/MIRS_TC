#!/bin/sh
#
# NPP_TC_MKCONFIG.sh - this script creates the file query_config.txt as input 
#                      for MIRS satellite info and dataset extraction by pool_query.py
#

# Directory where MIRS data are stored
mirsdir=$QUERYDIR

# Output directory
#procdir=/home/dostalek/code/mirs_data_pool/tmp/query_results
procdir=$STORMDIR

# Create variable containing number of satellites and an array containing 
# the names of the satellites for which processing will occur.
sats=( $SAT )

# Get date and time information. The file tilalo_hold comes from 
yr=`head -1 tilalo_hold | tail -1`
mo=`head -2 tilalo_hold | tail -1`
dy=`head -3 tilalo_hold | tail -1`
hr=`head -4 tilalo_hold | tail -1`
lat=`head -5 tilalo_hold | tail -1`
lon=`head -6 tilalo_hold | tail -1`
#rm -f tilalo_hold

# Loop over satellites in order to create a separate query_config file for 
# each.

for (( m=0 ; m < ${#sats[@]} ; m++ )) do

# Output file name
  outfile='query_config.'${sats[$m]}

# input section
  echo '[input]' > $outfile
  echo 'root_dir = '$mirsdir >> $outfile
  echo 'variable_list_filename = query_variable_list.txt' >> $outfile
  echo 'satellite_filename_id = '${sats[$m]} >> $outfile
  echo '' >> $outfile

# output section
  echo '[output]' >> $outfile
  echo 'output_dir_name = '$procdir >> $outfile
  echo 'should_output_point_indices = 1' >> $outfile
  echo 'should_output_scanline_center_coords = 1' >> $outfile
  echo '' >> $outfile

# time_window section
  echo '[time_window]' >> $outfile
  echo 'year = '$yr >> $outfile
  echo 'month = '$mo >> $outfile
  echo 'day = '$dy >> $outfile
  echo 'hour= '$hr >> $outfile
  echo "time_window_hours_after = ${MIRS_FRWD_HOURS}" >> $outfile
  echo "time_window_hours_before = ${MIRS_BKWD_HOURS}" >> $outfile
  echo '' >> $outfile

# space_window section
  echo '[space_window]' >> $outfile
  echo 'center_lat = '$lat >> $outfile
  echo 'center_lon = '$lon >> $outfile
  echo 'half_height_lat = 14' >> $outfile
  echo 'half_width_lon = 14' >> $outfile
  echo '' >> $outfile

# pressure_window section
  echo '[pressure_window]' >> $outfile
  echo 'top_pressure = -10' >> $outfile
  echo 'bottom_pressure = 1500' >> $outfile
  echo '' >> $outfile

# lock_file section
  echo '[lock_file]' >> $outfile
  echo 'use_update_lock = 1' >> $outfile
done

