# Makefile ShortTermTrack_mirs.x
#
# This makefile compiles a example program (ShortTermTrack_mirs.f90) that 
# produces an input file (e.g., al012012.inp) that contains the storm 
# identifier, the analysis date/time, the number of track points and the
# time series of latitude/longitude points.  The input file drives the 
# tropical cyclone surface wind analyses (a JHT sponsored project).
#
#an example input file fillows....
#al142011 20110911 1200
#   11 Number of Track Points
#YEARDATE  TIME   DELTA T  LATITUDE LONGITUDE
#20110910  1200  -24.0000     16.90    298.80
#20110910  1800  -18.0000     17.50    298.00
#20110911  0000  -12.0000     18.00    297.40
#20110911  0321   -8.6500     18.43    297.03
#20110911  0503   -6.9500     18.60    296.73
#20110911  0600   -6.0000     18.60    296.70
#20110911  1200    0.0000     18.80    296.00
#20110911  1359    1.9833     18.75    295.40
#20110911  1533    3.5500     19.37    295.57
#20110911  1720    5.3333     19.58    295.40
#20110912  0000   12.0000     21.50    293.20
#
# The program makes use of two modules to extract information contained in
# the adeck and fdeck database files, subroutines timediff.f90, and index.for
#
# Make file created by J. Knaff, NOAA/NESDIS January 12, 2012
#
# Last modified:  January 12, 2012
#
#*******************************************************************
#F90 = pgf90
F90 = gfortran 
# for development 
#FFLAGS = -C -g 
#FFLAGS = -fast
FFLAGS = -O2 
FLIBS = 
FSRC = ShortTermTrack_mirs.f90 extract_aid_module.f90 extract_fix_module.f90 indexx.f90 timediff.f90 dataio.f dtgutils.f
FOBS = ShortTermTrack_mirs.o extract_aid_module.o extract_fix_module.o indexx.o timediff.o dataio.o dtgutils.o


ShortTermTrack_mirs.x: $(FOBS) 
	$(F90) -o $@ $(FFLAGS) $(FOBS) $(COBS) $(FLIBS)
ShortTermTrack_mirs.o:ShortTermTrack_mirs.f90 extract_fix_module.o dataio.o dtgutils.o indexx.o timediff.o extract_aid_module.o   extract_fix_module.mod   extract_aid_module.mod   
	$(F90) -c $(FFLAGS) ShortTermTrack_mirs.f90 $(FLIBS)
extract_fix_module.mod: extract_fix_module.f90   
	$(F90) -c $(FFLAGS) extract_fix_module.f90 $(FLIBS)
extract_aid_module.mod: extract_aid_module.f90   
	$(F90) -c $(FFLAGS) extract_aid_module.f90 $(FLIBS)
extract_fix_module.o: extract_fix_module.f90 extract_fix_module.mod   
	$(F90) -c $(FFLAGS) extract_fix_module.f90 $(FLIBS)
extract_aid_module.o: extract_aid_module.f90 extract_aid_module.mod   
	$(F90) -c $(FFLAGS) extract_aid_module.f90 $(FLIBS)
dataio.o: dataio.f 
	$(F90) -c $(FFLAGS) dataio.f $(FLIBS)
dtgutils.o: dtgutils.f 
	$(F90) -c $(FFLAGS) dtgutils.f $(FLIBS)
timediff.o: timediff.f90 
	$(F90) -c $(FFLAGS) timediff.f90 $(FLIBS)
indexx.o: indexx.f90 
	$(F90) -c $(FFLAGS) indexx.f90 $(FLIBS)
clean: 
	rm -rf *.o *~ *.mod *.x


