# Makefile binZZ2pack.x
#
# Last modified:  June, 2013
#
#*******************************************************************
F90 = gfortran

FFLAGS = -O2
FLIBS = 
FSRC = bin2pack.f packsubs.f stndz.f llintp.f
FOBS = bin2pack.o packsubs.o stndz.o llintp.o

bin2pack.x: $(FOBS) 
	$(F90) -o $@ $(FFLAGS) $(FOBS) $(COBS) $(FLIBS)
bin2pack.o:bin2pack.f packsubs.o stndz.o llintp.o 
	$(F90) -c $(FFLAGS) bin2pack.f $(FLIBS)
packsubs.o: packsubs.f  
	$(F90) -c $(FFLAGS) packsubs.f $(FLIBS)
stndz.o: stndz.f 
	$(F90) -c $(FFLAGS) stndz.f $(FLIBS)
llintp.o: llintp.f 
	$(F90) -c $(FFLAGS) llintp.f $(FLIBS)
clean: 
	rm -rf *.o *~ *.mod *.x


