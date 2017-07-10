#FC	= pgf90
FC	= gfortran 
#FFLAGS	= -O2 -Mbounds
FFLAGS	= -O2 
#LD	= pgf90
LD	= gfortran 
LDFLAGS	=
RM	= /bin/rm -f

PROG	= satcenter.x 
SRC	= satcenter.f90 
MODSRC	= asciidat_routines.f90 

OBJ	= $(MODSRC:.f90=.o) $(SRC:.f90=.o) 
MOD	= $(MODSRC:.f90=.mod)
LIB	= 

.SUFFIXES:
.SUFFIXES: .f90 .f .o .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

.f90.mod:
	$(FC) $(FFLAGS) -c $<

all:	$(PROG)

$(PROG) : $(OBJ)
	$(LD) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIB)

satcenter.o:	asciidat_routines.mod 

clean:
	$(RM) $(OBJ) $(MOD) $(PROG)
