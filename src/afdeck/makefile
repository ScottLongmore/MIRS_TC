FC	= gfortran 
#FFLAGS	= -O2 -Mbounds
FFLAGS	= -g -O2 
LD	= gfortran 
LDFLAGS	=
RM	= /bin/rm -f

PROG	= afdeck.x
SRC	= afdeck.f90 
f77SRC	= dataio.f dtgutils.f 
MODSRC	= extract_aid_module.f90 extract_fix_module.f90

OBJ	= $(MODSRC:.f90=.o) $(SRC:.f90=.o) $(f77SRC:.f=.o)
MOD	= $(MODSRC:.f90=.mod)
LIB	= 

.SUFFIXES:
.SUFFIXES: .f90 .f .o .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

.f.o:
	$(FC) $(FFLAGS) -c $<

.f90.mod:
	$(FC) $(FFLAGS) -c $<

all:	$(PROG)

$(PROG) : $(OBJ)
	$(LD) $(LDFLAGS) -o $(PROG) $(OBJ) $(LIB)

extract_aid_module.o:	dataio.o dtgutils.o

extract_fix_module.o:	dataio.o dtgutils.o
 
afdeck.o:		extract_aid_module.mod extract_fix_module.mod 

clean:
	$(RM) $(OBJ) $(MOD) $(PROG)
