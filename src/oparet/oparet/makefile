#==============================================================================
#
# Makefile for:  The CIRA JHT project to estimate 
# 		 tropical cyclone intensity and wind structure using data from 
# 		 the Advanced Microwave Sounding Unit on the GOES satellite
#		 platform.
#
# This version is for the IBM
#
# Notes: make  - compilies the two executables (pickavn.exe and oparet.exe)
#   	 make clean  - removes the object files and executables 
#   	 make pickavn.exe  - compilies the executable pickavn.exe
#   	 make oparet.exe   - compilies the executable oparet.exe
#   	 make install	- places the *.exe files in the ../bin directory
#
# Executables:  pickavn.exe 
#		oparet.exe
#
# Written by J. Knaff and M. DeMaria, May 2003
# Modified by: A. Krautkramer 5/19/03
#		Added comments and standardized makefile
# Modified by: ARK for IBM 2/10/2005
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define default macros --
#-----------------------------------------------------------------------------

#--------------------
# Define default shell
#--------------------

SHELL = /bin/sh

RM	= rm -rf

CP	= cp

LOC	= ../../../bin

#-----------------------------------------------------------------------------
#                       -- Define file lists --
#-----------------------------------------------------------------------------

#---------
# Libraries
#---------

FLIBS	=

#--------
# Includes
#--------

FINCS =

#------------
# Source/Object/Executable files
#------------

#PSRC	= pickavn.f utils.f
#POBS	= $(PSRC:.f90=.o)

ASRC	= dims.f90 cons.f90 params.f90 ncepll.f90 cgrid.f90 ncepfg.f90 ncepp.f90 \
          sinfo1.f90 sinfo2.f90 sinfo3.f90 io.f90 asciidat_routines.f90 \
          utils.f90 hstatic.f90 sphfun.f90 srsubs.f90 bwcal.f90 \
	  vradsubs.f90 amsusubs.f90 besubs.f90 opsubs.f90 oparet.f90 
AOBS	= $(ASRC:.f90=.o)

#EXE	= pickavn oparet
#EXE	= oparet_calval.x
EXE	= oparet.x

#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

#------------------
# Compiler
#------------------
#... GNU fotran compiler 
FC	= gfortran

#... HP F90 compiler
#FC	= f95	

#... Linux PG F90 compiler
#FC	= pgf90 

#... IBM AIX F90 compiler
#FC      = xlf_r
#FC	= ncepxlf

#-----------------------------
# Compiler Flags 
#-----------------------------

#DEBUG_FLAGS=-g 
DEBUG_FLAGS=-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -ffpe-trap=zero,overflow,underflow -finit-real=nan 
#DEBUG_FLAGS=-g -pg -fbacktrace

#DEBUG_FLAGS=-g -Mdwarf2 
#DEBUG_FLAGS=-g -fdefault-real-8 -fdefault-double-8 
#DEBUG_FLAGS=-g -Mdwarf2 -fdefault-double-8 -fdefault-integer-8 -fdefault-real-8
#COMMON_FLAGS=$(DEBUG_FLAGS) +O2 -DA2.0 	# HPUX
#COMMON_FLAGS=$(DEBUG_FLAGS)  -DA2.0 -g77libs	# Linux
COMMON_FLAGS=$(DEBUG_FLAGS)                     # Linux
#COMMON_FLAGS=$(DEBUG_FLAGS) 			# IBM

#------------ 
# Common Flags 
#------------

FC_FLAGS=$(COMMON_FLAGS) -c 
#FC_FLAGS=$(COMMON_FLAGS) -c -O2
FL_FLAGS=$(COMMON_FLAGS) -o

#-------------
# Make program
#------------

all: $(EXE) 

#-------------
# pickavn
#---------------

pickavn: $(POBS)
	$(FC) $(FL_FLAGS) $@ $(POBS) $(FLIBS)
%.o:%.f90
	$(FC) $(FC_FLAGS) $<	

#-----------------
# oparet
#---------------

#oparet_calval.x: $(AOBS)
oparet.x: $(AOBS)
	$(FC) $(FL_FLAGS) $@ $(AOBS) $(FLIBS)
%.o:%.f90
	$(FC) $(FC_FLAGS) $<
	
#--------
# Clean up
#--------
	
clean:	
	$(RM) *.o *.mod

#clean:	
#	$(RM) *.o *.mod $(EXE) 

#--------
# Copy exe 
#--------
#
# all executables (defalt)
#
install: $(EXE)
	   $(CP) $(EXE) $(LOC)
