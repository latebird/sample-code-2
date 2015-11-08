#
# Make file to compiler the codes
    FC = gfortran
#
    MYFLAG = -O2 -fdefault-real-8
    FFLAGS = $(MYFLAG)
#
OBJLIBS = blas.o lapack.o
                        
MYCODE  = rateconstants.o basicKmatrix.o checkifexcited.o checkifOspresent.o  \
          RateMatrixauto.o checksite.o initial_di.o util.o iniexloop.o \
          iniex_di_none_Os.o os_distri.o RateMat3D.o

#EXPOKIT = expokit.o mataid.o

EXTRA   = linpack.o

OBJ     = $(MYCODE) $(EXPOKIT) $(OBJLIBS) $(EXTRA)

## LIBS    = -Wl,-framework -Wl,vecLib -lSystemStubs
LIBS    = 

clean :
	/bin/rm -f  *.o *.bak core *.mod *~ mof3D.exe  

###
#

all : $(OBJ) M_One_Main.o
	$(FC) $(FFLAGS) -o ./mof3D.exe M_One_Main.o $(OBJ) $(LIBS)

.SUFFIXES :
.SUFFIXES : .F .F90 .f90 .f .o

.f.o:
	$(FC) $(FFLAGS) -c -o $*.o $*.f
.F.o:
	$(FC) $(FFLAGS) -c -o $*.o $*.F
.F90.o :
	$(FC) $(FFLAGS) -c -o $*.o $*.F90

.f90.o :
	$(FC) $(FFLAGS) -c -o $*.o $*.f90


