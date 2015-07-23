.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options


OBJS =	graphics.o \
	module_netcdf_io.o \
	module_bondville_validation_data.o \
	kwm_date_utilities.o \
	kwm_plot_utilities.o \
	kwm_string_utilities.o \
	arguments_module.o \
	get_unused_unit.o

CMD = graphics.exe

FFLAGS 	=	$(FREESOURCE) $(F90FLAGS)
NCARGLIBS = 	-L$(NCARG_ROOT)/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lXext

#
# On some linux machines, NCAR-Graphics seems to require the g2c library.  
# All that stuff with G2CNAME, G2CPATH, and G2CLIB is my attempt to find 
# out where that library resides. I don't know how robust this 
# investigation is (probably not very).  If it's not working for you,
# take it out and hard-code the G2CLIB variable, if necessary.
#

ifeq ($(COMPILERF90),pgf90)
	G2CNAME = 	$(shell g77 -print-file-name=libg2c.a)
	G2CPATH =	$(dir ${G2CNAME})
	G2CLIB  =	-L${G2CPATH} -lg2c -lgfortran
endif

ifeq ($(COMPILERF90),g95)
	G2CLIB  =	-L/usr/lib/gcc/i386-redhat-linux/4.1.1 -lgfortran
endif

ifeq ($(COMPILERF90),gfortran)
	G2CNAME = 	$(shell g77 -print-file-name=libg2c.a)
	G2CPATH =	$(dir ${G2CNAME})
	G2CLIB  =	-L${G2CPATH} -lg2c
endif

ifeq ($(COMPILERF90),ifort)
	G2CLIB  =	-L/usr/lib/gcc/i386-redhat-linux/4.1.1 -lgfortran
endif

ifeq ($(COMPILERF90),xlf90)
	NCARGLIBS = -L$(NCARG_ROOT)/lib -L/usr/X11R6/lib -lncarg -lncarg_gks -lncarg_c -lX11 -lXext -lpng_ncl -lz_ncl
	G2CLIB =
endif

all:	$(CMD)

graphics.exe:	$(OBJS)
	$(COMPILERF90) -o $(@) $(OBJS) $(LIBS) $(NETCDFLIB) $(G2CLIB) $(NCARGLIBS)

.F.o:
	$(RM) $(*).f90
	$(CPP) $(CPPMACROS) $(NETCDFINC) $(*).F > $(*).f90
	$(COMPILERF90) -c $(NETCDFINC) $(FFLAGS) $(*).f90
	$(RM) $(*).f90

clean:
	$(RM) $(OBJS) $(CMD) *.mod *~

graphics.exe:		graphics.o
graphics.o:		kwm_plot_utilities.o
graphics.o:		kwm_date_utilities.o
graphics.o:		module_netcdf_io.o
graphics.o:		module_bondville_validation_data.o
graphics.o:		arguments_module.o
kwm_plot_utilities.o:	kwm_string_utilities.o
