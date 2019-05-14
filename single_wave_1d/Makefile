EXE = wave_1d
FC = ifort
IDIR =
CFLAGS = -O2 -g -module $(OBJS_DIR) $(IDIR)
LFLAGS =
LIBS =

OBJS_DIR = obj/
EXE_DIR = bin/

SRCS = \
	module_wave.f90 \
	single_wave_1d.f90 \
	module_1d_solver.f90 \
	module_equation.f90

OBJS = \
	module_wave.o \
	single_wave_1d.o \
	module_1d_solver.o \
	module_equation.o

OBJS_MAIN=$(addprefix $(OBJS_DIR),$(OBJS))

all : $(EXE)

$(EXE) : $(OBJS)
	@mkdir -p $(EXE_DIR)
	$(FC) -o $(EXE_DIR)$(EXE) $(OBJS_MAIN) $(LFLAGS) $(LIBS)

$(OBJS):
	@mkdir -p $(OBJS_DIR)
	$(FC) $(CFLAGS) -c $(@:.o=.f90) -o $(OBJS_DIR)$@

clean :
	rm -fr $(OBJS_DIR)
	rm -fr $(EXE_DIR)

# Dependencies of files
single_wave_1d.o: \
    single_wave_1d.f90 \
    module_1d_solver.o \
		module_equation.o
module_1d_solver.o: \
	module_1d_solver.f90 \
	module_wave.o \
	module_equation.o
module_equation.o: \
	module_equation.f90 \
	module_wave.o
module_wave.o: \
	module_wave.f90
