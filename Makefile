# Compiler and flags
FC = gfortran
FLAGS = -Wall -Wextra -O2
LDLIBS = -llapack -lblas

# Source files
SOURCES = numerics.f90 structure.f90 source.f90 main.f90
OBJECTS = $(SOURCES:.f90=.o)

# Executable name
EXEC = exec

# Default target
all: $(EXEC)

# Linking the executable
$(EXEC): $(OBJECTS)
	$(FC) $(FLAGS) -o $(EXEC) $(OBJECTS)

# Compiling source files
%.o: %.f90
	$(FC) $(FLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJECTS) $(EXEC) *.mod *.o

# Phony targets
.PHONY: all clean