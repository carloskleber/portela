FC = gfortran
FCFLAGS = -std=legacy -ffixed-form
SRC := $(wildcard *.f)
OBJ := $(patsubst %.f,%.o,$(SRC))

.PHONY: all clean

all: main

main: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ -L./ $(CXX_LIB)

%.o: %.f
	$(FC) $(FCFLAGS) -c $< -L./

clean: 
	rm -f main *.o *.mod