# Configure Matrix Port Lib
CXXFLAGS=-Wall -O3 -g -std=c++11
HEADER_FILES=src
SRC=$(wildcard src/*.cc)
OBJECTS=$(SRC:.cc=.o)
 
# Configure RPI-RGB-LED-Matrix
RGB_LIB_DISTRIBUTION=src/matrix
RGB_INCDIR=$(RGB_LIB_DISTRIBUTION)/include
RGB_LIBDIR=$(RGB_LIB_DISTRIBUTION)/lib
RGB_LIBRARY_NAME=rgbmatrix
RGB_LIBRARY=$(RGB_LIBDIR)/lib$(RGB_LIBRARY_NAME).a
LDFLAGS+=-L$(RGB_LIBDIR) -l$(RGB_LIBRARY_NAME) -lrt -lm -lpthread

# Make Port Lib

all: priv/c/matrix-port

priv/c/matrix-port: c_priv $(OBJECTS) $(RGB_LIBRARY)
	$(CXX) $(CXXFLAGS) -I $(HEADER_FILES) $(OBJECTS) -o $@ $(LDFLAGS)

$(RGB_LIBRARY):
	$(MAKE) -C $(RGB_LIBDIR)

%.o : %.cc
	$(CXX) -I$(RGB_INCDIR) $(CXXFLAGS) -c -o $@ $<

c_priv:
	mkdir -p priv/c

clean:
	rm -f priv/c/* $(OBJECTS) $(BEAM_FILES)
	$(MAKE) -C $(RGB_LIBDIR) clean
