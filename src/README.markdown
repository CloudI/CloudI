# CloudI Source Directory

## CloudI Development Information

### Building

To build CloudI from scratch, do the following steps:

    ./autogen.sh
    ./configure (--prefix=/path/to/installation/directory)
    make
    make install

### Cleaning

There are two configuration targets to simplify CloudI development:

    make clean
    make clean-configure

### Running

Within the installation directory the "bin/cloudi" script controls CloudI.

To start CloudI:

    bin/cloudi start

To stop CloudI:

    bin/cloudi stop

