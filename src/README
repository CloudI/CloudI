# CloudI Source Directory

## CloudI Development Information

### Building

To build CloudI from scratch, do the following steps:

    ./autogen.sh
    ./configure (--prefix=/path/to/installation/directory)
    make
    (sudo) make install

### Cleaning

There are two configuration targets to simplify CloudI development:

    make clean
    make clean-configure

### Running

Within the installation directory the cloudi script controls CloudI.

To start CloudI:

    (sudo or /path/to/installation/directory/bin/)cloudi start

To stop CloudI:

    (sudo or /path/to/installation/directory/bin/)cloudi stop

### Repository

When updating the repository code or committing changes, make sure to first run
the command:

    make clean-configure

The `"clean-configure"` makefile target will revert all external dependencies
back to their original filenames, to bring the repository back into its
original file/directory structure.

