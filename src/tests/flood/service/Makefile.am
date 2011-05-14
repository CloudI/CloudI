SUBDIRS = org jar

noinst_PROGRAMS = flood
flood_SOURCES = flood.c
flood_CFLAGS = -I$(top_srcdir)/api/c/
flood_LDFLAGS = -L$(top_builddir)/api/c/
flood_LDADD = -lcloudi -lstdc++

