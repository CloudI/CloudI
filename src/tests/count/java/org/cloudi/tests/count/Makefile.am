#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

jardir = $(top_builddir)/tests/count/java/jar
classpath = $(top_builddir)/api/java/jar/cloudi.jar
JAVACFLAGS = -Xlint -Werror
CLEANFILES = built
FILES = $(srcdir)/Main.java \
        $(srcdir)/Task.java

all-local: built

built: $(FILES)
	$(JAVAC) $(JAVACFLAGS) -cp $(classpath) -d $(jardir) $(FILES)
	touch built

