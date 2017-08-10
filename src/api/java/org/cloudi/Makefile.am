#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

jardir = $(top_builddir)/api/java/jar
JAVACFLAGS = -Xlint -Werror
CLEANFILES = built
FILES = $(srcdir)/API.java \
        $(srcdir)/Function9.java \
        $(srcdir)/FunctionInterface9.java \
        $(srcdir)/FunctionObject9.java

all-local: built

built: $(FILES)
	$(JAVAC) $(JAVACFLAGS) -cp $(jardir) -d $(jardir) $(FILES)
	touch built

