include worker_protocol.mk

# force usage of a local compiler for c++
CXXFLAGS=$(GXX_CXXFLAGS)
LDFLAGS=$(GXX_LDFLAGS)
CXX=$(GXX_BIN)
CC=$(GCC_BIN)

TARGET=libWorkerProtocol.a
CFLAGS+= -I. -w
#-DEMIT_ASN_DEBUG=1 -g -O0
CXXFLAGS+= -I. -std=c++0x -Wall -Wextra -I$(BOOST_INC_PATH) -I$(CLOUD_INC_PATH)
#-DEMIT_ASN_DEBUG=1 -g -O0
OBJS=${ASN_MODULE_SOURCES:.c=.o}

all: sizeof_WorkerMessage_t $(TARGET)

sizeof_WorkerMessage_t: \
           sizeof_WorkerMessage_t.cpp \
           WorkerMessage.h \
           Queries.h \
           Query.h
	@$(CXX) $(CXXFLAGS) -I. sizeof_WorkerMessage_t.cpp \
        -o sizeof_WorkerMessage_t $(LDFLAGS)

$(TARGET): ${OBJS} \
           sizeof_WorkerMessage_t \
           ../include/worker_protocol.hpp \
           worker_protocol.cpp
	@./sizeof_WorkerMessage_t ../include/worker_protocol_defs.hpp
	@$(CXX) $(CXXFLAGS) -I../include \
        -o ../priv/obj/worker_protocol.o -c worker_protocol.cpp
	@(cd ../priv/obj/ && \
      $(AR) -rfs ../lib/$(TARGET) worker_protocol.o ${OBJS} $(LIBS))

.SUFFIXES: .c .o

.c.o:
	@$(CC) $(CFLAGS) -o ../priv/obj/$@ -c $<

clean:
	@$(RM) ../priv/lib/$(TARGET)
	@$(RM) $(addprefix ../priv/obj/, ${OBJS})
	@$(RM) $(ASN_MODULE_SOURCES) $(ASN_CONVERTER_SOURCES) $(ASN_MODULE_HEADERS)
	@$(RM) Makefile.am.sample
	@$(RM) WorkerProtocol.erl WorkerProtocol.hrl
	@$(RM) WorkerProtocol.asn1db WorkerProtocol.beam
	@$(RM) sizeof_WorkerMessage_t

