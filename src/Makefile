#
#get more out of your make: "make 2>&1 | more"
#

# project variables
PROJECT_NAME=cloud
VERSION_NAME=0.0.2.0
PROJECT_REPO_URL=http:\/\/host1:3001\/repo\/
MAJOR_VERSION_NAME=0.0.2
# erlang variables
ERLANG_ARGUMENTS=+A 16 +K true
# use instrument module
#ERLANG_ARGUMENTS+=+Mim true +Mis true
# use erts_alloc_config module
#ERLANG_ARGUMENTS+=+Mea config
# erts_alloc_config suggested:
#ERLANG_ARGUMENTS+=$(shell grep '^ ' erts_alloc_config.log)

ERLANG_PATH=/home/user/installed
ERTS_VERSION=5.7.2
ERL_INTERFACE_VERSION=3.6.2
# erlware variables
ERLWARE_PATH=/home/user/installed/erlware

################################################################################

#DEADCODESTRIP arguments Step by step...
# (http://gcc.gnu.org/ml/gcc-help/2003-08/msg00128.html)
#
#-Wl,-static
#Link against static libraries.  Required for dead-code elimination.
#
#-fvtable-gc
#C++ virtual method table instrumented with garbage collection information for the linker.
#
#-fdata-sections
#Keeps data in separate data sections, so they can be discarded if unused.
#
#-ffunction-sections
#Keeps funcitons in separate data sections, so they can be discarded if unused.
#
#-Wl,--gc-sections
#Tell the linker to garbage collect and discard unused sections.
#
#-s
#Strip the debug information, so as to make the code as small as possible.
#
CXX_DEADCODESTRIP_ARGS=-Wl,-static -fvtable-gc -fdata-sections -ffunction-sections -Wl,--gc-sections -Wl,-s

FAILOVER_HOSTS_FILE=config/failover_hosts
ROOT_PATH=$(abspath .)
SINAN_PATH=$(ERLWARE_PATH)/bin/sinan
ERTS_PATH=$(ERLANG_PATH)/lib/erlang/erts-$(ERTS_VERSION)
ERL_INTERFACE_PATH=$(ERLANG_PATH)/lib/erlang/lib/erl_interface-$(ERL_INTERFACE_VERSION)
ERL_INTERFACE_INC_PATH=$(ERL_INTERFACE_PATH)/include
ERL_INTERFACE_LIB_PATH=$(ERL_INTERFACE_PATH)/lib
EPMD_PATH=$(ERLANG_PATH)/bin/epmd
ERL_PATH=$(ERLANG_PATH)/bin/erl
ERLC_PATH=$(ERLANG_PATH)/bin/erlc
DIALYZER_PATH=$(ERLANG_PATH)/bin/dialyzer
DIALYZER_PLT=dialyzer.plt
GEPD_INC_PATH=lib/gepd
WORKER_INC_PATH=lib/cloud_worker/include
WORKER_SRC_PATH=lib/cloud_worker/src
WORKER_OBJ_PATH=lib/cloud_worker/priv/obj
WORKER_LIB_PATH=lib/cloud_worker/priv/lib
WORKER_PROTOCOL_PATH=lib/cloud_worker/protocol
MAIN_SRC_PATH=lib/cloud/src
MAIN_INC_PATH=$(ROOT_PATH)/lib/cloud/include
MAIN_BIN_PATH=lib/cloud/priv
MAIN_CONF_PATH=lib/cloud/conf
GXX_SRC=lib/g++/gcc-4.4.0.tar.gz
GXX_SRC_PATH=$(ROOT_PATH)/lib/g++/releases/gcc-4.4.0
GXX_BIN_PATH=$(GXX_SRC_PATH)_install
GXX_BIN=$(GXX_BIN_PATH)/bin/g++
GCC_BIN=$(GXX_BIN_PATH)/bin/gcc
ASN1_SRC=lib/asn1/asn1c-0.9.21.tar.gz
ASN1_SRC_PATH=lib/asn1/releases/asn1c-0.9.21
ASN1_BIN_PATH=$(ASN1_SRC_PATH)_install
ASN1_BIN=$(ASN1_BIN_PATH)/bin/asn1c
GMP_SRC=lib/gmp/gmp-4.3.1.tar.gz
GMP_SRC_PATH=$(ROOT_PATH)/lib/gmp/releases/gmp-4.3.1
GMP_INC_PATH=$(GMP_SRC_PATH)_install/include
GMP_LIB_PATH=$(GMP_SRC_PATH)_install/lib
PTMALLOC_SRC=lib/ptmalloc/ptmalloc3-current.tar.gz
PTMALLOC_SRC_PATH=$(ROOT_PATH)/lib/ptmalloc/releases/ptmalloc3
PTMALLOC_LIB_PATH=$(PTMALLOC_SRC_PATH)/lib
#BOOST_SRC=lib/boost/boost_trunk.tar.gz
#BOOST_SRC_PATH=lib/boost/releases/boost_trunk
#BOOST_INC_PATH=$(BOOST_SRC_PATH)_install/include
BOOST_SRC=lib/boost/boost_1_39_0.tar.gz
BOOST_SRC_PATH=$(ROOT_PATH)/lib/boost/releases/boost_1_39_0
BOOST_INC_PATH=$(BOOST_SRC_PATH)_install/include/boost-1_39
BOOST_LIB_PATH=$(BOOST_SRC_PATH)_install/lib
LEAKTRACER_SRC_PATH=lib/LeakTracer
#LEAKTRACER_SRC_PATH=lib/LeakTracer-mg
CLOUD_BIN_PATH=_build/development/apps/$(PROJECT_NAME)-$(MAJOR_VERSION_NAME)/priv
WORK_TYPE1_SRC_PATH=lib/cloud_job_tests/src
WORK_TYPE1_OBJ_PATH=lib/cloud_job_tests/priv/obj
WORK_TYPE1_LIB_PATH=lib/cloud_job_tests/priv/lib

# force usage of a local compiler for c++
GXX_CXXFLAGS=-I$(GXX_BIN_PATH)/include/c++/4.4.0/
GXX_LDFLAGS=-L$(GXX_BIN_PATH)/lib \
            -Wl,-rpath $(GXX_BIN_PATH)/lib
LBITS=$(shell getconf LONG_BIT)
ifeq ($(LBITS),32)
GXX_LDFLAGS+=\
    -L$(GXX_BIN_PATH)/lib/gcc/i686-pc-linux-gnu/4.4.0/ \
    -Wl,-rpath $(GXX_BIN_PATH)/lib/gcc/i686-pc-linux-gnu/4.4.0/
else
ifeq ($(LBITS),64)
GXX_LDFLAGS+=\
    -L$(GXX_BIN_PATH)/lib/gcc/x86_64-unknown-linux-gnu/4.4.0/ \
    -L$(GXX_BIN_PATH)/lib64 -L$(GXX_BIN_PATH)/lib32 \
    -Wl,-rpath $(GXX_BIN_PATH)/lib/gcc/x86_64-unknown-linux-gnu/4.4.0/ \
    -Wl,-rpath $(GXX_BIN_PATH)/lib64 -Wl,-rpath $(GXX_BIN_PATH)/lib32
else
GXX_BIN=
GCC_BIN=
endif
endif
CXXFLAGS=$(GXX_CXXFLAGS)
LDFLAGS=$(GXX_LDFLAGS)
CXX=$(GXX_BIN)
CC=$(GCC_BIN)

# typical c++ compilation arguments
CXXFLAGS+=-std=c++0x -Wall -Wextra -I$(BOOST_INC_PATH)
# to determine different optimizations at different levels:
# lib/g++/releases/gcc-4.4.0_install/bin/g++ -c -Q -O3 --help=optimizers > /tmp/O3-opts
# lib/g++/releases/gcc-4.4.0_install/bin/g++ -c -Q -O2 --help=optimizers > /tmp/O2-opts
# diff /tmp/O2-opts /tmp/O3-opts | grep enable

# release or debug
#CXXFLAGS+=-O3
CXXFLAGS+=-g -O0

LDFLAGS+=-L$(BOOST_LIB_PATH)
# debug glibc
#/usr/lib/debug/lib  /usr/lib/debug/usr/lib /usr/lib/debug/usr/lib32  /usr/lib/debug/lib32 /usr/lib/debug/
LDFLAGS+=-L/usr/lib/debug/

# source files compiled with sinan
ERLANG_SRC_FILES=\
        $(MAIN_INC_PATH)/cloud_data_interface.erl \
        $(MAIN_INC_PATH)/cloud_logger.hrl \
        $(MAIN_INC_PATH)/cloud_work_interface.erl \
        $(MAIN_SRC_PATH)/cloud_api.erl \
        $(MAIN_SRC_PATH)/cloud_app.erl \
        $(MAIN_SRC_PATH)/cloud_configuration.erl \
        $(MAIN_SRC_PATH)/cloud_configuration.hrl \
        $(MAIN_SRC_PATH)/cloud_data_repository_sup.erl \
        $(MAIN_SRC_PATH)/cloud_leader.erl \
        $(MAIN_SRC_PATH)/cloud_logger.erl \
        $(MAIN_SRC_PATH)/cloud_peer.erl \
        $(MAIN_SRC_PATH)/cloud_run_queue.erl \
        $(MAIN_SRC_PATH)/cloud_run_queue.hrl \
        $(MAIN_SRC_PATH)/cloud_sup.erl \
        $(MAIN_SRC_PATH)/cloud_task_speed_lookup.erl \
        $(MAIN_SRC_PATH)/cloud_worker_nodes.erl \
        $(MAIN_SRC_PATH)/cloud_worker_port.erl \
        $(MAIN_SRC_PATH)/cloud_worker_port_sup.erl \
        $(MAIN_SRC_PATH)/cloud_worker_scheduler.erl \
        $(MAIN_SRC_PATH)/cloud_work_manager.erl \
        $(MAIN_SRC_PATH)/cloud_work_status.erl \
        $(MAIN_SRC_PATH)/cloud_work_status.hrl \
        $(MAIN_SRC_PATH)/cloud_work_sup.erl \
        $(MAIN_SRC_PATH)/immediate_gc.erl \
        $(MAIN_SRC_PATH)/lists_extensions.erl \
        $(MAIN_SRC_PATH)/math_extensions.erl \
        $(MAIN_SRC_PATH)/q.erl \
        $(MAIN_SRC_PATH)/string_extensions.erl \
        $(MAIN_SRC_PATH)/time_extensions.erl \
        $(MAIN_SRC_PATH)/tuple_extensions.erl \
        lib/cloud_data_pgsql/src/cloud_data_interface.erl \
        lib/cloud_data_pgsql/src/cloud_data_pgsql.erl \
        lib/rb/src/rbsets.erl \
        lib/rb/src/rbdict.erl \
        lib/epgsql/test_src/pgsql_tests.erl \
        lib/epgsql/src/pgsql_sock.erl \
        lib/epgsql/src/pgsql_idatetime.erl \
        lib/epgsql/src/pgsql.erl \
        lib/epgsql/src/pgsql_types.erl \
        lib/epgsql/src/pgsql_binary.erl \
        lib/epgsql/src/pgsql_connection.erl \
        lib/epgsql/src/pgsql_fdatetime.erl \
        lib/pgsql/src/epgsql_wrapper.erl \
        lib/pgsql/src/pgsql_util.erl \
        lib/pgsql/src/pgsql.erl \
        lib/pgsql/src/pgsql_tcp.erl \
        lib/pgsql/src/pgsql_proto.erl

all: build

_build.cfg: _build.cfg.template
	@sed "s/##PROJECT_NAME##/$(PROJECT_NAME)/;\
          s/##PROJECT_REPO_URL##/$(PROJECT_REPO_URL)/;\
          s/##VERSION_NAME##/$(VERSION_NAME)/"\
        _build.cfg.template > _build.cfg

#$(LEAKTRACER_SRC_PATH)/libleaktrace.a:
#	@(cd $(LEAKTRACER_SRC_PATH) && \
#      make\
#	 )

# force new instead of the stdlibc++ memory pool by
# -DGLIBCXX_FORCE_NEW=1
# -DGLIBCPP_FORCE_NEW=1   (g++ <= 3.3)
# -D__USE_MALLOC=1        (g++ <= 3.0)
#$(LEAKTRACER_SRC_PATH)/libLeakTracer.a:
#	@(cd $(LEAKTRACER_SRC_PATH) && \
#      make\
#     )

g++: $(GXX_BIN)
$(GXX_BIN): $(GXX_SRC) \
        lib/g++/gmp-4.2.4.tar.gz \
        lib/g++/mpfr-2.4.1.tar.gz
	@test -d $(GXX_SRC_PATH) || \
        (tar zxvf $(GXX_SRC) -C $(dir $(GXX_SRC_PATH)) && \
         mkdir $(GXX_SRC_PATH)/gmp && \
         tar zxvf lib/g++/gmp-4.2.4.tar.gz --strip-components 1 \
             -C $(GXX_SRC_PATH)/gmp && \
         mkdir $(GXX_SRC_PATH)/mpfr && \
         tar zxvf lib/g++/mpfr-2.4.1.tar.gz --strip-components 1 \
             -C $(GXX_SRC_PATH)/mpfr)
	@(cd $(GXX_SRC_PATH) && \
      ./configure --prefix=`pwd`_install \
--disable-libgloss --disable-newlib --disable-libmudflap --disable-libssp --disable-libobjc --disable-libada --disable-libffi --disable-zlib --disable-qthreads --disable-libjava --enable-languages=c,c++,fortran && \
      make && \
      make install\
     )

asn1c: $(ASN1_BIN)
$(ASN1_BIN): $(ASN1_SRC)
	@test -d $(ASN1_SRC_PATH) || \
        tar zxvf $(ASN1_SRC) -C $(dir $(ASN1_SRC_PATH))
	@(cd $(ASN1_SRC_PATH) && \
	  CXX='$(GXX_BIN)' CC='$(GCC_BIN)' LDFLAGS='$(GXX_LDFLAGS)' \
      ./configure --prefix=`pwd`_install && \
      make && \
      make install\
     )

gmp: $(GMP_LIB_PATH)
$(GMP_LIB_PATH): $(GMP_SRC)
	@test -d $(GMP_SRC_PATH) || \
        tar zxvf $(GMP_SRC) -C $(dir $(GMP_SRC_PATH))
	@(cd $(GMP_SRC_PATH) && \
      CXX='$(GXX_BIN)' CC='$(GCC_BIN)' \
      CC_FOR_BUILD='$(GCC_BIN)' CPP_FOR_BUILD='$(GXX_BIN)' \
      LDFLAGS='$(GXX_LDFLAGS)' \
      ./configure --prefix=`pwd`_install --enable-cxx && \
      make && \
      make check && \
      make install\
     )

ptmalloc: $(PTMALLOC_LIB_PATH)/libptmalloc3.a
$(PTMALLOC_LIB_PATH)/libptmalloc3.a: $(PTMALLOC_SRC)
	@test -d $(PTMALLOC_SRC_PATH) || \
        tar zxvf $(PTMALLOC_SRC) -C $(dir $(PTMALLOC_SRC_PATH))
	@(cd $(PTMALLOC_SRC_PATH) && \
      make CC=$(GCC_BIN) linux-pthread && \
      mkdir lib && \
      cp libptmalloc3.a lib )

# information about the amazing bjam
# http://www.boost.org/boost-build2/doc/html/bbv2/reference/tools.html
# http://www.boost.org/doc/libs/1_39_0/doc/html/bbv2/advanced.html
boost: $(BOOST_LIB_PATH)
boost-trunk:
	svn co http://svn.boost.org/svn/boost/trunk lib/boost/boost_trunk
	(cd lib/boost/boost_trunk; svn info | grep 'Revision' | cut -d' ' -f 2 > \
        ../revision)
	(cd lib/boost; tar cf boost_trunk.tar boost_trunk; gzip boost_trunk.tar)
	rm -rf lib/boost/boost_trunk
$(BOOST_LIB_PATH): $(BOOST_SRC)
	@test -d $(BOOST_SRC_PATH) || \
        tar zxvf $(BOOST_SRC) -C $(dir $(BOOST_SRC_PATH))
	find $(dir $(BOOST_SRC)) -name "boost_1_39_0_*.patch" | xargs -Ifile \
        cp file $(BOOST_SRC_PATH)
	@(cd $(BOOST_SRC_PATH) && \
      ls *.patch | xargs cat | patch -p1 && \
      ./bootstrap.sh --prefix=`pwd`_install --libdir=`pwd`_install/lib && \
	  echo "using gcc : 4.4.0 : $(GXX_BIN) : \
<root>$(GXX_BIN_PATH) \
<compileflags>$(GXX_CXXFLAGS) <linkflags>$(GXX_LDFLAGS) ; " > \
          tools/build/v2/user-config.jam && \
      ./bjam --debug-configuration --without-python --without-mpi \
             toolset=gcc-4.4.0 install\
     )

$(WORK_TYPE1_LIB_PATH)/libcloud_job_tests.so: \
        $(MAIN_INC_PATH)/cloud_work_interface.hpp \
        $(WORK_TYPE1_SRC_PATH)/cloud_job_tests.cpp \
        $(WORK_TYPE1_SRC_PATH)/piqpr8_gmp_verify.cpp \
        $(WORK_TYPE1_SRC_PATH)/piqpr8_gmp_verify.hpp \
        $(WORK_TYPE1_SRC_PATH)/piqpr8_gmp.cpp \
        $(WORK_TYPE1_SRC_PATH)/piqpr8_gmp.hpp
	@$(CXX) $(CXXFLAGS) -rdynamic -I$(MAIN_INC_PATH) \
        -I$(WORKER_INC_PATH) \
        -o $(WORK_TYPE1_OBJ_PATH)/cloud_job_tests.o \
        -c -fPIC $(WORK_TYPE1_SRC_PATH)/cloud_job_tests.cpp
	@$(CXX) $(CXXFLAGS) -rdynamic -I$(GMP_INC_PATH) \
        -o $(WORK_TYPE1_OBJ_PATH)/piqpr8_gmp.o \
        -c -fPIC $(WORK_TYPE1_SRC_PATH)/piqpr8_gmp.cpp
	@$(CXX) $(CXXFLAGS) -rdynamic -I$(GMP_INC_PATH) \
        -o $(WORK_TYPE1_OBJ_PATH)/piqpr8_gmp_verify.o \
        -c -fPIC $(WORK_TYPE1_SRC_PATH)/piqpr8_gmp_verify.cpp
	@$(CXX) -shared -Wl,-export-dynamic \
        -o $(WORK_TYPE1_LIB_PATH)/libcloud_job_tests.so \
        $(WORK_TYPE1_OBJ_PATH)/cloud_job_tests.o \
        $(WORK_TYPE1_OBJ_PATH)/piqpr8_gmp.o \
        $(WORK_TYPE1_OBJ_PATH)/piqpr8_gmp_verify.o \
        -L$(GMP_LIB_PATH) $(LDFLAGS) \
        -Wl,-rpath $(BOOST_LIB_PATH) \
        -Wl,-rpath $(GMP_LIB_PATH) \
        -lboost_thread-gcc44-mt-1_39 \
        -lboost_system-gcc44-mt-1_39 \
		-lgmp

$(WORK_TYPE1_LIB_PATH)/cloud_job_tests.beam: \
        $(MAIN_INC_PATH)/cloud_logger.hrl \
        $(MAIN_INC_PATH)/cloud_work_interface.erl \
        $(WORK_TYPE1_SRC_PATH)/cloud_job_tests.erl
	@$(ERLC_PATH) -o $(WORK_TYPE1_OBJ_PATH) \
        $(MAIN_INC_PATH)/cloud_work_interface.erl
	@$(ERLC_PATH) -o $(WORK_TYPE1_LIB_PATH) \
        -I $(MAIN_INC_PATH) \
        -pz $(WORK_TYPE1_OBJ_PATH) \
        $(WORK_TYPE1_SRC_PATH)/cloud_job_tests.erl

$(WORKER_LIB_PATH)/libWorkerProtocol.a: \
        $(ASN1_BIN) \
        $(WORKER_PROTOCOL_PATH)/worker_protocol.cpp \
        $(WORKER_INC_PATH)/worker_protocol.hpp \
        $(WORKER_PROTOCOL_PATH)/WorkerProtocol.asn1 \
        $(WORKER_INC_PATH)/realloc_ptr.hpp \
        $(WORKER_INC_PATH)/pool_copy_ptr.hpp \
        $(WORKER_INC_PATH)/boost_thread.hpp \
        $(WORKER_PROTOCOL_PATH)/Makefile
	@(cd $(WORKER_PROTOCOL_PATH) && \
      make clean && \
      ../../../$(ASN1_BIN) -Werror WorkerProtocol.asn1 \
                                2>&1 | cat > /dev/null && \
      rm converter-sample.c && \
      head -n -24 Makefile.am.sample           > worker_protocol.mk && \
      echo "CLOUD_INC_PATH=$(MAIN_INC_PATH)"  >> worker_protocol.mk && \
      echo "BOOST_INC_PATH=$(BOOST_INC_PATH)" >> worker_protocol.mk && \
      echo "GXX_CXXFLAGS=$(GXX_CXXFLAGS)"     >> worker_protocol.mk && \
      echo "GXX_LDFLAGS=$(GXX_LDFLAGS)"       >> worker_protocol.mk && \
      echo "GXX_BIN=$(GXX_BIN)"               >> worker_protocol.mk && \
      echo "GCC_BIN=$(GCC_BIN)"               >> worker_protocol.mk && \
      make)

$(MAIN_SRC_PATH)/WorkerProtocol.erl: \
        $(WORKER_PROTOCOL_PATH)/WorkerProtocol.asn1 \
        $(WORKER_PROTOCOL_PATH)/asn2erl.sh
	@(cd $(WORKER_PROTOCOL_PATH) && \
      $(ERLANG_PATH)/bin/escript ./asn2erl.sh WorkerProtocol && \
      rm WorkerProtocol.beam WorkerProtocol.asn1db)
	@mv $(WORKER_PROTOCOL_PATH)/WorkerProtocol.erl $(MAIN_SRC_PATH)
	@mv $(WORKER_PROTOCOL_PATH)/WorkerProtocol.hrl $(MAIN_SRC_PATH)

$(MAIN_BIN_PATH)/cloud_worker_port: \
        $(BOOST_LIB_PATH) \
        $(WORKER_LIB_PATH)/libWorkerProtocol.a \
        lib/gepd/port.cpp \
        lib/gepd/port.h \
        $(GEPD_INC_PATH)/port_main.h \
        $(WORKER_SRC_PATH)/cloud_worker.cpp \
        $(WORKER_INC_PATH)/cloud_worker.hpp \
        $(WORKER_SRC_PATH)/cloud_worker_common.cpp \
        $(WORKER_SRC_PATH)/cloud_worker_common.hpp \
        $(WORKER_SRC_PATH)/node_connections.cpp \
        $(WORKER_SRC_PATH)/node_connections.hpp \
        $(WORKER_SRC_PATH)/worker_controller.cpp \
        $(WORKER_SRC_PATH)/worker_controller.hpp \
        $(WORKER_SRC_PATH)/worker_execution.cpp \
        $(WORKER_SRC_PATH)/timer.cpp \
        $(WORKER_SRC_PATH)/timer.hpp \
        $(WORKER_SRC_PATH)/library.cpp \
        $(WORKER_SRC_PATH)/library.hpp \
        $(WORKER_SRC_PATH)/ei_x_buff_ptr.hpp \
        $(WORKER_SRC_PATH)/ei_x_buff_ptr.cpp \
        $(WORKER_INC_PATH)/realloc_ptr.hpp \
        $(WORKER_SRC_PATH)/copy_ptr.hpp \
        $(WORKER_SRC_PATH)/safe_shared_ptr.hpp \
        $(WORKER_INC_PATH)/pool_copy_ptr.hpp \
        $(WORKER_INC_PATH)/boost_thread.hpp \
        $(MAIN_INC_PATH)/cloud_work_interface_data.hpp \
        $(MAIN_INC_PATH)/cloud_worker_port.hpp
	# creating worker library for port binary function calls
	@$(CXX) $(CXXFLAGS) -I$(GEPD_INC_PATH) \
         -I$(ERL_INTERFACE_INC_PATH) \
         -I$(WORKER_INC_PATH) \
         -I$(MAIN_INC_PATH) \
	     -o $(WORKER_OBJ_PATH)/node_connections.o \
         -c $(WORKER_SRC_PATH)/node_connections.cpp 
	@$(CXX) $(CXXFLAGS) -I$(GEPD_INC_PATH) \
         -I$(ERL_INTERFACE_INC_PATH) \
		 -I$(WORKER_INC_PATH) -I$(WORKER_SRC_PATH) \
         -I$(MAIN_INC_PATH) \
	     -o $(WORKER_OBJ_PATH)/worker_controller.o \
         -c $(WORKER_SRC_PATH)/worker_controller.cpp 
	@$(CXX) $(CXXFLAGS) -I$(GEPD_INC_PATH) \
         -I$(ERL_INTERFACE_INC_PATH) \
		 -I$(WORKER_INC_PATH) -I$(WORKER_SRC_PATH) \
         -I$(MAIN_INC_PATH) \
	     -o $(WORKER_OBJ_PATH)/cloud_worker.o \
         -c $(WORKER_SRC_PATH)/cloud_worker.cpp 
	@$(CXX) $(CXXFLAGS)  \
	     -o $(WORKER_OBJ_PATH)/cloud_worker_common.o \
         -c $(WORKER_SRC_PATH)/cloud_worker_common.cpp 
	@$(CXX) $(CXXFLAGS) -I$(GEPD_INC_PATH) \
		 -I$(WORKER_INC_PATH) -I$(WORKER_SRC_PATH) \
         -I$(MAIN_INC_PATH) \
	     -o $(WORKER_OBJ_PATH)/worker_execution.o \
         -c $(WORKER_SRC_PATH)/worker_execution.cpp 
	@$(CXX) $(CXXFLAGS) \
	     -o $(WORKER_OBJ_PATH)/timer.o \
         -c $(WORKER_SRC_PATH)/timer.cpp 
	@$(CXX) $(CXXFLAGS) \
	     -o $(WORKER_OBJ_PATH)/library.o \
         -c $(WORKER_SRC_PATH)/library.cpp 
	@$(CXX) $(CXXFLAGS) \
         -I$(ERL_INTERFACE_INC_PATH) \
	     -o $(WORKER_OBJ_PATH)/ei_x_buff_ptr.o \
         -c $(WORKER_SRC_PATH)/ei_x_buff_ptr.cpp 
	# creating cpp port executable for erlang code to run
	@$(CXX) $(CXXFLAGS) -include $(MAIN_INC_PATH)/cloud_worker_port.hpp \
         -I$(ERL_INTERFACE_INC_PATH) \
         -I$(WORKER_INC_PATH) \
         -o $(WORKER_OBJ_PATH)/port.o -c lib/gepd/port.cpp 
	@$(CXX) -o $(MAIN_BIN_PATH)/cloud_worker_port \
         $(WORKER_OBJ_PATH)/port.o \
         $(WORKER_OBJ_PATH)/node_connections.o \
         $(WORKER_OBJ_PATH)/worker_controller.o \
         $(WORKER_OBJ_PATH)/worker_execution.o \
         $(WORKER_OBJ_PATH)/cloud_worker.o \
         $(WORKER_OBJ_PATH)/cloud_worker_common.o \
         $(WORKER_OBJ_PATH)/timer.o \
         $(WORKER_OBJ_PATH)/library.o \
         $(WORKER_OBJ_PATH)/ei_x_buff_ptr.o \
         $(LDFLAGS) \
         -Wl,-rpath $(BOOST_LIB_PATH) \
         -Wl,-Bdynamic \
         -lboost_thread-gcc44-mt-1_39  \
         -lboost_date_time-gcc44-mt-1_39 \
         -lboost_filesystem-gcc44-mt-1_39 \
         -lboost_system-gcc44-mt-1_39 \
         -Wl,-Bstatic \
         -L$(WORKER_LIB_PATH) -lWorkerProtocol \
         -L$(ERL_INTERFACE_LIB_PATH) -lerl_interface_st -lei_st \
         -Wl,-Bdynamic \
         -ldl -lrt \
         -lstdc++ \
         -Wl,-Bstatic \
         -L$(PTMALLOC_LIB_PATH) -lptmalloc3 \
         -Wl,-Bdynamic \
         -lc -lpthread

	@$(RM) $(WORKER_OBJ_PATH)/*.o

$(MAIN_SRC_PATH)/cloud_worker_port.hrl: \
        $(GEPD_INC_PATH)/erlang_functions_hrl.h \
        $(MAIN_INC_PATH)/cloud_worker_port.hpp
	# generating erlang header file that defines erlang function bindings
	@$(CXX) $(CXXFLAGS) -include $(MAIN_INC_PATH)/cloud_worker_port.hpp \
         -E -P lib/gepd/erlang_functions_hrl.h > \
         $(MAIN_SRC_PATH)/cloud_worker_port.hrl

build: \
        g++ boost ptmalloc asn1c gmp \
        _build.cfg \
        $(MAIN_BIN_PATH)/cloud_worker_port \
        $(MAIN_SRC_PATH)/cloud_worker_port.hrl \
        $(MAIN_SRC_PATH)/WorkerProtocol.erl \
        $(WORK_TYPE1_LIB_PATH)/libcloud_job_tests.so \
        $(WORK_TYPE1_LIB_PATH)/cloud_job_tests.beam \
        $(ERLANG_SRC_FILES)
	@$(SINAN_PATH) build

_build/development/tar/$(PROJECT_NAME).tar.gz: build
	@cp $(MAIN_BIN_PATH)/cloud_worker_port $(CLOUD_BIN_PATH) || exit 0
	@test -d $(CLOUD_BIN_PATH)/work_types && \
        $(RM) -rf $(CLOUD_BIN_PATH)/work_types || exit 0
	@mkdir $(CLOUD_BIN_PATH)/work_types
	@cp $(WORK_TYPE1_LIB_PATH)/libcloud_job_tests.so \
        $(WORK_TYPE1_LIB_PATH)/cloud_job_tests.beam \
        $(CLOUD_BIN_PATH)/work_types
	@$(SINAN_PATH) release
	
release: _build/development/tar/$(PROJECT_NAME).tar.gz
	@test -d _release && $(RM) -rf _release || exit 0
	@mkdir _release
	@tar zxvf _build/development/tar/$(PROJECT_NAME).tar.gz -C _release
	@cut -d: -f2 $(FAILOVER_HOSTS_FILE) | \
        xargs -I{} $(EPMD_PATH) -daemon -port {}

run: rsync release
	#@test "host1" = `hostname` && \
    #    ssh host2 'cd $(ROOT_PATH); make release' || exit 0
	@$(RM) logs/*.log
	bin/execute_erl 0 $(ERL_PATH) \
                -boot _release/releases/$(VERSION_NAME)/start \
                -config _release/releases/$(VERSION_NAME)/sys \
                $(ERLANG_ARGUMENTS) \
                `find _release/lib/ \
                 -name "ebin" -o -name "priv" -o -name "work_types" | \
                 sed 's/^/-pz /'`

rsync:
	#@test "host1" = `hostname` && \
    #    rsync -av --rsh=ssh --exclude-from=config/rsync_exclude_list.conf \
	#	    . host2:$(ROOT_PATH) || exit 0

rsync-clean_TARGETS:
	#@test "host1" = `hostname` && \
    #    ssh host2 'cd $(ROOT_PATH) && rm -rf *' || exit 0

rsync-clean: rsync-clean_TARGETS rsync

shell: build
	bin/execute_erl $(ERL_PATH) \
                -sname shell \
                `find _build/development/apps \
                 -name "ebin" -o -name "priv" -o -name "work_types" | \
                 sed 's/^/-pz /'`

clean: _build.cfg
	#@test "host1" = `hostname` && \
    #    ssh host2 'cd $(ROOT_PATH) && make clean' || exit 0
	@(cd $(WORKER_PROTOCOL_PATH); make clean)
	@$(RM) $(WORKER_LIB_PATH)/libWorkerProtocol.a \
           $(MAIN_SRC_PATH)/WorkerProtocol.erl \
           $(MAIN_SRC_PATH)/WorkerProtocol.hrl
	@$(RM) $(WORKER_OBJ_PATH)/*.o \
           $(MAIN_BIN_PATH)/cloud_worker_port \
           $(MAIN_SRC_PATH)/cloud_worker_port.hrl \
           $(WORK_TYPE1_OBJ_PATH)/*.o \
           $(WORK_TYPE1_OBJ_PATH)/cloud_work_interface.beam \
           $(WORK_TYPE1_LIB_PATH)/libcloud_job_tests.so \
           $(WORK_TYPE1_LIB_PATH)/cloud_job_tests.beam
	@$(RM) -r _build/* _release
	@$(SINAN_PATH) clean

test: _build.cfg
	@$(SINAN_PATH) test

$(DIALYZER_PLT): $(DIALYZER_PATH)
	$(DIALYZER_PATH) --build_plt --output_plt $(DIALYZER_PLT) -r \
        $(ERLANG_PATH)/lib/erlang/lib/erts-5.7.2/ebin \
        $(ERLANG_PATH)/lib/erlang/lib/kernel-2.13.2/ebin \
        $(ERLANG_PATH)/lib/erlang/lib/stdlib-1.16.2/ebin \
        $(ERLANG_PATH)/lib/erlang/lib/mnesia-4.4.10/ebin \
        $(ERLANG_PATH)/lib/erlang/lib/asn1-1.6.10/ebin \
        $(ERLANG_PATH)/lib/erlang/lib/compiler-4.6.2/ebin \
        $(ERLANG_PATH)/lib/erlang/lib/crypto-1.6/ebin
analyze-prepare: $(DIALYZER_PLT)
analyze-add: $(DIALYZER_PLT)
	@echo "$(DIALYZER_PATH) --add_to_plt --plt $(DIALYZER_PLT) -r PATH"
analyze: $(DIALYZER_PLT) \
        $(MAIN_SRC_PATH)/cloud_worker_port.hrl \
        $(MAIN_SRC_PATH)/WorkerProtocol.erl
	$(DIALYZER_PATH) --plt $(DIALYZER_PLT) --src \
        -Ilib/rb/include \
        -I$(MAIN_INC_PATH) \
        -I$(MAIN_SRC_PATH) -c \
        $(MAIN_SRC_PATH)/*.erl \
        $(WORK_TYPE1_SRC_PATH)/*.erl \
        $(MAIN_INC_PATH)/cloud_data_interface.erl \
        lib/cloud_data_pgsql/src/cloud_data_pgsql.erl \
        lib/rb/src/rbdict.erl \
        lib/rb/src/rbsets.erl
doc: _build.cfg
	@$(SINAN_PATH) doc
	@echo "DOCUMENTATION IS NOW IN _build/development/apps/$(PROJECT_NAME)-$(MAJOR_VERSION_NAME)/docs/"

