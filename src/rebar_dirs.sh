#!/bin/sh

# rebar is unable to separate the source directory path from
# the build directory path, so it is necessary to provide a directory
# structure that facilitates the autoconf/automake build directories when
# using rebar to build the source directories

REBAR_DIRS="\
    lib/cloudi_core \
    lib/cloudi_service_api_batch \
    lib/cloudi_service_api_requests \
    lib/cloudi_service_cron \
    lib/cloudi_service_db_mysql \
    lib/cloudi_service_db_pgsql \
    lib/cloudi_service_filesystem \
    lib/cloudi_service_funnel \
    lib/cloudi_service_health_check \
    lib/cloudi_service_http_client \
    lib/cloudi_service_http_cowboy \
    lib/cloudi_service_http_cowboy1 \
    lib/cloudi_service_http_elli \
    lib/cloudi_service_http_rest \
    lib/cloudi_service_map_reduce \
    lib/cloudi_service_monitoring \
    lib/cloudi_service_null \
    lib/cloudi_service_oauth1 \
    lib/cloudi_service_queue \
    lib/cloudi_service_quorum \
    lib/cloudi_service_request_rate \
    lib/cloudi_service_router \
    lib/cloudi_service_send \
    lib/cloudi_service_shell \
    lib/cloudi_service_tcp \
    lib/cloudi_service_udp \
    lib/cloudi_service_validate \
    external/cloudi_x_bear \
    external/cloudi_x_certifi \
    external/cloudi_x_cowboy \
    external/cloudi_x_cowboy1 \
    external/cloudi_x_cowlib \
    external/cloudi_x_cowlib1 \
    external/cloudi_x_elli \
    external/cloudi_x_emysql \
    external/cloudi_x_epgsql \
    external/cloudi_x_exometer \
    external/cloudi_x_exometer_core \
    external/cloudi_x_exometer_influxdb \
    external/cloudi_x_folsom \
    external/cloudi_x_hackney \
    external/cloudi_x_hut \
    external/cloudi_x_idna \
    external/cloudi_x_jsx \
    external/cloudi_x_metrics \
    external/cloudi_x_mimerl \
    external/cloudi_x_msgpack \
    external/cloudi_x_nodefinder \
    external/cloudi_x_parse_trans \
    external/cloudi_x_pgsql \
    external/cloudi_x_ranch \
    external/cloudi_x_ranch1 \
    external/cloudi_x_setup \
    external/cloudi_x_ssl_verify_fun \
    external/cloudi_x_unicode_util_compat \
    external/proper \
    lib/cgroups \
    lib/cpg \
    lib/erlang_term \
    lib/key2value \
    lib/keys1value \
    lib/pqueue \
    lib/quickrand \
    lib/reltool_util \
    lib/supool \
    lib/syslog_socket \
    lib/trie \
    lib/uuid \
    lib/varpool \
"

if [ $# -ne 3 ]; then
    echo "$0 create|destroy abs_top_srcdir abs_top_builddir"
    exit 1
fi
COMMAND=$1
abs_top_srcdir=$2
abs_top_builddir=$3

case $COMMAND in
    create)
        for d in $REBAR_DIRS; do
            application=`basename $d`
            if [ ! -d $abs_top_builddir/$d/ebin ]; then
                mkdir -p $abs_top_builddir/$d/ebin
            fi
            if [ ! -e $abs_top_srcdir/$d/ebin ]; then
                ln -s $abs_top_builddir/$d/ebin \
                      $abs_top_srcdir/$d/ebin
            fi
            if [ -d $abs_top_srcdir/$d/include -a \
                 ! -e $abs_top_builddir/$d/include ]; then
                ln -s $abs_top_srcdir/$d/include \
                      $abs_top_builddir/$d/include
            fi
            if [ -f $abs_top_builddir/$d/rebar.config -a \
                 ! -e $abs_top_srcdir/$d/rebar.config ]; then
                ln -s $abs_top_builddir/$d/rebar.config \
                      $abs_top_srcdir/$d/rebar.config
            fi
            if [ -f $abs_top_builddir/$d/src/$application.app.src -a \
                 ! -e $abs_top_srcdir/$d/src/$application.app.src ]; then
                ln -s $abs_top_builddir/$d/src/$application.app.src \
                      $abs_top_srcdir/$d/src/$application.app.src
            fi
        done
        ;;
    process)
        for d in $REBAR_DIRS; do
            application=`basename $d`
            # rebar2 creates _drv.so files for no reason
            rm -f $abs_top_builddir/$d/priv/${application}_drv.so \
                  $abs_top_builddir/$d/priv/cloudi_x_${application}_drv.so
        done
        ;;
    destroy)
        for d in $REBAR_DIRS; do
            application=`basename $d`
            if [ -d $abs_top_builddir/$d/ebin ]; then
                rm -f $abs_top_builddir/$d/ebin/*
                rmdir $abs_top_builddir/$d/ebin
            fi
            if [ -h $abs_top_srcdir/$d/ebin ]; then
                rm -f $abs_top_srcdir/$d/ebin
            fi
            if [ -h $abs_top_builddir/$d/include ]; then
                rm -f $abs_top_builddir/$d/include
            fi
            if [ -h $abs_top_srcdir/$d/rebar.config ]; then
                rm -f $abs_top_srcdir/$d/rebar.config
            fi
            if [ -h $abs_top_srcdir/$d/src/$application.app.src ]; then
                rm -f $abs_top_srcdir/$d/src/$application.app.src
            fi
        done
        ;;
    *)
        echo "command invalid: $COMMAND"
        exit 1
        ;;
esac

