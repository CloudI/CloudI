# Cloudi build configuration

# project variables
PROJECT_NAME=cloud
VERSION_NAME=0.0.8.0
PROJECT_REPO_URL=http:\/\/host1:3001\/repo\/
MAJOR_VERSION_NAME=0.0.8
# erlang variables
ERLANG_ARGUMENTS=+A 16 +K true
# use instrument module
#ERLANG_ARGUMENTS+=+Mim true +Mis true
# use erts_alloc_config module
#ERLANG_ARGUMENTS+=+Mea config
# erts_alloc_config suggested:
#ERLANG_ARGUMENTS+=$(shell grep '^ ' erts_alloc_config.log)

ERLANG_PATH=/home/user/installed
# R13B01 values:
ERTS_VERSION=5.7.2
ERL_INTERFACE_VERSION=3.6.2
# R13B02 values:
#ERTS_VERSION=5.7.3
#ERL_INTERFACE_VERSION=3.6.3
# erlware variables
ERLWARE_PATH=/home/user/installed/erlware

DEBUG_BUILD=true

