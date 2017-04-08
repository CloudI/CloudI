#ifndef CLOUDI_OS_SPAWN_H
#define CLOUDI_OS_SPAWN_H

#if ! defined(CURRENT_VERSION)
#error CURRENT_VERSION is required as part of the port/port_driver name \
       for code upgrades/downgrades
#endif

//////////////////////////////////////////////////////////////////////////////
// Port Declaration
//////////////////////////////////////////////////////////////////////////////

// specify the name of the port, as provided to port initialization
// (e.g., erlang:open_port/2, executable name)
#define PORT_NAME_PREFIX cloudi_os_spawn

// specify the C or C++ include file with the functions that will be called
// from within the Erlang code
#define PORT_CXX_FUNCTIONS_HEADER_FILE "cloudi_os_spawn.hpp"

// specify all the functions to generate bindings for
//  __________________________________________________________________________
//  || FUNCTION     || ARITY/TYPES                           || RETURN TYPE ||
#define PORT_FUNCTIONS \
    ((spawn,           14, (char, pchar_len, puint32_len, pchar_len,         \
                            uint64_t, pchar_len, uint64_t, pchar_len,        \
                            int32_t, pchar_len, pchar_len,                   \
                            pchar_len, pchar_len, pchar_len),       int32_t ))

//////////////////////////////////////////////////////////////////////////////

#include <boost/preprocessor/cat.hpp>

#if defined(PORT_DRIVER_NAME_PREFIX)
#define PORT_DRIVER_NAME \
    BOOST_PP_CAT(BOOST_PP_CAT(PORT_DRIVER_NAME_PREFIX, _), CURRENT_VERSION)
#endif
#if defined(PORT_NAME_PREFIX)
#define PORT_NAME \
    BOOST_PP_CAT(BOOST_PP_CAT(PORT_NAME_PREFIX, _), CURRENT_VERSION)
#endif

#endif // CLOUDI_OS_SPAWN_H
