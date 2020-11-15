#ifndef CLOUDI_OS_SPAWN_H
#define CLOUDI_OS_SPAWN_H

//////////////////////////////////////////////////////////////////////////////
// Port Declaration
//////////////////////////////////////////////////////////////////////////////

// specify the name of the port, as provided to port initialization
// (e.g., erlang:open_port/2, executable name)
#define PORT_NAME cloudi_os_spawn

// specify the C or C++ include file with the functions that will be called
// from within the Erlang code
#define PORT_CXX_FUNCTIONS_HEADER_FILE "cloudi_os_spawn.hpp"

// specify all the functions to generate bindings for
//  ________________________________________________________________________
//  || FUNCTION     || ARITY/TYPES                         || RETURN TYPE ||
#define PORT_FUNCTIONS                                                       \
    ((terminate_now,   0, (),                                 bool        )) \
    ((spawn,           15, (char, pchar_len, puint32_len, pchar_len,         \
                            uint64_t, pchar_len, uint64_t, pchar_len,        \
                            int32_t, pchar_len, pchar_len, pchar_len,        \
                            pchar_len, pchar_len, pchar_len), int32_t     ))

//////////////////////////////////////////////////////////////////////////////

#endif // CLOUDI_OS_SPAWN_H
