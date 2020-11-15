#ifndef CLOUDI_OS_COMMAND_H
#define CLOUDI_OS_COMMAND_H

//////////////////////////////////////////////////////////////////////////////
// Port Declaration
//////////////////////////////////////////////////////////////////////////////

// specify the name of the port, as provided to port initialization
// (e.g., erlang:open_port/2, executable name)
#define PORT_NAME cloudi_os_command

// specify the C or C++ include file with the functions that will be called
// from within the Erlang code
#define PORT_CXX_FUNCTIONS_HEADER_FILE "cloudi_os_command.hpp"

// specify all the functions to generate bindings for
//  ________________________________________________________________________
//  || FUNCTION     || ARITY/TYPES                         || RETURN TYPE ||
#define PORT_FUNCTIONS                                                       \
    ((terminate_now,   0, (),                                 bool        )) \
    ((kill_pids,       3, (uint32_t, bool, puint32_len),      pchar_nofree))

//////////////////////////////////////////////////////////////////////////////

#endif // CLOUDI_OS_COMMAND_H
