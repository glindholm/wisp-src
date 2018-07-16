// Copyright (c) Lexical Software, 1992.  All rights reserved.
//
// Module : network.hpp
// Author : George Soules
// Date   : 26 July 1992

#if NETWORK

#ifndef NETWORK__HPP
#define NETWORK__HPP

// Classes
#include "machine.hpp"

// Definitions and subprograms
#include "environ.hpp"


char    *nw_network_address();
Boolean  nw_network_available();
char    *nw_user_full_name();
Boolean  nw_user_logged_in();
char    *nw_user_login_name();

#endif
#endif

