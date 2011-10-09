/* 
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Purpose:  Dynamically loadable NIF library for DTrace
 */


#include "erl_nif.h"
#include "config.h"
#include "sys.h"
#define DTRACE_DRIVER_SKIP_FUNC_DECLARATIONS
#include "dtrace-wrapper.h"
#include "dtrace_user.h"

#ifdef VALGRIND
    #  include <valgrind/memcheck.h>
#endif

#ifdef __GNUC__
    #  define INLINE __inline__
#else
    #  define INLINE
#endif

#define MESSAGE_BUFSIZ 1024

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

/* The NIFs: */
static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"available", 0, available},
    {"user_trace", 1, user_trace}
};

ERL_NIF_INIT(dtrace, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_not_available;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_true = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_error = enif_make_atom(env,"error");
    atom_not_available = enif_make_atom(env,"not_available");
    atom_badarg = enif_make_atom(env,"badarg");
    atom_ok = enif_make_atom(env,"ok");

    return 0;
}

static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_DTRACE
    return atom_true;
#else
    return atom_false;
#endif
}

static ERL_NIF_TERM user_trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_DTRACE
    ErlNifBinary message_bin;
    char messagebuf[MESSAGE_BUFSIZ + 1];

    if (DTRACE_ENABLED(user_trace)) {
	if (!enif_inspect_iolist_as_binary(env, argv[0], &message_bin) ||
	    message_bin.size > MESSAGE_BUFSIZ) {
	    return atom_badarg;
	}
	strcpy(messagebuf, (char *) message_bin.data);
	DTRACE1(user_trace, messagebuf);
	return atom_true;
    } else {
	return atom_false;
    }
#else
    return atom_error;
#endif
}

