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

#ifndef __DTRACE_WRAPPER_H
#define __DTRACE_WRAPPER_H

#ifdef  HAVE_DTRACE

#include "dtrace-probes.h"

#define DTRACE_ENABLED(name)                         \
    erlang_vm_##name##_enabled()
#define DTRACE0(name)                                \
    erlang_vm_##name()
#define DTRACE1(name, a0)                            \
    erlang_vm_##name(a0)
#define DTRACE2(name, a0, a1)                        \
    erlang_vm_##name((a0), (a1))
#define DTRACE3(name, a0, a1, a2)                    \
    erlang_vm_##name((a0), (a1), (a2))
#define DTRACE4(name, a0, a1, a2, a3)                \
    erlang_vm_##name((a0), (a1), (a2), (a3))
#define DTRACE5(name, a0, a1, a2, a3, a4)            \
    erlang_vm_##name((a0), (a1), (a2), (a3), (a4))
#define DTRACE6(name, a0, a1, a2, a3, a4, a5)        \
    erlang_vm_##name((a0), (a1), (a2), (a3), (a4), (a5))
#define DTRACE10(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
    erlang_vm_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9))

#else   /* HAVE_DTRACE */

/* Render all macros to do nothing */
#define DTRACE_ENABLED(name)                         0
#define DTRACE0(name)                                do {} while (0)
#define DTRACE1(name, a0)                            do {} while (0)
#define DTRACE2(name, a0, a1)                        do {} while (0)
#define DTRACE3(name, a0, a1, a2)                    do {} while (0)
#define DTRACE4(name, a0, a1, a2, a3)                do {} while (0)
#define DTRACE5(name, a0, a1, a2, a3, a4)            do {} while (0)
#define DTRACE6(name, a0, a1, a2, a3, a4, a5)        do {} while (0)
#define DTRACE10(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
                                                     do {} while (0)
#endif  /* HAVE_DTRACE */

#endif  /* __DTRACE_WRAPPER_H */
