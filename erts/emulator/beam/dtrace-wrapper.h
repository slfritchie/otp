/*
 * %CopyrightBegin%
 *
 * Copyright Dustin Sallings, Michal Ptaszek, Scott Lystig Fritchie 2011.
 * All Rights Reserved.
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

#define DTRACE_TERM_BUF_SIZE 256

#ifndef	DTRACE_DRIVER_SKIP_FUNC_DECLARATIONS
inline void dtrace_proc_str(Process *process, char *process_buf);
inline void dtrace_pid_str(Eterm pid, char *process_buf);
inline void dtrace_port_str(Port *port, char *port_buf);
inline void dtrace_fun_decode(Process *process,
			      Eterm module, Eterm function, int arity,
			      char *process_buf, char *mfa_buf);
#endif

#ifdef  HAVE_DTRACE

#include "erlang_dtrace.h"

#define DTRACE_ENABLED(name)                         \
    erlang_##name##_enabled()
#define DTRACE0(name)                                \
    erlang_##name()
#define DTRACE1(name, a0)                            \
    erlang_##name(a0)
#define DTRACE2(name, a0, a1)                        \
    erlang_##name((a0), (a1))
#define DTRACE3(name, a0, a1, a2)                    \
    erlang_##name((a0), (a1), (a2))
#define DTRACE4(name, a0, a1, a2, a3)                \
    erlang_##name((a0), (a1), (a2), (a3))
#define DTRACE5(name, a0, a1, a2, a3, a4)            \
    erlang_##name((a0), (a1), (a2), (a3), (a4))
#define DTRACE6(name, a0, a1, a2, a3, a4, a5)        \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5))
#define DTRACE7(name, a0, a1, a2, a3, a4, a5, a6)    \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6))
#define DTRACE10(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9))
#define DTRACE11(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9), (a10))

#if defined(_SDT_PROBE) && !defined(STAP_PROBE11)
/* SLF: This is Ubuntu 11-style SystemTap hackery */
/* work arround for missing STAP macro */
#define STAP_PROBE11(provider,name,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) \
  _SDT_PROBE(provider, name, 11, \
             (arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11))
#define _SDT_ASM_OPERANDS_11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) \
  _SDT_ASM_OPERANDS_10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,arg10), \
    _SDT_ARG(11, arg11)
#endif

#ifdef STAP_PROBE_ADDR
/* SLF: This is CentOS 5-style SystemTap hackery */
#if ! defined EXPERIMENTAL_KPROBE_SDT
#define STAP_PROBE11(provider,probe,parm1,parm2,parm3,parm4,parm5,parm6,parm7,parm8,parm9,parm10,parm11) \
  do { STAP_SDT_VOLATILE STAP_TYPE(parm1) arg1 = parm1;                 \
  STAP_SDT_VOLATILE STAP_TYPE(parm2) arg2 = parm2;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm3) arg3 = parm3;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm4) arg4 = parm4;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm5) arg5 = parm5;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm6) arg6 = parm6;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm7) arg7 = parm7;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm8) arg8 = parm8;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm9) arg9 = parm9;                      \
  STAP_SDT_VOLATILE STAP_TYPE(parm10) arg10 = parm10;                   \
  STAP_SDT_VOLATILE STAP_TYPE(parm11) arg11 = parm11;                   \
  STAP_PROBE_POINT(provider,probe, 11, "%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10", arg11); \
  } while (0)
#else /* ! defined EXPERIMENTAL_KPROBE_SDT */
#define STAP_PROBE11(provider,probe,parm1,parm2,parm3,parm4,parm5,parm6,parm7,parm8,parm9,parm10,parm11) \
do { __extension__ struct {size_t arg1 __attribute__((aligned(8)));     \
          size_t arg2 __attribute__((aligned(8)));                      \
          size_t arg3 __attribute__((aligned(8)));                      \
          size_t arg4 __attribute__((aligned(8)));                      \
          size_t arg5 __attribute__((aligned(8)));                      \
          size_t arg6 __attribute__((aligned(8)));                      \
          size_t arg7 __attribute__((aligned(8)));                      \
          size_t arg8 __attribute__((aligned(8)));                      \
          size_t arg9 __attribute__((aligned(8)));                      \
          size_t arg10 __attribute__((aligned(8)));                     \
          size_t arg11 __attribute__((aligned(8)));}                    \
  stap_probe11_args = {(size_t)parm1, (size_t)parm2, (size_t)parm3, (size_t)parm4, \
        (size_t)parm5, (size_t)parm6, (size_t)parm7, (size_t)parm8, (size_t)parm9, (size_t)parm10, (size_t)parm11}; \
  STAP_PROBE_DATA(provider,probe,STAP_GUARD,11);                        \
  syscall (STAP_SYSCALL, #probe, STAP_GUARD, &stap_probe11_args);       \
  } while (0)
#endif /* ! defined EXPERIMENTAL_KPROBE_SDT */
#endif /* STAP_PROBE_ADDR */

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
#define DTRACE7(name, a0, a1, a2, a3, a4, a5, a6)    do {} while (0)
#define DTRACE10(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
                                                     do {} while (0)
#define DTRACE11(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
                                                     do {} while (0)

#endif  /* HAVE_DTRACE */

#endif  /* __DTRACE_WRAPPER_H */
