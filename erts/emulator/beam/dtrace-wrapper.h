
#ifndef __DTRACE_WRAPPER_H
#define __DTRACE_WRAPPER_H

#ifdef  HAVE_DTRACE

#include "dtrace-probes.h"

#define DTRACE_ENABLED(func, name)                         \
    erlang_vm_##func##_##name##_enabled()
#define DTRACE0(func, name)                                \
    erlang_vm_##func##_##name()
#define DTRACE1(func, name, a0)                            \
    erlang_vm_##func##_##name(a0)
#define DTRACE2(func, name, a0, a1)                        \
    erlang_vm_##func##_##name((a0), (a1))
#define DTRACE3(func, name, a0, a1, a2)                    \
    erlang_vm_##func##_##name((a0), (a1), (a2))
#define DTRACE4(func, name, a0, a1, a2, a3)                \
    erlang_vm_##func##_##name((a0), (a1), (a2), (a3))
#define DTRACE5(func, name, a0, a1, a2, a3, a4)            \
    erlang_vm_##func##_##name((a0), (a1), (a2), (a3), (a4))
#define DTRACE6(func, name, a0, a1, a2, a3, a4, a5)        \
    erlang_vm_##func##_##name((a0), (a1), (a2), (a3), (a4), (a5))

#else   /* HAVE_DTRACE */

/* Render all macros to do nothing */
#define DTRACE_ENABLED(func, name)                         0
#define DTRACE0(func, name)                                do {} while (0)
#define DTRACE1(func, name, a0)                            do {} while (0)
#define DTRACE2(func, name, a0, a1)                        do {} while (0)
#define DTRACE3(func, name, a0, a1, a2)                    do {} while (0)
#define DTRACE4(func, name, a0, a1, a2, a3)                do {} while (0)
#define DTRACE5(func, name, a0, a1, a2, a3, a4)            do {} while (0)
#define DTRACE6(func, name, a0, a1, a2, a3, a4, a5)        do {} while (0)

#endif  /* HAVE_DTRACE */

#endif  /* __DTRACE_WRAPPER_H */
